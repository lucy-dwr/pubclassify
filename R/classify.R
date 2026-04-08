#' Classify publications into a scholarly field taxonomy
#'
#' Uses a large language model (via `ellmer`) to assign each publication in
#' `pubs` to a field in `taxonomy`. Classification text is built as
#' `title + abstract` when an abstract is present, or `title` alone when it
#' is not (see [pc_fetch_abstracts()] to backfill missing abstracts first).
#'
#' By default (`use_embeddings = FALSE`), the full taxonomy is included in
#' every LLM prompt. For taxonomies with 40 or more fields, set
#' `use_embeddings = TRUE` to first narrow candidates via embedding similarity,
#' passing only the top `top_k` fields to the LLM per publication.
#'
#' @param pubs A [tibble::tibble()] returned by [pc_search()] or
#'   [pc_combine()], containing at minimum `title` and `abstract` columns.
#' @param taxonomy A [pc_taxonomy()] object defining the fields to classify
#'   into.
#' @param provider Character. LLM provider passed to `ellmer`. One of
#'   `"anthropic"`, `"openai"`, or `"openai-compatible"`. Use
#'   `"openai-compatible"` for any OpenAI-compatible gateway (e.g. a
#'   multi-model proxy) that implements the `/chat/completions` endpoint but
#'   not the newer OpenAI Responses API. Falls back to the value set with
#'   [pc_configure()], then `"anthropic"`.
#' @param model Character. Model identifier. If `NULL`, uses the provider
#'   default as determined by `ellmer`.
#' @param api_key Character. API key for the LLM provider. Falls back to the
#'   value set with [pc_configure()], then the `PUBCLASSIFY_LLM_KEY`
#'   environment variable, then the provider's own default variable
#'   (e.g. `OPENAI_API_KEY`).
#' @param base_url Character. Base URL for an OpenAI-compatible API endpoint.
#'   Only used when `provider = "openai"`. Falls back to the value set with
#'   [pc_configure()], then the `PUBCLASSIFY_LLM_BASE_URL` environment
#'   variable. If still `NULL`, the standard OpenAI endpoint is used.
#' @param system_prompt Character. Replaces the default system message sent to
#'   the LLM. The default instructs the model to act as a scientific literature
#'   classifier and return structured JSON. Override this to change the model's
#'   persona or task framing. The output format instruction is always appended
#'   after this value.
#' @param classify_instructions Character. Optional additional instructions
#'   injected into the user message after the taxonomy definitions and before
#'   the publications. Use this for domain-specific guidance such as priority
#'   rules, tie-break logic, or category-specific signals.
#' @param use_embeddings Logical. If `TRUE`, retrieves `top_k` candidate
#'   fields via embedding similarity before the LLM call. Requires
#'   `embed_provider` and a valid API key. Default `FALSE`.
#' @param embed_provider Character. Embedding provider
#'   (e.g. `"openai"`, `"cohere"`). Required when `use_embeddings = TRUE`.
#' @param embed_key Character. Embedding API key. Falls back to the
#'   provider-specific environment variable when `NULL`.
#' @param top_k Integer. Number of candidate taxonomy fields passed to the LLM
#'   per publication when `use_embeddings = TRUE`. Default `5`.
#' @param batch_size Integer. Number of publications per LLM call. Batching
#'   reduces the number of API round-trips for large result sets. Default `10`.
#' @param ... Additional arguments passed to the `ellmer` chat constructor
#'   (e.g. `seed`).
#'
#' @return The input tibble with four additional columns:
#'   \describe{
#'     \item{pc_field}{Assigned taxonomy field name (character)}
#'     \item{pc_rationale}{Brief LLM reasoning for the assignment (character)}
#'     \item{pc_text_source}{Text used for classification:
#'           `"title+abstract"` or `"title"` (character)}
#'     \item{pc_classified_by}{Method used: `"llm-full"` (full taxonomy) or
#'           `"llm-rag"` (embedding-narrowed) (character)}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' tax  <- pc_taxonomy_example()
#' pubs <- pc_search("University of Edinburgh", field = "affiliation",
#'                   sources = "openalex")
#' pubs <- pc_classify(pubs, tax, provider = "openai", model = "gpt-4o")
#' }
pc_classify <- function(
    pubs,
    taxonomy,
    provider              = NULL,
    model                 = NULL,
    api_key               = NULL,
    base_url              = NULL,
    system_prompt         = NULL,
    classify_instructions = NULL,
    use_embeddings        = FALSE,
    embed_provider        = NULL,
    embed_key             = NULL,
    top_k                 = 5L,
    batch_size            = 10L,
    ...
) {
  if (!inherits(taxonomy, "pc_taxonomy")) {
    cli::cli_abort(
      "{.arg taxonomy} must be a {.cls pc_taxonomy} object. See {.fn pc_taxonomy}."
    )
  }

  if (!use_embeddings && nrow(taxonomy) >= 40L) {
    cli::cli_warn(c(
      "Your taxonomy has {nrow(taxonomy)} fields.",
      "i" = "With {.code use_embeddings = FALSE}, the full taxonomy is \\
             included in every LLM call. Consider \\
             {.code use_embeddings = TRUE} to reduce cost and improve focus."
    ))
  }

  # Credential lookup: argument -> .pc_env -> environment variable -> default
  provider <- provider %||% .pc_env$llm_provider %||% "anthropic"

  api_key <- api_key %||% .pc_env$llm_key %||% {
    val <- Sys.getenv("PUBCLASSIFY_LLM_KEY", unset = NA_character_)
    if (is.na(val)) NULL else val
  }

  base_url <- base_url %||% .pc_env$llm_base_url %||% {
    val <- Sys.getenv("PUBCLASSIFY_LLM_BASE_URL", unset = NA_character_)
    if (is.na(val)) NULL else val
  }

  # Build the system message
  sys_msg <- system_prompt %||% paste(
    "You are a scientific literature classifier.",
    "You will be given a list of peer-reviewed publications (titles and/or",
    "abstracts) and a taxonomy of research categories.",
    "For each publication, assign exactly one category based on its central",
    "scientific objective - not the data type or methods used.",
    "Respond only with the structured output requested.",
    "Do not add commentary outside the JSON."
  )

  if (use_embeddings) {
    if (is.null(embed_provider)) {
      cli::cli_abort(
        "{.arg embed_provider} is required when {.code use_embeddings = TRUE}."
      )
    }
    embed_key     <- embed_key %||% .pc_env$embed_key
    taxonomy_embs <- .pc_build_taxonomy_embeddings(taxonomy, embed_provider,
                                                   embed_key)
  } else {
    taxonomy_embs <- NULL
  }

  # Pre-build the full taxonomy prompt section (reused across batches when not
  # using embeddings)
  full_taxonomy_prompt <- if (!use_embeddings) {
    .pc_build_taxonomy_prompt(taxonomy)
  } else {
    NULL
  }

  # Add output metadata columns; initialise classification columns to NA
  has_abstract           <- !is.na(pubs$abstract) & nzchar(pubs$abstract)
  pubs$pc_text_source    <- ifelse(has_abstract, "title+abstract", "title")
  pubs$pc_classified_by  <- if (use_embeddings) "llm-rag" else "llm-full"
  pubs$pc_field          <- NA_character_
  pubs$pc_rationale      <- NA_character_

  indices <- seq_len(nrow(pubs))
  batches <- split(indices, ceiling(indices / batch_size))

  cli::cli_progress_bar("Classifying publications", total = nrow(pubs))

  for (batch_idx in batches) {
    batch <- pubs[batch_idx, ]
    texts <- .pc_build_classify_text(batch$title, batch$abstract)

    if (use_embeddings) {
      # Narrow taxonomy per publication and classify individually
      for (i in seq_along(texts)) {
        pub_emb    <- .pc_embed_text(texts[[i]], embed_provider, embed_key)
        candidates <- .pc_retrieve_candidates(pub_emb, taxonomy_embs,
                                              taxonomy, k = top_k)
        tax_prompt <- .pc_build_taxonomy_prompt(candidates)
        chat       <- .pc_make_chat(provider, model, api_key, base_url,
                                    sys_msg, ...)
        classified <- .pc_classify_batch(texts[i], tax_prompt,
                                         classify_instructions, chat,
                                         candidates$field)
        if (!is.null(classified)) {
          row <- batch_idx[[i]]
          pubs$pc_field[row]     <- classified$field[[1L]]
          pubs$pc_rationale[row] <- classified$rationale[[1L]]
        }
      }
    } else {
      chat       <- .pc_make_chat(provider, model, api_key, base_url,
                                  sys_msg, ...)
      classified <- .pc_classify_batch(texts, full_taxonomy_prompt,
                                       classify_instructions, chat,
                                       taxonomy$field)
      if (!is.null(classified)) {
        pubs$pc_field[batch_idx]     <- classified$field
        pubs$pc_rationale[batch_idx] <- classified$rationale
      }
    }

    cli::cli_progress_update(inc = length(batch_idx))
  }

  cli::cli_progress_done()
  pubs
}


# Create an ellmer chat object for the specified provider.
# Returns a chat object with the system prompt set.
#' @noRd
.pc_make_chat <- function(provider, model, api_key, base_url, system_prompt,
                          ...) {
  switch(
    provider,
    openai = ellmer::chat_openai(
      system_prompt = system_prompt,
      base_url      = base_url,
      credentials   = if (!is.null(api_key)) function() api_key else NULL,
      model         = model,
      ...
    ),
    `openai-compatible` = ellmer::chat_openai_compatible(
      base_url      = base_url,
      system_prompt = system_prompt,
      credentials   = if (!is.null(api_key)) function() api_key else NULL,
      model         = model,
      ...
    ),
    anthropic = ellmer::chat_anthropic(
      system_prompt = system_prompt,
      credentials   = if (!is.null(api_key)) function() api_key else NULL,
      model         = model,
      ...
    ),
    cli::cli_abort(
      c(
        "Unsupported LLM provider: {.val {provider}}.",
        "i" = "Supported providers: {.val {c('openai', 'openai-compatible', 'anthropic')}}."
      )
    )
  )
}


# Classify a batch of publications via a single LLM call.
#
# texts            - character vector, one entry per publication
# taxonomy_prompt  - pre-built taxonomy section string
# classify_instructions - optional extra guidance string (or NULL)
# chat             - an ellmer chat object
# valid_fields     - character vector of valid taxonomy field names
#
# Returns a data.frame(field, rationale) with one row per text, in the same
# order as texts. On failure returns a data.frame of NAs.
#' @noRd
.pc_classify_batch <- function(texts, taxonomy_prompt, classify_instructions,
                               chat, valid_fields) {
  n <- length(texts)

  papers_text <- paste(
    vapply(
      seq_along(texts),
      function(i) paste0(i, ". ", texts[[i]]),
      character(1L)
    ),
    collapse = "\n\n"
  )

  instructions_section <- if (!is.null(classify_instructions) &&
                               nzchar(classify_instructions)) {
    paste0("\n\n", classify_instructions)
  } else {
    ""
  }

  user_prompt <- paste0(
    taxonomy_prompt,
    instructions_section,
    "\n\nPublications:\n\n",
    papers_text,
    '\n\nReturn a JSON array with exactly one object per publication, ',
    'in the same order. Output raw JSON only, no markdown fences:\n',
    '[{"index": 1, "field": "...", "rationale": "..."}, ...]'
  )

  tryCatch({
    response <- chat$chat(user_prompt)

    # Strip markdown code fences if the model wraps its output
    response <- gsub("^```(?:json)?[[:space:]]*|[[:space:]]*```$", "",
                     trimws(response), perl = TRUE)

    raw <- jsonlite::parse_json(response, simplifyVector = TRUE)

    # parse_json with simplifyVector = TRUE returns a data.frame for an array
    # of objects; fall back to list-of-lists handling if needed
    if (is.data.frame(raw)) {
      indices    <- as.integer(raw[["index"]])
      fields     <- as.character(raw[["field"]])
      rationales <- as.character(raw[["rationale"]])
    } else {
      indices    <- vapply(raw, function(x) as.integer(x[["index"]]),       integer(1L))
      fields     <- vapply(raw, function(x) as.character(x[["field"]]),     character(1L))
      rationales <- vapply(raw, function(x) as.character(x[["rationale"]]), character(1L))
    }

    # Validate returned field names against the taxonomy
    invalid <- !fields %in% valid_fields
    if (any(invalid)) {
      cli::cli_warn(c(
        "{sum(invalid)} publication{?s} received an unrecognised category.",
        "i" = "Setting {.field pc_field} to {.val NA} for {?that/those} row{?s}."
      ))
      fields[invalid]     <- NA_character_
      rationales[invalid] <- NA_character_
    }

    # Re-order to match input order using returned indices
    ord <- order(indices)
    data.frame(
      field     = fields[ord],
      rationale = rationales[ord],
      stringsAsFactors = FALSE
    )
  },
  error = function(e) {
    cli::cli_warn(c(
      "Batch classification failed: {conditionMessage(e)}",
      "i" = "Setting {.field pc_field} and {.field pc_rationale} to \\
             {.val NA} for this batch."
    ))
    data.frame(
      field     = rep(NA_character_, n),
      rationale = rep(NA_character_, n),
      stringsAsFactors = FALSE
    )
  })
}


# Build the text string used to represent a single publication.
# Vectorised: accepts character vectors of equal length.
#' @noRd
.pc_build_classify_text <- function(title, abstract) {
  has_abstract <- !is.na(abstract) & nzchar(abstract)
  ifelse(
    has_abstract,
    paste0("Title: ", title, "\nAbstract: ", abstract),
    paste0("Title: ", title)
  )
}


# Build the taxonomy section of the LLM user message.
# Returns a single string listing each field and its definition.
#' @noRd
.pc_build_taxonomy_prompt <- function(taxonomy) {
  lines <- paste0("- ", taxonomy$field, ": ", taxonomy$definition)
  paste(c("Taxonomy:", lines), collapse = "\n")
}
