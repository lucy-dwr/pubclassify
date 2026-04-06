#' Combine results from multiple searches
#'
#' A convenience wrapper that row-binds multiple result tibbles returned by
#' [pc_search()] or source-specific search functions, then deduplicates by
#' DOI. Useful when running several searches (different queries or fields)
#' that you want to treat as a single corpus.
#'
#' @param ... Two or more tibbles returned by [pc_search()] or source-specific
#'   search functions, all conforming to the standard pubclassify schema.
#' @param deduplicate Logical. If `TRUE` (default), deduplicate by DOI after
#'   combining. See [pc_deduplicate()].
#'
#' @return A [tibble::tibble()] with the standard pubclassify schema.
#' @export
#'
#' @examples
#' \dontrun{
#' affil   <- pc_search("University of Edinburgh", field = "affiliation")
#' funder  <- pc_search("Wellcome Trust", field = "funder")
#' corpus  <- pc_combine(affil, funder)
#' }
pc_combine <- function(..., deduplicate = TRUE) {
  results_list <- list(...)
  if (length(results_list) == 0L) {
    cli::cli_abort("Provide at least one result tibble to combine.")
  }

  combined <- do.call(rbind, results_list)

  if (deduplicate && nrow(combined) > 0L) {
    combined <- pc_deduplicate(combined)
  }

  combined
}

#' Deduplicate publications by DOI
#'
#' Removes duplicate records sharing the same DOI. When duplicates exist,
#' the retained record is chosen by preferring: (1) a non-`NA` abstract,
#' (2) more affiliations, (3) source priority (`openalex` > `scopus` >
#' `crossref`). Records without a DOI are always retained as-is.
#'
#' @param pubs A [tibble::tibble()] returned by [pc_search()] or
#'   [pc_combine()].
#'
#' @return A deduplicated [tibble::tibble()].
#' @export
#'
#' @examples
#' \dontrun{
#' deduped <- pc_deduplicate(pubs)
#' }
pc_deduplicate <- function(pubs) {
  if (nrow(pubs) == 0L) return(pubs)

  # Records without a DOI cannot be deduplicated — keep them all
  no_doi  <- pubs[is.na(pubs$doi) | !nzchar(pubs$doi), ]
  has_doi <- pubs[!is.na(pubs$doi) & nzchar(pubs$doi), ]

  if (nrow(has_doi) == 0L) return(pubs)

  source_rank <- c(openalex = 1L, scopus = 2L, crossref = 3L)

  # For each unique DOI, score rows and keep the best one
  has_doi$`.rank_abstract` <- as.integer(is.na(has_doi$abstract))
  has_doi$`.rank_affil`    <- -vapply(has_doi$affiliations, length, integer(1L))
  has_doi$`.rank_source`   <- source_rank[has_doi$source] %||% 99L

  has_doi <- has_doi[order(
    has_doi$doi,
    has_doi$`.rank_abstract`,
    has_doi$`.rank_affil`,
    has_doi$`.rank_source`
  ), ]

  deduped <- has_doi[!duplicated(has_doi$doi), ]
  deduped <- deduped[, setdiff(names(deduped), c(".rank_abstract", ".rank_affil", ".rank_source"))]

  rbind(deduped, no_doi)
}

#' Backfill metadata from OpenAlex by DOI
#'
#' For each record that has a DOI and is missing `abstract`, `authors`,
#' `affiliations`, or `grant_numbers`, queries the
#' [OpenAlex Works endpoint](https://docs.openalex.org/api-entities/works) in
#' batches to fill in those fields. Existing non-empty values are preserved.
#' `funders` is filled in from OpenAlex awards only when the field is empty.
#'
#' This is the recommended way to enrich Scopus results retrieved without
#' COMPLETE view access, or Crossref results which rarely include abstracts.
#'
#' @param pubs A [tibble::tibble()] returned by any `pc_search_*()` function,
#'   conforming to the standard pubclassify schema.
#' @param email Character. Email for the OpenAlex polite pool. Falls back to
#'   the value set with [pc_configure()], then the `PUBCLASSIFY_EMAIL`
#'   environment variable.
#' @param batch_size Integer. Number of DOIs per OpenAlex request. Default `50`.
#'
#' @return The input tibble with `abstract`, `authors`, `affiliations`,
#'   `grant_numbers`, and (where empty) `funders` filled in for records whose
#'   DOI was found in OpenAlex.
#' @export
#'
#' @examples
#' \dontrun{
#' pubs <- pc_search_scopus("California Natural Resources Agency",
#'                          field = "funder")
#' pubs <- pc_fetch_abstracts(pubs)
#' }
pc_fetch_abstracts <- function(pubs, email = NULL, batch_size = 50L) {
  email <- email %||% .pc_env$email %||%
    Sys.getenv("PUBCLASSIFY_EMAIL", unset = NA_character_)

  doi_present <- !is.na(pubs$doi) & nzchar(pubs$doi)

  # Rows that have a DOI and would benefit from OpenAlex enrichment:
  # - missing abstract or authors (Crossref, Scopus STANDARD fallback), OR
  # - sourced from Scopus, which always returns incomplete funder data
  #   (Search API only returns one sponsor regardless of view)
  needs_fill <- doi_present & (
    is.na(pubs$abstract) |
    vapply(pubs$authors, length, integer(1L)) == 0L |
    pubs$source == "scopus"
  )

  if (!any(needs_fill)) {
    cli::cli_inform("All records already have metadata; nothing to fetch.")
    return(pubs)
  }

  fill_idx  <- which(needs_fill)
  fill_dois <- pubs$doi[fill_idx]
  n_fill    <- length(fill_idx)

  cli::cli_inform(
    "Fetching metadata for {n_fill} record{?s} from OpenAlex."
  )

  select_fields <- I(paste(c(
    "doi",
    "abstract_inverted_index",
    "authorships",
    "awards",
    "primary_location"
  ), collapse = ","))

  batches <- split(seq_len(n_fill), ceiling(seq_len(n_fill) / batch_size))

  cli::cli_progress_bar("Fetching from OpenAlex", total = n_fill)
  fetched <- 0L

  for (b in batches) {
    b_dois <- fill_dois[b]
    b_rows <- fill_idx[b]

    filter_str <- paste0("doi:", paste(b_dois, collapse = "|"))

    resp <- httr2::request("https://api.openalex.org/works") |>
      httr2::req_url_query(
        filter   = filter_str,
        per_page = length(b_dois),
        select   = select_fields,
        mailto   = if (!is.na(email)) email else NULL
      ) |>
      httr2::req_retry(
        max_tries    = 3L,
        is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 503L)
      ) |>
      httr2::req_perform()

    works <- httr2::resp_body_json(resp)[["results"]] %||% list()

    for (work in works) {
      # Normalize DOI from OpenAlex (strip https://doi.org/ prefix)
      work_doi <- .pc_na_chr(work[["doi"]])
      if (!is.na(work_doi)) {
        work_doi <- sub("^https://doi\\.org/", "", work_doi)
      }
      if (is.na(work_doi)) next

      # Match to rows in this batch (case-insensitive)
      match_pos  <- which(tolower(b_dois) == tolower(work_doi))
      if (length(match_pos) == 0L) next
      match_rows <- b_rows[match_pos]

      # Reconstruct full abstract from OpenAlex inverted index
      abstract <- .pc_reconstruct_abstract(work[["abstract_inverted_index"]])

      # Authors and per-author institution affiliations
      authorships  <- work[["authorships"]] %||% list()
      authors      <- vapply(
        authorships,
        function(a) a[["author"]][["display_name"]] %||% NA_character_,
        character(1L)
      )
      affiliations <- lapply(authorships, function(a) {
        insts <- a[["institutions"]] %||% list()
        vapply(insts,
               function(i) i[["display_name"]] %||% NA_character_,
               character(1L))
      })

      # Funders and grant IDs from awards
      awards       <- work[["awards"]] %||% list()
      oa_funders   <- vapply(
        awards,
        function(g) g[["funder_display_name"]] %||% NA_character_,
        character(1L)
      )
      oa_funders   <- oa_funders[!is.na(oa_funders)]
      grant_nums   <- vapply(
        awards,
        function(g) g[["award_id"]] %||% NA_character_,
        character(1L)
      )
      grant_nums   <- grant_nums[!is.na(grant_nums)]

      for (row in match_rows) {
        # abstract: fill if missing
        if (!is.na(abstract) && is.na(pubs$abstract[[row]])) {
          pubs$abstract[[row]] <- abstract
        }
        # authors: fill if empty
        if (length(authors) > 0L && length(pubs$authors[[row]]) == 0L) {
          pubs$authors[[row]] <- authors
        }
        # affiliations: fill if empty
        if (length(affiliations) > 0L &&
            length(pubs$affiliations[[row]]) == 0L) {
          pubs$affiliations[[row]] <- affiliations
        }
        # grant_numbers: merge with any existing values
        pubs$grant_numbers[[row]] <- unique(
          c(pubs$grant_numbers[[row]], grant_nums)
        )
        # funders: merge Scopus and OpenAlex sources — both may be partial.
        # Scopus Search API returns only the primary sponsor; OpenAlex often
        # has the full co-funder list. Union gives the most complete picture.
        pubs$funders[[row]] <- unique(
          c(pubs$funders[[row]], oa_funders)
        )
      }
    }

    fetched <- fetched + length(b)
    cli::cli_progress_update(set = fetched)
  }

  cli::cli_progress_done()
  pubs
}

#' Flag records matching one or more award ID patterns
#'
#' Adds a logical `award_match` column to a results tibble, set to `TRUE` for
#' rows where any element of `grant_numbers` matches **any** of the supplied
#' patterns. Useful for identifying records funded under a specific contract
#' structure when the funder name alone is ambiguous or inconsistently recorded.
#'
#' Award ID patterns are often more reliable than funder name strings because
#' they are structured and consistent. For example, California Department of
#' Water Resources contracts follow a `4600XXXXXX` format; U.S. Army Corps of
#' Engineers contracts begin with `W912`.
#'
#' @param pubs A [tibble::tibble()] returned by any `pc_search_*()` function.
#' @param pattern Character vector. One or more regular expressions matched
#'   case-insensitively against each element of `grant_numbers`. A row is
#'   flagged `TRUE` if **any** grant number matches **any** pattern. For
#'   example `c("^4600", "^W912")` flags both DWR and USACE contracts.
#'
#' @return The input tibble with an `award_match` logical column appended.
#'   Rows with no grant numbers always return `FALSE`.
#' @export
#'
#' @examples
#' \dontrun{
#' pubs <- pc_search_scopus("California Department of Water Resources",
#'                          field = "funder")
#' # Single pattern
#' pubs <- pc_flag_awards(pubs, "^4600")
#' # Multiple patterns
#' pubs <- pc_flag_awards(pubs, c("^4600", "^W912"))
#' pubs[pubs$award_match, ]
#' }
pc_flag_awards <- function(pubs, pattern) {
  pubs$award_match <- vapply(
    pubs$grant_numbers,
    function(gns) {
      length(gns) > 0L &&
        any(vapply(pattern, function(p)
          any(grepl(p, gns, ignore.case = TRUE, perl = TRUE)),
          logical(1L)))
    },
    logical(1L)
  )
  pubs
}

# ---------------------------------------------------------------------------
# Acknowledgments retrieval — Elsevier TDM API
# ---------------------------------------------------------------------------

# Strip XML/HTML tags from a string and collapse internal whitespace.
# Returns NULL if the result is empty after stripping.
#' @noRd
.pc_strip_xml_tags <- function(x) {
  clean <- gsub("<[^>]+>", " ", x)
  clean <- gsub("\\s+", " ", clean)
  clean <- trimws(clean)
  if (nzchar(clean)) clean else NULL
}

# Extract the acknowledgment section from Elsevier full-text XML.
# Tries the CE schema <ce:acknowledgment> element first, then falls back
# to JATS <ack>. Returns NULL if neither element is found.
#' @noRd
.pc_extract_ack_elsevier <- function(xml_text) {
  m <- regexpr("(?s)<ce:acknowledgment[^>]*>(.*?)</ce:acknowledgment>",
               xml_text, perl = TRUE)
  if (m != -1L) return(.pc_strip_xml_tags(regmatches(xml_text, m)))
  .pc_extract_ack_jats(xml_text)
}

# Extract the <ack> block from JATS XML.
# Returns NULL if the element is not present.
#' @noRd
.pc_extract_ack_jats <- function(xml_text) {
  m <- regexpr("(?s)<ack[^>]*>(.*?)</ack>", xml_text, perl = TRUE)
  if (m == -1L) return(NULL)
  .pc_strip_xml_tags(regmatches(xml_text, m))
}

# Attempt to retrieve acknowledgment text via the Elsevier TDM API.
# Returns the acknowledgment string, or NULL on any failure (404 for
# non-Elsevier DOIs, 403 for non-subscribed content, network errors).
#' @noRd
.pc_fetch_ack_elsevier <- function(doi, api_key, insttoken) {
  tryCatch({
    resp <- httr2::request(
      paste0("https://api.elsevier.com/content/article/doi/", doi)
    ) |>
      httr2::req_headers(
        "X-ELS-APIKey"    = api_key,
        "X-ELS-Insttoken" = insttoken,   # NULL is silently dropped by httr2
        "Accept"          = "application/xml"
      ) |>
      httr2::req_retry(
        max_tries    = 2L,
        is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 503L)
      ) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200L) return(NULL)
    .pc_extract_ack_elsevier(httr2::resp_body_string(resp))
  }, error = function(e) NULL)
}

#' Retrieve acknowledgment text for publications
#'
#' For each record with a DOI, attempts to retrieve the full acknowledgment
#' section from the article's full text via the Elsevier TDM API
#' (`https://api.elsevier.com/content/article/doi/`), which returns structured
#' XML including a `<ce:acknowledgment>` element. Requires a Scopus API key;
#' an institutional token (`SCOPUS_INSTTOKEN`) extends access to off-campus
#' subscriptions. Returns `NULL` for non-Elsevier DOIs or non-subscribed
#' content.
#'
#' Records for which the Elsevier TDM API returns no text are left as `NA`.
#'
#' @param pubs A [tibble::tibble()] returned by any `pc_search_*()` function,
#'   conforming to the standard pubclassify schema.
#' @param api_key Character. Scopus/Elsevier API key used for the TDM API.
#'   Falls back to the value set with [pc_configure()], then the
#'   `SCOPUS_API_KEY` environment variable. If no key is found, the Elsevier
#'   TDM step is skipped and only Europe PMC is tried.
#' @param ... Reserved for future use.
#'
#' @return The input tibble with an `acknowledgment` character column
#'   appended. Values are `NA` where acknowledgment text could not be
#'   retrieved.
#' @export
#'
#' @examples
#' \dontrun{
#' pubs <- pc_search_scopus("California Natural Resources Agency",
#'                          field = "funder")
#' pubs <- pc_fetch_acknowledgments(pubs)
#' pubs[!is.na(pubs$acknowledgment), c("doi", "acknowledgment")]
#' }
pc_fetch_acknowledgments <- function(pubs, api_key = NULL, ...) {
  api_key <- api_key %||% .pc_env$scopus_key %||%
    Sys.getenv("SCOPUS_API_KEY", unset = NA_character_)
  if (is.na(api_key)) api_key <- NULL

  insttoken <- .pc_env$scopus_insttoken %||%
    Sys.getenv("SCOPUS_INSTTOKEN", unset = NA_character_)
  if (is.na(insttoken)) insttoken <- NULL

  if (is.null(api_key)) {
    cli::cli_inform(c(
      "!" = "No Scopus API key found; skipping Elsevier TDM API.",
      "i" = "Only Europe PMC will be tried. Set {.envvar SCOPUS_API_KEY} \\
             or call {.fn pc_configure} for broader coverage."
    ))
  }

  pubs$acknowledgment <- NA_character_

  doi_present <- !is.na(pubs$doi) & nzchar(pubs$doi)
  if (!any(doi_present)) {
    cli::cli_inform("No DOIs available; cannot fetch acknowledgments.")
    return(pubs)
  }

  doi_idx <- which(doi_present)
  n       <- length(doi_idx)

  cli::cli_inform("Fetching acknowledgments for {n} record{?s}.")
  cli::cli_progress_bar("Fetching acknowledgments", total = n)

  for (k in seq_along(doi_idx)) {
    row <- doi_idx[[k]]
    doi <- pubs$doi[[row]]

    ack <- NULL

    if (!is.null(api_key)) {
      ack <- .pc_fetch_ack_elsevier(doi, api_key, insttoken)
    }

    if (!is.null(ack)) {
      pubs$acknowledgment[[row]] <- ack
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  n_found <- sum(!is.na(pubs$acknowledgment))
  cli::cli_inform("Retrieved acknowledgments for {n_found} of {n} record{?s}.")

  pubs
}
