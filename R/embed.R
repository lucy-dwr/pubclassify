# Embedding utilities for the use_embeddings = TRUE path in pc_classify().
# All functions here are internal and not exported.

#' @noRd
.pc_embed_text <- function(text, provider, key) {
  switch(provider,
    openai = .pc_embed_openai(text, key),
    cohere = .pc_embed_cohere(text, key),
    cli::cli_abort(
      "Unsupported embedding provider: {.val {provider}}. \\
       Supported: {.val {c('openai', 'cohere')}}."
    )
  )
}

# Embed a single string using the OpenAI Embeddings API.
# Returns a numeric vector (length 1536 for text-embedding-3-small).
#' @noRd
.pc_embed_openai <- function(text, key) {
  # TODO: implement
  # req <- httr2::request("https://api.openai.com/v1/embeddings") |>
  #   httr2::req_auth_bearer_token(key) |>
  #   httr2::req_body_json(list(
  #     input = text,
  #     model = "text-embedding-3-small"
  #   )) |>
  #   httr2::req_user_agent(.pc_user_agent()) |>
  #   httr2::req_retry(max_tries = 3L)
  # resp <- httr2::req_perform(req)
  # httr2::resp_body_json(resp)[["data"]][[1L]][["embedding"]]
  stop("Not yet implemented")
}

# Embed a single string using the Cohere Embed API.
# Returns a numeric vector.
#' @noRd
.pc_embed_cohere <- function(text, key) {
  # TODO: implement
  # req <- httr2::request("https://api.cohere.com/v2/embed") |>
  #   httr2::req_auth_bearer_token(key) |>
  #   httr2::req_body_json(list(
  #     texts          = list(text),
  #     model          = "embed-english-v3.0",
  #     input_type     = "search_document",
  #     embedding_types = list("float")
  #   )) |>
  #   httr2::req_user_agent(.pc_user_agent()) |>
  #   httr2::req_retry(max_tries = 3L)
  # resp <- httr2::req_perform(req)
  # httr2::resp_body_json(resp)[["embeddings"]][["float"]][[1L]]
  stop("Not yet implemented")
}

# Embed all taxonomy definitions and return a matrix.
# Rows correspond to taxonomy fields; rownames are set to taxonomy$field.
# ncol is the embedding dimension (provider-dependent).
#' @noRd
.pc_build_taxonomy_embeddings <- function(taxonomy, provider, key) {
  cli::cli_inform("Embedding {nrow(taxonomy)} taxonomy field{?s}.")
  embs <- lapply(taxonomy$definition, .pc_embed_text,
                 provider = provider, key = key)
  mat <- do.call(rbind, embs)
  rownames(mat) <- taxonomy$field
  mat
}

# Compute cosine similarity between two numeric vectors.
#' @noRd
.pc_cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# Given an embedding for a single abstract, return the top-k most similar
# taxonomy fields as a pc_taxonomy-compatible data frame.
#' @noRd
.pc_retrieve_candidates <- function(abstract_emb, taxonomy_embs, taxonomy, k) {
  sims    <- apply(taxonomy_embs, 1L, .pc_cosine_similarity, b = abstract_emb)
  top_idx <- order(sims, decreasing = TRUE)[seq_len(min(k, length(sims)))]
  taxonomy[top_idx, , drop = FALSE]
}
