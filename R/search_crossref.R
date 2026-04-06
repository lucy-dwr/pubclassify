#' Search Crossref for publications
#'
#' Queries the [Crossref REST API](https://api.crossref.org) for journal
#' articles matching a keyword in a specified field. No API key is required;
#' providing an email via [pc_configure()] enables the polite pool for
#' faster, more reliable responses.
#'
#' @details
#' **Abstract coverage**: Crossref abstracts are frequently absent. After
#' searching, consider calling [pc_fetch_abstracts()] to backfill missing
#' abstracts from OpenAlex using DOIs.
#'
#' **Funder coverage**: Crossref funder search (`query.funder-name`) matches
#' on funder names deposited by publishers and has lower coverage than
#' OpenAlex or Scopus. Results may be incomplete.
#'
#' @param query Character. Keyword or phrase to search.
#' @param field Character. One of `"abstract"`, `"affiliation"`, `"funder"`.
#' @param max_results Integer. Maximum number of records to return. Crossref
#'   pages in batches of up to 1000; pagination via `offset` is handled
#'   automatically. Default `500`.
#' @param email Character. Email for the Crossref polite pool. Falls back to
#'   the value set with [pc_configure()], then `PUBCLASSIFY_EMAIL`.
#' @param ... Reserved for future use.
#'
#' @return A [tibble::tibble()] with the standard pubclassify schema.
#'   See [pc_search()] for column details. `abstract` will be `NA` for many
#'   records.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- pc_search_crossref("climate change", field = "abstract")
#' results <- pc_fetch_abstracts(results)  # backfill missing abstracts
#' }
pc_search_crossref <- function(
    query,
    field       = c("abstract", "affiliation", "funder"),
    max_results = 500L,
    email       = NULL,
    ...
) {
  field <- rlang::arg_match(field)
  email <- email %||% .pc_env$email %||%
    Sys.getenv("PUBCLASSIFY_EMAIL", unset = NA_character_)

  # Map field argument to the Crossref query parameter name
  query_param <- switch(field,
    abstract    = "query.abstract",
    affiliation = "query.affiliation",
    funder      = "query.funder-name"
  )

  # Crossref etiquette: include email in the User-Agent string.
  # Format: "ToolName/version (mailto:email)" — see
  # https://github.com/CrossRef/rest-api-doc#good-manners--rate-limits
  user_agent <- if (!is.na(email)) {
    sprintf("pubclassify/%s (mailto:%s)", .pc_version(), email)
  } else {
    sprintf("pubclassify/%s", .pc_version())
  }

  # TODO: implement offset-based pagination loop
  #
  # Algorithm:
  #   offset <- 0L
  #   page_size <- min(1000L, max_results)
  #   collected <- list()
  #   repeat {
  #     req <- httr2::request("https://api.crossref.org/works") |>
  #       httr2::req_url_query(
  #         !!query_param := query,   # dynamic param name via rlang injection
  #         rows           = page_size,
  #         offset         = offset,
  #         filter         = "type:journal-article",
  #         select         = "DOI,title,abstract,published-print,
  #                           published-online,author,funder,container-title"
  #       ) |>
  #       httr2::req_user_agent(user_agent) |>
  #       httr2::req_retry(max_tries = 3L)
  #     resp <- httr2::req_perform(req)
  #     page <- httr2::resp_body_json(resp)
  #     collected <- c(collected, list(.pc_parse_crossref(page)))
  #     total_results <- page[["message"]][["total-results"]]
  #     offset <- offset + page_size
  #     if (offset >= total_results || offset >= max_results) break
  #   }
  #   result <- do.call(rbind, collected)
  #   result[seq_len(min(nrow(result), max_results)), ]
  #
  # NOTE: Crossref's `query.*` parameters are relevance-ranked, not exact.
  # For true exact matching, consider post-filtering on the returned results.

  .pc_empty_result()
}
