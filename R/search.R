#' Search bibliometric APIs for publications
#'
#' Unified interface to search OpenAlex, Scopus, and/or Crossref for
#' peer-reviewed publications matching a keyword in a specified field.
#' Results from multiple sources are automatically combined and deduplicated
#' by DOI.
#'
#' @param query Character. Keyword or phrase to search for. Matched against
#'   the target `field` in each API.
#' @param field Character. The metadata field to search within. One of:
#'   - `"abstract"`: keyword appears in the abstract
#'   - `"affiliation"`: keyword appears in an author's institutional affiliation
#'   - `"funder"`: keyword appears in a funding acknowledgment
#' @param sources Character vector. One or more of `"openalex"`, `"scopus"`,
#'   `"crossref"`. Defaults to all three.
#' @param max_results Integer. Maximum number of records to return *per
#'   source*. Pagination is handled automatically. Default `500`.
#' @param deduplicate Logical. If `TRUE` (default), deduplicate across sources
#'   by DOI after combining, preferring the richest record. See
#'   [pc_deduplicate()].
#' @param ... Additional arguments passed to the source-specific search
#'   functions (e.g. `email`, `api_key`).
#'
#' @return A [tibble::tibble()] with columns:
#'   \describe{
#'     \item{doi}{Digital Object Identifier (character)}
#'     \item{title}{Article title (character)}
#'     \item{abstract}{Article abstract; `NA` if unavailable (character)}
#'     \item{year}{Publication year (integer)}
#'     \item{authors}{Author display names (list of character vectors)}
#'     \item{affiliations}{Institutional affiliations per author
#'           (list of character vectors)}
#'     \item{funders}{Funder names (list of character vectors)}
#'     \item{journal}{Journal or venue name (character)}
#'     \item{source}{API source: `"openalex"`, `"scopus"`, or `"crossref"`
#'           (character)}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search OpenAlex and Crossref for papers with a funder keyword
#' results <- pc_search(
#'   query   = "California Natural Resources Agency",
#'   field   = "funder",
#'   sources = c("openalex", "crossref")
#' )
#'
#' # Search all sources for an affiliation
#' results <- pc_search("University of California", field = "affiliation")
#' }
pc_search <- function(
    query,
    field       = c("abstract", "affiliation", "funder"),
    sources     = c("openalex", "scopus", "crossref"),
    max_results = 500L,
    deduplicate = TRUE,
    ...
) {
  field   <- rlang::arg_match(field)
  sources <- rlang::arg_match(sources, multiple = TRUE)

  if (!is.character(query) || length(query) != 1L || !nzchar(trimws(query))) {
    cli::cli_abort("{.arg query} must be a single non-empty string.")
  }

  results_list <- lapply(sources, function(src) {
    switch(src,
      openalex = pc_search_openalex(query, field = field,
                                    max_results = max_results, ...),
      scopus   = pc_search_scopus(query,   field = field,
                                  max_results = max_results, ...),
      crossref = pc_search_crossref(query, field = field,
                                    max_results = max_results, ...)
    )
  })

  combined <- do.call(rbind, results_list)

  if (deduplicate && nrow(combined) > 0L) {
    combined <- pc_deduplicate(combined)
  }

  combined
}
