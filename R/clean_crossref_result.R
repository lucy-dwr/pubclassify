#' Clean and filter Crossref API results
#'
#' @description
#' Cleans and filters data frames returned from Crossref API queries. This
#' function standardizes column names, removes unwanted columns, filters results
#' based on a search pattern, and cleans JATS-formatted abstracts.
#'
#' @param df A data frame containing Crossref API results.
#' @param pattern Optional character string or regular expression. When provided,
#'   the function will filter the data frame to retain only rows where this pattern
#'   appears in content columns (excluding references). Default is NULL (no filtering).
#' @param drop_cols Optional character vector of column names to remove from the
#'   data frame. Default is NULL (no columns removed).
#' @param abstract_col Character string specifying the name of the column containing
#'   abstracts. Default is "abstract".
#' @param abstract_output_col Character string specifying the name of the column
#'   where cleaned abstracts will be stored. Default is "abstract_text".
#'
#' @return A tibble with cleaned column names, filtered rows (if pattern is
#'   provided), and cleaned abstracts.
#'
#' @details
#' The function performs several cleaning operations:
#' 1. Standardizes column names using `janitor::clean_names()`
#' 2. Removes specified columns if `drop_cols` is provided
#' 3. If `pattern` is provided, filters rows to those containing the pattern in
#'    content fields (excluding references)
#' 4. Cleans JATS-formatted abstracts using the `clean_jats_abstracts()` function
#'
#' The function depends on two helper functions:
#' - `search_cols()`: Searches columns for a pattern and adds a `matching_cols` column
#' - `clean_jats_abstracts()`: Extracts and cleans text from JATS-formatted XML abstracts
#'
#' @examples
#' \dontrun{
#' # get publications from Crossref
#' crossref_results <- rcrossref::cr_works(query = "machine learning", limit = 20)
#'
#' # clean the results
#' cleaned_crossref_results <- clean_crossref_result(
#'   df = crossref_results$data,
#'   pattern = "neural network",
#'   drop_cols = c("reference", "container_title_short"),
#'   abstract_col = "abstract"
#' )
#' }
#'
#' @importFrom dplyr select filter any_of
#' @importFrom janitor clean_names
#' @importFrom purrr map_lgl
#'
#' @export
clean_crossref_result <- function(df,
                                  pattern = NULL,
                                  drop_cols = NULL,
                                  abstract_col = "abstract",
                                  abstract_output_col = "abstract_text") {
  # clean columns
  out <- janitor::clean_names(df)

  if (!is.null(drop_cols) && length(drop_cols) > 0L) {
    out <- out |>
      dplyr::select(-dplyr::any_of(drop_cols))
  }

  # only filter by search pattern if one is provided
  if (!is.null(pattern)) {
    # retain only those observations where the search query appears and is not
    # only in references
    out <- search_cols(df = out, pattern = pattern)

    out <- out |>
      dplyr::filter(
        !purrr::map_lgl(matching_cols, ~ length(.x) == 0L ||
                          (length(.x) == 1L && .x == "reference"))
      )
  }

  # fix abstract formatting
  out <- clean_jats_abstracts(
    df = out,
    abstract_col = abstract_col,
    abstract_output_col = abstract_output_col
  )

  out
}
