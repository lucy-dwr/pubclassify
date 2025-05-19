#' Search for Pattern Across All Columns in a Data Frame
#'
#' @description
#' Searches for a pattern within all columns of a data frame and identifies which
#' columns in each row contain matches. The function handles complex data structures
#' like nested data frames and lists, recursively searching within them.
#'
#' @param df A data frame to search within.
#' @param pattern A character string to search for. The pattern is treated as a
#'   fixed string (not a regular expression).
#' @param ignore_case Logical indicating whether to perform a case-insensitive search.
#'   Default is TRUE.
#'
#' @return The original data frame with an additional list-column named `matching_cols`.
#'   Each element of `matching_cols` is a character vector containing the names of
#'   columns where the pattern was found for that row.
#'
#' @details
#' The function performs a search using fixed pattern matching (not regular expressions).
#' By default, it is case-insensitive but this can be controlled with the `ignore_case`
#' parameter. It recursively searches through:
#' \itemize{
#'   \item Atomic vectors (converting to character for comparison)
#'   \item Nested lists (searching each element recursively)
#'   \item Nested data frames (searching all values as character strings)
#' }
#'
#' This function is especially useful for:
#' \itemize{
#'   \item Finding which columns contain specific text in complex data structures
#'   \item Filtering rows based on pattern presence across any column
#'   \item Identifying where specific terms appear in publication metadata
#' }
#'
#' @examples
#' # do a simple data frame search (case-insensitive by default)
#' df <- data.frame(
#'   title = c("Climate Change Study", "Economic Analysis", "CLIMATE Policy"),
#'   abstract = c("Effects of warming on ecosystems", "Market trends", "Environmental regulations"),
#'   stringsAsFactors = FALSE
#' )
#' result <- search_cols(df, "climate")
#'
#' # both "Climate Change Study" and "CLIMATE Policy" will match
#' result$matching_cols
#'
#' # do a case-sensitive search (will only match exact case)
#' result_sensitive <- search_cols(df, "Climate", ignore_case = FALSE)
#' result_sensitive$matching_cols
#'
#' # filter to show only rows where the pattern appears in any column
#' result[lengths(result$matching_cols) > 0, ]
#'
#' @seealso
#' \code{\link{grepl}} which is used internally for pattern matching.
#'
#' @export
search_cols <- function(df, pattern, ignore_case = TRUE) {
  # prepare pattern - convert to lowercase if ignore_case is TRUE
  if (ignore_case) {
    pattern <- tolower(pattern)
  }

  # create a helper to test if 'pattern' appears anywhere in object x
  search_in_obj <- function(x) {
    # deal with a nested data.frame
    if (inherits(x, "data.frame")) {
      vals <- unlist(lapply(x, as.character), use.names = FALSE)
      # Convert values to lowercase if ignore_case is TRUE
      if (ignore_case) {
        vals <- tolower(vals)
      }
      return(any(grepl(pattern, vals, fixed = TRUE)))
    }
    # deal with a list
    if (is.list(x)) {
      # recurse into each element
      return(any(vapply(x, search_in_obj, logical(1))))
    }
    # deal with an atomic vector
    x_char <- as.character(x)
    # convert to lowercase if ignore_case is TRUE
    if (ignore_case) {
      x_char <- tolower(x_char)
    }
    return(any(grepl(pattern, x_char, fixed = TRUE)))
  }

  n   <- nrow(df)
  out <- vector("list", length = n)

  # for each row, test every column
  for (i in seq_len(n)) {
    hits <- vapply(
      names(df),
      function(col) search_in_obj(df[[col]][[i]]),
      logical(1)
    )
    out[[i]] <- names(hits)[hits]
  }

  # attach result as a new list-column
  df$matching_cols <- out
  df
}
