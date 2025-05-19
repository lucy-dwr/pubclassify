#' Extract author information from publication metadata
#'
#' @description
#' Extracts key information about the lead author from publication metadata.
#' Identifies the first author's given (first) and family (last) names and counts
#' the total number of authors. Handles both individual authors and organizational
#' authors with special logic.
#'
#' @param author_list A data frame containing author information. Expected to
#'   have the following columns:
#'   \itemize{
#'     \item sequence: character indicating author order, with "first" identifying
#'        the lead author
#'     \item given: given/first name for individual authors
#'     \item family: family/last name for individual authors
#'     \item name: (optional) full name for organizational authors
#'   }
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item lead_author_first_name: character, the first name of the lead author
#'        (NA if not found)
#'     \item lead_author_last_name: character, the last name of the lead author
#'        (NA if not found)
#'     \item author_count: integer, the total number of authors
#'   }
#'
#' @details
#' This function processes author information typically obtained from publication
#' metadata sources such as CrossRef. It handles several special cases:
#'
#' \itemize{
#'   \item When the first author is an organization, it searches for the first
#'     individual author in the list to extract personal name information
#'   \item When no authors are found or author_list is NULL, it returns NA
#'     values for names and 0 for author count
#'   \item It specifically looks for the "sequence" field with value "first" to
#'     identify the lead author
#' }
#'
#' The function is particularly useful for creating citation keys, identifying
#' first authors for sorting, and gathering bibliographic metadata.
#'
#' @examples
#' # try an example with individual authors
#' authors <- data.frame(
#'   sequence = c("first", "additional", "additional"),
#'   given = c("John", "Jane", "Robert"),
#'   family = c("Smith", "Johnson", "Williams"),
#'   stringsAsFactors = FALSE
#' )
#' extract_author_info(authors)
#' # returns list(lead_author_first_name = "John", lead_author_last_name = "Smith", author_count = 3)
#'
#' # try an example with an organization as first author
#' mixed_authors <- data.frame(
#'   sequence = c("first", "additional"),
#'   given = c(NA, "Maria"),
#'   family = c(NA, "Garcia"),
#'   name = c("National Science Foundation", NA),
#'   stringsAsFactors = FALSE
#' )
#' extract_author_info(mixed_authors)
#' # returns list(lead_author_first_name = "Maria", lead_author_last_name = "Garcia", author_count = 2)
#'
#' @export
extract_author_info <- function(author_list) {
  # initialize default values
  lead_first_name <- NA_character_
  lead_last_name <- NA_character_
  n_authors <- 0

  # if author_list is NULL or empty, return defaults
  if (is.null(author_list) || length(author_list) == 0) {
    return(list(
      lead_author_first_name = lead_first_name,
      lead_author_last_name = lead_last_name,
      author_count = n_authors
    ))
  }

  # count the total number of authors
  n_authors <- nrow(author_list)

  # find the first author
  first_author_idx <- which(author_list$sequence == "first")

  if (length(first_author_idx) > 0) {
    first_author <- author_list[first_author_idx[1], ]

    # check if it is an organization
    is_org <- "name" %in% names(first_author) && !is.na(first_author$name) &&
      (is.null(first_author$given) || is.na(first_author$given)) &&
      (is.null(first_author$family) || is.na(first_author$family))

    if (is_org) {
      # if it is an organization look for the first individual person author
      for (i in 1:nrow(author_list)) {
        current <- author_list[i, ]

        # check if this entry has a person's name
        if ((!is.null(current$given) && !is.na(current$given)) ||
            (!is.null(current$family) && !is.na(current$family))) {
          lead_first_name <- if (is.null(current$given)) NA_character_ else current$given
          lead_last_name <- if (is.null(current$family)) NA_character_ else current$family
          break
        }
      }
    } else {
      # process a normal case with given and family names
      if (!is.null(first_author$given)) {
        lead_first_name <- first_author$given
      }

      if (!is.null(first_author$family)) {
        lead_last_name <- first_author$family
      }
    }
  }

  # return the extracted information as a list
  return(list(
    lead_author_first_name = lead_first_name,
    lead_author_last_name = lead_last_name,
    author_count = n_authors
  ))
}
