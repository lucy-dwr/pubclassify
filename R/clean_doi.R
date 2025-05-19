#' Clean and standardize DOIs
#'
#' @description
#' Processes Digital Object Identifier (DOI) strings to extract and standardize
#' the DOI value by removing various prefixes, URL components, and extraneous
#' characters. This function helps normalize DOIs that may be presented in
#' different formats (URLs, citations, etc.) into a consistent standard form.
#'
#' @param doi A character vector containing DOI strings to be cleaned, or
#'    NULL/NA values.
#' @param warn Logical. If TRUE, generate warnings for NULL or all-NA inputs.
#'    Default is FALSE.
#'
#' @return A character vector of the same length as the input, with cleaned DOI
#'   values. Empty strings are converted to NA. If the input is NULL or all NA,
#'   it is returned unchanged.
#'
#' @details
#' This function performs the following cleaning operations:
#' \itemize{
#'   \item Removes leading and trailing whitespace
#'   \item Removes quotation marks at the beginning and end
#'   \item Removes URL prefixes like "http://doi.org/", "https://dx.doi.org/", etc.
#'   \item Removes the "doi:" prefix (case insensitive)
#'   \item Handles special cases like missing slashes after ".org"
#'   \item Removes publisher-specific URL formats (e.g., "https://ascelibrary.org/doi/abs/")
#'   \item Converts empty strings to NA
#' }
#'
#' If the input is not a character vector, it is returned unchanged.
#'
#' @examples
#' # do a basic DOI cleaning
#' clean_doi("10.1234/example.doi.123")
#'
#' # clean DOIs with URL prefixes
#' clean_doi(c("https://doi.org/10.1234/example", "doi:10.5678/another.example"))
#'
#' # handle invalid inputs
#' clean_doi("")   # returns NA
#' clean_doi(NA)   # returns NA
#' clean_doi(NULL) # returns NULL
#'
#' # enable warnings
#' clean_doi(NULL, warn = TRUE) # returns NULL with warning
#'
#' @export
clean_doi <- function(doi, warn = FALSE) {
  # check if input is NULL or NA
  if (is.null(doi)) {
    if (warn) warning("NULL input provided to clean_doi()")
    return(doi)
  }

  if (all(is.na(doi))) {
    if (warn) warning("All NA input provided to clean_doi()")
    return(doi)
  }

  # make a vector to store cleaned DOIs
  cleaned <- doi

  # handle character vectors
  if (is.character(doi)) {
    # remove leading/trailing whitespace
    cleaned <- trimws(cleaned)

    # remove quotation marks at start/end
    cleaned <- gsub("^\"|\"$", "", cleaned)

    # remove various URL prefixes
    cleaned <- gsub("^https?://((www|dx)\\.)?doi\\.org/", "", cleaned)

    # handle the case where the slash is missing after .org (https://doi.org10...)
    cleaned <- gsub("^https?://((www|dx)\\.)?doi\\.org", "", cleaned)

    # remove "doi:" prefix, case insensitive
    cleaned <- gsub("^(?i)doi\\s*:", "", cleaned)

    # handle ascelibrary URL format
    cleaned <- gsub("^https?://ascelibrary\\.org/doi/abs/", "", cleaned)

    # handle other DOI formats
    cleaned <- gsub("^https?://dx\\.doi\\.org/", "", cleaned)

    # do a final clean-up of any remaining quotation marks
    cleaned <- gsub("\"", "", cleaned)

    # remove any remaining leading/trailing whitespace
    cleaned <- trimws(cleaned)

    # convert empty strings to NA_character_
    cleaned[cleaned == ""] <- NA_character_
  }

  return(cleaned)
}
