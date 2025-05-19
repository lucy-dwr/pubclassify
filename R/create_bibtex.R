#' Create BibTeX entry from publication data
#'
#' @description
#' Generates a structured BibTeX entry from a row of publication data, typically
#' obtained from CrossRef or a similar source. The function validates required
#' fields, creates appropriate BibTeX entry types based on publication type, and
#' returns both a structured list of fields and a formatted BibTeX string.
#'
#' @param pub_row A data frame row or list containing publication metadata.
#'   Must include: doi, title, container_title, pub_year, and author.
#'   May optionally include: volume, issue, page, publisher, url, and other fields.
#' @param key_prefix Optional character string to prepend to the citation key.
#'   Default is an empty string.
#'
#' @return A list containing BibTeX fields and the raw BibTeX string, or NA if
#'   required fields are missing. The list includes:
#'   \itemize{
#'     \item entry_type: The BibTeX entry type (article, book, incollection, etc.)
#'     \item key: The citation key
#'     \item author: Formatted author string
#'     \item title: The publication title
#'     \item year: Publication year
#'     \item doi: Digital Object Identifier
#'     \item url: URL to the publication
#'     \item Additional fields based on publication type (journal, volume, pages, etc.)
#'     \item raw_bibtex: The complete formatted BibTeX entry as a string
#'   }
#'
#' @details
#' This function maps CrossRef publication types to appropriate BibTeX entry types:
#' \itemize{
#'   \item "journal-article" to "article"
#'   \item "book-chapter" to "incollection"
#'   \item "book" to "book"
#'   \item "proceedings-article" to "inproceedings"
#'   \item others default to "article"
#' }
#'
#' The citation key is generated using the format: [key_prefix][last_name][year].
#' The function expects the author information in pub_row$author to be a list,
#' with the first element containing author data.
#'
#' The function depends on helper functions:
#' \itemize{
#'   \item format_bibtex_authors(): Formats author data into a BibTeX-compatible string
#'   \item clean_text(): Cleans and escapes text for use in BibTeX entries
#' }
#'
#' @examples
#' \dontrun{
#' # create an input publication row
#' pub_data <- data.frame(
#'   doi = "10.1234/journal.example",
#'   title = "Example Research Paper",
#'   container_title = "Journal of Examples",
#'   pub_year = 2023,
#'   lead_author_last_name = "Smith",
#'   author = I(list(list(
#'     family = "Smith",
#'     given = "John"
#'   ))),
#'   volume = "45",
#'   issue = "2",
#'   page = "123-145",
#'   type = "journal-article",
#'   url = "https://doi.org/10.1234/journal.example"
#' )
#'
#' # generate BibTeX with a prefix
#' bibtex_entry <- create_bibtex(pub_data, key_prefix = "ref:")
#'
#' # access specific fields
#' bibtex_entry$key        # citation key
#' bibtex_entry$raw_bibtex # complete BibTeX string
#' }
#'
#' @export
create_bibtex <- function(pub_row, key_prefix = "") {
  # extract and validate pub_year
  raw_year <- pub_row$pub_year
  if (length(raw_year) != 1 || is.na(raw_year)) {
    return(NA_character_)
  }
  year <- as.character(raw_year)

  # validate essential fields (doi, title, container_title)
  if (any(
    length(pub_row$doi)             != 1 || is.na(pub_row$doi),
    length(pub_row$title)           != 1 || is.na(pub_row$title),
    length(pub_row$container_title) != 1 || is.na(pub_row$container_title)
  )) {
    return(NA_character_)
  }

  # build the citation key
  last_name    <- gsub("\\s+", "", pub_row$lead_author_last_name)
  citation_key <- paste0(key_prefix, last_name, year)

  # map CrossRef type to BibTeX entry type
  pub_type <- switch(
    pub_row$type,
    "journal-article"     = "article",
    "book-chapter"        = "incollection",
    "book"                = "book",
    "proceedings-article" = "inproceedings",
    "article"
  )

  # format authors vector into a single string
  authors <- format_bibtex_authors(pub_row$author[[1]])

  # assemble core BibTeX fields
  bibtex_fields <- list(
    entry_type = pub_type,
    key        = citation_key,
    author     = authors,
    title      = clean_text(pub_row$title),
    year       = year,
    doi        = pub_row$doi,
    url        = pub_row$url
  )

  # add type-specific fields
  if (pub_type == "article") {
    bibtex_fields$journal <- clean_text(pub_row$container_title)
    if (!is.na(pub_row$volume)) bibtex_fields$volume <- pub_row$volume
    if (!is.na(pub_row$issue))  bibtex_fields$number <- pub_row$issue
    if (!is.na(pub_row$page))   bibtex_fields$pages  <- pub_row$page

  } else if (pub_type %in% c("incollection", "inproceedings")) {
    bibtex_fields$booktitle <- clean_text(pub_row$container_title)
    if (!is.na(pub_row$publisher)) {
      bibtex_fields$publisher <- clean_text(pub_row$publisher)
    }

  } else if (pub_type == "book") {
    if (!is.na(pub_row$publisher)) {
      bibtex_fields$publisher <- clean_text(pub_row$publisher)
    }
  }

  # render the raw BibTeX entry
  bibtex_str <- sprintf("@%s{%s,\n", bibtex_fields$entry_type, bibtex_fields$key)
  for (fld in names(bibtex_fields)) {
    if (!fld %in% c("entry_type", "key") &&
        !is.na(bibtex_fields[[fld]]) &&
        bibtex_fields[[fld]] != "") {

      val <- bibtex_fields[[fld]]
      if (fld == "title") val <- paste0("{", val, "}")
      bibtex_str <- paste0(
        bibtex_str,
        "  ", fld, " = {", val, "},\n"
      )
    }
  }
  bibtex_str <- paste0(bibtex_str, "}")

  # return both the structured list and the raw string
  bibtex_fields$raw_bibtex <- bibtex_str
  return(bibtex_fields)
}


#' Format author information for BibTeX entries
#'
#' @description
#' Converts a data frame of author information into a properly formatted author
#' string for BibTeX entries. Handles individual authors, organizations, and
#' partial author information according to BibTeX conventions.
#'
#' @param author_list A data frame containing author information. Expected columns are:
#'   \itemize{
#'     \item family: Family/last name for individual authors
#'     \item given: Given/first name for individual authors
#'     \item name: Full name for organizational authors
#'   }
#'
#' @return A character string containing the formatted author list for a BibTeX
#'   entry. Authors are separated by " and " as per BibTeX standards. If no valid
#'   authors are found, returns "Unknown".
#'
#' @details
#' The function processes different types of author entities:
#' \itemize{
#'   \item Individual authors: formatted as "Family, Given"
#'   \item Organizations: enclosed in braces like "\{Organization Name\}"
#'   \item Partial information: sses available name parts, with "Unknown" for
#'      missing family names
#' }
#'
#' Special handling:
#' \itemize{
#'   \item If the first author is an organization but individual authors follow,
#'     the organization is dropped to favor individual attributions
#'   \item Empty or NULL author lists return "Unknown"
#'   \item The function processes each row of the author_list data frame sequentially
#' }
#'
#' @examples
#' # handle individual authors
#' authors <- data.frame(
#'   family = c("Smith", "Johnson", "Lee"),
#'   given = c("John", "Sarah", "David"),
#'   name = c(NA, NA, NA),
#'   stringsAsFactors = FALSE
#' )
#' format_bibtex_authors(authors)
#' # returns: Smith, John and Johnson, Sarah and Lee, David
#'
#' # handle an organization as author
#' org_author <- data.frame(
#'   family = NA_character_,
#'   given = NA_character_,
#'   name = "National Science Foundation",
#'   stringsAsFactors = FALSE
#' )
#' # for organizations, the output will have the name wrapped in braces
#' result <- format_bibtex_authors(org_author)
#' # will return the organization name enclosed in braces
#'
#' # handle mixed authors (organization will be dropped)
#' mixed <- rbind(org_author, authors[1, ])
#' format_bibtex_authors(mixed)
#' # returns: Smith, John
#'
#' @seealso \code{\link{create_bibtex}} which uses this function to format author
#'   information in complete BibTeX entries.
#'
#' @export
format_bibtex_authors <- function(author_list) {
  if (is.null(author_list) || nrow(author_list) == 0) {
    return("Unknown")
  }

  author_strings <- character(0)

  for (i in seq_len(nrow(author_list))) {
    current <- author_list[i, ]

    # deal with individual (has both given and family)
    if (!is.na(current$family) && !is.na(current$given)) {
      author_strings <- c(
        author_strings,
        paste0(current$family, ", ", current$given)
      )

      # deal with organization
    } else if (!is.na(current$name)) {
      if (length(author_strings) == 0) {
        author_strings <- c(
          author_strings,
          paste0("{", current$name, "}")
        )
      }

      # deal with partial name
    } else if (!is.na(current$family) || !is.na(current$given)) {
      fam            <- if (!is.na(current$family)) current$family else "Unknown"
      giv            <- if (!is.na(current$given))  current$given  else ""
      author_strings <- c(author_strings, paste0(fam, ", ", giv))
    }
  }

  # if first is an org but there are individuals, drop the org
  if (length(author_strings) > 1 && substr(author_strings[1],1,1) == "{") {
    author_strings <- author_strings[-1]
  }

  if (length(author_strings) == 0) {
    return("Unknown")
  }

  # join with " and "
  paste(author_strings, collapse = " and ")
}


#' Extract publication year from various date fields
#'
#' @description
#' Extracts the publication year from multiple possible date fields using a
#' vectorized approach. The function prioritizes dates in the following order:
#' print publication date, online publication date, and creation date.
#'
#' @param published_print Character vector of print publication dates (format:
#'    "YYYY-MM-DD" or similar)
#' @param published_online Character vector of online publication dates (format:
#'    "YYYY-MM-DD" or similar)
#' @param created Character vector of content creation dates (format: "YYYY-MM-DD"
#'    or similar)
#'
#' @return A character vector containing the extracted years. For each element,
#'    returns the year from the first available date field according to priority
#'    order. If no valid date is found, returns NA_integer_.
#'
#' @details
#' The function applies the following logic to each element in the input vectors:
#' \enumerate{
#'   \item Extract the first 4 characters (year) from each date field
#'   \item Use the year from published_print if available
#'   \item Otherwise, use the year from published_online if available
#'   \item Otherwise, use the year from created if available
#'   \item If none are available, return NA_integer_
#' }
#'
#' This function is designed to work with publication metadata from sources like
#' CrossRef, where different types of dates may be available for different
#' publications.
#'
#' @examples
#' # extract year from different date combinations
#' extract_year_vectorized(
#'   published_print = c("2022-01-15", NA, NA, "2023-06-30"),
#'   published_online = c(NA, "2021-12-10", NA, "2023-05-15"),
#'   created = c(NA, NA, "2020-07-22", "2023-01-01")
#' )
#' # returns c("2022", "2021", "2020", "2023")
#'
#' # handle missing values
#' extract_year_vectorized(
#'   published_print = c(NA, NA),
#'   published_online = c(NA, "2021-12-10"),
#'   created = c(NA, NA)
#' )
#' # returns c(NA_integer_, "2021")
#'
#' @export
extract_year_vectorized <- function(published_print, published_online, created) {
  year_print <- substr(published_print, 1, 4)
  year_online <- substr(published_online, 1, 4)
  year_created <- substr(created, 1, 4)
  result <- ifelse(
    !is.na(year_print) & year_print != "",
    year_print,
    ifelse(
      !is.na(year_online) & year_online != "",
      year_online,
      ifelse(
        !is.na(year_created) & year_created != "",
        year_created,
        NA_integer_
      )
    )
  )

  return(result)
}


#' Clean and escape text for BibTeX entries
#'
#' @description
#' Prepares text strings for use in BibTeX entries by escaping special characters
#' that have special meaning in LaTeX. This helps prevent syntax errors when the
#' BibTeX file is processed by LaTeX.
#'
#' @param text A character string to be cleaned and escaped for BibTeX.
#'
#' @return A character string with special characters escaped for BibTeX.
#'   If the input is NA, returns an empty string.
#'
#' @details
#' This function escapes the following special characters:
#' \itemize{
#'   \item Ampersand (&) to \\\\&
#'   \item Percent sign (%) to \\\\%
#'   \item Underscore (_) to \\\\_
#' }
#'
#' These characters have special meanings in LaTeX and must be escaped to be
#' displayed correctly in the final document. The function is used internally
#' by other BibTeX generation functions to ensure valid output.
#'
#' @examples
#' # escape special characters
#' clean_text("Machine Learning & Statistics")
#' # returns "Machine Learning \\&amp; Statistics"
#'
#' # handle multiple special characters
#' clean_text("50% Reduction in Error_Rate")
#' # returns "50\\% Reduction in Error\\_Rate"
#'
#' # handle NA values
#' clean_text(NA)
#' # returns ""
#'
#' @seealso \code{\link{create_bibtex}} which uses this function to prepare
#'   text fields for BibTeX entries.
#'
#' @export
clean_text <- function(text) {
  if (is.na(text)) return("")

  # escape special characters for BibTeX
  text <- gsub("&", "\\\\&", text)
  text <- gsub("%", "\\\\%", text)
  text <- gsub("_", "\\\\_", text)

  return(text)
}
