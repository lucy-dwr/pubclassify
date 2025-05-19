#' Format citation in various academic styles
#'
#' @description
#' Creates formatted citations in various academic styles from publication data.
#' This function serves as a central dispatcher that supports multiple citation
#' styles: APA, Nature, Chicago, Science, and AGU formats.
#'
#' @param pub_data Input publication data in one of three forms:
#'   \itemize{
#'     \item A BibTeX string to be parsed
#'     \item A data frame row with publication metadata
#'     \item A list with BibTeX fields already extracted
#'   }
#' @param style Character string specifying the citation style. Options are:
#'   \itemize{
#'     \item "apa" - American Psychological Association (default)
#'     \item "nature" - Nature journal
#'     \item "chicago" - Chicago Manual of Style
#'     \item "science" - Science journal
#'     \item "agu" - American Geophysical Union
#'   }
#' @param link_doi Logical indicating whether DOIs should be formatted as
#'    hyperlinks. Default is TRUE.
#'
#' @return A character string containing the formatted citation, or NA if
#'    formatting fails.
#'
#' @details
#' This function handles multiple input formats for flexibility. It dispatches
#' the citation formatting to style-specific formatters. Each style follows the
#' conventional formatting rules for that publication or style guide, including:
#'
#' \itemize{
#'   \item Author formatting (number of authors shown, use of "et al.")
#'   \item Title formatting (quotes, italics)
#'   \item Order and formatting of journal, volume, issue, pages
#'   \item Year placement
#'   \item DOI formatting
#' }
#'
#' If an unsupported style is requested, the function defaults to APA style with
#' a warning.
#'
#' @examples
#' # format a publication from a data frame
#' pub_data <- data.frame(
#'   author = I(list(data.frame(family = "Smith", given = "John"))),
#'   title = "Example Research Paper",
#'   journal = "Journal of Examples",
#'   year = 2023,
#'   volume = "45",
#'   number = "2",
#'   pages = "123-145",
#'   doi = "10.1234/example",
#'   stringsAsFactors = FALSE
#' )
#'
#' # format in different styles
#' format_citation(pub_data, style = "apa")
#' format_citation(pub_data, style = "nature")
#' format_citation(pub_data, style = "chicago")
#'
#' # format without DOI hyperlinks
#' format_citation(pub_data, style = "apa", link_doi = FALSE)
#'
#' @seealso
#' \code{\link{format_apa}}, \code{\link{format_nature}}, \code{\link{format_chicago}},
#' \code{\link{format_science}}, \code{\link{format_agu}} for style-specific formatters.
#'
#' @export
format_citation <- function(pub_data, style = "apa", link_doi = TRUE) {
  available_styles <- c("apa", "nature", "chicago", "science", "agu")
  if (!style %in% available_styles) {
    warning("Unsupported citation style '", style, "'. Using 'apa' instead.")
    style <- "apa"
  }

  # dispatch input
  if (is.character(pub_data) && length(pub_data) == 1) {
    bib <- parse_bibtex(pub_data)
  } else if (is.data.frame(pub_data) && nrow(pub_data) == 1) {
    bib <- create_bibtex(pub_data)
  } else if (is.list(pub_data) && !is.null(pub_data$title)) {
    bib <- pub_data
  } else {
    warning("format_citation(): invalid input; returning NA.")
    return(NA_character_)
  }

  out <- switch(
    style,
    apa     = format_apa(bib, link_doi),
    nature  = format_nature(bib, link_doi),
    chicago = format_chicago(bib, link_doi),
    science = format_science(bib, link_doi),
    agu     = format_agu(bib, link_doi)
  )

  if (is.null(out) || length(out) != 1) {
    warning("format_citation(): formatter '", style, "' failed; returning NA.")
    return(NA_character_)
  }

  out
}


#' Format citation in APA style
#'
#' @description
#' Creates a citation formatted according to American Psychological Association
#' (APA) style guidelines from BibTeX data.
#'
#' @param bibtex_data A list containing BibTeX fields for a publication.
#' @param link_doi Logical indicating whether DOIs should be formatted as
#'    hyperlinks. Default is TRUE.
#'
#' @return A character string containing the APA-formatted citation, or NA if
#'    formatting fails.
#'
#' @details
#' APA style formatting includes:
#' \itemize{
#'   \item Authors: last name, initials format with "&" before final author
#'   \item For 1-7 authors: all authors listed
#'   \item For 8+ authors: first 6 authors, ellipsis, last author
#'   \item Year in parentheses after authors
#'   \item Title in sentence case without quotes
#'   \item Journal name and volume in italics
#'   \item Issue in parentheses (not italicized)
#' }
#'
#' @examples
#' # create BibTeX data
#' bibtex_data <- list(
#'   entry_type = "article",
#'   author = "Smith, J. and Johnson, S.",
#'   year = "2023",
#'   title = "Example research paper",
#'   journal = "Journal of Examples",
#'   volume = "45",
#'   number = "2",
#'   pages = "123-145",
#'   doi = "10.1234/example"
#' )
#'
#' format_apa(bibtex_data)
#'
#' @seealso
#' \code{\link{format_citation}} for the main citation formatting function.
#'
#' @export
format_apa <- function(bibtex_data, link_doi = TRUE) {
  # validate BibTeX data
  if (!validate_bibtex_data(bibtex_data)) {
    warning("format_nature(): invalid bibtex data; returning NA.")
    return(NA_character_)
  }

  # format authors for APA style
  authors <- format_apa_authors(bibtex_data$author)

  # build the citation
  citation <- paste0(authors, " (", bibtex_data$year, "). ", bibtex_data$title, ". ")

  # add container information
  if (bibtex_data$entry_type == "article" && !is.null(bibtex_data$journal)) {
    citation <- paste0(citation, "*", bibtex_data$journal, "*")

    # add volume and issue if available
    if (!is.null(bibtex_data$volume)) {
      citation <- paste0(citation, ", *", bibtex_data$volume, "*")
      if (!is.null(bibtex_data$number)) {
        citation <- paste0(citation, "(", bibtex_data$number, ")")
      }
    }

    # add page number if available
    if (!is.null(bibtex_data$pages)) {
      citation <- paste0(citation, ", ", bibtex_data$pages)
    }

    citation <- paste0(citation, ". ")
  }

  # add DOI as hyperlink if requested
  if (link_doi && !is.null(bibtex_data$doi)) {
    citation <- paste0(citation, "[doi:", bibtex_data$doi, "](https://doi.org/", bibtex_data$doi, ")")
  } else if (!is.null(bibtex_data$doi)) {
    citation <- paste0(citation, "doi:", bibtex_data$doi)
  }

  return(citation)
}


#' Format author string in APA style
#'
#' @description
#' Formats author names according to American Psychological Association (APA)
#' style guidelines.
#'
#' @param author_str A character string containing author names in "Last, First"
#'    format, separated by " and ".
#'
#' @return A character string containing the authors formatted in APA style.
#'
#' @details
#' APA author formatting rules:
#' \itemize{
#'   \item Single author: unchanged
#'   \item Two authors: "Last, F. M. & Last, F. M."
#'   \item 3-7 authors: "Last, F. M., Last, F. M., ... & Last, F. M."
#'   \item 8+ authors: First 6 authors, ellipsis, last author
#' }
#'
#' @examples
#' \dontrun{
#' format_apa_authors("Smith, John and Johnson, Sarah")
#' format_apa_authors("Smith, John and Johnson, Sarah and Lee, David")
#' }
#'
#' @seealso
#' \code{\link{format_apa}} which uses this function to format citations.
#'
#' @keywords internal
format_apa_authors <- function(author_str) {
  # split author string on " and "
  authors <- strsplit(author_str, " and ", fixed = TRUE)[[1]]

  if (length(authors) == 1) {
    # process single author
    return(authors[1])
  } else if (length(authors) == 2) {
    # process two authors: Last, F. M., & Last, F. M.
    return(paste0(authors[1], " & ", authors[2]))
  } else if (length(authors) <= 7) {
    # process 3-7 authors: Last, F. M., Last, F. M., Last, F. M., & Last, F. M.
    return(paste0(
      paste(authors[1:(length(authors) - 1)], collapse = ", "),
      ", & ",
      authors[length(authors)]
    ))
  } else {
    # process 8+ authors: First 6 authors, ..., last author
    return(paste0(
      paste(authors[1:6], collapse = ", "),
      ", ... ",
      authors[length(authors)]
    ))
  }
}


#' Format citation in Nature style
#'
#' @description
#' Creates a citation formatted according to Nature journal style guidelines
#' from BibTeX data.
#'
#' @param bibtex_data A list containing BibTeX fields for a publication.
#' @param link_doi Logical indicating whether DOIs should be formatted as
#'    hyperlinks. Default is TRUE.
#'
#' @return A character string containing the Nature-formatted citation, or NA if
#'    ormatting fails.
#'
#' @details
#' Nature style formatting includes:
#' \itemize{
#'   \item Authors: last name, initials format with "&" before final author
#'   \item For 1-5 authors: all authors listed
#'   \item For 6+ authors: first author et al.
#'   \item Title without quotation marks
#'   \item Journal name in italics
#'   \item Volume in bold
#'   \item Format: Authors. Title. Journal Volume, Pages (Year).
#' }
#'
#' @examples
#' # create BibTeX data
#' bibtex_data <- list(
#'   entry_type = "article",
#'   author = "Smith, J. and Johnson, S.",
#'   year = "2023",
#'   title = "Example research paper",
#'   journal = "Nature",
#'   volume = "612",
#'   pages = "123-145",
#'   doi = "10.1038/example"
#' )
#'
#' format_nature(bibtex_data)
#'
#' @seealso
#' \code{\link{format_citation}} for the main citation formatting function.
#'
#' @export
format_nature <- function(bibtex_data, link_doi = TRUE) {
  # validate BibTeX data
  if (!validate_bibtex_data(bibtex_data)) {
    warning("format_nature(): invalid bibtex data; returning NA.")
    return(NA_character_)
  }

  # format authors for Nature style
  authors <- format_nature_authors(bibtex_data$author)

  # build the citation - Nature uses no quotation marks around article titles
  citation <- paste0(authors, " ", bibtex_data$title, ". ")

  # add journal information
  if (bibtex_data$entry_type == "article" && !is.null(bibtex_data$journal)) {
    citation <- paste0(citation, "*", bibtex_data$journal, "*")

    # add volume, pages, and year - Nature format: journal volume, pages (year)
    if (!is.null(bibtex_data$volume)) {
      citation <- paste0(citation, " **", bibtex_data$volume, "**")

      # add page number
      if (!is.null(bibtex_data$pages)) {
        citation <- paste0(citation, ", ", bibtex_data$pages)
      }

      # add year in parentheses
      citation <- paste0(citation, " (", bibtex_data$year, ")")
    } else {
      # if no volume, add year differently
      citation <- paste0(citation, " (", bibtex_data$year, ")")
    }

    citation <- paste0(citation, ". ")
  }

  # add DOI as hyperlink if requested
  if (link_doi && !is.null(bibtex_data$doi)) {
    citation <- paste0(citation, "[doi:", bibtex_data$doi, "](https://doi.org/", bibtex_data$doi, ")")
  } else if (!is.null(bibtex_data$doi)) {
    citation <- paste0(citation, "doi:", bibtex_data$doi)
  }

  return(citation)
}


#' Format author string in Nature style
#'
#' @description
#' Formats author names according to Nature journal style guidelines.
#'
#' @param author_str A character string containing author names in "Last, First"
#'    format, separated by " and ".
#'
#' @return A character string containing the authors formatted in Nature style.
#'
#' @details
#' Nature author formatting rules:
#' \itemize{
#'   \item Single author: "Last, F." with period
#'   \item 2-5 authors: "Last, F., Last, F. & Last, F."
#'   \item 6+ authors: "Last, F. et al."
#' }
#'
#' @examples
#' \dontrun{
#' format_nature_authors("Smith, John")
#' format_nature_authors("Smith, John and Johnson, Sarah and Lee, David")
#' }
#'
#' @seealso
#' \code{\link{format_nature}} which uses this function to format citations.
#'
#' @keywords internal
format_nature_authors <- function(author_str) {
  # split author string on " and "
  authors <- strsplit(author_str, " and ", fixed = TRUE)[[1]]

  if (length(authors) == 1) {
    # process single author
    if (!grepl("\\.$", authors[1])) {
      return(paste0(authors[1], "."))
    } else {
      return(authors[1])
    }
  } else if (length(authors) <= 5) {
    # process 2-5 authors: all authors with commas
    last_author <- authors[length(authors)]
    if (length(authors) == 2) {
      return(paste0(authors[1], " & ", last_author))
    } else {
      return(paste0(
        paste(authors[1:(length(authors) - 1)], collapse = ", "),
        " & ",
        last_author
      ))
    }
  } else {
    # process 6+ authors: first author et al.
    return(paste0(authors[1], " et al."))
  }
}


#' Format citation in Chicago style
#'
#' @description
#' Creates a citation formatted according to Chicago Manual of Style guidelines
#' from BibTeX data.
#'
#' @param bibtex_data A list containing BibTeX fields for a publication.
#' @param link_doi Logical indicating whether DOIs should be formatted as
#'    hyperlinks. Default is TRUE.
#'
#' @return A character string containing the Chicago-formatted citation, or NA
#'    if formatting fails.
#'
#' @details
#' Chicago style formatting includes:
#' \itemize{
#'   \item Authors: first author in "Last, First" format, subsequent authors in "First Last" format
#'   \item For 1-3 authors: all authors listed
#'   \item For 4+ authors: first author et al.
#'   \item Year after authors
#'   \item Title in quotation marks
#'   \item Journal name in italics
#'   \item Format: Author(s). Year. "Title." Journal Volume, no. Issue: Pages.
#' }
#'
#' @examples
#' # create BibTeX data
#' bibtex_data <- list(
#'   entry_type = "article",
#'   author = "Smith, John and Johnson, Sarah",
#'   year = "2023",
#'   title = "Example research paper",
#'   journal = "Journal of Examples",
#'   volume = "45",
#'   number = "2",
#'   pages = "123-145",
#'   doi = "10.1234/example"
#' )
#'
#' format_chicago(bibtex_data)
#'
#' @seealso
#' \code{\link{format_citation}} for the main citation formatting function.
#'
#' @export
format_chicago <- function(bibtex_data, link_doi = TRUE) {
  # validate BibTeX data
  if (!validate_bibtex_data(bibtex_data)) {
    warning("format_nature(): invalid bibtex data; returning NA.")
    return(NA_character_)
  }

  # format authors for Chicago style
  authors <- format_chicago_authors(bibtex_data$author)

  # build the citation
  citation <- paste0(authors, ". ", bibtex_data$year, ". \"", bibtex_data$title, ".\" ")

  # add journal information
  if (bibtex_data$entry_type == "article" && !is.null(bibtex_data$journal)) {
    citation <- paste0(citation, "*", bibtex_data$journal, "*")

    # add volume and issue
    if (!is.null(bibtex_data$volume)) {
      citation <- paste0(citation, " ", bibtex_data$volume)
      if (!is.null(bibtex_data$number)) {
        citation <- paste0(citation, ", no. ", bibtex_data$number)
      }
    }

    # add page number
    if (!is.null(bibtex_data$pages)) {
      citation <- paste0(citation, ": ", bibtex_data$pages)
    } else {
      citation <- paste0(citation, ".")
    }
  }

  # add publisher if available
  if (!is.null(bibtex_data$publisher)) {
    if (!grepl("\\.$", citation)) {
      citation <- paste0(citation, ".")
    }
    citation <- paste0(citation, " ", bibtex_data$publisher, ".")
  }

  # add DOI as hyperlink if requested
  if (link_doi && !is.null(bibtex_data$doi)) {
    citation <- paste0(citation, " [doi:", bibtex_data$doi, "](https://doi.org/", bibtex_data$doi, ")")
  } else if (!is.null(bibtex_data$doi)) {
    citation <- paste0(citation, " doi:", bibtex_data$doi)
  }

  return(citation)
}


#' Format author string in Chicago style
#'
#' @description
#' Formats author names according to Chicago Manual of Style guidelines.
#'
#' @param author_str A character string containing author names in "Last, First"
#'    format, separated by " and ".
#'
#' @return A character string containing the authors formatted in Chicago style.
#'
#' @details
#' Chicago author formatting rules:
#' \itemize{
#'   \item Single author: "Last, First"
#'   \item 2-3 authors: first author as "Last, First", subsequent authors as "First Last"
#'   \item 4+ authors: first author et al.
#' }
#'
#' @examples
#' \dontrun{
#' format_chicago_authors("Smith, John")
#' format_chicago_authors("Smith, John and Johnson, Sarah")
#' format_chicago_authors("Smith, John and Johnson, Sarah and Lee, David and Brown, Robert")
#' }
#'
#' @seealso
#' \code{\link{format_chicago}} which uses this function to format citations.
#'
#' @keywords internal
format_chicago_authors <- function(author_str) {
  # split author string on " and "
  authors <- strsplit(author_str, " and ", fixed = TRUE)[[1]]

  if (length(authors) == 1) {
    # process single author
    return(authors[1])
  } else if (length(authors) <= 3) {
    # process 2-3 authors: list all with proper formatting
    # first author: Last, First
    # subsequent authors: First Last
    first_author <- authors[1]

    # convert subsequent authors from "Last, First" to "First Last"
    subsequent_authors <- sapply(authors[2:length(authors)], function(author) {
      parts <- strsplit(author, ",", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        return(paste0(trimws(parts[2]), " ", trimws(parts[1])))
      } else {
        return(author)
      }
    })

    if (length(authors) == 2) {
      return(paste0(first_author, ", and ", subsequent_authors))
    } else {
      return(paste0(
        first_author, ", ",
        subsequent_authors[1], ", and ",
        subsequent_authors[2]
      ))
    }
  } else {
    # process 4+ authors: first author et al.
    return(paste0(authors[1], " et al"))
  }
}


#' Format citation in Science style
#'
#' @description
#' Creates a citation formatted according to Science journal style guidelines
#' from BibTeX data.
#'
#' @param bibtex_data A list containing BibTeX fields for a publication.
#' @param link_doi Logical indicating whether DOIs should be formatted as
#'    hyperlinks. Default is TRUE.
#'
#' @return A character string containing the Science-formatted citation, or NA
#'    if formatting fails.
#'
#' @details
#' Science style formatting includes:
#' \itemize{
#'   \item Authors: single author's last name or first author's last name followed by "et al."
#'   \item Title without quotation marks
#'   \item Journal name in italics
#'   \item Volume in bold
#'   \item Format: Last_name [et al.], Title. Journal Volume, Pages (Year).
#' }
#'
#' @examples
#' # create BibTeX data
#' bibtex_data <- list(
#'   entry_type = "article",
#'   author = "Smith, John and Johnson, Sarah",
#'   year = "2023",
#'   title = "Example research paper",
#'   journal = "Science",
#'   volume = "380",
#'   pages = "123-145",
#'   doi = "10.1126/science.example"
#' )
#'
#' format_science(bibtex_data)
#'
#' @seealso
#' \code{\link{format_citation}} for the main citation formatting function.
#'
#' @export
format_science <- function(bibtex_data, link_doi = TRUE) {
  # validate BibTeX data
  if (!validate_bibtex_data(bibtex_data)) {
    warning("format_nature(): invalid bibtex data; returning NA.")
    return(NA_character_)
  }

  # format authors for Science style
  authors <- format_science_authors(bibtex_data$author)

  # build the citation - Science uses period after authors, no quotes around title
  if (length(strsplit(bibtex_data$author, " and ", fixed = TRUE)[[1]]) == 1) {
    # for single authors, don't add an extra period
    citation <- paste0(authors, ", ", bibtex_data$title, ". ")
  } else {
    # for multiple authors (author et al), add a period
    citation <- paste0(authors, "., ", bibtex_data$title, ". ")
  }

  # add journal information
  if (bibtex_data$entry_type == "article" && !is.null(bibtex_data$journal)) {
    citation <- paste0(citation, "*", bibtex_data$journal, "* **")

    # use Science format: journal volume, pages (year)
    if (!is.null(bibtex_data$volume)) {
      citation <- paste0(citation, bibtex_data$volume, "**")

      # add page number
      if (!is.null(bibtex_data$pages)) {
        citation <- paste0(citation, ", ", bibtex_data$pages)
      }

      # add year in parentheses
      citation <- paste0(citation, " (", bibtex_data$year, ").")
    } else {
      # if no volume, add year differently
      citation <- paste0(citation, "** (", bibtex_data$year, ").")
    }
  }

  # add DOI as hyperlink if requested
  if (link_doi && !is.null(bibtex_data$doi)) {
    citation <- paste0(citation, " [doi:", bibtex_data$doi, "](https://doi.org/", bibtex_data$doi, ")")
  } else if (!is.null(bibtex_data$doi)) {
    citation <- paste0(citation, " doi:", bibtex_data$doi)
  }

  return(citation)
}


#' Format author string in Science style
#'
#' @description
#' Formats author names according to Science journal style guidelines.
#'
#' @param author_str A character string containing author names in "Last, First"
#'    format, separated by " and ".
#'
#' @return A character string containing the authors formatted in Science style.
#'
#' @details
#' Science author formatting rules:
#' \itemize{
#'   \item Single author: last name only
#'   \item Multiple authors: first author's last name followed by "et al"
#' }
#'
#' @examples
#' \dontrun{
#' format_science_authors("Smith, John")
#' format_science_authors("Smith, John and Johnson, Sarah and Lee, David")
#' }
#'
#' @seealso
#' \code{\link{format_science}} which uses this function to format citations.
#'
#' @keywords internal
format_science_authors <- function(author_str) {
  # split author string on " and "
  authors <- strsplit(author_str, " and ", fixed = TRUE)[[1]]

  # extract just the last name of the first author
  first_author <- authors[1]
  last_name <- sub(",.*", "", first_author)

  if (length(authors) == 1) {
    # process single author - just last name (without a period)
    return(last_name)
  } else {
    # process multiple authors - First author's last name + et al
    return(paste0(last_name, " et al"))
  }
}


#' Format citation in AGU style
#'
#' @description
#' Creates a citation formatted according to American Geophysical Union (AGU)
#' style guidelines from BibTeX data.
#'
#' @param bibtex_data A list containing BibTeX fields for a publication.
#' @param link_doi Logical indicating whether DOIs should be formatted as
#'    hyperlinks. Default is TRUE.
#'
#' @return A character string containing the AGU-formatted citation, or NA if
#'    formatting fails.
#'
#' @details
#' AGU style formatting includes:
#' \itemize{
#'   \item Authors: initials followed by last name (e.g., "J. M. Smith")
#'   \item For 1-10 authors: all authors listed
#'   \item For 11+ authors: first author et al.
#'   \item Year in parentheses after authors
#'   \item Title without quotation marks
#'   \item Journal name in italics
#'   \item Format: Author(s) (Year), Title, Journal, Volume(Issue), Pages, DOI
#' }
#'
#' @examples
#' # create BibTeX data
#' bibtex_data <- list(
#'   entry_type = "article",
#'   author = "Smith, John M. and Johnson, Sarah P.",
#'   year = "2023",
#'   title = "Example research paper",
#'   journal = "Journal of Geophysical Research",
#'   volume = "128",
#'   number = "4",
#'   pages = "123-145",
#'   doi = "10.1029/example"
#' )
#'
#' format_agu(bibtex_data)
#'
#' @seealso
#' \code{\link{format_citation}} for the main citation formatting function.
#'
#' @export
format_agu <- function(bibtex_data, link_doi = TRUE) {
  # validate BibTeX data
  if (!validate_bibtex_data(bibtex_data)) {
    warning("format_nature(): invalid bibtex data; returning NA.")
    return(NA_character_)
  }
  # format authors for AGU style
  authors <- format_agu_authors(bibtex_data$author)

  # build the citation
  citation <- paste0(authors, " (", bibtex_data$year, "), ", bibtex_data$title, ", ")

  # add journal information
  if (bibtex_data$entry_type == "article" && !is.null(bibtex_data$journal)) {
    citation <- paste0(citation, "*", bibtex_data$journal, "*")

    # add volume and issue - AGU format: journal, volume(issue), pages
    if (!is.null(bibtex_data$volume)) {
      citation <- paste0(citation, ", ", bibtex_data$volume)
      if (!is.null(bibtex_data$number)) {
        citation <- paste0(citation, "(", bibtex_data$number, ")")
      }

      # add page number
      if (!is.null(bibtex_data$pages)) {
        citation <- paste0(citation, ", ", bibtex_data$pages)
      }
    }
  }

  # add DOI - AGU style includes DOI with https format
  if (!is.null(bibtex_data$doi)) {
    if (link_doi) {
      citation <- paste0(citation, ", [https://doi.org/", bibtex_data$doi, "](https://doi.org/", bibtex_data$doi, ")")
    } else {
      citation <- paste0(citation, ", https://doi.org/", bibtex_data$doi)
    }
  } else {
    citation <- paste0(citation, ".")
  }

  return(citation)
}


#' Format author string in AGU style
#'
#' @description
#' Formats author names according to American Geophysical Union (AGU) style
#' guidelines.
#'
#' @param author_str A character string containing author names in "Last, First"
#'    format, separated by " and ".
#'
#' @return A character string containing the authors formatted in AGU style.
#'
#' @details
#' AGU author formatting rules:
#' \itemize{
#'   \item Authors are formatted as "F. M. Last" (initials followed by last name)
#'   \item Single author: just the formatted name
#'   \item Two authors: names joined by "and"
#'   \item 3-10 authors: names separated by commas with "and" before the last author
#'   \item 11+ authors: first author followed by "et al."
#' }
#'
#' @examples
#' \dontrun{
#' format_agu_authors("Smith, John M.")
#' format_agu_authors("Smith, John M. and Johnson, Sarah P.")
#' format_agu_authors("Smith, John and Johnson, Sarah and Lee, David and Brown, Robert")
#' }
#'
#' @seealso
#' \code{\link{format_agu}} which uses this function to format citations.
#'
#' @keywords internal
format_agu_authors <- function(author_str) {
  # split author string on " and "
  authors <- strsplit(author_str, " and ", fixed = TRUE)[[1]]

  # process each author for AGU style - converting from "Last, First" to "F. M. Last"
  formatted_authors <- sapply(authors, function(author) {
    parts <- strsplit(author, ",", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      last_name <- trimws(parts[1])
      first_names <- trimws(parts[2])

      # extract initials from first names
      initials <- strsplit(first_names, " ")[[1]]
      initials <- sapply(initials, function(name) {
        paste0(substr(name, 1, 1), ".")
      })

      return(paste0(paste(initials, collapse = " "), " ", last_name))
    } else {
      # if can't parse, return as is
      return(author)
    }
  })

  if (length(formatted_authors) == 1) {
    # process single author
    return(formatted_authors[1])
  } else if (length(formatted_authors) == 2) {
    # process two authors joined by "and"
    return(paste0(formatted_authors[1], " and ", formatted_authors[2]))
  } else if (length(formatted_authors) <= 10) {
    # process 3-10 authors: list all names with commas and "and" before the last
    return(paste0(
      paste(formatted_authors[1:(length(formatted_authors) - 1)], collapse = ", "),
      ", and ",
      formatted_authors[length(formatted_authors)]
    ))
  } else {
    # process more than 10 authors: First author et al.
    return(paste0(formatted_authors[1], " et al."))
  }
}


#' Validate BibTeX data structure
#'
#' @description
#' Validates that a BibTeX data structure contains required fields.
#'
#' @param bibtex_data A list containing BibTeX fields.
#' @param required_fields Character vector of field names that must be present.
#'   Default is c("author", "title", "year").
#'
#' @return Logical indicating whether the BibTeX data is valid.
#'
#' @details
#' This function performs several checks on the provided BibTeX data:
#' \itemize{
#'   \item Verifies that bibtex_data is not NULL
#'   \item Confirms that bibtex_data is a list with at least one element
#'   \item Checks that all required fields are present in the list
#' }
#'
#' If any check fails, the function returns FALSE, otherwise it returns TRUE.
#'
#' @examples
#' \dontrun{
#' # check valid BibTeX data
#' valid_data <- list(
#'   author = "Smith, John",
#'   title = "Example Paper",
#'   year = "2023",
#'   journal = "Journal of Examples"
#' )
#' validate_bibtex_data(valid_data)
#'
#' # check invalid BibTeX data (missing year)
#' invalid_data <- list(
#'   author = "Smith, John",
#'   title = "Example Paper"
#' )
#' validate_bibtex_data(invalid_data)
#' }
#'
#' @keywords internal
validate_bibtex_data <- function(bibtex_data,
                                 required_fields = c("author", "title", "year")) {
  # check if bibtex_data is NULL or not a list
  if (is.null(bibtex_data) || !is.list(bibtex_data) || length(bibtex_data) == 0) {
    return(FALSE)
  }

  # check for required fields
  for (field in required_fields) {
    if (is.null(bibtex_data[[field]])) {
      return(FALSE)
    }
  }

  # return that all checks passed
  return(TRUE)
}
