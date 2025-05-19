#' Clean JATS XML abstracts
#'
#' @description
#' Processes JATS XML-formatted abstract fields in publication data, converting
#' them to plain text. The function handles XML namespaces, extracts text
#' content, and standardizes whitespace to produce clean, readable abstracts.
#'
#' @param df A data frame containing abstract information in JATS XML format.
#' @param abstract_col Character string specifying the name of the column
#'   containing JATS XML abstracts. Default is "abstract".
#' @param abstract_output_col Character string specifying the name of the column
#'   where cleaned abstracts will be stored. Default is "abstract_text".
#'
#' @return The input data frame with an added or modified column containing the
#'   cleaned abstract text. The original data frame is preserved except for the
#'   addition/modification of the abstract_output_col.
#'
#' @details
#' This function performs several cleaning operations on JATS XML abstracts:
#' \itemize{
#'   \item Strips namespace prefixes from XML tags
#'   \item Parses the XML and extracts text content
#'   \item Collapses multiple whitespace characters to a single space
#'   \item Removes leading and trailing whitespace
#'   \item Removes leading "Abstract:" or "abstract:" text
#' }
#'
#' Empty or NA abstracts are preserved as NA in the output.
#'
#' @examples
#' \dontrun{
#' # create example data frame with JATS XML abstracts
#' publications <- data.frame(
#'   doi = c("10.1234/example.1", "10.1234/example.2"),
#'   abstract = c(
#'     "<jats:p>This is a sample abstract with <jats:italic>formatting</jats:italic>.</jats:p>",
#'     "<jats:abstract><jats:p>Another example abstract.</jats:p></jats:abstract>"
#'   )
#' )
#'
#' # clean the abstracts
#' publications_clean <- clean_jats_abstracts(publications)
#'
#' # use custom column names
#' publications_clean <- clean_jats_abstracts(
#'   publications,
#'   abstract_col = "abstract",
#'   abstract_output_col = "clean_abstract"
#' )
#' }
#'
#' @importFrom xml2 read_xml xml_ns_strip xml_text
#'
#' @export
clean_jats_abstracts <- function(
    df,
    abstract_col          = "abstract",
    abstract_output_col   = "abstract_text"
) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Please install xml2: install.packages('xml2')")
  }

  raw_abs <- df[[abstract_col]]
  cleaned_abs <- vapply(raw_abs, FUN.VALUE = character(1), function(txt) {
    if (is.na(txt) || txt == "") return(NA_character_)

    # strip namespace prefixes
    txt_nsfree <- gsub("<(/?)[A-Za-z0-9_\\-]+:", "<\\1", txt)

    # wrap, parse, strip NS, extract text
    wrapped <- paste0("<root>", txt_nsfree, "</root>")
    doc     <- xml2::read_xml(wrapped)
    doc     <- xml2::xml_ns_strip(doc)
    body    <- xml2::xml_text(doc)

    # collapse whitespace & trim
    body <- gsub("\\s+", " ", body)
    body <- trimws(body)

    # remove leading “Abstract” case-insensitively
    body <- sub("(?i)^Abstract:?\\s*", "", body, perl = TRUE)

    body
  })

  # return
  df[[abstract_output_col]] <- cleaned_abs
  df
}
