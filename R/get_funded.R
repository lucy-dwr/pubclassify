#' Retrieve publications funded by a specific funder
#'
#' @description
#' Fetches metadata for publications funded by a specific organization, using
#' the CrossRef API. The function handles pagination to retrieve larger result
#' sets and enforces a maximum number of results to prevent excessive API calls.
#'
#' @param funder_id A character string containing a valid FundRef identifier.
#'    This is the unique ID for a funding organization in the CrossRef Funder
#'    Registry.
#' @param filter A named character vector of additional filters to apply to the
#'    CrossRef query. Default is c(type = "journal-article") to retrieve only
#'    journal articles. See \code{\link[rcrossref]{cr_works}} for all available
#'    filter options.
#' @param total An integer specifying the maximum number of records to retrieve.
#'    Default is 10000. The function will stop when this many records have been
#'    fetched or when no more records are available.
#' @param per_page An integer specifying how many records to fetch per API call.
#'   Must be between 1 and 1000. Default is 1000 (the maximum allowed by CrossRef).
#'
#' @return A data frame containing publication metadata for works funded by the
#'    specified funder. The structure follows the CrossRef API output format as
#'    processed by rcrossref.
#'
#' @details
#' This function uses the rcrossref package to query the CrossRef API for works
#' associated with a specific funder. It handles pagination automatically, making
#' multiple API calls as needed to retrieve the requested number of records.
#'
#' The function will stop fetching records when any of these conditions are met:
#' \itemize{
#'   \item The total number of requested records has been fetched
#'   \item No more records are available from CrossRef
#'   \item A page returns fewer records than requested, indicating the end of available data
#' }
#'
#' FundRef IDs can be found in the CrossRef Funder Registry or via the search
#' function \code{\link{get_funder_info}}. Some common funders include:
#' \itemize{
#'   \item 100000001: National Science Foundation (NSF)
#'   \item 100000002: National Institutes of Health (NIH)
#'   \item 100000104: National Aeronautics and Space Administration (NASA)
#'   \item 100007782: California Natural Resources Agency (CNRA)
#' }
#'
#' @examples
#' \dontrun{
#' # get up to 500 NSF-funded journal articles
#' nsf_pubs <- get_funded("100000001", total = 500)
#'
#' # get NIH-funded conference papers
#' nih_conf <- get_funded(
#'   funder_id = "100000002",
#'   filter = c(type = "proceedings-article"),
#'   total = 200
#' )
#'
#' # get recent CNRA publications
#' cnra_recent <- get_funded(
#'   funder_id = "100007782",
#'   filter = c(type = "journal-article", from_pub_date = "2023-01-01"),
#'   total = 100
#' )
#' }
#'
#' @seealso
#' \code{\link{get_funder_info}} for finding FundRef IDs by searching for funder
#' names. This can be useful when you don't already know the FundRef ID for a
#' funding organization.
#'
#'
#' @importFrom dplyr bind_rows
#' @importFrom rcrossref cr_works
#'
#' @export
get_funded <- function(funder_id,
                       filter    = c(type = "journal-article"),
                       total     = 10000L,
                       per_page  = 1000L) {
  if (!is.character(funder_id) || length(funder_id) != 1L) {
    stop("`funder_id` must be a single FundRef ID string.")
  }
  total    <- as.integer(total)
  per_page <- as.integer(per_page)
  if (per_page < 1L || per_page > 1000L) {
    stop("`per_page` must be between 1 and 1000.")
  }

  # always include the funder filter
  filt <- c(funder = funder_id, filter)

  # run through pagination loop
  offset  <- 0L
  fetched <- 0L
  pages   <- list()

  while (fetched < total) {
    message("Fetching records ", offset + 1L, "–", offset + per_page)

    res <- rcrossref::cr_works(
      filter = filt,
      limit  = per_page,
      offset = offset
    )

    page <- res$data
    n_pg <- nrow(page)
    if (n_pg == 0L) {
      message("No more records available; stopping at ", fetched, " total.")
      break
    }

    pages[[length(pages) + 1L]] <- page
    fetched <- fetched + n_pg
    offset  <- offset + per_page

    if (n_pg < per_page) {
      message("Last page returned fewer than ", per_page, " records; stopping.")
      break
    }
  }

  # combine and return
  all_works <- dplyr::bind_rows(pages)
  if (nrow(all_works) > total) {
    all_works <- all_works[seq_len(total), ]
  }

  all_works
}


#' Search for funder information in the CrossRef Funder Registry
#'
#' @description
#' Searches the CrossRef Funder Registry for funding organizations matching a
#' query string. This function is useful for finding FundRef IDs when you know
#' the name of a funding organization but not its identifier.
#'
#' @param query A character string containing the search query. This can be a
#'    full or partial name of a funding organization (e.g., "National Science
#'    Foundation" or "NSF").
#' @param limit An integer specifying the maximum number of results to return.
#'   Default is 20.
#'
#' @return A data frame containing information about funding organizations
#'    matching the query.
#'    Columns typically include:
#'    \itemize{
#'      \item id: the FundRef identifier (used for \code{\link{get_funded}})
#'      \item name: the full name of the funding organization
#'      \item alt-names: alternative names or abbreviations
#'      \item uri: URI for the funder in the Funder Registry
#'      \item location: geographic location of the organization
#'    }
#'
#' @details
#' This function uses the rcrossref package to query the CrossRef Funder Registry
#' API. The search is case-insensitive and partial matches are supported. Results
#' are typically sorted by relevance to the query.
#'
#' The FundRef IDs returned by this function can be used with \code{\link{get_funded}}
#' to retrieve publications funded by a specific organization.
#'
#' @examples
#' \dontrun{
#' # search for the National Science Foundation
#' nsf_info <- get_funder_info("National Science Foundation")
#'
#' # Search with an abbreviation
#' nih_info <- get_funder_info("NIH")
#'
#' # Search for funders in a specific country
#' uk_funders <- get_funder_info("United Kingdom")
#'
#' # Get more results
#' many_funders <- get_funder_info("climate change", limit = 50)
#'
#' # Use with get_funded to retrieve publications
#' rlf_info <- get_funder_info("Resources Legacy Fund")
#' rlf_id <- rlf_info$id[1]
#' rlf_pubs <- get_funded(rlf_id, total = 100)
#' }
#'
#' @seealso
#' \code{\link{get_funded}} for retrieving publications funded by a specific funder.
#'
#' @importFrom rcrossref cr_funders
#'
#' @export
get_funder_info <- function(query, limit = 20) {
  res <- rcrossref::cr_funders(
    query  = query,
    limit  = 20
  )

  res_df <- res$data
  return(res_df)
}
