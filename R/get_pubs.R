#' Retrieve publication metadata from CrossRef
#'
#' @description
#' Fetches publication metadata from the CrossRef API using either a search query,
#' filters, or a list of Digital Object Identifiers (DOIs). The function handles
#' pagination for search queries and processes DOIs individually to track
#' successes and failures.
#'
#' @param query Character string containing a search query. This can be used to
#'   search across multiple fields. Default is NULL.
#' @param filter Named character vector of filters to apply to the CrossRef query.
#'   See \code{\link[rcrossref]{cr_works}} for available filter options.
#'   Default is NULL.
#' @param dois Character vector of DOIs to retrieve. If provided, query and filter
#'   parameters are ignored. Default is NULL.
#' @param total Integer specifying the maximum number of records to retrieve.
#'   Default is 10000. The function will stop when this many records have been
#'   fetched or when no more records are available.
#' @param per_page Integer specifying how many records to fetch per API call when
#'   using query or filter mode. Must be between 1 and 1000. Default is 1000
#'   (the maximum allowed by CrossRef).
#' @param preserve_404 Logical indicating whether to include placeholder rows for
#'   DOIs that could not be retrieved. If TRUE (default), the result will contain
#'   one row for each input DOI, with NA values for those not found. If FALSE,
#'   unfound DOIs will be omitted from the result.
#'
#' @return A data frame containing publication metadata from CrossRef. When
#' using DOI mode with preserve_404 = TRUE, the result will include:
#'   \itemize{
#'     \item All standard CrossRef metadata fields
#'     \item original_doi: The DOI as provided in the input
#'     \item doi_retrieved: Logical indicating whether the DOI was successfully retrieved
#'   }
#'
#' @details
#' This function provides two modes of operation:
#'
#' \itemize{
#'   \item Search mode: when query or filter parameters are provided, the function
#'     performs a search using the CrossRef API and handles pagination to retrieve
#'     multiple pages of results.
#'
#'   \item DOI mode: when a list of DOIs is provided, the function retrieves
#'     metadata for each DOI individually. This allows for precise tracking of
#'     which DOIs were successfully retrieved.
#' }
#'
#' When using DOI mode, a small delay (0.1 seconds) is added between requests to
#' avoid overwhelming the CrossRef API.
#'
#' For large DOI lists, the function displays progress messages every 10 DOIs.
#'
#' @examples
#' \dontrun{
#' # search for publications by keyword
#' climate_pubs <- get_pubs(query = "climate change", total = 500)
#'
#' # search with filters
#' recent_ecology <- get_pubs(
#'   filter = c(
#'     from_pub_date = "2022-01-01",
#'     container_title = "ecology"
#'   ),
#'   total = 100
#' )
#'
#' # retrieve specific publications by DOI
#' my_dois <- c(
#'   "10.1038/s41586-019-1401-2",
#'   "10.1126/science.aaf7671",
#'   "10.1111/invalid-doi"  # this DOI doesn't exist
#' )
#' my_pubs <- get_pubs(dois = my_dois)
#'
#' # check which DOIs were not successfully retrieved
#' my_pubs[my_pubs$doi_retrieved == FALSE, ]
#'
#' # get only successfully retrieved DOIs
#' found_pubs <- get_pubs(dois = my_dois, preserve_404 = FALSE)
#' }
#'
#' @seealso
#' \code{\link[rcrossref]{cr_works}} for details on the underlying CrossRef API
#' function and available filter options.
#'
#' @importFrom dplyr bind_rows left_join
#' @importFrom rcrossref cr_works
#'
#' @export
get_pubs <- function(query = NULL,
                     filter = NULL,
                     dois = NULL,
                     total = 10000L,
                     per_page = 1000L,
                     preserve_404 = TRUE) {
  # check for ambiguous input
  query_mode <- !is.null(query) || !is.null(filter)
  doi_mode <- !is.null(dois)

  if (query_mode && doi_mode) {
    stop("Please provide either dois or query/filter, but not both.")
  }

  # validate total parameter
  if (!is.numeric(total) || length(total) != 1 || total <= 0 || !is.finite(total)) {
    stop("`total` must be a positive finite number.")
  }
  total <- as.integer(total)

  # validate per_page parameter
  if (!is.numeric(per_page) || length(per_page) != 1 || !is.finite(per_page)) {
    stop("`per_page` must be a finite number.")
  }
  per_page <- as.integer(per_page)
  if (per_page < 1 || per_page > 1000) {
    stop("`per_page` must be between 1 and 1000.")
  }

  # validate preserve_404 parameter
  if (!is.logical(preserve_404) || length(preserve_404) != 1 || is.na(preserve_404)) {
    stop("`preserve_404` must be a single logical value (TRUE or FALSE).")
  }

  # validate dois parameter if in DOI mode
  if (doi_mode) {
    if (!is.vector(dois) || !is.character(dois)) {
      stop("`dois` must be a character vector.")
    }
  }

  # set up call
  total    <- as.integer(total)
  per_page <- as.integer(per_page)
  offset   <- 0L
  fetched  <- 0L
  results  <- list()

  # if DOIs are provided, handle them directly
  if (doi_mode) {
    # prep to track DOI successes
    doi_results <- list()

    # create a data frame to store which DOIs were found
    doi_status <- data.frame(
      original_index = seq_along(dois),
      doi = dois,
      retrieved = FALSE,
      stringsAsFactors = FALSE
    )

    # process each DOI individually in order to precisely track unsuccessful calls
    for (i in seq_along(dois)) {
      if (i %% 10 == 1 || i == 1) {
        message("Processing DOI ", i, " of ", length(dois))
      }

      # skip NA or empty DOIs
      if (is.na(dois[i]) || dois[i] == "") {
        next
      }

      # call Crossref API for this specific DOI
      res <- tryCatch({
        rcrossref::cr_works(dois = dois[i])
      }, error = function(e) {
        message("Error retrieving DOI (", dois[i], "): ", e$message)
        return(list(data = NULL))
      })

      # check if the result is valid
      if (!is.null(res$data) && nrow(res$data) > 0) {
        # store the result
        doi_results[[i]] <- res$data
        doi_status$retrieved[i] <- TRUE
        fetched <- fetched + 1
      } else {
        # create an empty placeholder for this position
        doi_results[[i]] <- NULL
      }

      # check if the total limit has been reached
      if (fetched >= total) {
        message("Reached requested total of ", total, " records.")
        break
      }

      # add a small delay to avoid overwhelming the API
      Sys.sleep(0.1)
    }

    # process results based on preserve_404 flag
    if (preserve_404) {
      # get column names from a successful result
      successful_idx <- which(doi_status$retrieved)[1]

      if (!is.na(successful_idx)) {
        col_names <- colnames(doi_results[[successful_idx]])

        # create the final result list with proper structure
        ordered_results <- vector("list", length(dois))

        # fill in the successful results
        for (i in seq_along(dois)) {
          if (doi_status$retrieved[i]) {
            ordered_results[[i]] <- doi_results[[i]]
          } else {
            # create an empty row with the same structure for unsuccessful results
            empty_row <- as.data.frame(matrix(NA, nrow = 1, ncol = length(col_names)))
            colnames(empty_row) <- col_names

            # add the original DOI to the row so we know what it was
            if ("doi" %in% col_names) {
              empty_row$doi <- dois[i]
            }

            ordered_results[[i]] <- empty_row
          }
        }

        # combine all rows in the correct order
        all_works <- dplyr::bind_rows(ordered_results)

        # add the original DOI list as a column for reference
        all_works$original_doi <- dois

        # add a flag indicating if the record was successfully retrieved
        all_works$doi_retrieved <- doi_status$retrieved
      } else {
        # if no successful retrievals, return empty data frame with original DOIs
        all_works <- data.frame(doi = dois, doi_retrieved = FALSE)
        all_works$original_doi <- dois
      }
    } else {
      # otherwise only include successfully retrieved results
      successful_idx <- which(doi_status$retrieved)
      successful_results <- doi_results[successful_idx]

      if (length(successful_results) > 0) {
        all_works <- dplyr::bind_rows(successful_results)

        # add the original DOI and status information
        all_works <- dplyr::left_join(
          all_works,
          doi_status,
          by = c("doi" = "doi")
        )
      } else {
        # if no successful retrievals, return empty data frame
        all_works <- data.frame()
      }
    }

  } else {
    # implement query- and filter-based retrieval
    while (fetched < total) {
      message("Fetching records ", offset + 1L, "–", offset + per_page)
      res <- rcrossref::cr_works(
        query  = query,
        filter = filter,
        limit  = per_page,
        offset = offset
      )

      page <- res$data
      n_pg <- nrow(page)
      if (n_pg == 0L) {
        message("No more records available; stopping at ", fetched, " total.")
        break
      }

      results[[length(results) + 1L]] <- page
      fetched <- fetched + n_pg
      offset  <- offset + per_page

      if (n_pg < per_page) {
        message("Last page returned fewer than ", per_page, " records; stopping.")
        break
      }
    }

    # combine results
    all_works <- dplyr::bind_rows(results)
    if (nrow(all_works) > total) {
      all_works <- all_works[seq_len(total), ]
    }
  }

  # return results
  all_works
}
