#' Search Scopus for publications
#'
#' Queries the
#' [Elsevier Scopus Search API](https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl)
#' for peer-reviewed works matching a keyword in a specified field using
#' Scopus Query Language (SQL). A Scopus API key is required; institutional
#' access is typically needed for full metadata retrieval.
#'
#' @param query Character. Keyword or phrase to search. The value is embedded
#'   in a Scopus Query Language expression, e.g. `ABS("climate change")`.
#' @param field Character. One of `"abstract"`, `"affiliation"`, `"funder"`.
#' @param max_results Integer. Maximum number of records to return. Scopus
#'   pages in batches of 25; pagination via `start` offset is handled
#'   automatically. The Scopus Search API caps accessible results at 5,000
#'   regardless of total result count. Default `500`.
#' @param api_key Character. Scopus API key. Falls back to the value set with
#'   [pc_configure()], then the `SCOPUS_API_KEY` environment variable.
#' @param auto_fetch Logical. If `TRUE` (default), automatically calls
#'   [pc_fetch_abstracts()] after searching to backfill `authors`, `abstract`,
#'   `affiliations`, and `grant_numbers` via OpenAlex for any records where
#'   those fields are empty. Most useful when the Scopus COMPLETE view is not
#'   available (e.g. off-campus access). Set to `FALSE` to skip the backfill.
#' @param email Character. Email for the OpenAlex polite pool used by
#'   [pc_fetch_abstracts()] when `auto_fetch = TRUE`. Falls back to the value
#'   set with [pc_configure()], then the `PUBCLASSIFY_EMAIL` environment
#'   variable.
#' @param start_year Integer. Earliest publication year to include (inclusive).
#'   Appended to the query as `PUBYEAR AFT <start_year - 1>`. `NULL` (default)
#'   applies no lower year bound.
#' @param end_year Integer. Latest publication year to include (inclusive).
#'   Appended to the query as `PUBYEAR BEF <end_year + 1>`. `NULL` (default)
#'   applies no upper year bound.
#' @param doc_type Character vector. One or more document types to restrict
#'   results. Accepted values: `"article"`, `"review"`, `"conference_paper"`,
#'   `"book"`, `"book_chapter"`, `"letter"`, `"editorial"`, `"short_survey"`.
#'   Multiple values are combined with OR. `NULL` (default) returns all
#'   document types.
#' @param award_pattern Character vector. One or more optional regular
#'   expressions matched case-insensitively against `grant_numbers` to append
#'   an `award_match` logical column. A record is flagged if any grant number
#'   matches any pattern. Award IDs are often more reliable than funder name
#'   strings for identifying specific funding programmes. For example,
#'   `"^4600"` flags California DWR contracts; `c("^4600", "^W912")` flags
#'   both DWR and U.S. Army Corps contracts. `NULL` (default) skips flagging.
#'   See [pc_flag_awards()].
#' @param ... Reserved for future use.
#'
#' @return A [tibble::tibble()] with the standard pubclassify schema.
#'   See [pc_search()] for column details.
#'
#' @section COMPLETE view and institutional access:
#'   This function requests the Scopus Search API COMPLETE view, which returns
#'   author names (`dc:creator` + `author` array), an abstract snippet
#'   (`dc:description`), and funding metadata (`fund-sponsor`, `fund-no`).
#'   The COMPLETE view requires an institutional Scopus subscription; requests
#'   from non-subscriber API keys will receive only the STANDARD view fields
#'   (title, DOI, journal, date, affiliation) and an HTTP 401/403 error.
#'
#'   Note that `dc:description` is a short snippet, not the full abstract, and
#'   funding metadata may be sparse. Use [pc_fetch_abstracts()] to backfill
#'   full abstracts and funding data via OpenAlex after searching.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pc_search_scopus("California Natural Resources Agency", field = "funder")
#' pc_search_scopus("University of California", field = "affiliation")
#' }
pc_search_scopus <- function(
    query,
    field         = c("abstract", "affiliation", "funder"),
    max_results   = 500L,
    api_key       = NULL,
    auto_fetch    = TRUE,
    email         = NULL,
    start_year    = NULL,
    end_year      = NULL,
    doc_type      = NULL,
    award_pattern = NULL,
    ...
) {
  field      <- rlang::arg_match(field)
  api_key    <- api_key %||% .pc_env$scopus_key %||%
    .pc_env_key("SCOPUS_API_KEY", "Scopus")
  insttoken  <- .pc_env$scopus_insttoken %||%
    Sys.getenv("SCOPUS_INSTTOKEN", unset = NA_character_)
  if (is.na(insttoken)) insttoken <- NULL

  # Map field argument to the Scopus Query Language field code
  sql_field <- switch(field,
    abstract    = "ABS",
    affiliation = "AFFIL",
    funder      = "FUND-ALL"
  )

  # Compose the SQL query: FIELD("query")
  # Strip internal double-quotes to avoid breaking the SQL string
  scopus_query <- sprintf('%s("%s")', sql_field,
                          gsub('"', "", query, fixed = TRUE))

  # Append optional year and document-type filters
  if (!is.null(start_year)) {
    scopus_query <- paste0(scopus_query, " AND PUBYEAR AFT ", as.integer(start_year) - 1L)
  }
  if (!is.null(end_year)) {
    scopus_query <- paste0(scopus_query, " AND PUBYEAR BEF ", as.integer(end_year) + 1L)
  }
  if (!is.null(doc_type)) {
    doc_type <- rlang::arg_match(
      doc_type,
      values   = c("article", "review", "conference_paper", "book",
                   "book_chapter", "letter", "editorial", "short_survey"),
      multiple = TRUE
    )
    # Map human-readable names to Scopus DOCTYPE codes
    doc_type_map <- c(
      article          = "ar",
      review           = "re",
      conference_paper = "cp",
      book             = "bk",
      book_chapter     = "ch",
      letter           = "le",
      editorial        = "ed",
      short_survey     = "sh"
    )
    type_clauses <- paste0("DOCTYPE(", doc_type_map[doc_type], ")",
                           collapse = " OR ")
    scopus_query <- paste0(scopus_query, " AND (", type_clauses, ")")
  }

  # Per the Scopus API guide (§15.1, p.48): dc:description, the author array,
  # fund-no, and fund-sponsor are COMPLETE-view-only fields. STANDARD view only
  # returns dc:creator (first author string) and the affiliation block.
  # Wrapped in I() so httr2 does not percent-encode the commas to %2C.
  fsel_complete <- I(paste(c(
    "dc:title", "prism:doi", "dc:description", "prism:coverDate",
    "prism:publicationName", "dc:creator", "author", "affiliation",
    "fund-no", "fund-sponsor", "subtypeDescription"
  ), collapse = ","))
  fsel_standard <- I(paste(c(
    "dc:title", "prism:doi", "prism:coverDate",
    "prism:publicationName", "dc:creator", "affiliation",
    "subtypeDescription"
  ), collapse = ","))

  # Inner helper: build a paged httr2 request for a given start offset.
  # Accepts view and field_sel so the pagination loop can use whichever view
  # succeeded. Captures scopus_query and api_key from the enclosing scope.
  .build_req <- function(start, view, field_sel) {
    httr2::request("https://api.elsevier.com/content/search/scopus") |>
      httr2::req_url_query(
        query = scopus_query,
        count = 25L,
        start = start,
        field = field_sel,
        view  = view
      ) |>
      httr2::req_headers(
        "X-ELS-APIKey"    = api_key,
        "X-ELS-Insttoken" = insttoken,   # NULL is silently dropped by httr2
        "Accept"          = "application/json"
        # Scopus: identified via X-ELS-APIKey header; insttoken grants
        # off-campus COMPLETE view access.
        # https://dev.elsevier.com/tecdoc_api_authentication.html
      ) |>
      httr2::req_retry(
        max_tries    = 3L,
        is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 503L)
      )
  }

  # First request: attempt COMPLETE view, suppress auto-error to inspect status.
  # Fall back to STANDARD if the API key lacks entitlement (401/403).
  first_resp <- .build_req(0L, "COMPLETE", fsel_complete) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(first_resp) %in% c(401L, 403L)) {
    cli::cli_warn(c(
      "Scopus COMPLETE view returned HTTP {httr2::resp_status(first_resp)}.",
      "i" = "Falling back to STANDARD view: {.field authors}, {.field abstract}, \\
             and {.field funders} will be empty.",
      "i" = "For full metadata, connect via your institution's network or contact \\
             your library about an insttoken."
    ))
    active_view <- "STANDARD"
    active_fsel <- fsel_standard
    first_resp  <- httr2::req_perform(
      .build_req(0L, "STANDARD", fsel_standard)
    )
  } else {
    httr2::resp_check_status(first_resp)
    active_view <- "COMPLETE"
    active_fsel <- fsel_complete
  }

  first_page <- httr2::resp_body_json(first_resp)

  total_n <- as.integer(
    first_page[["search-results"]][["opensearch:totalResults"]] %||% 0L
  )

  if (total_n == 0L) {
    cli::cli_inform("No results found in Scopus for this query.")
    return(.pc_empty_result())
  }

  # Scopus only paginates up to 5,000 results regardless of total count
  # Warn if the query exceeds this ceiling
  scopus_cap <- 5000L
  if (total_n > scopus_cap) {
    cli::cli_warn(c(
      "Scopus returned {total_n} total result{?s} but only \
       {scopus_cap} are accessible via the Search API.",
      "i" = "Consider narrowing your query for more comprehensive coverage."
    ))
  }

  effective_max <- min(max_results, total_n, scopus_cap)

  cli::cli_inform(c(
    "Found {total_n} result{?s} in Scopus.",
    "i" = "Retrieving up to {effective_max}."
  ))

  collected   <- list(.pc_parse_scopus(first_page))
  rows_so_far <- nrow(collected[[1L]])
  start       <- 25L

  # Remaining pages
  cli::cli_progress_bar("Retrieving from Scopus", total = effective_max)
  cli::cli_progress_update(set = min(rows_so_far, effective_max))

  while (rows_so_far < effective_max) {
    resp <- httr2::req_perform(.build_req(start, active_view, active_fsel))
    page <- httr2::resp_body_json(resp)

    # An empty entry list means we've exhausted available results
    entries <- page[["search-results"]][["entry"]] %||% list()
    if (length(entries) == 0L) break

    collected   <- c(collected, list(.pc_parse_scopus(page)))
    rows_so_far <- sum(vapply(collected, nrow, integer(1L)))
    start       <- start + 25L

    cli::cli_progress_update(set = min(rows_so_far, effective_max))
  }

  cli::cli_progress_done()

  result <- do.call(rbind, collected)
  result <- result[seq_len(min(nrow(result), max_results)), ]

  if (auto_fetch) {
    result <- pc_fetch_abstracts(result, email = email)
  }

  if (!is.null(award_pattern)) {
    result <- pc_flag_awards(result, award_pattern)
  }

  result
}
