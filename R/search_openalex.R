#' Search OpenAlex for publications
#'
#' Queries the [OpenAlex API](https://docs.openalex.org) for peer-reviewed
#' works matching a keyword in a specified field. No API key is required;
#' providing an email via [pc_configure()] or the `email` argument places
#' requests in the polite pool for better rate limits and higher quotas.
#'
#' @param query Character. Keyword or phrase to search.
#' @param field Character. One of `"abstract"`, `"affiliation"`, `"funder"`.
#' @param max_results Integer. Maximum number of records to return. OpenAlex
#'   uses cursor-based pagination in batches of up to 200; this is handled
#'   automatically. Default `500`.
#' @param email Character. Email for the OpenAlex polite pool. Falls back to
#'   the value set with [pc_configure()], then the `PUBCLASSIFY_EMAIL`
#'   environment variable. `NA` if none is found (still works through the slower
#'   pool).
#' @param funder_id Character. One or more OpenAlex funder entity IDs
#'   (e.g. `"https://openalex.org/F4320308027"`). When provided with
#'   `field = "funder"`, skips the name-based lookup entirely and filters works
#'   directly by these IDs. Use [pc_find_funder()] to discover IDs. Ignored
#'   when `field` is not `"funder"`.
#' @param ... Reserved for future use.
#'
#' @return A [tibble::tibble()] with the standard pubclassify schema.
#'   See [pc_search()] for column details.
#' @export
#'
#' @examples
#' \dontrun{
#' pc_search_openalex("University of California", field = "affiliation")
#'
#' # Disambiguate first, then pin the exact funder
#' pc_find_funder("Department of Water Resources")
#' pc_search_openalex(
#'   query     = "Department of Water Resources",
#'   field     = "funder",
#'   funder_id = "https://openalex.org/F4320308027"
#' )
#' }
pc_search_openalex <- function(
    query,
    field       = c("abstract", "affiliation", "funder"),
    max_results = 500L,
    email       = NULL,
    funder_id   = NULL,
    ...
) {
  field <- rlang::arg_match(field)
  email <- email %||% .pc_env$email %||%
    Sys.getenv("PUBCLASSIFY_EMAIL", unset = NA_character_)

  # Sanitize query: strip literal double-quotes to avoid confusing filter parsers
  safe_query <- gsub('"', "", query, fixed = TRUE)

  # Build the filter string.
  # For abstract and affiliation, OpenAlex has native .search filter operators.
  # For funder, no awards.funder_display_name.search filter exists; instead we
  # resolve matching funder entity IDs via the Funders endpoint first, then
  # filter works by funders.id. A pinned funder_id bypasses the name lookup.
  if (field == "funder") {
    if (!is.null(funder_id)) {
      cli::cli_inform(c(
        "i" = "Using {length(funder_id)} pinned funder ID{?s}."
      ))
      filter_str <- paste0("funders.id:", paste(funder_id, collapse = "|"))
    } else {
      cli::cli_inform(c("i" = "Resolving funder IDs via OpenAlex Funders endpoint\u2026"))
      filter_str <- .pc_openalex_funder_filter(safe_query, email)
      if (is.null(filter_str)) {
        cli::cli_inform("No funders found in OpenAlex matching this query.")
        return(.pc_empty_result())
      }
    }
  } else {
    filter_key <- switch(field,
      abstract    = "abstract.search",
      affiliation = "raw_affiliation_strings.search"
    )
    filter_str <- paste0(filter_key, ":", safe_query)
  }

  # Request only the fields the parser uses — keeps response payloads small.
  # Wrapped in I() so httr2 does not percent-encode the commas to %2C.
  select_fields <- I(paste(c(
    "doi",
    "title",
    "abstract_inverted_index",
    "publication_year",
    "authorships",
    "awards",
    "primary_location"
  ), collapse = ","))

  # Inner helper: build a single paged httr2 request for a given cursor value
  # Captures filter_str, select_fields, and email from the enclosing scope
  .build_req <- function(cursor) {
    httr2::request("https://api.openalex.org/works") |>
      httr2::req_url_query(
        filter   = filter_str,
        per_page = 200L,
        cursor   = cursor,
        select   = select_fields,
        mailto   = if (!is.na(email)) email else NULL
        # OpenAlex polite pool: identified via mailto= query parameter
        # https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication
      ) |>
      httr2::req_retry(
        max_tries    = 3L,
        is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 503L)
      )
  }

  # First request: establish total count before starting the progress bar

  first_resp <- httr2::req_perform(.build_req("*"))
  first_page <- httr2::resp_body_json(first_resp)

  total_n       <- first_page[["meta"]][["count"]] %||% 0L
  effective_max <- min(max_results, total_n)

  if (total_n == 0L) {
    cli::cli_inform("No results found in OpenAlex for this query.")
    return(.pc_empty_result())
  }

  cli::cli_inform(c(
    "Found {total_n} result{?s} in OpenAlex.",
    "i" = "Retrieving up to {effective_max}."
  ))

  collected   <- list(.pc_parse_openalex(first_page))
  cursor      <- first_page[["meta"]][["next_cursor"]]
  rows_so_far <- nrow(collected[[1L]])

  # Remaining pages

  cli::cli_progress_bar("Retrieving from OpenAlex", total = effective_max)
  cli::cli_progress_update(set = min(rows_so_far, effective_max))

  while (!is.null(cursor) && rows_so_far < max_results) {
    resp <- httr2::req_perform(.build_req(cursor))
    page <- httr2::resp_body_json(resp)

    collected   <- c(collected, list(.pc_parse_openalex(page)))
    cursor      <- page[["meta"]][["next_cursor"]]
    rows_so_far <- sum(vapply(collected, nrow, integer(1L)))

    cli::cli_progress_update(set = min(rows_so_far, effective_max))
  }

  cli::cli_progress_done()

  result <- do.call(rbind, collected)
  result[seq_len(min(nrow(result), max_results)), ]
}

#' Find funder entities in OpenAlex by name
#'
#' Searches the [OpenAlex Funders endpoint](https://docs.openalex.org/api-entities/funders)
#' and returns a table of matching entities. Use the `id` column from the
#' result to pin a specific funder in [pc_search_openalex()] via the
#' `funder_id` argument, avoiding ambiguous name matching.
#'
#' @param query Character. Name or partial name of the funder to search for.
#' @param email Character. Email for the OpenAlex polite pool. Falls back to
#'   the value set with [pc_configure()], then the `PUBCLASSIFY_EMAIL`
#'   environment variable.
#' @param max_results Integer. Maximum number of funder records to return.
#'   Default `25`.
#'
#' @return A [tibble::tibble()] with columns:
#'   \describe{
#'     \item{id}{OpenAlex funder entity ID. Pass this to
#'       [pc_search_openalex()] as `funder_id` to pin a specific funder.}
#'     \item{display_name}{Full funder name as recorded in OpenAlex.}
#'     \item{country_code}{ISO 3166-1 alpha-2 country code.}
#'     \item{works_count}{Number of works in OpenAlex attributed to this funder.}
#'     \item{description}{Brief description of the funder, if available.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Inspect matches before searching, then pin the right one
#' pc_find_funder("Department of Water Resources")
#'
#' pc_search_openalex(
#'   query     = "Department of Water Resources",
#'   field     = "funder",
#'   funder_id = "https://openalex.org/F4320332161"
#' )
#' }
pc_find_funder <- function(query, email = NULL, max_results = 25L) {
  email <- email %||% .pc_env$email %||%
    Sys.getenv("PUBCLASSIFY_EMAIL", unset = NA_character_)

  safe_query <- gsub('"', "", query, fixed = TRUE)

  resp <- httr2::request("https://api.openalex.org/funders") |>
    httr2::req_url_query(
      filter   = paste0("display_name.search:", safe_query),
      per_page = min(max_results, 200L),
      mailto   = if (!is.na(email)) email else NULL
    ) |>
    httr2::req_retry(
      max_tries    = 3L,
      is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 503L)
    ) |>
    httr2::req_perform()

  data    <- httr2::resp_body_json(resp)
  results <- data[["results"]]

  if (length(results) == 0L) {
    cli::cli_inform("No funders found in OpenAlex matching {.val {query}}.")
    return(tibble::tibble(
      id           = character(),
      display_name = character(),
      country_code = character(),
      works_count  = integer(),
      description  = character()
    ))
  }

  tibble::tibble(
    id           = vapply(results,
                          function(f) f[["id"]]           %||% NA_character_, character(1L)),
    display_name = vapply(results,
                          function(f) f[["display_name"]] %||% NA_character_, character(1L)),
    country_code = vapply(results,
                          function(f) toupper(f[["country_code"]] %||% NA_character_), character(1L)),
    works_count  = vapply(results,
                          function(f) f[["works_count"]]  %||% NA_integer_,   integer(1L)),
    description  = vapply(results,
                          function(f) f[["description"]]  %||% NA_character_, character(1L))
  )
}

# Resolve OpenAlex funder entity IDs for a text query, returning a
# funders.id filter string suitable for use in the works endpoint.
# Returns NULL if no matching funders are found.
.pc_openalex_funder_filter <- function(query, email) {
  resp <- httr2::request("https://api.openalex.org/funders") |>
    httr2::req_url_query(
      filter   = paste0("display_name.search:", query),
      per_page = 25L,
      mailto   = if (!is.na(email)) email else NULL
    ) |>
    httr2::req_retry(
      max_tries    = 3L,
      is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 503L)
    ) |>
    httr2::req_perform()

  data    <- httr2::resp_body_json(resp)
  results <- data[["results"]]

  if (length(results) > 0L) {
    # Found registered funder entities — show the user exactly what was matched
    # (name + country code) so they can spot false positives before results load.
    cli::cli_inform(c("i" = "Matched {length(results)} funder entit{?y/ies} in OpenAlex:"))
    for (f in results) {
      country <- f[["country_code"]] %||% "??"
      cli::cli_bullets(c(" " = "{f[['display_name']]} [{toupper(country)}]"))
    }

    # Filter works by their OpenAlex IDs.
    # This is the most precise path: IDs are stable identifiers regardless of
    # how the funder name appears in individual publications.
    ids <- vapply(results, function(f) f[["id"]], character(1L))
    return(paste0("funders.id:", paste(ids, collapse = "|")))
  }

  # No funder entity found — fall back to exact name matching in the awards
  # field on works. This path handles funding bodies not tracked as OpenAlex
  # Funder entities (e.g. smaller government agencies). The match is
  # case-insensitive but exact, so the query must match the funder name as
  # it appears in the publication's structured award metadata.
  cli::cli_inform(c(
    "!" = "No funder entity found in OpenAlex for {.val {query}}.",
    "i" = "Falling back to exact name match in awards data."
  ))
  paste0("awards.funder_display_name:", query)
}
