# Internal parsers: raw API response (parsed JSON list) → standardised tibble.
#
# Each function processes a single page of results from its respective API
# and returns a tibble conforming to the pubclassify result schema defined
# by .pc_empty_result(). They are called inside the pagination loops in the
# source-specific search functions.

#' @noRd
.pc_parse_openalex <- function(response) {
  results <- response[["results"]]
  if (length(results) == 0L) return(.pc_empty_result())

  rows <- lapply(results, function(item) {
    # doi: strip the "https://doi.org/" prefix if present
    doi <- .pc_na_chr(item[["doi"]])
    if (!is.na(doi)) doi <- sub("^https://doi\\.org/", "", doi)

    # abstract: OpenAlex stores abstracts as an inverted index
    # (word → list of positions). Reconstruct by sorting positions.
    abstract <- .pc_reconstruct_abstract(item[["abstract_inverted_index"]])

    # authors and affiliations come from the authorships array
    authorships  <- item[["authorships"]] %||% list()
    authors      <- vapply(authorships,
                           function(a) a[["author"]][["display_name"]] %||% NA_character_,
                           character(1L))
    affiliations <- lapply(authorships, function(a) {
      insts <- a[["institutions"]] %||% list()
      vapply(insts, function(i) i[["display_name"]] %||% NA_character_, character(1L))
    })

    # OpenAlex renamed "grants" to "awards" in the works schema
    funders <- vapply(
      item[["awards"]] %||% list(),
      function(g) g[["funder_display_name"]] %||% NA_character_,
      character(1L)
    )

    tibble::tibble(
      doi           = doi,
      title         = .pc_na_chr(item[["title"]]),
      abstract      = abstract,
      year          = .pc_na_int(item[["publication_year"]]),
      doc_type      = NA_character_,
      authors       = list(authors),
      affiliations  = list(affiliations),
      funders       = list(funders),
      grant_numbers = list(character(0L)),  # populated later via OpenAlex awards
      journal       = .pc_na_chr(item[["primary_location"]][["source"]][["display_name"]]),
      source        = "openalex"
    )
  })

  do.call(rbind, rows)
}

# Reconstruct a plain-text abstract from OpenAlex's inverted index format.
# The inverted index is a named list: word -> integer vector of positions.
# Returns NA_character_ if the index is NULL or empty.
#' @noRd
.pc_reconstruct_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0L) {
    return(NA_character_)
  }
  words <- names(inverted_index)
  positions <- unlist(inverted_index, use.names = FALSE)
  word_vec <- rep(words, times = lengths(inverted_index))
  paste(word_vec[order(positions)], collapse = " ")
}

# Coerce a Scopus field value to a plain character vector.
# Scopus returns scalar fields as a single string and multi-value fields as a
# list of strings. Both arrive as character or list after jsonlite parsing.
#' @noRd
.pc_scopus_strlist <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0L))
  if (is.character(x)) return(x[nzchar(x)])   # drop empty strings
  vals <- vapply(x, function(v) if (is.null(v)) NA_character_ else as.character(v),
                 character(1L))
  vals[!is.na(vals) & nzchar(vals)]
}

#' @noRd
.pc_parse_scopus <- function(response) {
  entries <- response[["search-results"]][["entry"]]
  if (length(entries) == 0L) return(.pc_empty_result())

  rows <- lapply(entries, function(item) {
    # year: extract from prism:coverDate (format "YYYY-MM-DD")
    cover_date <- .pc_na_chr(item[["prism:coverDate"]])
    year <- if (!is.na(cover_date)) {
      .pc_na_int(as.integer(substr(cover_date, 1L, 4L)))
    } else {
      NA_integer_
    }

    # authors: Scopus COMPLETE view returns all authors in the author array
    # (each with an authname field). dc:creator holds only the first author
    # and is available in both views — use it as a fallback when the author
    # array is absent (STANDARD view).
    first_author <- .pc_na_chr(item[["dc:creator"]])
    author_list  <- item[["author"]] %||% list()
    authors <- if (length(author_list) > 0L) {
      vapply(author_list,
             function(a) a[["authname"]] %||% NA_character_,
             character(1L))
    } else {
      if (!is.na(first_author)) first_author else character(0L)
    }

    # affiliations: top-level affiliation block in search results
    affil_list   <- item[["affiliation"]] %||% list()
    affiliations <- list(vapply(affil_list,
                                function(a) a[["affilname"]] %||% NA_character_,
                                character(1L)))

    # funders and grant numbers: COMPLETE view only (§15.1, p.48).
    # Scopus may return these as a single string or a list of strings.
    funders       <- list(.pc_scopus_strlist(item[["fund-sponsor"]]))
    grant_numbers <- list(.pc_scopus_strlist(item[["fund-no"]]))

    tibble::tibble(
      doi           = .pc_na_chr(item[["prism:doi"]]),
      title         = .pc_na_chr(item[["dc:title"]]),
      abstract      = .pc_na_chr(item[["dc:description"]]),
      year          = year,
      doc_type      = .pc_na_chr(item[["subtypeDescription"]]),
      authors       = list(authors),
      affiliations  = affiliations,
      funders       = funders,
      grant_numbers = grant_numbers,
      journal       = .pc_na_chr(item[["prism:publicationName"]]),
      source        = "scopus"
    )
  })

  do.call(rbind, rows)
}

#' @noRd
.pc_parse_crossref <- function(response) {
  items <- response[["message"]][["items"]]
  if (length(items) == 0L) return(.pc_empty_result())

  rows <- lapply(items, function(item) {
    # year: prefer published-print, fall back to published-online
    year_parts <- item[["published-print"]][["date-parts"]][[1L]] %||%
      item[["published-online"]][["date-parts"]][[1L]]
    year <- if (!is.null(year_parts)) .pc_na_int(year_parts[[1L]]) else NA_integer_

    # abstract: may be absent or wrapped in JATS XML tags — strip them
    abstract_raw <- .pc_na_chr(item[["abstract"]])
    abstract <- if (!is.na(abstract_raw)) {
      gsub("<[^>]+>", "", abstract_raw)
    } else {
      NA_character_
    }

    author_list  <- item[["author"]] %||% list()
    authors      <- vapply(author_list, function(a) {
      given  <- a[["given"]]  %||% ""
      family <- a[["family"]] %||% ""
      trimws(paste(given, family))
    }, character(1L))

    affiliations <- list(lapply(author_list, function(a) {
      vapply(a[["affiliation"]] %||% list(),
             function(af) af[["name"]] %||% NA_character_,
             character(1L))
    }))

    funders <- list(vapply(
      item[["funder"]] %||% list(),
      function(f) f[["name"]] %||% NA_character_,
      character(1L)
    ))

    title_parts   <- item[["title"]]          %||% list()
    journal_parts <- item[["container-title"]] %||% list()

    tibble::tibble(
      doi           = .pc_na_chr(item[["DOI"]]),
      title         = .pc_na_chr(if (length(title_parts)   > 0L) title_parts[[1L]]   else NULL),
      abstract      = abstract,
      year          = year,
      doc_type      = NA_character_,
      authors       = list(authors),
      affiliations  = affiliations,
      funders       = funders,
      grant_numbers = list(character(0L)),  # populated later
      journal       = .pc_na_chr(if (length(journal_parts) > 0L) journal_parts[[1L]] else NULL),
      source        = "crossref"
    )
  })

  do.call(rbind, rows)
}
