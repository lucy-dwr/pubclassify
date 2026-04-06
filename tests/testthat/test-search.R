# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.load_fixture <- function(name) {
  jsonlite::read_json(
    testthat::test_path("fixtures", name),
    simplifyVector = FALSE
  )
}

# Build a mock httr2 response carrying a JSON fixture body.
.mock_raw <- function(fixture_name, status = 200L) {
  path    <- testthat::test_path("fixtures", fixture_name)
  raw_body <- as.raw(iconv(paste(readLines(path, warn = FALSE),
                                 collapse = "\n"),
                            to = "UTF-8", toRaw = TRUE)[[1L]])
  httr2::response(
    status_code = status,
    headers     = list(`Content-Type` = "application/json"),
    body        = raw_body
  )
}

# ---------------------------------------------------------------------------
# pc_search() — input validation (no HTTP required)
# ---------------------------------------------------------------------------

test_that("pc_search() rejects empty query strings", {
  expect_error(pc_search("",    field = "abstract"), "non-empty")
  expect_error(pc_search("  ", field = "abstract"), "non-empty")
})

test_that("pc_search() rejects invalid field values", {
  expect_error(pc_search("climate", field = "title"))
})

test_that("pc_search() rejects invalid source values", {
  expect_error(pc_search("climate", field = "abstract", sources = "pubmed"))
})

test_that("pc_search() returns a tibble with the standard schema", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) .mock_raw("openalex_works_empty.json"),
      {
        result <- pc_search("climate", field = "abstract", sources = "openalex")
        expect_s3_class(result, "tbl_df")
        expect_named(result, c("doi", "title", "abstract", "year", "doc_type",
                                "authors", "affiliations", "funders",
                                "grant_numbers", "journal", "source"))
      }
    )
  })
})

# ---------------------------------------------------------------------------
# pc_search_openalex() — abstract and affiliation fields
# ---------------------------------------------------------------------------

test_that("pc_search_openalex() returns empty tibble when API returns zero results", {
  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_works_empty.json"),
    {
      result <- pc_search_openalex("xyzzy_no_results", field = "abstract")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0L)
    }
  )
})

test_that("pc_search_openalex() parses a single-page result correctly", {
  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_works.json"),
    {
      result <- pc_search_openalex("delta smelt", field = "abstract")
      expect_equal(nrow(result), 2L)
      expect_equal(result$source[[1L]], "openalex")
      expect_false(any(grepl("^https://doi\\.org/", result$doi[!is.na(result$doi)])))
    }
  )
})

test_that("pc_search_openalex() respects max_results cap", {
  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_works.json"),
    {
      result <- pc_search_openalex("climate", field = "abstract",
                                   max_results = 1L)
      expect_equal(nrow(result), 1L)
    }
  )
})

test_that("pc_search_openalex() paginates until max_results reached", {
  # Fixture has 2 records and next_cursor = null (single page).
  # To simulate pagination we return page1 first (with cursor), then page2.
  page1_raw <- .load_fixture("openalex_works.json")
  page1_raw[["meta"]][["count"]]       <- 4L      # tell the function 4 total exist
  page1_raw[["meta"]][["next_cursor"]] <- "cursor_page2"

  page2_raw <- .load_fixture("openalex_works.json")
  page2_raw[["meta"]][["next_cursor"]] <- NULL

  pages    <- list(page1_raw, page2_raw)
  call_idx <- 0L

  httr2::with_mocked_responses(
    function(req) {
      call_idx <<- call_idx + 1L
      raw_body <- as.raw(iconv(jsonlite::toJSON(pages[[call_idx]], auto_unbox = TRUE),
                               to = "UTF-8", toRaw = TRUE)[[1L]])
      httr2::response(status_code = 200L,
                      headers = list(`Content-Type` = "application/json"),
                      body    = raw_body)
    },
    {
      result <- pc_search_openalex("climate", field = "abstract",
                                   max_results = 4L)
      expect_equal(nrow(result), 4L)
    }
  )
})

# ---------------------------------------------------------------------------
# pc_search_openalex() — funder field
# ---------------------------------------------------------------------------

test_that("pc_search_openalex() returns empty when no funder entity found", {
  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_funders_empty.json"),
    {
      result <- pc_search_openalex("nonexistent funder", field = "funder")
      expect_equal(nrow(result), 0L)
    }
  )
})

test_that("pc_search_openalex() resolves funder IDs and returns works", {
  call_idx <- 0L
  httr2::with_mocked_responses(
    function(req) {
      call_idx <<- call_idx + 1L
      if (call_idx == 1L) .mock_raw("openalex_funders.json")
      else                .mock_raw("openalex_works.json")
    },
    {
      result <- pc_search_openalex("California Water", field = "funder")
      expect_gt(nrow(result), 0L)
    }
  )
})

test_that("pc_search_openalex() uses pinned funder_id without lookup", {
  httr2::with_mocked_responses(
    function(req) {
      # Should not hit the funders endpoint; only the works endpoint
      expect_false(grepl("openalex.org/funders", req$url))
      .mock_raw("openalex_works.json")
    },
    {
      result <- pc_search_openalex(
        "California Water",
        field     = "funder",
        funder_id = "https://openalex.org/F4320308027"
      )
      expect_gt(nrow(result), 0L)
    }
  )
})

# ---------------------------------------------------------------------------
# pc_find_funder()
# ---------------------------------------------------------------------------

test_that("pc_find_funder() returns empty tibble when no match", {
  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_funders_empty.json"),
    {
      result <- pc_find_funder("nonexistent")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0L)
      expect_named(result, c("id", "display_name", "country_code",
                              "works_count", "description"))
    }
  )
})

test_that("pc_find_funder() parses funder records correctly", {
  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_funders.json"),
    {
      result <- pc_find_funder("California Water")
      expect_equal(nrow(result), 2L)
      expect_equal(result$id[[1L]], "https://openalex.org/F4320308027")
      expect_equal(result$display_name[[1L]],
                   "California Department of Water Resources")
      expect_equal(result$country_code[[1L]], "US")   # uppercased
      expect_equal(result$works_count[[1L]], 412L)
      # Second record: null description -> NA
      expect_true(is.na(result$description[[2L]]))
    }
  )
})

# ---------------------------------------------------------------------------
# pc_search_scopus() — COMPLETE view (happy path)
# ---------------------------------------------------------------------------

test_that("pc_search_scopus() returns empty tibble when API returns zero results", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) .mock_raw("scopus_empty.json"),
      {
        result <- pc_search_scopus("xyzzy", field = "abstract",
                                   auto_fetch = FALSE)
        expect_s3_class(result, "tbl_df")
        expect_equal(nrow(result), 0L)
      }
    )
  })
})

test_that("pc_search_scopus() parses COMPLETE-view results", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) .mock_raw("scopus_complete.json"),
      {
        result <- pc_search_scopus("estuarine", field = "abstract",
                                   auto_fetch = FALSE)
        expect_equal(nrow(result), 2L)
        expect_equal(result$source[[1L]], "scopus")
        expect_equal(result$doi[[1L]], "10.1000/scopus.001")
        expect_equal(result$authors[[1L]], c("Darell, B.", "Riose, B."))
        expect_equal(result$funders[[1L]], "National Science Foundation")
        expect_equal(result$grant_numbers[[1L]], "OCE-1234567")
      }
    )
  })
})

# ---------------------------------------------------------------------------
# pc_search_scopus() — COMPLETE -> STANDARD fallback on 401/403
# ---------------------------------------------------------------------------

test_that("pc_search_scopus() falls back to STANDARD view on 401", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    call_idx <- 0L
    httr2::with_mocked_responses(
      function(req) {
        call_idx <<- call_idx + 1L
        if (call_idx == 1L) {
          # First call: COMPLETE view request -> 401
          httr2::response(status_code = 401L,
                          headers = list(`Content-Type` = "application/json"),
                          body    = raw(0L))
        } else {
          # Second call: STANDARD view request -> success
          .mock_raw("scopus_standard.json")
        }
      },
      {
        expect_warning(
          result <- pc_search_scopus("estuarine", field = "abstract",
                                     auto_fetch = FALSE),
          "STANDARD"
        )
        expect_equal(nrow(result), 2L)
        # STANDARD view: no abstract, no funders, no full author list
        expect_true(all(is.na(result$abstract)))
        expect_equal(result$funders[[1L]], character(0L))
      }
    )
  })
})

test_that("pc_search_scopus() falls back to STANDARD view on 403", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    call_idx <- 0L
    httr2::with_mocked_responses(
      function(req) {
        call_idx <<- call_idx + 1L
        if (call_idx == 1L)
          httr2::response(status_code = 403L,
                          headers = list(`Content-Type` = "application/json"),
                          body    = raw(0L))
        else
          .mock_raw("scopus_standard.json")
      },
      {
        expect_warning(
          pc_search_scopus("estuarine", field = "abstract", auto_fetch = FALSE),
          "STANDARD"
        )
      }
    )
  })
})

# ---------------------------------------------------------------------------
# pc_search_scopus() — optional filters
# ---------------------------------------------------------------------------

test_that("pc_search_scopus() appends year filters to the query", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) {
        decoded_url <- utils::URLdecode(req$url)
        expect_match(decoded_url, "PUBYEAR AFT 2019")
        expect_match(decoded_url, "PUBYEAR BEF 2024")
        .mock_raw("scopus_complete.json")
      },
      {
        pc_search_scopus("climate", field = "abstract",
                         start_year = 2020L, end_year = 2023L,
                         auto_fetch = FALSE)
      }
    )
  })
})

test_that("pc_search_scopus() appends doc_type filter to the query", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) {
        expect_match(utils::URLdecode(req$url), "DOCTYPE")
        .mock_raw("scopus_complete.json")
      },
      {
        pc_search_scopus("climate", field = "abstract",
                         doc_type = "article", auto_fetch = FALSE)
      }
    )
  })
})

test_that("pc_search_scopus() appends award_match column when award_pattern set", {
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) .mock_raw("scopus_complete.json"),
      {
        result <- pc_search_scopus("estuarine", field = "abstract",
                                   award_pattern = "^OCE",
                                   auto_fetch = FALSE)
        expect_true("award_match" %in% names(result))
        expect_true(result$award_match[[1L]])    # OCE-1234567 matches ^OCE
        expect_false(result$award_match[[2L]])   # DE-SC0001 / DEB does not
      }
    )
  })
})

test_that("pc_search_scopus() warns when total results exceed Scopus 5000 cap", {
  big_response <- .load_fixture("scopus_complete.json")
  big_response[["search-results"]][["opensearch:totalResults"]] <- "6000"

  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) {
        raw_body <- as.raw(iconv(jsonlite::toJSON(big_response, auto_unbox = TRUE),
                                 to = "UTF-8", toRaw = TRUE)[[1L]])
        httr2::response(status_code = 200L,
                        headers = list(`Content-Type` = "application/json"),
                        body    = raw_body)
      },
      {
        expect_warning(
          pc_search_scopus("climate", field = "abstract", auto_fetch = FALSE),
          "5000"
        )
      }
    )
  })
})
