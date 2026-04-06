# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.load_fixture <- function(name) {
  jsonlite::read_json(
    testthat::test_path("fixtures", name),
    simplifyVector = FALSE
  )
}

.mock_raw <- function(fixture_name, status = 200L) {
  path     <- testthat::test_path("fixtures", fixture_name)
  raw_body <- as.raw(iconv(paste(readLines(path, warn = FALSE),
                                 collapse = "\n"),
                            to = "UTF-8", toRaw = TRUE)[[1L]])
  httr2::response(
    status_code = status,
    headers     = list(`Content-Type` = "application/json"),
    body        = raw_body
  )
}

# Build a minimal standard-schema tibble for testing without HTTP calls.
.make_pubs <- function(n = 2L,
                       dois        = paste0("10.1000/x.", seq_len(n)),
                       sources     = rep("openalex", n),
                       abstracts   = rep("Some abstract.", n),
                       authors     = rep(list(c("A Author")), n),
                       affiliations = rep(list(list(c("Uni A"))), n),
                       funders     = rep(list(c("NSF")), n),
                       grant_numbers = rep(list(character(0L)), n),
                       years       = seq_len(n) + 2019L) {
  tibble::tibble(
    doi           = dois,
    title         = paste("Title", seq_len(n)),
    abstract      = abstracts,
    year          = as.integer(years),
    doc_type      = NA_character_,
    authors       = authors,
    affiliations  = affiliations,
    funders       = funders,
    grant_numbers = grant_numbers,
    journal       = "Test Journal",
    source        = sources
  )
}

# ---------------------------------------------------------------------------
# pc_deduplicate()
# ---------------------------------------------------------------------------

test_that("pc_deduplicate() passes through a zero-row tibble unchanged", {
  empty <- .pc_empty_result()
  result <- pc_deduplicate(empty)
  expect_equal(nrow(result), 0L)
})

test_that("pc_deduplicate() keeps unique DOIs unchanged", {
  pubs   <- .make_pubs(3L)
  result <- pc_deduplicate(pubs)
  expect_equal(nrow(result), 3L)
})

test_that("pc_deduplicate() removes exact duplicate DOIs", {
  pubs   <- .make_pubs(2L, dois = c("10.1000/dup", "10.1000/dup"))
  result <- pc_deduplicate(pubs)
  expect_equal(nrow(result), 1L)
})

test_that("pc_deduplicate() prefers the record with a non-NA abstract", {
  pubs <- .make_pubs(
    2L,
    dois      = c("10.1000/dup", "10.1000/dup"),
    sources   = c("crossref", "openalex"),
    abstracts = c(NA_character_, "Has abstract.")
  )
  result <- pc_deduplicate(pubs)
  expect_equal(result$abstract[[1L]], "Has abstract.")
})

test_that("pc_deduplicate() breaks abstract tie by preferring more affiliations", {
  # Both have abstracts; record 2 has more affiliations.
  pubs <- .make_pubs(
    2L,
    dois         = c("10.1000/dup", "10.1000/dup"),
    sources      = c("crossref", "scopus"),
    affiliations = list(list(c("A")), list(c("A"), c("B")))
  )
  result <- pc_deduplicate(pubs)
  expect_equal(length(result$affiliations[[1L]]), 2L)
})

test_that("pc_deduplicate() breaks affiliation tie by source priority (openalex > scopus > crossref)", {
  pubs <- .make_pubs(
    3L,
    dois    = c("10.1000/dup", "10.1000/dup", "10.1000/dup"),
    sources = c("crossref", "scopus", "openalex")
  )
  result <- pc_deduplicate(pubs)
  expect_equal(result$source[[1L]], "openalex")
})

test_that("pc_deduplicate() always retains records without a DOI", {
  pubs <- .make_pubs(3L, dois = c(NA_character_, "10.1000/x.2", NA_character_))
  result <- pc_deduplicate(pubs)
  # The two NA-DOI rows are kept as-is; the one with a DOI is also kept
  expect_equal(nrow(result), 3L)
  expect_equal(sum(is.na(result$doi)), 2L)
})

test_that("pc_deduplicate() handles all-NA-DOI input", {
  pubs <- .make_pubs(2L, dois = c(NA_character_, NA_character_))
  result <- pc_deduplicate(pubs)
  expect_equal(nrow(result), 2L)
})

# ---------------------------------------------------------------------------
# pc_combine()
# ---------------------------------------------------------------------------

test_that("pc_combine() aborts when called with no arguments", {
  expect_error(pc_combine(), "at least one")
})

test_that("pc_combine() row-binds two result tibbles", {
  a      <- .make_pubs(2L, dois = c("10.1000/a.1", "10.1000/a.2"))
  b      <- .make_pubs(2L, dois = c("10.1000/b.1", "10.1000/b.2"))
  result <- pc_combine(a, b, deduplicate = FALSE)
  expect_equal(nrow(result), 4L)
})

test_that("pc_combine() deduplicates by default", {
  a      <- .make_pubs(2L, dois = c("10.1000/shared", "10.1000/a.only"))
  b      <- .make_pubs(2L, dois = c("10.1000/shared", "10.1000/b.only"))
  result <- pc_combine(a, b)
  expect_equal(nrow(result), 3L)
})

test_that("pc_combine() skips deduplication when deduplicate = FALSE", {
  a      <- .make_pubs(1L, dois = "10.1000/shared")
  b      <- .make_pubs(1L, dois = "10.1000/shared")
  result <- pc_combine(a, b, deduplicate = FALSE)
  expect_equal(nrow(result), 2L)
})

test_that("pc_combine() works with a single tibble", {
  a      <- .make_pubs(2L)
  result <- pc_combine(a, deduplicate = FALSE)
  expect_equal(nrow(result), 2L)
})

# ---------------------------------------------------------------------------
# pc_flag_awards()
# ---------------------------------------------------------------------------

test_that("pc_flag_awards() adds award_match column", {
  pubs   <- .make_pubs(1L, grant_numbers = list("4600012345"))
  result <- pc_flag_awards(pubs, "^4600")
  expect_true("award_match" %in% names(result))
})

test_that("pc_flag_awards() flags rows where any grant matches any pattern", {
  pubs <- .make_pubs(
    3L,
    grant_numbers = list("4600012345", "W91200001", character(0L))
  )
  result <- pc_flag_awards(pubs, c("^4600", "^W912"))
  expect_equal(result$award_match, c(TRUE, TRUE, FALSE))
})

test_that("pc_flag_awards() is case-insensitive", {
  pubs   <- .make_pubs(1L, grant_numbers = list("oce-1234567"))
  result <- pc_flag_awards(pubs, "^OCE")
  expect_true(result$award_match[[1L]])
})

test_that("pc_flag_awards() returns FALSE for rows with no grant numbers", {
  pubs   <- .make_pubs(1L, grant_numbers = list(character(0L)))
  result <- pc_flag_awards(pubs, "^4600")
  expect_false(result$award_match[[1L]])
})

# ---------------------------------------------------------------------------
# pc_fetch_abstracts()
# ---------------------------------------------------------------------------

test_that("pc_fetch_abstracts() returns input unchanged when all records already filled", {
  pubs <- .make_pubs(2L)
  # All records already have abstract, authors, affiliations; not scopus source
  httr2::with_mocked_responses(
    function(req) stop("No HTTP call should be made"),
    {
      result <- pc_fetch_abstracts(pubs)
      expect_equal(result, pubs)
    }
  )
})

test_that("pc_fetch_abstracts() fills missing abstract from OpenAlex", {
  pubs <- .make_pubs(
    1L,
    dois      = "10.1000/test.001",    # matches fixture DOI
    sources   = "crossref",
    abstracts = NA_character_,
    authors   = list(character(0L)),
    affiliations = list(list())
  )

  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_works.json"),
    {
      result <- pc_fetch_abstracts(pubs)
      expect_false(is.na(result$abstract[[1L]]))
    }
  )
})

test_that("pc_fetch_abstracts() does not overwrite an existing abstract", {
  pubs <- .make_pubs(
    1L,
    dois      = "10.1000/test.001",
    sources   = "openalex",
    abstracts = "Existing abstract.",
    authors   = list(c("Existing Author")),
    affiliations = list(list(c("Existing Uni")))
  )

  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_works.json"),
    {
      result <- pc_fetch_abstracts(pubs)
      expect_equal(result$abstract[[1L]], "Existing abstract.")
    }
  )
})

test_that("pc_fetch_abstracts() merges grant numbers from OpenAlex", {
  pubs <- .make_pubs(
    1L,
    dois          = "10.1000/test.001",
    sources       = "scopus",
    abstracts     = NA_character_,
    authors       = list(character(0L)),
    affiliations  = list(list()),
    grant_numbers = list("EXISTING-001")
  )

  httr2::with_mocked_responses(
    function(req) .mock_raw("openalex_works.json"),
    {
      result <- pc_fetch_abstracts(pubs)
      # fixture record has award_id = "4600012345"; should be unioned
      expect_true("EXISTING-001" %in% result$grant_numbers[[1L]])
      expect_true("4600012345"   %in% result$grant_numbers[[1L]])
    }
  )
})

test_that("pc_fetch_abstracts() skips records without a DOI", {
  pubs <- .make_pubs(
    1L,
    dois     = NA_character_,
    sources  = "scopus",
    abstracts = NA_character_,
    authors  = list(character(0L)),
    affiliations = list(list())
  )

  httr2::with_mocked_responses(
    function(req) stop("No HTTP call should be made for NA DOI"),
    {
      result <- pc_fetch_abstracts(pubs)
      expect_true(is.na(result$abstract[[1L]]))
    }
  )
})

# ---------------------------------------------------------------------------
# pc_fetch_acknowledgments()
# ---------------------------------------------------------------------------

test_that("pc_fetch_acknowledgments() adds acknowledgment column", {
  pubs <- .make_pubs(1L)
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    elsevier_xml <- "<article><ce:acknowledgment>We thank NSF.</ce:acknowledgment></article>"
    raw_xml      <- as.raw(iconv(elsevier_xml, to = "UTF-8", toRaw = TRUE)[[1L]])
    httr2::with_mocked_responses(
      function(req) httr2::response(
        status_code = 200L,
        headers     = list(`Content-Type` = "application/xml"),
        body        = raw_xml
      ),
      {
        result <- pc_fetch_acknowledgments(pubs)
        expect_true("acknowledgment" %in% names(result))
        expect_match(result$acknowledgment[[1L]], "We thank NSF")
      }
    )
  })
})

test_that("pc_fetch_acknowledgments() leaves acknowledgment NA when API returns non-200", {
  pubs <- .make_pubs(1L)
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) httr2::response(
        status_code = 404L,
        headers     = list(`Content-Type` = "application/json"),
        body        = raw(0L)
      ),
      {
        result <- pc_fetch_acknowledgments(pubs)
        expect_true(is.na(result$acknowledgment[[1L]]))
      }
    )
  })
})

test_that("pc_fetch_acknowledgments() skips Elsevier call when no API key", {
  pubs <- .make_pubs(1L)
  withr::with_envvar(c(SCOPUS_API_KEY = NA_character_), {
    httr2::with_mocked_responses(
      function(req) stop("No HTTP call should occur without API key"),
      {
        expect_message(
          result <- pc_fetch_acknowledgments(pubs, api_key = NULL),
          "No Scopus API key"
        )
        expect_true(is.na(result$acknowledgment[[1L]]))
      }
    )
  })
})

test_that("pc_fetch_acknowledgments() skips records without a DOI", {
  pubs <- .make_pubs(1L, dois = NA_character_)
  withr::with_envvar(c(SCOPUS_API_KEY = "fake"), {
    httr2::with_mocked_responses(
      function(req) stop("No HTTP call for NA DOI"),
      {
        result <- pc_fetch_acknowledgments(pubs)
        expect_true(is.na(result$acknowledgment[[1L]]))
      }
    )
  })
})
