# Helper: load a JSON fixture as an R list
.load_fixture <- function(name) {
  jsonlite::read_json(
    testthat::test_path("fixtures", name),
    simplifyVector = FALSE
  )
}

# ---------------------------------------------------------------------------
# .pc_reconstruct_abstract
# ---------------------------------------------------------------------------

test_that(".pc_reconstruct_abstract() returns NA for NULL input", {
  expect_identical(pubclassify:::.pc_reconstruct_abstract(NULL), NA_character_)
})

test_that(".pc_reconstruct_abstract() returns NA for empty list", {
  expect_identical(pubclassify:::.pc_reconstruct_abstract(list()), NA_character_)
})

test_that(".pc_reconstruct_abstract() reconstructs word order from positions", {
  # "Water quality" stored as inverted index
  idx <- list(Water = list(0L), quality = list(1L))
  expect_equal(pubclassify:::.pc_reconstruct_abstract(idx), "Water quality")
})

test_that(".pc_reconstruct_abstract() handles repeated words at multiple positions", {
  # "water quantity affects water quality" — "water" appears at 0 and 3
  idx <- list(water = list(0L, 3L), quantity = list(1L), affects = list(2L), quality = list(4L))
  result <- pubclassify:::.pc_reconstruct_abstract(idx)
  expect_equal(result, "water quantity affects water quality")
})

# ---------------------------------------------------------------------------
# .pc_scopus_strlist
# ---------------------------------------------------------------------------

test_that(".pc_scopus_strlist() returns character(0) for NULL", {
  expect_equal(pubclassify:::.pc_scopus_strlist(NULL), character(0L))
})

test_that(".pc_scopus_strlist() returns character(0) for empty list", {
  expect_equal(pubclassify:::.pc_scopus_strlist(list()), character(0L))
})

test_that(".pc_scopus_strlist() passes through a character scalar", {
  expect_equal(pubclassify:::.pc_scopus_strlist("NSF"), "NSF")
})

test_that(".pc_scopus_strlist() flattens a list of strings", {
  expect_equal(pubclassify:::.pc_scopus_strlist(list("NSF", "DOE")), c("NSF", "DOE"))
})

test_that(".pc_scopus_strlist() drops empty strings", {
  expect_equal(pubclassify:::.pc_scopus_strlist(list("NSF", "", "DOE")), c("NSF", "DOE"))
  expect_equal(pubclassify:::.pc_scopus_strlist(""), character(0L))
})

test_that(".pc_scopus_strlist() drops NA values from lists", {
  expect_equal(pubclassify:::.pc_scopus_strlist(list("NSF", NULL, "DOE")), c("NSF", "DOE"))
})

# ---------------------------------------------------------------------------
# .pc_parse_openalex
# ---------------------------------------------------------------------------

test_that(".pc_parse_openalex() returns empty result for zero results", {
  page <- .load_fixture("openalex_works_empty.json")
  result <- pubclassify:::.pc_parse_openalex(page)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("doi", "title", "abstract", "year", "doc_type",
                          "authors", "affiliations", "funders",
                          "grant_numbers", "journal", "source"))
})

test_that(".pc_parse_openalex() parses standard records correctly", {
  page   <- .load_fixture("openalex_works.json")
  result <- pubclassify:::.pc_parse_openalex(page)

  expect_equal(nrow(result), 2L)
  expect_equal(result$doi[[1L]], "10.1000/test.001")
  expect_equal(result$doi[[2L]], "10.1000/test.002")
  expect_equal(result$year[[1L]], 2022L)
  expect_equal(result$journal[[1L]], "Nature Sustainability")
  expect_equal(result$source[[1L]], "openalex")
  expect_true(all(is.na(result$doc_type)))
})

test_that(".pc_parse_openalex() strips https://doi.org/ prefix", {
  page   <- .load_fixture("openalex_works.json")
  result <- pubclassify:::.pc_parse_openalex(page)
  expect_false(any(grepl("^https://doi\\.org/", result$doi[!is.na(result$doi)])))
})

test_that(".pc_parse_openalex() reconstructs abstract from inverted index", {
  page   <- .load_fixture("openalex_works.json")
  result <- pubclassify:::.pc_parse_openalex(page)
  expect_equal(result$abstract[[1L]], "This study examines salinity")
  expect_equal(result$abstract[[2L]], "Habitat restoration improves fish passage")
})

test_that(".pc_parse_openalex() parses authors and multi-institution affiliations", {
  page   <- .load_fixture("openalex_works.json")
  result <- pubclassify:::.pc_parse_openalex(page)

  authors1 <- result$authors[[1L]]
  expect_equal(authors1, c("Gaal Dornick", "Salvor Hardin"))

  # Second author has two institutions
  affils1 <- result$affiliations[[1L]]
  expect_equal(affils1[[2L]], c("USGS", "UC Davis"))
})

test_that(".pc_parse_openalex() parses funders and grant numbers", {
  page   <- .load_fixture("openalex_works.json")
  result <- pubclassify:::.pc_parse_openalex(page)

  expect_equal(result$funders[[1L]], "California Dept of Water Resources")
  expect_equal(length(result$funders[[2L]]), 0L)   # no awards
})

test_that(".pc_parse_openalex() handles sparse/missing fields gracefully", {
  page   <- .load_fixture("openalex_works_sparse.json")
  result <- pubclassify:::.pc_parse_openalex(page)

  expect_equal(nrow(result), 3L)
  # Record 1: null doi -> NA
  expect_true(is.na(result$doi[[1L]]))
  # Record 1: null abstract index -> NA
  expect_true(is.na(result$abstract[[1L]]))
  # Record 1: empty authorships -> zero-length vector
  expect_equal(length(result$authors[[1L]]), 0L)
  # Record 2: null title -> NA
  expect_true(is.na(result$title[[2L]]))
  # Record 2: null publication_year -> NA
  expect_true(is.na(result$year[[2L]]))
  # Record 2: empty abstract index -> NA
  expect_true(is.na(result$abstract[[2L]]))
  # Record 2: null primary_location -> NA journal
  expect_true(is.na(result$journal[[2L]]))
  # Record 3: repeated-position abstract reconstructs correctly
  # positions: water@0, quantity@1, affects@2, water@3, quality@4
  expect_equal(result$abstract[[3L]], "water quantity affects water quality")
})

# ---------------------------------------------------------------------------
# .pc_parse_scopus
# ---------------------------------------------------------------------------

test_that(".pc_parse_scopus() returns empty result for zero entries", {
  page   <- .load_fixture("scopus_empty.json")
  result <- pubclassify:::.pc_parse_scopus(page)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that(".pc_parse_scopus() parses COMPLETE-view records correctly", {
  page   <- .load_fixture("scopus_complete.json")
  result <- pubclassify:::.pc_parse_scopus(page)

  expect_equal(nrow(result), 2L)
  expect_equal(result$doi[[1L]], "10.1000/scopus.001")
  expect_equal(result$year[[1L]], 2022L)
  expect_equal(result$journal[[1L]], "Estuarine, Coastal and Shelf Science")
  expect_equal(result$doc_type[[1L]], "Article")
  expect_equal(result$source[[1L]], "scopus")
})

test_that(".pc_parse_scopus() uses author array when present (COMPLETE view)", {
  page   <- .load_fixture("scopus_complete.json")
  result <- pubclassify:::.pc_parse_scopus(page)
  expect_equal(result$authors[[1L]], c("Darell, B.", "Riose, B."))
})

test_that(".pc_parse_scopus() falls back to dc:creator when author array absent", {
  page   <- .load_fixture("scopus_standard.json")
  result <- pubclassify:::.pc_parse_scopus(page)
  # STANDARD view: no author array, only dc:creator
  expect_equal(result$authors[[1L]], "Darell, B.")
})

test_that(".pc_parse_scopus() parses scalar and list fund-sponsor/fund-no", {
  page   <- .load_fixture("scopus_complete.json")
  result <- pubclassify:::.pc_parse_scopus(page)

  # Record 1: scalar funder and grant
  expect_equal(result$funders[[1L]], "National Science Foundation")
  expect_equal(result$grant_numbers[[1L]], "OCE-1234567")

  # Record 2: list of funders and grants
  expect_equal(result$funders[[2L]], c("Dept of Energy", "NSF"))
  expect_equal(result$grant_numbers[[2L]], c("DE-SC0001", "DEB-9876543"))
})

test_that(".pc_parse_scopus() extracts year from prism:coverDate", {
  page   <- .load_fixture("scopus_complete.json")
  result <- pubclassify:::.pc_parse_scopus(page)
  expect_equal(result$year[[1L]], 2022L)
  expect_equal(result$year[[2L]], 2023L)
})

test_that(".pc_parse_scopus() handles sparse/missing fields gracefully", {
  page   <- .load_fixture("scopus_sparse.json")
  result <- pubclassify:::.pc_parse_scopus(page)

  expect_equal(nrow(result), 2L)
  # Record 1: null doi, null date, null creator, empty author array
  expect_true(is.na(result$doi[[1L]]))
  expect_true(is.na(result$year[[1L]]))
  expect_equal(length(result$authors[[1L]]), 0L)
  expect_equal(result$funders[[1L]], character(0L))
  expect_equal(result$grant_numbers[[1L]], character(0L))
  # Record 2: empty-string fund fields -> character(0)
  expect_equal(result$funders[[2L]], character(0L))
  expect_equal(result$grant_numbers[[2L]], character(0L))
})

# ---------------------------------------------------------------------------
# .pc_parse_crossref
# ---------------------------------------------------------------------------

test_that(".pc_parse_crossref() returns empty result for zero items", {
  empty <- list(message = list(items = list()))
  result <- pubclassify:::.pc_parse_crossref(empty)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that(".pc_parse_crossref() parses standard records correctly", {
  page   <- .load_fixture("crossref_works.json")
  result <- pubclassify:::.pc_parse_crossref(page)

  expect_equal(nrow(result), 2L)
  expect_equal(result$doi[[1L]], "10.1000/crossref.001")
  expect_equal(result$title[[1L]], "Groundwater recharge under climate change")
  expect_equal(result$year[[1L]], 2022L)
  expect_equal(result$journal[[1L]], "Water Resources Research")
  expect_equal(result$source[[1L]], "crossref")
  expect_true(all(is.na(result$doc_type)))
})

test_that(".pc_parse_crossref() strips JATS XML tags from abstract", {
  page   <- .load_fixture("crossref_works.json")
  result <- pubclassify:::.pc_parse_crossref(page)
  expect_equal(result$abstract[[1L]], "This paper examines recharge rates.")
  expect_true(is.na(result$abstract[[2L]]))
})

test_that(".pc_parse_crossref() falls back to published-online when published-print absent", {
  page   <- .load_fixture("crossref_works.json")
  result <- pubclassify:::.pc_parse_crossref(page)
  # Record 2 has published-online only
  expect_equal(result$year[[2L]], 2023L)
})

test_that(".pc_parse_crossref() parses authors and affiliations", {
  page   <- .load_fixture("crossref_works.json")
  result <- pubclassify:::.pc_parse_crossref(page)

  expect_equal(result$authors[[1L]], c("Hari Seldon", "Ebling Mis"))
  affils <- result$affiliations[[1L]]
  expect_equal(affils[[1L]], "Arizona State University")
  expect_equal(affils[[2L]], character(0L))   # Tom Chen has no affiliation
})

test_that(".pc_parse_crossref() parses funders", {
  page   <- .load_fixture("crossref_works.json")
  result <- pubclassify:::.pc_parse_crossref(page)

  expect_equal(result$funders[[1L]], c("US Geological Survey", "NSF"))
  expect_equal(result$funders[[2L]], character(0L))
})

test_that(".pc_parse_crossref() handles sparse/missing fields gracefully", {
  page   <- .load_fixture("crossref_works_sparse.json")
  result <- pubclassify:::.pc_parse_crossref(page)

  expect_equal(nrow(result), 2L)
  # Record 1: no authors, no funders
  expect_equal(result$authors[[1L]], character(0L))
  expect_equal(result$funders[[1L]], character(0L))
  # Record 2: null DOI -> NA
  expect_true(is.na(result$doi[[2L]]))
  # Record 2: empty title list -> NA
  expect_true(is.na(result$title[[2L]]))
})

# ---------------------------------------------------------------------------
# .pc_strip_xml_tags / .pc_extract_ack_elsevier
# ---------------------------------------------------------------------------

test_that(".pc_strip_xml_tags() removes tags and collapses whitespace", {
  result <- pubclassify:::.pc_strip_xml_tags("<p>Hello  <b>world</b></p>")
  expect_equal(result, "Hello world")
})

test_that(".pc_strip_xml_tags() returns NULL for tag-only input", {
  expect_null(pubclassify:::.pc_strip_xml_tags("<p></p>"))
})

test_that(".pc_extract_ack_elsevier() extracts ce:acknowledgment element", {
  xml <- "<article><ce:acknowledgment>We thank NSF.</ce:acknowledgment></article>"
  result <- pubclassify:::.pc_extract_ack_elsevier(xml)
  expect_match(result, "We thank NSF")
})

test_that(".pc_extract_ack_elsevier() falls back to JATS ack element", {
  xml <- "<article><ack><p>Funded by DOE.</p></ack></article>"
  result <- pubclassify:::.pc_extract_ack_elsevier(xml)
  expect_match(result, "Funded by DOE")
})

test_that(".pc_extract_ack_elsevier() returns NULL when neither element present", {
  expect_null(pubclassify:::.pc_extract_ack_elsevier("<article><body>text</body></article>"))
})
