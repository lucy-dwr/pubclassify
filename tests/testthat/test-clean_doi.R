test_that("clean_doi handles NULL input correctly", {
  # test with warnings disabled
  testthat::expect_null(clean_doi(NULL))
  testthat::expect_no_warning(clean_doi(NULL))

  # test with warnings enabled
  testthat::expect_null(clean_doi(NULL, warn = TRUE))
  testthat::expect_warning(clean_doi(NULL, warn = TRUE), "NULL input provided to clean_doi()")
})


test_that("clean_doi handles NA input correctly", {
  # try a single NA
  testthat::expect_identical(clean_doi(NA_character_), NA_character_)
  testthat::expect_no_warning(clean_doi(NA_character_))

  # try a vector of NAs
  na_vector <- rep(NA_character_, 3)
  testthat::expect_identical(clean_doi(na_vector), na_vector)
  testthat::expect_no_warning(clean_doi(na_vector))

  # try with warnings enabled
  testthat::expect_warning(
    clean_doi(NA_character_, warn = TRUE),
    "All NA input provided to clean_doi()"
  )

  testthat::expect_warning(
    clean_doi(na_vector, warn = TRUE),
    "All NA input provided to clean_doi()"
  )
})


test_that("clean_doi handles empty strings correctly", {
  # empty string should be converted to NA
  testthat::expect_identical(clean_doi(""), NA_character_)

  # test a vector with empty strings
  mixed_vector <- c("10.1234/test", "", "10.5678/another")
  expected <- c("10.1234/test", NA_character_, "10.5678/another")
  testthat::expect_identical(clean_doi(mixed_vector), expected)
})


test_that("clean_doi removes whitespace correctly", {
  # try with leading and trailing whitespace
  testthat::expect_equal(clean_doi(" 10.1234/test "), "10.1234/test")

  # try with mixed whitespace cases
  whitespace_cases <- c(" 10.1234/test", "10.1234/test ", "  10.1234/test  ")
  expected <- rep("10.1234/test", 3)
  testthat::expect_equal(clean_doi(whitespace_cases), expected)
})


test_that("clean_doi removes quotation marks correctly", {
  # try with quotation marks at beginning and end
  testthat::expect_equal(clean_doi("\"10.1234/test\""), "10.1234/test")

  # try with quotation marks in different positions
  quote_cases <- c("\"10.1234/test", "10.1234/test\"", "\"10.1234/test\"")
  expected <- rep("10.1234/test", 3)
  testthat::expect_equal(clean_doi(quote_cases), expected)

  # try with embedded quotation marks
  testthat::expect_equal(clean_doi("10.1234/test\"ing"), "10.1234/testing")
})


test_that("clean_doi removes URL prefixes correctly", {
  # use standard DOI URL
  testthat::expect_equal(clean_doi("https://doi.org/10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("http://doi.org/10.1234/test"), "10.1234/test")

  # use www variant
  testthat::expect_equal(clean_doi("https://www.doi.org/10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("http://www.doi.org/10.1234/test"), "10.1234/test")

  # use dx variant
  testthat::expect_equal(clean_doi("https://dx.doi.org/10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("http://dx.doi.org/10.1234/test"), "10.1234/test")
})


test_that("clean_doi handles missing slash after .org correctly", {
  testthat::expect_equal(clean_doi("https://doi.org10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("http://doi.org10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("https://www.doi.org10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("http://dx.doi.org10.1234/test"), "10.1234/test")
})


test_that("clean_doi removes 'doi:' prefix correctly", {
  # try standard doi: prefix
  testthat::expect_equal(clean_doi("doi:10.1234/test"), "10.1234/test")

  # try case variations
  testthat::expect_equal(clean_doi("DOI:10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("Doi:10.1234/test"), "10.1234/test")

  # try space variations
  testthat::expect_equal(clean_doi("doi: 10.1234/test"), "10.1234/test")
  testthat::expect_equal(clean_doi("doi:  10.1234/test"), "10.1234/test")
})


test_that("clean_doi handles publisher-specific URL formats correctly", {
  # use ASCE library format
  testthat::expect_equal(
    clean_doi("https://ascelibrary.org/doi/abs/10.1234/test"),
    "10.1234/test"
  )
})


test_that("clean_doi handles complex mixed cases correctly", {
  # try complex case with multiple issues
  testthat::expect_equal(
    clean_doi(" \"https://doi.org/10.1234/test\" "),
    "10.1234/test"
  )

  # try another complex case
  testthat::expect_equal(
    clean_doi("doi:  \"10.1234/test\""),
    "10.1234/test"
  )
})


test_that("clean_doi handles non-character input correctly", {
  # numeric input should be returned unchanged
  numeric_input <- c(1, 2, 3)
  testthat::expect_identical(clean_doi(numeric_input), numeric_input)

  # list input should be returned unchanged
  list_input <- list("10.1234/test", NA)
  testthat::expect_identical(clean_doi(list_input), list_input)
})


test_that("clean_doi handles vectors correctly", {
  # try vector of different DOI formats
  mixed_vector <- c(
    "https://doi.org/10.1234/test1",
    "doi:10.1234/test2",
    "10.1234/test3",
    NA_character_,
    ""
  )

  expected <- c(
    "10.1234/test1",
    "10.1234/test2",
    "10.1234/test3",
    NA_character_,
    NA_character_
  )

  testthat::expect_identical(clean_doi(mixed_vector), expected)
})


test_that("clean_doi preserves actual DOI content", {
  # ensure the actual DOI content isn't modified
  original_doi <- "10.1234/journal.example-2023_01.test"
  testthat::expect_equal(clean_doi(original_doi), original_doi)

  # test with unusual but valid DOI characters
  unusual_doi <- "10.1234/journal+symbol~special_chars.123"
  testthat::expect_equal(clean_doi(unusual_doi), unusual_doi)
})
