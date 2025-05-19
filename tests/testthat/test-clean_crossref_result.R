test_that("clean_crossref_result standardizes column names", {
  # create a test data frame with non-standard column names
  test_df <- tibble::tibble(
    "DOI" = c("10.1234/test.1", "10.1234/test.2"),
    "Title" = c("Test Paper 1", "Test Paper 2"),
    "Abstract" = c("This is abstract 1.", "This is abstract 2."),
  )

  # apply the function
  result <- clean_crossref_result(test_df)

  # check column names are standardized
  testthat::expect_true(all(c("doi", "title", "abstract") %in% names(result)))
  testthat::expect_false(any(c("DOI", "Title", "Abstract") %in% names(result)))
})


test_that("clean_crossref_result drops specified columns", {
  test_df <- tibble::tibble(
    doi = c("10.1234/test.1", "10.1234/test.2"),
    title = c("Test Paper 1", "Test Paper 2"),
    abstract = c("This is abstract 1.", "This is abstract 2."),
    url = c("http://example.com/1", "http://example.com/2"),
    reference = list(c("ref1", "ref2"), c("ref3", "ref4"))
  )

  # drop columns
  drop_cols <- c("url", "reference")
  result <- clean_crossref_result(test_df, drop_cols = drop_cols)

  # check columns are dropped
  testthat::expect_false(any(drop_cols %in% names(result)))
  testthat::expect_true(all(c("doi", "title", "abstract") %in% names(result)))
})


test_that("clean_crossref_result filters by pattern correctly", {
  # create a test data frame with a mock search_cols implementation
  test_df <- tibble::tibble(
    doi = c("10.1234/test.1", "10.1234/test.2", "10.1234/test.3"),
    title = c("Machine Learning", "Deep Neural Networks", "Statistical Analysis"),
    abstract = c(
      "This paper discusses machine learning applications.",
      "A study of neural networks in depth.",
      "Statistical methods for data analysis."
    )
  )

  # mock the search_cols function
  search_cols_mock <- function(df, pattern) {
    # add a matching_cols column based on where the pattern appears
    df$matching_cols <- list(
      c("title", "abstract"),
      c("title"),
      c("reference")
    )
    return(df)
  }

  # mock the clean_jats_abstracts function
  clean_jats_mock <- function(df, abstract_col, abstract_output_col) {
    df[[abstract_output_col]] <- df[[abstract_col]]
    return(df)
  }

  mockery::stub(clean_crossref_result, "search_cols", search_cols_mock)
  mockery::stub(clean_crossref_result, "clean_jats_abstracts", clean_jats_mock)

  # apply the function with a pattern
  result <- clean_crossref_result(test_df, pattern = "neural")

  # expect only rows 1 and 2 to be kept (row 3 has pattern only in references)
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$doi, c("10.1234/test.1", "10.1234/test.2"))
})


test_that("clean_crossref_result handles abstract cleaning correctly", {
  test_df <- tibble::tibble(
    doi = c("10.1234/test.1", "10.1234/test.2"),
    title = c("Test Paper 1", "Test Paper 2"),
    abstract = c(
      "<jats:p>This is a JATS formatted abstract.</jats:p>",
      "<jats:p>Another JATS abstract with <jats:italic>formatting</jats:italic>.</jats:p>"
    )
  )

  expected_abstracts <- c(
    "This is a JATS formatted abstract.",
    "Another JATS abstract with formatting."
  )

  # mock the clean_jats_abstracts function
  clean_jats_mock <- function(df, abstract_col, abstract_output_col) {
    df[[abstract_output_col]] <- expected_abstracts
    return(df)
  }

  search_cols_mock <- function(df, pattern) {
    df$matching_cols <- list(c("title", "abstract"), c("title", "abstract"))
    return(df)
  }

  mockery::stub(clean_crossref_result, "clean_jats_abstracts", clean_jats_mock)
  mockery::stub(clean_crossref_result, "search_cols", search_cols_mock)

  result <- clean_crossref_result(test_df)

  testthat::expect_equal(result$abstract_text, expected_abstracts)
})


test_that("clean_crossref_result uses custom abstract column names", {
  # create a test data frame with non-standard abstract column
  test_df <- tibble::tibble(
    doi = c("10.1234/test.1", "10.1234/test.2"),
    title = c("Test Paper 1", "Test Paper 2"),
    full_abstract = c("Abstract 1", "Abstract 2")
  )

  # mock the clean_jats_abstracts function to verify parameter passing
  clean_jats_mock_params <- NULL
  clean_jats_mock <- function(df, abstract_col, abstract_output_col) {
    # store the parameters to verify later
    clean_jats_mock_params <<- list(
      abstract_col = abstract_col,
      abstract_output_col = abstract_output_col
    )

    # add the output column
    df[[abstract_output_col]] <- df[[abstract_col]]
    return(df)
  }

  mockery::stub(clean_crossref_result, "clean_jats_abstracts", clean_jats_mock)

  result <- clean_crossref_result(
    test_df,
    abstract_col = "full_abstract",
    abstract_output_col = "clean_abstract"
  )

  testthat::expect_equal(clean_jats_mock_params$abstract_col, "full_abstract")
  testthat::expect_equal(clean_jats_mock_params$abstract_output_col, "clean_abstract")

  testthat::expect_true("clean_abstract" %in% names(result))
  testthat::expect_equal(result$clean_abstract, c("Abstract 1", "Abstract 2"))
})


test_that("clean_crossref_result handles empty input correctly", {
  empty_df <- tibble::tibble(
    doi = character(0),
    title = character(0),
    abstract = character(0)
  )

  clean_jats_mock <- function(df, abstract_col, abstract_output_col) {
    df[[abstract_output_col]] <- character(0)
    return(df)
  }

  mockery::stub(clean_crossref_result, "clean_jats_abstracts", clean_jats_mock)

  result <- clean_crossref_result(empty_df)

  # check that result is an empty tibble with expected structure
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 0)
  testthat::expect_true("abstract_text" %in% names(result))
})


test_that("clean_crossref_result integrates with janitor::clean_names correctly", {
  # create a test data frame with messy column names
  test_df <- tibble::tibble(
    `DOI Number` = c("10.1234/test.1", "10.1234/test.2"),
    `Paper Title` = c("Test Paper 1", "Test Paper 2"),
    `Abstract (text)` = c("Abstract 1", "Abstract 2")
  )

  expected_cols <- c("doi_number", "paper_title", "abstract_text")

  clean_jats_mock <- function(df, abstract_col, abstract_output_col) {
    # return df unchanged for this test
    return(df)
  }

  mockery::stub(clean_crossref_result, "clean_jats_abstracts", clean_jats_mock)

  result <- clean_crossref_result(
    test_df,
    abstract_col = "abstract_text",
    abstract_output_col = "abstract_text"
  )

  testthat::expect_true(all(expected_cols %in% names(result)))
})


test_that("clean_crossref_result handles NULL pattern correctly", {
  test_df <- tibble::tibble(
    doi = c("10.1234/test.1", "10.1234/test.2"),
    title = c("Test Paper 1", "Test Paper 2"),
    abstract = c("Abstract 1", "Abstract 2")
  )

  # create a counter to ensure search_cols is NOT called when pattern is NULL
  search_cols_call_count <- 0
  search_cols_mock <- function(df, pattern) {
    search_cols_call_count <<- search_cols_call_count + 1
    return(df)
  }

  clean_jats_mock <- function(df, abstract_col, abstract_output_col) {
    df[[abstract_output_col]] <- df[[abstract_col]]
    return(df)
  }

  mockery::stub(clean_crossref_result, "search_cols", search_cols_mock)
  mockery::stub(clean_crossref_result, "clean_jats_abstracts", clean_jats_mock)

  result <- clean_crossref_result(test_df, pattern = NULL)

  # verify search_cols was not called
  testthat::expect_equal(search_cols_call_count, 0)

  # verify all original rows are preserved
  testthat::expect_equal(nrow(result), nrow(test_df))
})
