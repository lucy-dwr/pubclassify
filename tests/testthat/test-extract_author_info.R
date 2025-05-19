test_that("extract_author_info handles normal case with individual authors", {
  # create test data with individual authors
  authors <- data.frame(
    sequence = c("first", "additional", "additional"),
    given = c("John", "Jane", "Robert"),
    family = c("Smith", "Johnson", "Williams"),
    stringsAsFactors = FALSE
  )

  result <- extract_author_info(authors)

  # check results
  testthat::expect_equal(result$lead_author_first_name, "John")
  testthat::expect_equal(result$lead_author_last_name, "Smith")
  testthat::expect_equal(result$author_count, 3)
})


test_that("extract_author_info handles organization as first author", {
  # create test data with organization as first author
  mixed_authors <- data.frame(
    sequence = c("first", "additional"),
    given = c(NA, "Maria"),
    family = c(NA, "Garcia"),
    name = c("National Science Foundation", NA),
    stringsAsFactors = FALSE
  )

  result <- extract_author_info(mixed_authors)

  # should find the first individual author
  testthat::expect_equal(result$lead_author_first_name, "Maria")
  testthat::expect_equal(result$lead_author_last_name, "Garcia")
  testthat::expect_equal(result$author_count, 2)
})


test_that("extract_author_info handles NULL or empty author list", {
  # test with NULL
  result_null <- extract_author_info(NULL)

  testthat::expect_equal(result_null$lead_author_first_name, NA_character_)
  testthat::expect_equal(result_null$lead_author_last_name, NA_character_)
  testthat::expect_equal(result_null$author_count, 0)

  # test with empty data frame
  empty_authors <- data.frame(
    sequence = character(0),
    given = character(0),
    family = character(0),
    stringsAsFactors = FALSE
  )

  result_empty <- extract_author_info(empty_authors)

  testthat::expect_equal(result_empty$lead_author_first_name, NA_character_)
  testthat::expect_equal(result_empty$lead_author_last_name, NA_character_)
  testthat::expect_equal(result_empty$author_count, 0)
})


test_that("extract_author_info handles missing sequence field", {
  # create test data without sequence field
  authors_no_sequence <- data.frame(
    given = c("John", "Jane", "Robert"),
    family = c("Smith", "Johnson", "Williams"),
    stringsAsFactors = FALSE
  )

  result <- extract_author_info(authors_no_sequence)

  # should not find a lead author
  testthat::expect_equal(result$lead_author_first_name, NA_character_)
  testthat::expect_equal(result$lead_author_last_name, NA_character_)
  testthat::expect_equal(result$author_count, 3)
})


test_that("extract_author_info handles no 'first' sequence value", {
  # create test data without 'first' sequence
  authors_no_first <- data.frame(
    sequence = c("additional", "additional", "additional"),
    given = c("John", "Jane", "Robert"),
    family = c("Smith", "Johnson", "Williams"),
    stringsAsFactors = FALSE
  )

  result <- extract_author_info(authors_no_first)

  # should not find a lead author
  testthat::expect_equal(result$lead_author_first_name, NA_character_)
  testthat::expect_equal(result$lead_author_last_name, NA_character_)
  testthat::expect_equal(result$author_count, 3)
})


test_that("extract_author_info handles partial author information", {
  # create test data with partial information
  partial_authors <- data.frame(
    sequence = c("first", "additional", "additional"),
    given = c("John", NA, "Robert"),
    family = c(NA, "Johnson", "Williams"),
    stringsAsFactors = FALSE
  )

  result <- extract_author_info(partial_authors)

  # should get first name but not last name
  testthat::expect_equal(result$lead_author_first_name, "John")
  testthat::expect_equal(result$lead_author_last_name, NA_character_)
  testthat::expect_equal(result$author_count, 3)
})


test_that("extract_author_info handles multiple 'first' authors", {
  # create test data with multiple 'first' sequence values
  multi_first <- data.frame(
    sequence = c("first", "first", "additional"),
    given = c("John", "Jane", "Robert"),
    family = c("Smith", "Johnson", "Williams"),
    stringsAsFactors = FALSE
  )

  result <- extract_author_info(multi_first)

  # should use the first one marked as 'first'
  testthat::expect_equal(result$lead_author_first_name, "John")
  testthat::expect_equal(result$lead_author_last_name, "Smith")
  testthat::expect_equal(result$author_count, 3)
})


test_that("extract_author_info handles all organizational authors", {
  # create test data with only organizational authors
  org_authors <- data.frame(
    sequence = c("first", "additional"),
    given = c(NA, NA),
    family = c(NA, NA),
    name = c("National Science Foundation", "World Health Organization"),
    stringsAsFactors = FALSE
  )

  result <- extract_author_info(org_authors)

  # should not find individual author information
  testthat::expect_equal(result$lead_author_first_name, NA_character_)
  testthat::expect_equal(result$lead_author_last_name, NA_character_)
  testthat::expect_equal(result$author_count, 2)
})


test_that("extract_author_info handles different data structures", {
  # test with list of lists
  author_list <- list(
    list(sequence = "first", given = "John", family = "Smith"),
    list(sequence = "additional", given = "Jane", family = "Johnson")
  )

  # convert to data frame
  authors_df <- do.call(rbind, lapply(author_list, as.data.frame))

  result <- extract_author_info(authors_df)

  testthat::expect_equal(result$lead_author_first_name, "John")
  testthat::expect_equal(result$lead_author_last_name, "Smith")
  testthat::expect_equal(result$author_count, 2)
})
