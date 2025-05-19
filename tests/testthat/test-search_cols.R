test_that("search_cols finds matches in simple data frame", {
  # create test data frame
  df <- data.frame(
    title = c("Climate Change Study", "Economic Analysis", "Climate Policy"),
    abstract = c("Effects of warming on ecosystems", "Market trends", "Environmental regulations"),
    stringsAsFactors = FALSE
  )

  # search for "climate" (case-insensitive by default)
  result <- search_cols(df, "climate")

  # verify matching columns for each row
  testthat::expect_equal(result$matching_cols[[1]], "title")
  testthat::expect_equal(result$matching_cols[[2]], character(0))
  testthat::expect_equal(result$matching_cols[[3]], "title")

  # verify original data is preserved
  testthat::expect_equal(result$title, df$title)
  testthat::expect_equal(result$abstract, df$abstract)
})


test_that("search_cols handles case sensitivity correctly", {
  # create test data frame with mixed case
  df <- data.frame(
    title = c("Climate Change Study", "Economic Analysis", "CLIMATE Policy"),
    abstract = c("effects of warming on ecosystems", "Market trends", "Environmental regulations"),
    stringsAsFactors = FALSE
  )

  # case-insensitive search (default)
  insensitive_result <- search_cols(df, "climate")

  # both "Climate" and "CLIMATE" should match
  testthat::expect_equal(insensitive_result$matching_cols[[1]], "title")
  testthat::expect_equal(insensitive_result$matching_cols[[3]], "title")

  # case-sensitive search
  sensitive_result <- search_cols(df, "Climate", ignore_case = FALSE)

  # only exact case "Climate" should match
  testthat::expect_equal(sensitive_result$matching_cols[[1]], "title")
  testthat::expect_equal(sensitive_result$matching_cols[[3]], character(0))

  # search for lowercase
  lowercase_result <- search_cols(df, "climate", ignore_case = FALSE)

  # there should be no matches since "climate" isn't in the data
  testthat::expect_equal(lowercase_result$matching_cols[[1]], character(0))
  testthat::expect_equal(lowercase_result$matching_cols[[3]], character(0))
})


test_that("search_cols can find pattern in multiple columns", {
  # create test data frame with pattern in multiple columns
  df <- data.frame(
    title = c("Climate Change Study", "Climate Economics", "Policy Report"),
    abstract = c("Effects of climate on ecosystems", "Economic climate analysis", "Environmental policy"),
    stringsAsFactors = FALSE
  )

  # search for "climate"
  result <- search_cols(df, "climate")

  # verify matches in multiple columns
  testthat::expect_equal(sort(result$matching_cols[[1]]), sort(c("title", "abstract")))
  testthat::expect_equal(sort(result$matching_cols[[2]]), sort(c("title", "abstract")))
  testthat::expect_equal(result$matching_cols[[3]], character(0))
})


test_that("search_cols handles nested data frames", {
  # create data frame with a nested data frame column
  nested_df <- data.frame(
    id = 1:2,
    stringsAsFactors = FALSE
  )

  # add a column containing nested data frames
  nested_df$metadata <- list(
    data.frame(key = c("subject", "location"), value = c("Climate Science", "Arctic"), stringsAsFactors = FALSE),
    data.frame(key = c("subject", "location"), value = c("Economics", "Global"), stringsAsFactors = FALSE)
  )

  # search for "climate"
  result <- search_cols(nested_df, "climate")

  # verify the match in the nested data frame
  testthat::expect_equal(result$matching_cols[[1]], "metadata")
  testthat::expect_equal(result$matching_cols[[2]], character(0))
})


test_that("search_cols handles nested lists", {
  # create data frame with nested lists
  list_df <- data.frame(
    id = 1:2,
    stringsAsFactors = FALSE
  )

  # add a column containing nested lists
  list_df$keywords <- list(
    list(main = "Climate", secondary = c("Temperature", "Warming")),
    list(main = "Economics", secondary = c("Markets", "Policy"))
  )

  # search for "climate"
  result <- search_cols(list_df, "climate")

  # verify the match in the nested list
  testthat::expect_equal(result$matching_cols[[1]], "keywords")
  testthat::expect_equal(result$matching_cols[[2]], character(0))
})


test_that("search_cols handles deeply nested structures", {
  # create data frame with deeply nested structures
  deep_df <- data.frame(
    id = 1:1,
    stringsAsFactors = FALSE
  )

  # create a deeply nested structure
  deep_df$deep <- list(
    list(
      level1 = list(
        level2 = list(
          level3 = "No climate here",
          level3b = list(
            level4 = "Climate is hidden deep"
          )
        )
      )
    )
  )

  # search for "climate"
  result <- search_cols(deep_df, "climate")

  # verify the match in the deeply nested structure
  testthat::expect_equal(result$matching_cols[[1]], "deep")
})


test_that("search_cols handles numeric and logical values", {
  # create data frame with different data types
  mixed_df <- data.frame(
    text = c("Climate text", "Temperature", "None"),
    number = c(123, 456, 789),
    logical = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  # search in text column
  result_text <- search_cols(mixed_df, "temp")
  testthat::expect_equal(result_text$matching_cols[[1]], character(0))
  testthat::expect_equal(result_text$matching_cols[[2]], "text")
  testthat::expect_equal(result_text$matching_cols[[3]], character(0))

  # search in number column (converted to character)
  result_number <- search_cols(mixed_df, "123")
  testthat::expect_equal(result_number$matching_cols[[1]], "number")

  # search in logical column (converted to character)
  result_logical <- search_cols(mixed_df, "TRUE")
  testthat::expect_equal(result_logical$matching_cols[[1]], "logical")
  testthat::expect_equal(result_logical$matching_cols[[3]], "logical")
})


test_that("search_cols returns empty matches when pattern not found", {
  # create test data frame
  df <- data.frame(
    title = c("Article One", "Article Two"),
    abstract = c("First abstract", "Second abstract"),
    stringsAsFactors = FALSE
  )

  # search for a pattern that doesn't exist
  result <- search_cols(df, "climate")

  # verify all rows have empty match vectors
  testthat::expect_equal(result$matching_cols[[1]], character(0))
  testthat::expect_equal(result$matching_cols[[2]], character(0))
})


test_that("search_cols works with single-row data frames", {
  # create a single-row data frame
  df <- data.frame(
    title = "Climate Article",
    abstract = "About climate change",
    stringsAsFactors = FALSE
  )

  # search for "climate"
  result <- search_cols(df, "climate")

  # verify matches
  testthat::expect_equal(sort(result$matching_cols[[1]]), sort(c("title", "abstract")))
})


test_that("search_cols works with empty data frames", {
  # create an empty data frame with columns
  df <- data.frame(
    title = character(0),
    abstract = character(0),
    stringsAsFactors = FALSE
  )

  # search in empty data frame
  result <- search_cols(df, "climate")

  # verify result is also an empty data frame with matching_cols
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 0)
  testthat::expect_true("matching_cols" %in% names(result))
})


test_that("search_cols handles factor columns", {
  # create data frame with factor columns
  df <- data.frame(
    title = factor(c("Climate Study", "Economics Report")),
    category = factor(c("Science", "Social Science"))
  )

  # search for "climate"
  result <- search_cols(df, "climate")

  # verify match in factor column
  testthat::expect_equal(result$matching_cols[[1]], "title")
  testthat::expect_equal(result$matching_cols[[2]], character(0))
})


test_that("search_cols handles special characters in pattern", {
  # create test data frame with special characters
  df <- data.frame(
    title = c("Study (2020)", "Analysis: 50% Complete", "Report & Summary"),
    abstract = c("Contains * and +", "Has $ symbol", "Uses / and \\"),
    stringsAsFactors = FALSE
  )

  # search for special characters that would be regex metacharacters
  result1 <- search_cols(df, "(2020)")
  testthat::expect_equal(result1$matching_cols[[1]], "title")

  result2 <- search_cols(df, "50%")
  testthat::expect_equal(result2$matching_cols[[2]], "title")

  result3 <- search_cols(df, "&")
  testthat::expect_equal(result3$matching_cols[[3]], "title")

  result4 <- search_cols(df, "*")
  testthat::expect_equal(result4$matching_cols[[1]], "abstract")
})
