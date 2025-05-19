test_that("classify_pubs_gemini correctly processes publication data", {
  # create mock data
  test_pubs <- tibble::tibble(
    doi = c("10.1234/test.1", "10.1234/test.2"),
    abstract_text = c(
      "This paper explores machine learning applications in climate modeling.",
      "A study of protein folding mechanisms using computational methods."
    )
  )

  # create a mock chat function that returns predetermined responses
  mock_chat <- list(
    chat = function(prompt) {
      if (grepl("10.1234/test.1", prompt)) {
        return('{"doi":"10.1234/test.1","first_level":"Computer Science","second_level":"Machine Learning","third_level":"Climate Modeling"}')
      } else {
        return('{"doi":"10.1234/test.2","first_level":"Biology","second_level":"Molecular Biology","third_level":"Protein Structure"}')
      }
    }
  )

  # mock the ellmer::chat_google_gemini function to return the mock chat
  mockery::stub(classify_pubs_gemini, "ellmer::chat_google_gemini", function(...) mock_chat)

  # run the function with the corrected parameter
  result <- classify_pubs_gemini(test_pubs)

  # test the output
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$doi, c("10.1234/test.1", "10.1234/test.2"))
  testthat::expect_equal(result$first_level, c("Computer Science", "Biology"))
  testthat::expect_equal(result$second_level, c("Machine Learning", "Molecular Biology"))
  testthat::expect_equal(result$third_level, c("Climate Modeling", "Protein Structure"))
  testthat::expect_type(result$raw_response, "list")
})


test_that("classify_pubs_gemini handles empty input correctly", {
  # test with empty dataframe
  empty_pubs <- tibble::tibble(
    doi = character(0),
    abstract_text = character(0)
  )

  # mock the necessary functions
  mockery::stub(classify_pubs_gemini, "ellmer::chat_google_gemini", function(...) list(chat = function(x) "{}"))

  # run the function with empty input and expect it to return an empty tibble;
  # since empty input has no abstracts, the result should be an empty tibble
  result <- classify_pubs_gemini(empty_pubs)
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 0)
})

test_that("classify_pubs_gemini handles model parameter correctly", {
  # create test data
  test_pub <- tibble::tibble(
    doi = "10.1234/test.3",
    abstract_text = "Abstract for testing model parameter."
  )

  mock_model_param <- NULL
  mock_chat <- list(
    chat = function(prompt) {
      return('{"doi":"10.1234/test.3","first_level":"Test","second_level":"Test","third_level":"Test"}')
    }
  )

  # mock with a function that captures the model parameter
  mockery::stub(classify_pubs_gemini, "ellmer::chat_google_gemini", function(system_prompt, model) {
    mock_model_param <<- model
    return(mock_chat)
  })

  # test with default model
  classify_pubs_gemini(test_pub)
  testthat::expect_equal(mock_model_param, "gemini-2.0-flash")

  # test with custom model
  classify_pubs_gemini(test_pub, model = "gemini-custom-model")
  testthat::expect_equal(mock_model_param, "gemini-custom-model")
})


test_that("classify_pubs_gemini handles JSON parsing errors", {
  # create test data
  test_pub <- tibble::tibble(
    doi = "10.1234/test.error",
    abstract_text = "Test abstract for error handling."
  )

  # create a mock that returns invalid JSON
  invalid_json_mock <- list(
    chat = function(prompt) {
      return("This is not valid JSON")
    }
  )

  mockery::stub(classify_pubs_gemini, "ellmer::chat_google_gemini", function(...) invalid_json_mock)

  # the function will attempt to extract JSON with str_extract and then parse it;
  # this should cause an error in jsonlite::fromJSON
  testthat::expect_error(classify_pubs_gemini(test_pub))
})


test_that("classify_pubs_gemini shows progress bar", {
  skip_on_ci()

  test_pubs <- tibble::tibble(
    doi = c("10.1234/test.progress"),
    abstract_text = c("Testing progress bar.")
  )

  mock_chat <- list(
    chat = function(prompt) {
      return('{"doi":"10.1234/test.progress","first_level":"Test","second_level":"Test","third_level":"Test"}')
    }
  )

  # mock progress bar to capture if it was created properly
  mock_progress_bar <- NULL
  mockery::stub(classify_pubs_gemini, "ellmer::chat_google_gemini", function(...) mock_chat)
  mockery::stub(classify_pubs_gemini, "progress::progress_bar$new", function(...) {
    args <- list(...)
    mock_progress_bar <<- args
    return(list(tick = function() NULL))
  })

  # run function
  classify_pubs_gemini(test_pubs)

  # verify progress bar was created with expected parameters
  testthat::expect_true(!is.null(mock_progress_bar))
  testthat::expect_true("total" %in% names(mock_progress_bar))
  testthat::expect_equal(mock_progress_bar$total, 1)
})
