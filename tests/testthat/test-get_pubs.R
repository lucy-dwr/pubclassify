test_that("get_pubs validates input parameters correctly", {
  # test with both query and dois provided (ambiguous input)
  testthat::expect_error(
    get_pubs(query = "climate", dois = c("10.1038/nature12345")),
    "Please provide either dois or query/filter, but not both."
  )

  # test invalid total parameter
  testthat::expect_error(
    get_pubs(query = "climate", total = -10),
    "`total` must be a positive finite number."
  )

  testthat::expect_error(
    get_pubs(query = "climate", total = "not a number"),
    "`total` must be a positive finite number."
  )

  testthat::expect_error(
    get_pubs(query = "climate", total = c(100, 200)),
    "`total` must be a positive finite number."
  )

  testthat::expect_error(
    get_pubs(query = "climate", total = Inf),
    "`total` must be a positive finite number."
  )

  # test invalid per_page parameter
  testthat::expect_error(
    get_pubs(query = "climate", per_page = 0),
    "`per_page` must be between 1 and 1000."
  )

  testthat::expect_error(
    get_pubs(query = "climate", per_page = 1001),
    "`per_page` must be between 1 and 1000."
  )

  testthat::expect_error(
    get_pubs(query = "climate", per_page = "not a number"),
    "`per_page` must be a finite number."
  )

  testthat::expect_error(
    get_pubs(query = "climate", per_page = c(100, 200)),
    "`per_page` must be a finite number."
  )

  # test invalid preserve_404 parameter
  testthat::expect_error(
    get_pubs(dois = c("10.1038/nature12345"), preserve_404 = "yes"),
    "`preserve_404` must be a single logical value \\(TRUE or FALSE\\)."
  )

  testthat::expect_error(
    get_pubs(dois = c("10.1038/nature12345"), preserve_404 = c(TRUE, FALSE)),
    "`preserve_404` must be a single logical value \\(TRUE or FALSE\\)."
  )

  testthat::expect_error(
    get_pubs(dois = c("10.1038/nature12345"), preserve_404 = NA),
    "`preserve_404` must be a single logical value \\(TRUE or FALSE\\)."
  )

  # test invalid dois parameter
  testthat::expect_error(
    get_pubs(dois = 12345),
    "`dois` must be a character vector."
  )
})


test_that("get_pubs processes query mode correctly", {
  # mock successful cr_works response with two pages
  page1 <- data.frame(
    doi = c("10.1234/abc1", "10.1234/abc2"),
    title = c("Title 1", "Title 2"),
    stringsAsFactors = FALSE
  )

  page2 <- data.frame(
    doi = c("10.1234/abc3", "10.1234/abc4"),
    title = c("Title 3", "Title 4"),
    stringsAsFactors = FALSE
  )

  # create a counter for cr_works calls
  call_count <- 0

  # mock cr_works to return different pages
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    # verify the call is using the correct parameters
    if (!is.null(dois)) {
      testthat::fail("cr_works should not be called with dois in query mode")
    }

    # track calls
    call_count <<- call_count + 1

    # return different pages based on call count
    if (call_count == 1) {
      testthat::expect_equal(query, "climate")
      testthat::expect_equal(limit, 10)
      testthat::expect_equal(offset, 0)
      return(list(data = page1))
    } else {
      testthat::expect_equal(query, "climate")
      testthat::expect_equal(limit, 10)
      testthat::expect_equal(offset, 10)
      return(list(data = page2))
    }
  })

  # mock message to suppress output
  mockery::stub(get_pubs, "message", function(...) NULL)

  # test query mode with multiple pages
  result <- get_pubs(query = "climate", total = 100, per_page = 10)

  # verify result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 4)
  testthat::expect_equal(result$doi, c("10.1234/abc1", "10.1234/abc2", "10.1234/abc3", "10.1234/abc4"))
})


test_that("get_pubs handles filter parameter correctly", {
  # mock successful cr_works response
  mock_data <- data.frame(
    doi = c("10.1234/abc1", "10.1234/abc2"),
    title = c("Title 1", "Title 2"),
    stringsAsFactors = FALSE
  )

  # mock cr_works to verify filter parameter
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    # verify filter is passed correctly
    testthat::expect_null(query)
    testthat::expect_equal(filter, c(from_pub_date = "2022-01-01", type = "journal-article"))
    return(list(data = mock_data))
  })

  # mock message to suppress output
  mockery::stub(get_pubs, "message", function(...) NULL)

  # test filter mode
  result <- get_pubs(
    filter = c(from_pub_date = "2022-01-01", type = "journal-article"),
    total = 10
  )

  # verify result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2)
})


test_that("get_pubs limits results to requested total", {
  # mock data with more records than requested
  mock_data <- data.frame(
    doi = paste0("10.1234/abc", 1:20),
    title = paste0("Title ", 1:20),
    stringsAsFactors = FALSE
  )

  # mock cr_works to return more data than requested
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    return(list(data = mock_data))
  })

  # mock message to suppress output
  mockery::stub(get_pubs, "message", function(...) NULL)

  # test with total less than available data
  result <- get_pubs(query = "test", total = 15)

  # verify result is truncated
  testthat::expect_equal(nrow(result), 15)
  testthat::expect_equal(result$doi[15], "10.1234/abc15")
})


test_that("get_pubs processes DOIs correctly with preserve_404 = TRUE", {
  # create mock DOIs - some will be found, some won't
  test_dois <- c("10.1234/valid1", "10.1234/notfound", "10.1234/valid2")

  # mock successful result
  valid_result1 <- data.frame(
    doi = "10.1234/valid1",
    title = "Valid Title 1",
    author = "Author 1",
    stringsAsFactors = FALSE
  )

  valid_result2 <- data.frame(
    doi = "10.1234/valid2",
    title = "Valid Title 2",
    author = "Author 2",
    stringsAsFactors = FALSE
  )

  # mock cr_works for DOI mode
  call_count <- 0
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    call_count <<- call_count + 1

    # verify we're using DOI mode
    testthat::expect_not_null(dois)

    if (dois == "10.1234/valid1") {
      return(list(data = valid_result1))
    } else if (dois == "10.1234/valid2") {
      return(list(data = valid_result2))
    } else {
      # Simulate a not found DOI
      return(list(data = data.frame()))
    }
  })

  # mock sleep to speed up test
  mockery::stub(get_pubs, "Sys.sleep", function(x) NULL)

  # mock message to suppress output
  mockery::stub(get_pubs, "message", function(...) NULL)

  # test DOI mode with preserve_404 = TRUE
  result <- get_pubs(dois = test_dois, preserve_404 = TRUE)

  # verify result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 3)
  testthat::expect_equal(result$original_doi, test_dois)
  testthat::expect_equal(result$doi_retrieved, c(TRUE, FALSE, TRUE))

  # make it so that row for valid1 should have data
  valid1_row <- which(result$original_doi == "10.1234/valid1")
  testthat::expect_equal(result$title[valid1_row], "Valid Title 1")

  # make it so that row for notfound should have NA for most fields
  notfound_row <- which(result$original_doi == "10.1234/notfound")
  testthat::expect_true(is.na(result$title[notfound_row]))

  # chec that the doi field contains the original DOI
  testthat::expect_equal(result$doi[notfound_row], "10.1234/notfound")
})


test_that("get_pubs processes DOIs correctly with preserve_404 = FALSE", {
  # create mock DOIs - some will be found, some won't
  test_dois <- c("10.1234/valid1", "10.1234/notfound", "10.1234/valid2")

  # mock successful result
  valid_result1 <- data.frame(
    doi = "10.1234/valid1",
    title = "Valid Title 1",
    author = "Author 1",
    stringsAsFactors = FALSE
  )

  valid_result2 <- data.frame(
    doi = "10.1234/valid2",
    title = "Valid Title 2",
    author = "Author 2",
    stringsAsFactors = FALSE
  )

  # mock cr_works for DOI mode
  call_count <- 0
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    call_count <<- call_count + 1

    # verify we're using DOI mode
    testthat::expect_not_null(dois)

    if (dois == "10.1234/valid1") {
      return(list(data = valid_result1))
    } else if (dois == "10.1234/valid2") {
      return(list(data = valid_result2))
    } else {
      # simulate a not found DOI
      return(list(data = data.frame()))
    }
  })

  # mock sleep to speed up test
  mockery::stub(get_pubs, "Sys.sleep", function(x) NULL)

  # mock message to suppress output
  mockery::stub(get_pubs, "message", function(...) NULL)

  # test DOI mode with preserve_404 = FALSE
  result <- get_pubs(dois = test_dois, preserve_404 = FALSE)

  # verify result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true(all(result$doi_retrieved))
  testthat::expect_setequal(result$doi, c("10.1234/valid1", "10.1234/valid2"))
})


test_that("get_pubs handles all DOIs not found correctly", {
  # create mock DOIs - none will be found
  test_dois <- c("10.1234/notfound1", "10.1234/notfound2")

  # mock cr_works to return empty results
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    return(list(data = data.frame()))
  })

  # mock sleep to speed up test
  mockery::stub(get_pubs, "Sys.sleep", function(x) NULL)

  # mock message to suppress output
  mockery::stub(get_pubs, "message", function(...) NULL)

  # test with preserve_404 = TRUE
  result_with_preserve <- get_pubs(dois = test_dois, preserve_404 = TRUE)

  # verify result with preserve_404 = TRUE
  testthat::expect_s3_class(result_with_preserve, "data.frame")
  testthat::expect_equal(nrow(result_with_preserve), 2)
  testthat::expect_equal(result_with_preserve$original_doi, test_dois)
  testthat::expect_equal(result_with_preserve$doi_retrieved, c(FALSE, FALSE))

  # test with preserve_404 = FALSE
  result_without_preserve <- get_pubs(dois = test_dois, preserve_404 = FALSE)

  # verify result with preserve_404 = FALSE
  testthat::expect_s3_class(result_without_preserve, "data.frame")
  testthat::expect_equal(nrow(result_without_preserve), 0)
})


test_that("get_pubs handles empty or NA DOIs correctly", {
  # mock DOIs including empty and NA values
  test_dois <- c("10.1234/valid", "", NA, "10.1234/alsovalid")

  # mock successful results
  valid_result1 <- data.frame(
    doi = "10.1234/valid",
    title = "Valid Title",
    stringsAsFactors = FALSE
  )

  valid_result2 <- data.frame(
    doi = "10.1234/alsovalid",
    title = "Also Valid Title",
    stringsAsFactors = FALSE
  )

  # mock cr_works function to skip empty and NA DOIs
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    if (dois == "10.1234/valid") {
      return(list(data = valid_result1))
    } else if (dois == "10.1234/alsovalid") {
      return(list(data = valid_result2))
    } else {
      testthat::fail(paste("cr_works should not be called with DOI:", dois))
    }
  })

  # mock sleep to speed up test
  mockery::stub(get_pubs, "Sys.sleep", function(x) NULL)

  # mock message to suppress output
  mockery::stub(get_pubs, "message", function(...) NULL)

  # run the function
  result <- get_pubs(dois = test_dois)

  # verify that empty and NA DOIs were handled correctly
  testthat::expect_equal(nrow(result), 4)
  testthat::expect_equal(sum(result$doi_retrieved), 2)

  # verify the original DOIs are preserved correctly
  testthat::expect_equal(result$original_doi, test_dois)
})


test_that("get_pubs respects the total parameter in DOI mode", {
  # create extra DOIs
  test_dois <- paste0("10.1234/valid", 1:5)

  # mock cr_works to return success for each DOI
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    # extract the numeric part for the index
    idx <- as.integer(gsub("10.1234/valid", "", dois))
    return(list(data = data.frame(
      doi = dois,
      title = paste("Title", idx),
      stringsAsFactors = FALSE
    )))
  })

  # mock sleep to speed up test
  mockery::stub(get_pubs, "Sys.sleep", function(x) NULL)

  # capture messages to verify total limit message
  messages <- character(0)
  mockery::stub(get_pubs, "message", function(...) {
    msg <- paste0(...)
    messages <<- c(messages, msg)
  })

  # run with total = 3 (less than the number of DOIs)
  result <- get_pubs(dois = test_dois, total = 3)

  # verify total limit was respected
  testthat::expect_equal(sum(result$doi_retrieved), 3)
  testthat::expect_true(any(grepl("Reached requested total of 3 records", messages)))
})


test_that("get_pubs handles cr_works errors gracefully", {
  # mock DOIs
  test_dois <- c("10.1234/valid", "10.1234/error", "10.1234/alsovalid")

  # mock successful and error results
  valid_result <- data.frame(
    doi = "10.1234/valid",
    title = "Valid Title",
    stringsAsFactors = FALSE
  )

  valid_result2 <- data.frame(
    doi = "10.1234/alsovalid",
    title = "Also Valid Title",
    stringsAsFactors = FALSE
  )

  # mock cr_works function to throw an error for one DOI
  mockery::stub(get_pubs, "rcrossref::cr_works", function(query, filter, limit, offset, dois) {
    if (dois == "10.1234/valid") {
      return(list(data = valid_result))
    } else if (dois == "10.1234/alsovalid") {
      return(list(data = valid_result2))
    } else if (dois == "10.1234/error") {
      stop("API error for this DOI")
    } else {
      return(list(data = data.frame()))
    }
  })

  # mock sleep to speed up test
  mockery::stub(get_pubs, "Sys.sleep", function(x) NULL)

  # capture error messages
  messages <- character(0)
  mockery::stub(get_pubs, "message", function(...) {
    msg <- paste0(...)
    messages <<- c(messages, msg)
  })

  # run the function - it should handle the error and continue
  result <- get_pubs(dois = test_dois)

  # verify results
  testthat::expect_equal(nrow(result), 3)
  testthat::expect_equal(sum(result$doi_retrieved), 2)  # Only 2 successful DOIs
  testthat::expect_true(any(grepl("Error retrieving DOI", messages)))
})
