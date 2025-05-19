# test get_funder_info function
test_that("get_funder_info calls cr_funders with correct parameters", {
  # mock the cr_funders function
  mock_result <- list(
    data = data.frame(
      id = c("100000001", "100000123"),
      name = c("National Science Foundation", "NSF Related Org"),
      stringsAsFactors = FALSE
    )
  )

  mockery::stub(get_funder_info, "rcrossref::cr_funders", function(query, limit) {
    # verify the parameters are passed correctly
    testthat::expect_equal(query, "NSF")
    testthat::expect_equal(limit, 30)
    return(mock_result)
  })

  # run the function
  result <- get_funder_info("NSF", limit = 30)

  # verify the result
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$id, c("100000001", "100000123"))
})


test_that("get_funder_info uses default limit when not specified", {
  # mock the cr_funders function
  mockery::stub(get_funder_info, "rcrossref::cr_funders", function(query, limit) {
    # verify the parameters are passed correctly
    testthat::expect_equal(limit, 20)
    return(list(data = data.frame()))
  })

  # run the function with default limit
  get_funder_info("test query")
})


test_that("get_funder_info returns the data frame from the API response", {
  # create a mock return value
  mock_data <- data.frame(
    id = "100000001",
    name = "test Funder",
    `alt-names` = "TF",
    uri = "http://example.com/funder",
    location = "USA",
    stringsAsFactors = FALSE
  )

  # mock the cr_funders function
  mockery::stub(get_funder_info, "rcrossref::cr_funders", function(query, limit) {
    return(list(data = mock_data))
  })

  # run the function
  result <- get_funder_info("test Funder")

  # verify the result
  testthat::expect_identical(result, mock_data)
})


# test get_funded function
test_that("get_funded validates input parameters correctly", {
  # test invalid funder_id (not a character)
  testthat::expect_error(get_funded(123), "`funder_id` must be a single FundRef ID string.")

  # test invalid funder_id (multiple values)
  testthat::expect_error(get_funded(c("id1", "id2")), "`funder_id` must be a single FundRef ID string.")

  # test invalid per_page (too small)
  testthat::expect_error(get_funded("100000001", per_page = 0), "`per_page` must be between 1 and 1000.")

  # test invalid per_page (too large)
  testthat::expect_error(get_funded("100000001", per_page = 1001), "`per_page` must be between 1 and 1000.")
})


test_that("get_funded combines funder ID with additional filters", {
  # mock cr_works to verify filter construction
  mockery::stub(get_funded, "rcrossref::cr_works", function(filter, limit, offset) {
    # verify filter construction
    testthat::expect_equal(filter, c(funder = "100000001", type = "journal-article", from_pub_date = "2023-01-01"))
    testthat::expect_equal(limit, 500)
    testthat::expect_equal(offset, 0)

    # return empty result to prevent further processing
    return(list(data = data.frame()))
  })

  # mock other functions to prevent execution
  mockery::stub(get_funded, "dplyr::bind_rows", function(x) data.frame())

  # run the function
  get_funded(
    funder_id = "100000001",
    filter = c(type = "journal-article", from_pub_date = "2023-01-01"),
    total = 1000,
    per_page = 500
  )
})

test_that("get_funded handles multiple pages correctly", {
  # create three pages of mock data
  page1 <- data.frame(id = paste0("id", 1:100), title = paste("Title", 1:100), stringsAsFactors = FALSE)
  page2 <- data.frame(id = paste0("id", 101:200), title = paste("Title", 101:200), stringsAsFactors = FALSE)
  page3 <- data.frame(id = paste0("id", 201:270), title = paste("Title", 201:270), stringsAsFactors = FALSE)

  # create a counter for the cr_works calls
  call_count <- 0

  # mock cr_works to return different pages on successive calls
  mockery::stub(get_funded, "rcrossref::cr_works", function(filter, limit, offset) {
    call_count <<- call_count + 1

    testthat::expect_equal(limit, 100)

    if (call_count == 1) {
      testthat::expect_equal(offset, 0)
      return(list(data = page1))
    } else if (call_count == 2) {
      testthat::expect_equal(offset, 100)
      return(list(data = page2))
    } else if (call_count == 3) {
      testthat::expect_equal(offset, 200)
      return(list(data = page3))
    } else {
      # this should not be called
      testthat::fail("cr_works called more times than expected")
    }
  })

  # mock message to suppress output
  mockery::stub(get_funded, "message", function(...) NULL)

  # run the function to get all three pages
  result <- get_funded(
    funder_id = "100000001",
    total = 300,
    per_page = 100
  )

  # verify that result has all rows from all pages
  testthat::expect_equal(nrow(result), 270)
  testthat::expect_equal(result$id[1], "id1")
  testthat::expect_equal(result$id[270], "id270")
})


test_that("get_funded stops when no more records are available", {
  # create one page of mock data and an empty second page
  page1 <- data.frame(id = paste0("id", 1:100), title = paste("Title", 1:100), stringsAsFactors = FALSE)
  empty_page <- data.frame()

  # create a counter for the cr_works calls
  call_count <- 0

  # mock cr_works to return one page then an empty page
  mockery::stub(get_funded, "rcrossref::cr_works", function(filter, limit, offset) {
    call_count <<- call_count + 1

    if (call_count == 1) {
      return(list(data = page1))
    } else {
      return(list(data = empty_page))
    }
  })

  # mock message to capture output
  messages <- character(0)
  mockery::stub(get_funded, "message", function(...) {
    msg <- paste0(...)
    messages <<- c(messages, msg)
  })

  # run the function
  result <- get_funded(
    funder_id = "100000001",
    total = 1000,
    per_page = 100
  )

  # verify the result and that the correct message was displayed
  testthat::expect_equal(nrow(result), 100)
  testthat::expect_true(any(grepl("No more records available", messages)))
})


test_that("get_funded stops when a page returns fewer records than requested", {
  # create a full page and a partial page
  page1 <- data.frame(id = paste0("id", 1:100), title = paste("Title", 1:100), stringsAsFactors = FALSE)
  partial_page <- data.frame(id = paste0("id", 101:150), title = paste("Title", 101:150), stringsAsFactors = FALSE)

  # create a counter for the cr_works calls
  call_count <- 0

  # mock cr_works to return a full page then a partial page
  mockery::stub(get_funded, "rcrossref::cr_works", function(filter, limit, offset) {
    call_count <<- call_count + 1

    if (call_count == 1) {
      return(list(data = page1))
    } else {
      return(list(data = partial_page))
    }
  })

  # mock message to capture output
  messages <- character(0)
  mockery::stub(get_funded, "message", function(...) {
    msg <- paste0(...)
    messages <<- c(messages, msg)
  })

  # run the function
  result <- get_funded(
    funder_id = "100000001",
    total = 1000,
    per_page = 100
  )

  # verify the result and that the correct message was displayed
  testthat::expect_equal(nrow(result), 150)
  testthat::expect_true(any(grepl("Last page returned fewer than", messages)))
})


test_that("get_funded truncates results to requested total", {
  # create a page with more data than requested
  page1 <- data.frame(id = paste0("id", 1:200), title = paste("Title", 1:200), stringsAsFactors = FALSE)

  # mock cr_works to return this page
  mockery::stub(get_funded, "rcrossref::cr_works", function(filter, limit, offset) {
    return(list(data = page1))
  })

  # mock message to suppress output
  mockery::stub(get_funded, "message", function(...) NULL)

  # run the function with a total less than the page size
  result <- get_funded(
    funder_id = "100000001",
    total = 150,
    per_page = 200
  )

  # verify the result was truncated
  testthat::expect_equal(nrow(result), 150)
  testthat::expect_equal(result$id[150], "id150")
})
