# create common test fixtures used by multiple functions, starting with sample
# publication data for testing
test_pub_data <- data.frame(
  doi = "10.1234/journal.example",
  title = "Example Research Paper",
  container_title = "Journal of Examples",
  pub_year = 2023,
  lead_author_last_name = "Smith",
  type = "journal-article",
  volume = "45",
  issue = "2",
  page = "123-145",
  url = "https://doi.org/10.1234/journal.example",
  stringsAsFactors = FALSE
)

# wrap author data to match expected structure
test_pub_data$author <- list(
  data.frame(
    family = "Smith",
    given = "John",
    name = NA_character_,
    stringsAsFactors = FALSE
  )
)

# make a book example
test_book_data <- data.frame(
  doi = "10.5678/book.example",
  title = "Comprehensive Guide to R",
  container_title = NA_character_,
  pub_year = 2022,
  lead_author_last_name = "Johnson",
  type = "book",
  publisher = "Data Science Publishing",
  url = "https://doi.org/10.5678/book.example",
  stringsAsFactors = FALSE
)

test_book_data$author <- list(
  data.frame(
    family = "Johnson",
    given = "Robert",
    name = NA_character_,
    stringsAsFactors = FALSE
  )
)

# make a chapter example
test_chapter_data <- data.frame(
  doi = "10.9012/chapter.example",
  title = "Advanced Techniques in Data Visualization",
  container_title = "Handbook of Data Science",
  pub_year = 2021,
  lead_author_last_name = "Lee",
  type = "book-chapter",
  publisher = "Academic Press",
  url = "https://doi.org/10.9012/chapter.example",
  stringsAsFactors = FALSE
)

test_chapter_data$author <- list(
  data.frame(
    family = "Lee",
    given = "David",
    name = NA_character_,
    stringsAsFactors = FALSE
  )
)

# create sample author data
individual_authors <- data.frame(
  family = c("Smith", "Johnson", "Lee"),
  given = c("John", "Sarah", "David"),
  name = rep(NA_character_, 3),
  stringsAsFactors = FALSE
)

organization_author <- data.frame(
  family = NA_character_,
  given = NA_character_,
  name = "National Science Foundation",
  stringsAsFactors = FALSE
)

mixed_authors <- rbind(
  organization_author,
  individual_authors[1:2, ]
)

partial_author <- data.frame(
  family = "Zhang",
  given = NA_character_,
  name = NA_character_,
  stringsAsFactors = FALSE
)


# create tests for clean_text function
test_that("clean_text escapes special characters correctly", {
  testthat::expect_equal(clean_text("Machine Learning & Statistics"),
                         "Machine Learning \\&amp; Statistics")
  testthat::expect_equal(clean_text("50% Reduction in Error_Rate"),
                         "50\\% Reduction in Error\\_Rate")
  testthat::expect_equal(clean_text("Title with multiple & special % characters_here"),
                         "Title with multiple \\&amp; special \\% characters\\_here")
})


test_that("clean_text handles NA values", {
  testthat::expect_equal(clean_text(NA), "")
})


test_that("clean_text handles empty strings", {
  testthat::expect_equal(clean_text(""), "")
})


test_that("clean_text preserves normal text", {
  testthat::expect_equal(clean_text("Normal text without special characters"),
                         "Normal text without special characters")
})


# create tests for format_bibtex_authors function
test_that("format_bibtex_authors formats individual authors correctly", {
  # start with a single author
  testthat::expect_equal(
    format_bibtex_authors(individual_authors[1, , drop = FALSE]),
    "Smith, John"
  )

  # then do multiple authors
  testthat::expect_equal(
    format_bibtex_authors(individual_authors),
    "Smith, John and Johnson, Sarah and Lee, David"
  )
})


test_that("format_bibtex_authors handles organizations correctly", {
  testthat::expect_equal(
    format_bibtex_authors(organization_author),
    "{National Science Foundation}"
  )
})


test_that("format_bibtex_authors drops organization when individuals follow", {
  testthat::expect_equal(
    format_bibtex_authors(mixed_authors),
    "Smith, John and Johnson, Sarah"
  )
})


test_that("format_bibtex_authors handles partial author information", {
  testthat::expect_equal(
    format_bibtex_authors(partial_author),
    "Zhang, "
  )

  # create author with only given name
  given_only <- data.frame(
    family = NA_character_,
    given = "Maria",
    name = NA_character_,
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(
    format_bibtex_authors(given_only),
    "Unknown, Maria"
  )
})


test_that("format_bibtex_authors handles NULL or empty input", {
  testthat::expect_equal(format_bibtex_authors(NULL), "Unknown")

  empty_df <- data.frame(
    family = character(0),
    given = character(0),
    name = character(0),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(format_bibtex_authors(empty_df), "Unknown")
})


test_that("format_bibtex_authors handles mixed complete and partial authors", {
  mixed_complete_partial <- rbind(
    individual_authors[1, ],
    partial_author
  )

  testthat::expect_equal(
    format_bibtex_authors(mixed_complete_partial),
    "Smith, John and Zhang, "
  )
})


# create tests for extract_year_vectorized function
test_that("extract_year_vectorized follows correct priority order", {
  # test with all dates available - should use print date
  testthat::expect_equal(
    extract_year_vectorized(
      published_print = "2022-01-15",
      published_online = "2021-12-10",
      created = "2020-07-22"
    ),
    "2022"
  )

  # test with only online and created dates - should use online date
  testthat::expect_equal(
    extract_year_vectorized(
      published_print = NA,
      published_online = "2021-12-10",
      created = "2020-07-22"
    ),
    "2021"
  )

  # test with only created date - should use created date
  testthat::expect_equal(
    extract_year_vectorized(
      published_print = NA,
      published_online = NA,
      created = "2020-07-22"
    ),
    "2020"
  )
})


test_that("extract_year_vectorized handles vectors correctly", {
  testthat::expect_equal(
    extract_year_vectorized(
      published_print = c("2022-01-15", NA, NA, "2023-06-30"),
      published_online = c(NA, "2021-12-10", NA, "2023-05-15"),
      created = c(NA, NA, "2020-07-22", "2023-01-01")
    ),
    c("2022", "2021", "2020", "2023")
  )
})


test_that("extract_year_vectorized handles empty strings", {
  testthat::expect_equal(
    extract_year_vectorized(
      published_print = "",
      published_online = "2021-12-10",
      created = "2020-07-22"
    ),
    # empty string should be treated like NA
    "2021"
  )
})


test_that("extract_year_vectorized handles all missing values", {
  testthat::expect_equal(
    extract_year_vectorized(
      published_print = NA,
      published_online = NA,
      created = NA
    ),
    NA_integer_
  )

  testthat::expect_equal(
    extract_year_vectorized(
      published_print = c(NA, NA),
      published_online = c(NA, NA),
      created = c(NA, NA)
    ),
    c(NA_integer_, NA_integer_)
  )
})


test_that("extract_year_vectorized handles unusual date formats", {
  # function should still extract first 4 characters
  testthat::expect_equal(
    extract_year_vectorized(
      published_print = "2022",
      published_online = NA,
      created = NA
    ),
    "2022"
  )

  testthat::expect_equal(
    extract_year_vectorized(
      published_print = NA,
      published_online = "2021/01/15",
      created = NA
    ),
    "2021"
  )
})


# create tests for create_bibtex function
test_that("create_bibtex creates valid article entry", {
  result <- create_bibtex(test_pub_data)

  testthat::expect_equal(result$entry_type, "article")
  testthat::expect_equal(result$key, "Smith2023")
  testthat::expect_equal(result$author, "Smith, John")
  testthat::expect_equal(result$title, "Example Research Paper")
  testthat::expect_equal(result$year, "2023")
  testthat::expect_equal(result$journal, "Journal of Examples")
  testthat::expect_equal(result$volume, "45")
  testthat::expect_equal(result$number, "2")
  testthat::expect_equal(result$pages, "123-145")

  # check if the raw BibTeX string contains expected elements
  testthat::expect_true(grepl("@article\\{Smith2023,", result$raw_bibtex))
  testthat::expect_true(grepl("journal = \\{Journal of Examples\\},", result$raw_bibtex))
  testthat::expect_true(grepl("title = \\{\\{Example Research Paper\\}\\},", result$raw_bibtex))
})


test_that("create_bibtex creates valid book entry", {
  result <- create_bibtex(test_book_data)

  testthat::expect_equal(result$entry_type, "book")
  testthat::expect_equal(result$key, "Johnson2022")
  testthat::expect_equal(result$publisher, "Data Science Publishing")

  # try a book since it doesn't doesn't have journal field
  testthat::expect_null(result$journal)

  # check raw BibTeX
  testthat::expect_true(grepl("@book\\{Johnson2022,", result$raw_bibtex))
  testthat::expect_true(grepl("publisher = \\{Data Science Publishing\\},", result$raw_bibtex))
})


test_that("create_bibtex creates valid book chapter entry", {
  result <- create_bibtex(test_chapter_data)

  testthat::expect_equal(result$entry_type, "incollection")
  testthat::expect_equal(result$key, "Lee2021")
  testthat::expect_equal(result$booktitle, "Handbook of Data Science")
  testthat::expect_equal(result$publisher, "Academic Press")

  # try a chapter that doesn't have journal field
  testthat::expect_null(result$journal)

  # check raw BibTeX
  testthat::expect_true(grepl("@incollection\\{Lee2021,", result$raw_bibtex))
  testthat::expect_true(grepl("booktitle = \\{Handbook of Data Science\\},", result$raw_bibtex))
})


test_that("create_bibtex handles custom key prefix", {
  result <- create_bibtex(test_pub_data, key_prefix = "ref:")

  testthat::expect_equal(result$key, "ref:Smith2023")
  testthat::expect_true(grepl("@article\\{ref:Smith2023,", result$raw_bibtex))
})


test_that("create_bibtex handles missing required fields", {
  # try a missing DOI
  invalid_data <- test_pub_data
  invalid_data$doi <- NA

  testthat::expect_equal(create_bibtex(invalid_data), NA_character_)

  # try missing title
  invalid_data <- test_pub_data
  invalid_data$title <- NA

  testthat::expect_equal(create_bibtex(invalid_data), NA_character_)

  # try missing container title
  invalid_data <- test_pub_data
  invalid_data$container_title <- NA

  testthat::expect_equal(create_bibtex(invalid_data), NA_character_)

  # try missing year
  invalid_data <- test_pub_data
  invalid_data$pub_year <- NA

  testthat::expect_equal(create_bibtex(invalid_data), NA_character_)
})


test_that("create_bibtex handles special characters in titles", {
  special_data <- test_pub_data
  special_data$title <- "Research & Development: 100% Effective Methods"

  result <- create_bibtex(special_data)

  # title should be escaped and wrapped in braces
  testthat::expect_true(grepl("title = \\{\\{Research \\\\& Development: 100\\\\% Effective Methods\\}\\},",
                              result$raw_bibtex))
})


test_that("create_bibtex handles publication with unknown type", {
  unknown_type_data <- test_pub_data
  unknown_type_data$type <- "unknown-type"

  result <- create_bibtex(unknown_type_data)

  # should default to article
  testthat::expect_equal(result$entry_type, "article")
  testthat::expect_true(grepl("@article\\{", result$raw_bibtex))
})
