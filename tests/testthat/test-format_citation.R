# create test data that will be used across multiple tests
test_bibtex_data <- list(
  entry_type = "article",
  author = "Smith, John and Johnson, Sarah and Lee, David",
  year = "2023",
  title = "Example research paper with some special characters: & % _",
  journal = "Journal of Examples",
  volume = "45",
  number = "2",
  pages = "123-145",
  doi = "10.1234/example",
  url = "https://doi.org/10.1234/example"
)

test_bibtex_book <- list(
  entry_type = "book",
  author = "Smith, John",
  year = "2022",
  title = "Comprehensive Guide to R",
  publisher = "Data Science Publishing",
  doi = "10.5678/book.example"
)

test_bibtex_many_authors <- list(
  entry_type = "article",
  author = paste(
    "Smith, John", "Johnson, Sarah", "Lee, David", "Brown, Robert",
    "Wilson, Emily", "Clark, Richard", "Martinez, Maria", "Taylor, Thomas",
    "Anderson, James", "White, Jennifer", "Garcia, Carlos", "Hill, Elizabeth",
    sep = " and "
  ),
  year = "2023",
  title = "Collaborative research with many contributors",
  journal = "Large Team Science",
  volume = "12",
  number = "1",
  pages = "1-20",
  doi = "10.1234/large-team"
)

test_pub_data <- data.frame(
  doi = "10.1234/test.article",
  title = "test Article for Format Citation",
  container_title = "test Journal",
  pub_year = 2023,
  lead_author_last_name = "Smith",
  type = "journal-article",
  volume = "1",
  issue = "2",
  page = "3-4",
  url = "https://doi.org/10.1234/test.article",
  stringsAsFactors = FALSE
)

# mock the lead author data structure
test_pub_data$author <- list(
  data.frame(
    family = c("Smith", "Johnson"),
    given = c("John", "Sarah"),
    sequence = c("first", "additional"),
    stringsAsFactors = FALSE
  )
)


# test format_citation function
test_that("format_citation handles different input types", {
  # set up mock functions to test input handling
  mockery::stub(format_citation, "parse_bibtex", function(x) test_bibtex_data)
  mockery::stub(format_citation, "create_bibtex", function(x) test_bibtex_data)
  mockery::stub(format_citation, "format_apa", function(x, y) "APA formatted citation")

  # test with character input (BibTeX string)
  char_result <- format_citation("@article{key, title = {Title}}")
  testthat::expect_equal(char_result, "APA formatted citation")

  # test with data frame input
  df_result <- format_citation(test_pub_data)
  testthat::expect_equal(df_result, "APA formatted citation")

  # test with list input (BibTeX fields)
  list_result <- format_citation(test_bibtex_data)
  testthat::expect_equal(list_result, "APA formatted citation")
})


test_that("format_citation uses different style formatters", {
  # mock style-specific formatters
  mockery::stub(format_citation, "create_bibtex", function(x) test_bibtex_data)
  mockery::stub(format_citation, "format_apa", function(x, y) "APA format")
  mockery::stub(format_citation, "format_nature", function(x, y) "Nature format")
  mockery::stub(format_citation, "format_chicago", function(x, y) "Chicago format")
  mockery::stub(format_citation, "format_science", function(x, y) "Science format")
  mockery::stub(format_citation, "format_agu", function(x, y) "AGU format")

  # test different styles
  testthat::expect_equal(format_citation(test_bibtex_data, style = "apa"), "APA format")
  testthat::expect_equal(format_citation(test_bibtex_data, style = "nature"), "Nature format")
  testthat::expect_equal(format_citation(test_bibtex_data, style = "chicago"), "Chicago format")
  testthat::expect_equal(format_citation(test_bibtex_data, style = "science"), "Science format")
  testthat::expect_equal(format_citation(test_bibtex_data, style = "agu"), "AGU format")
})


test_that("format_citation handles unsupported styles", {
  # mock the format_apa function
  mockery::stub(format_citation, "format_apa", function(x, y) "APA format")

  # test that an unsupported style defaults to APA with a warning
  testthat::expect_warning(
    result <- format_citation(test_bibtex_data, style = "unsupported"),
    "Unsupported citation style 'unsupported'. Using 'apa' instead."
  )
  testthat::expect_equal(result, "APA format")
})


test_that("format_citation handles invalid input", {
  # test with invalid input (not character, data frame, or list with title)
  testthat::expect_warning(
    result <- format_citation(list()),
    "format_citation\\(\\): invalid input; returning NA."
  )
  testthat::expect_equal(result, NA_character_)
})


test_that("format_citation handles formatter failures", {
  # mock formatter to return NULL (failure case)
  mockery::stub(format_citation, "format_apa", function(x, y) NULL)

  # test that formatter failure produces a warning and returns NA
  testthat::expect_warning(
    result <- format_citation(test_bibtex_data),
    "format_citation\\(\\): formatter 'apa' failed; returning NA."
  )
  testthat::expect_equal(result, NA_character_)
})


# test format_apa and format_apa_authors
test_that("format_apa_authors formats authors correctly", {
  # test single author
  testthat::expect_equal(
    format_apa_authors("Smith, John"),
    "Smith, John"
  )

  # test two authors
  testthat::expect_equal(
    format_apa_authors("Smith, John and Johnson, Sarah"),
    "Smith, John & Johnson, Sarah"
  )

  # test three authors
  testthat::expect_equal(
    format_apa_authors("Smith, John and Johnson, Sarah and Lee, David"),
    "Smith, John, Johnson, Sarah, & Lee, David"
  )

  # test seven authors (all should be included)
  seven_authors <- paste(
    "Author1, A", "Author2, B", "Author3, C", "Author4, D",
    "Author5, E", "Author6, F", "Author7, G",
    sep = " and "
  )
  seven_result <- format_apa_authors(seven_authors)
  testthat::expect_true(grepl("Author7, G$", seven_result))

  # test eight authors (should truncate to six + last)
  eight_authors <- paste(
    "Author1, A", "Author2, B", "Author3, C", "Author4, D",
    "Author5, E", "Author6, F", "Author7, G", "Author8, H",
    sep = " and "
  )
  eight_result <- format_apa_authors(eight_authors)
  testthat::expect_true(grepl("Author1, A, Author2, B, Author3, C, Author4, D, Author5, E, Author6, F, ... Author8, H$", eight_result))
})


test_that("format_apa formats citations correctly", {
  # mock validate_bibtex_data and format_apa_authors
  mockery::stub(format_apa, "validate_bibtex_data", function(x) TRUE)
  mockery::stub(format_apa, "format_apa_authors", function(x) "Smith, John & Johnson, Sarah")

  # create test data
  apa_test_data <- list(
    entry_type = "article",
    author = "test Authors",
    year = "2023",
    title = "test Title",
    journal = "test Journal",
    volume = "45",
    number = "2",
    pages = "123-145",
    doi = "10.1234/test"
  )

  # test APA format with DOI link
  with_link <- format_apa(apa_test_data, link_doi = TRUE)
  testthat::expect_true(grepl("Smith, John & Johnson, Sarah \\(2023\\)", with_link))
  testthat::expect_true(grepl("test Title\\.", with_link))
  testthat::expect_true(grepl("\\*test Journal\\*", with_link))
  testthat::expect_true(grepl("\\*45\\*\\(2\\)", with_link))
  testthat::expect_true(grepl("123-145", with_link))
  testthat::expect_true(grepl("\\[doi:10.1234/test\\]\\(https://doi.org/10.1234/test\\)", with_link))

  # test APA format without DOI link
  without_link <- format_apa(apa_test_data, link_doi = FALSE)
  testthat::expect_true(grepl("doi:10.1234/test$", without_link))
  testthat::expect_false(grepl("\\[doi:", without_link))
})


test_that("format_apa handles invalid input", {
  # test invalid input
  mockery::stub(format_apa, "validate_bibtex_data", function(x) FALSE)

  testthat::expect_warning(
    result <- format_apa(list()),
    "format_nature\\(\\): invalid bibtex data; returning NA."
  )
  testthat::expect_equal(result, NA_character_)
})


# test format_nature and format_nature_authors
test_that("format_nature_authors formats authors correctly", {
  # test single author
  testthat::expect_equal(
    format_nature_authors("Smith, John"),
    "Smith, John."
  )

  # test single author with period
  testthat::expect_equal(
    format_nature_authors("Smith, J."),
    "Smith, J."
  )

  # test two authors
  testthat::expect_equal(
    format_nature_authors("Smith, John and Johnson, Sarah"),
    "Smith, John & Johnson, Sarah"
  )

  # test five authors (all should be included)
  five_authors <- paste(
    "Author1, A", "Author2, B", "Author3, C", "Author4, D", "Author5, E",
    sep = " and "
  )
  five_result <- format_nature_authors(five_authors)
  testthat::expect_equal(
    five_result,
    "Author1, A, Author2, B, Author3, C, Author4, D & Author5, E"
  )

  # test six authors (should use et al.)
  six_authors <- paste(
    "Smith, John", "Johnson, Sarah", "Lee, David",
    "Brown, Robert", "Wilson, Emily", "Clark, Richard",
    sep = " and "
  )
  six_result <- format_nature_authors(six_authors)
  testthat::expect_equal(
    six_result,
    "Smith, John et al."
  )
})


test_that("format_nature formats citations correctly", {
  # mock validate_bibtex_data and format_nature_authors
  mockery::stub(format_nature, "validate_bibtex_data", function(x) TRUE)
  mockery::stub(format_nature, "format_nature_authors", function(x) "Smith, J. et al.")

  # create test data
  nature_test_data <- list(
    entry_type = "article",
    author = "test Authors",
    year = "2023",
    title = "test Title",
    journal = "Nature",
    volume = "600",
    pages = "123-145",
    doi = "10.1038/test"
  )

  # test Nature format with DOI link
  with_link <- format_nature(nature_test_data, link_doi = TRUE)
  testthat::expect_true(grepl("Smith, J. et al. test Title\\.", with_link))
  testthat::expect_true(grepl("\\*Nature\\*", with_link))
  testthat::expect_true(grepl("\\*\\*600\\*\\*", with_link))
  testthat::expect_true(grepl("123-145", with_link))
  testthat::expect_true(grepl("\\(2023\\)", with_link))
  testthat::expect_true(grepl("\\[doi:10.1038/test\\]\\(https://doi.org/10.1038/test\\)", with_link))

  # test Nature format without DOI link
  without_link <- format_nature(nature_test_data, link_doi = FALSE)
  testthat::expect_true(grepl("doi:10.1038/test$", without_link))
  testthat::expect_false(grepl("\\[doi:", without_link))

  # test Nature format without volume
  no_volume <- list(
    entry_type = "article",
    author = "test Authors",
    year = "2023",
    title = "test Title",
    journal = "Nature",
    doi = "10.1038/test"
  )
  no_volume_result <- format_nature(no_volume, link_doi = FALSE)
  testthat::expect_true(grepl("\\*Nature\\* \\(2023\\)\\.", no_volume_result))
})


# test format_chicago and format_chicago_authors
test_that("format_chicago_authors formats authors correctly", {
  # test single author
  testthat::expect_equal(
    format_chicago_authors("Smith, John"),
    "Smith, John"
  )

  # test two authors (second author should be converted to First Last format)
  testthat::expect_equal(
    format_chicago_authors("Smith, John and Johnson, Sarah"),
    "Smith, John, and Sarah Johnson"
  )

  # test three authors
  testthat::expect_equal(
    format_chicago_authors("Smith, John and Johnson, Sarah and Lee, David"),
    "Smith, John, Sarah Johnson, and David Lee"
  )

  # test four authors (should use et al.)
  four_authors <- paste(
    "Smith, John", "Johnson, Sarah", "Lee, David", "Brown, Robert",
    sep = " and "
  )
  four_result <- format_chicago_authors(four_authors)
  testthat::expect_equal(
    four_result,
    "Smith, John et al"
  )
})


test_that("format_chicago formats citations correctly", {
  # mock validate_bibtex_data and format_chicago_authors
  mockery::stub(format_chicago, "validate_bibtex_data", function(x) TRUE)
  mockery::stub(format_chicago, "format_chicago_authors", function(x) "Smith, John, Sarah Johnson, and David Lee")

  # create test data
  chicago_test_data <- list(
    entry_type = "article",
    author = "test Authors",
    year = "2023",
    title = "test Title",
    journal = "Journal of Examples",
    volume = "45",
    number = "2",
    pages = "123-145",
    doi = "10.1234/test"
  )

  # test Chicago format with DOI link
  with_link <- format_chicago(chicago_test_data, link_doi = TRUE)
  testthat::expect_true(grepl("Smith, John, Sarah Johnson, and David Lee\\. 2023\\.", with_link))
  testthat::expect_true(grepl("\"test Title\\.\"", with_link))
  testthat::expect_true(grepl("\\*Journal of Examples\\*", with_link))
  testthat::expect_true(grepl("45, no\\. 2", with_link))
  testthat::expect_true(grepl(": 123-145", with_link))
  testthat::expect_true(grepl("\\[doi:10.1234/test\\]\\(https://doi.org/10.1234/test\\)", with_link))

  # test Chicago format with publisher
  with_publisher <- list(
    entry_type = "book",
    author = "test Authors",
    year = "2023",
    title = "test Book",
    publisher = "test Publisher",
    doi = "10.1234/test"
  )
  publisher_result <- format_chicago(with_publisher, link_doi = FALSE)
  testthat::expect_true(grepl("test Publisher\\.", publisher_result))
})


# test format_science and format_science_authors
test_that("format_science_authors formats authors correctly", {
  # test single author (should extract last name)
  testthat::expect_equal(
    format_science_authors("Smith, John"),
    "Smith"
  )

  # test multiple authors (should use et al.)
  testthat::expect_equal(
    format_science_authors("Smith, John and Johnson, Sarah and Lee, David"),
    "Smith et al"
  )
})

test_that("format_science formats citations correctly", {
  # mock validate_bibtex_data and format_science_authors
  mockery::stub(format_science, "validate_bibtex_data", function(x) TRUE)

  # test single author (should not add period after author)
  single_author_data <- list(
    entry_type = "article",
    author = "Smith, John",
    year = "2023",
    title = "test Title",
    journal = "Science",
    volume = "380",
    pages = "123-145",
    doi = "10.1126/test"
  )
  single_result <- format_science(single_author_data)
  testthat::expect_true(grepl("Smith, test Title\\.", single_result))

  # test multiple authors (should add period after et al)
  multi_author_data <- list(
    entry_type = "article",
    author = "Smith, John and Johnson, Sarah",
    year = "2023",
    title = "test Title",
    journal = "Science",
    volume = "380",
    pages = "123-145",
    doi = "10.1126/test"
  )
  multi_result <- format_science(multi_author_data)
  testthat::expect_true(grepl("Smith et al\\., test Title\\.", multi_result))

  # test format with volume and pages
  testthat::expect_true(grepl("\\*Science\\* \\*\\*380\\*\\*, 123-145", multi_result))

  # test format with year in parentheses
  testthat::expect_true(grepl("\\(2023\\)\\.", multi_result))
})


# test format_agu and format_agu_authors
test_that("format_agu_authors formats authors correctly", {
  # test single author (should convert to initials first)
  testthat::expect_equal(
    format_agu_authors("Smith, John M."),
    "J. M. Smith"
  )

  # test two authors
  testthat::expect_equal(
    format_agu_authors("Smith, John M. and Johnson, Sarah P."),
    "J. M. Smith and S. P. Johnson"
  )

  # test three authors (use commas and "and")
  three_authors <- paste(
    "Smith, John", "Johnson, Sarah", "Lee, David",
    sep = " and "
  )
  three_result <- format_agu_authors(three_authors)
  testthat::expect_equal(
    three_result,
    "J. Smith, S. Johnson, and D. Lee"
  )

  # test ten authors (should list all)
  ten_authors <- paste(
    "Smith, John", "Johnson, Sarah", "Lee, David", "Brown, Robert",
    "Wilson, Emily", "Clark, Richard", "Martinez, Maria", "Taylor, Thomas",
    "Anderson, James", "White, Jennifer",
    sep = " and "
  )
  ten_result <- format_agu_authors(ten_authors)
  testthat::expect_true(grepl("J. White$", ten_result))

  # test eleven authors (should use et al.)
  eleven_authors <- paste(
    ten_authors, "Garcia, Carlos",
    sep = " and "
  )
  eleven_result <- format_agu_authors(eleven_authors)
  testthat::expect_equal(
    eleven_result,
    "J. Smith et al."
  )
})


test_that("format_agu formats citations correctly", {
  # mock validate_bibtex_data
  mockery::stub(format_agu, "validate_bibtex_data", function(x) TRUE)

  # create test data with real author string to test format_agu_authors integration
  agu_test_data <- list(
    entry_type = "article",
    author = "Smith, John M. and Johnson, Sarah P.",
    year = "2023",
    title = "test Title",
    journal = "Journal of Geophysical Research",
    volume = "128",
    number = "4",
    pages = "123-145",
    doi = "10.1029/test"
  )

  # test AGU format with DOI link
  with_link <- format_agu(agu_test_data, link_doi = TRUE)
  testthat::expect_true(grepl("J. M. Smith and S. P. Johnson \\(2023\\)", with_link))
  testthat::expect_true(grepl("test Title,", with_link))
  testthat::expect_true(grepl("\\*Journal of Geophysical Research\\*", with_link))
  testthat::expect_true(grepl("128\\(4\\)", with_link))
  testthat::expect_true(grepl("123-145", with_link))
  testthat::expect_true(grepl("\\[https://doi.org/10.1029/test\\]\\(https://doi.org/10.1029/test\\)", with_link))

  # test AGU format without DOI link
  without_link <- format_agu(agu_test_data, link_doi = FALSE)
  testthat::expect_true(grepl("https://doi.org/10.1029/test$", without_link))
  testthat::expect_false(grepl("\\[https:", without_link))

  # test AGU format without DOI at all
  no_doi <- list(
    entry_type = "article",
    author = "Smith, John M.",
    year = "2023",
    title = "test Title",
    journal = "Journal of Geophysical Research",
    volume = "128"
  )
  no_doi_result <- format_agu(no_doi)
  testthat::expect_true(grepl("\\.$", no_doi_result))
})


# test validate_bibtex_data
test_that("validate_bibtex_data checks required fields", {
  # test valid data
  valid_data <- list(
    author = "Smith, John",
    title = "test Title",
    year = "2023"
  )
  testthat::expect_true(validate_bibtex_data(valid_data))

  # test missing author
  missing_author <- list(
    title = "test Title",
    year = "2023"
  )
  testthat::expect_false(validate_bibtex_data(missing_author))

  # test missing title
  missing_title <- list(
    author = "Smith, John",
    year = "2023"
  )
  testthat::expect_false(validate_bibtex_data(missing_title))

  # test missing year
  missing_year <- list(
    author = "Smith, John",
    title = "test Title"
  )
  testthat::expect_false(validate_bibtex_data(missing_year))

  # test NULL input
  testthat::expect_false(validate_bibtex_data(NULL))

  # test empty list
  testthat::expect_false(validate_bibtex_data(list()))

  # test non-list input
  testthat::expect_false(validate_bibtex_data("not a list"))

  # test custom required fields
  custom_fields <- list(
    field1 = "value1",
    field2 = "value2"
  )
  testthat::expect_true(validate_bibtex_data(custom_fields, required_fields = c("field1", "field2")))
})
