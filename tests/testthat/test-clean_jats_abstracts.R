test_that("clean_jats_abstracts handles basic JATS XML correctly", {
  # create a simple test data frame with JATS XML
  test_df <- tibble::tibble(
    doi = c("10.1234/test.1", "10.1234/test.2"),
    abstract = c(
      "<jats:p>This is a simple abstract.</jats:p>",
      "<jats:abstract><jats:p>Another abstract with nested tags.</jats:p></jats:abstract>"
    )
  )

  # establish expected output
  expected_text <- c(
    "This is a simple abstract.",
    "Another abstract with nested tags."
  )

  # apply function
  result <- clean_jats_abstracts(test_df)

  # check output
  testthat::expect_equal(result$abstract_text, expected_text)
  testthat::expect_equal(names(result), c("doi", "abstract", "abstract_text"))
})


test_that("clean_jats_abstracts handles formatted text correctly", {
  test_df <- tibble::tibble(
    doi = "10.1234/test.3",
    abstract = "<jats:p>This abstract has <jats:italic>italic</jats:italic> and <jats:bold>bold</jats:bold> text.</jats:p>"
  )

  result <- clean_jats_abstracts(test_df)

  testthat::expect_equal(result$abstract_text, "This abstract has italic and bold text.")
})


test_that("clean_jats_abstracts handles whitespace correctly", {
  test_df <- tibble::tibble(
    doi = "10.1234/test.4",
    abstract = "<jats:p>This   abstract  has   extra   whitespace.  </jats:p>"
  )

  result <- clean_jats_abstracts(test_df)

  testthat::expect_equal(result$abstract_text, "This abstract has extra whitespace.")
})


test_that("clean_jats_abstracts handles missing or empty abstracts", {
  test_df <- tibble::tibble(
    doi = c("10.1234/test.5", "10.1234/test.6", "10.1234/test.7"),
    abstract = c(NA_character_, "", "<jats:p></jats:p>")
  )

  result <- clean_jats_abstracts(test_df)

  testthat::expect_equal(result$abstract_text[1], NA_character_)
  testthat::expect_equal(result$abstract_text[2], NA_character_)
  testthat::expect_equal(result$abstract_text[3], "")
})


test_that("clean_jats_abstracts handles different namespaces", {
  test_df <- tibble::tibble(
    doi = c("10.1234/test.8", "10.1234/test.9"),
    abstract = c(
      "<jats:p>Standard JATS namespace.</jats:p>",
      "<custom:p>Custom namespace prefix.</custom:p>"
    )
  )

  result <- clean_jats_abstracts(test_df)

  testthat::expect_equal(result$abstract_text, c(
    "Standard JATS namespace.",
    "Custom namespace prefix."
  ))
})


test_that("clean_jats_abstracts handles non-JATS XML correctly", {
  test_df <- tibble::tibble(
    doi = "10.1234/test.10",
    abstract = "<p>This is not JATS, just regular XML.</p>"
  )

  result <- clean_jats_abstracts(test_df)

  testthat::expect_equal(result$abstract_text, "This is not JATS, just regular XML.")
})


test_that("clean_jats_abstracts removes leading 'Abstract:' text", {
  test_df <- tibble::tibble(
    doi = c("10.1234/test.11", "10.1234/test.12", "10.1234/test.13"),
    abstract = c(
      "<jats:p>Abstract: The main content starts here.</jats:p>",
      "<jats:p>ABSTRACT: Another example with uppercase.</jats:p>",
      "<jats:p>abstract This one without colon.</jats:p>"
    )
  )

  result <- clean_jats_abstracts(test_df)

  testthat::expect_equal(result$abstract_text, c(
    "The main content starts here.",
    "Another example with uppercase.",
    "abstract This one without colon."
  ))
})


test_that("clean_jats_abstracts uses custom column names", {
  test_df <- tibble::tibble(
    doi = "10.1234/test.14",
    custom_abstract = "<jats:p>Using custom column names.</jats:p>"
  )

  result <- clean_jats_abstracts(
    test_df,
    abstract_col = "custom_abstract",
    abstract_output_col = "clean_abstract"
  )

  testthat::expect_true("clean_abstract" %in% names(result))
  testthat::expect_equal(result$clean_abstract, "Using custom column names.")
  testthat::expect_equal(names(result), c("doi", "custom_abstract", "clean_abstract"))
})


test_that("clean_jats_abstracts handles complex nested structures", {
  test_df <- tibble::tibble(
    doi = "10.1234/test.15",
    abstract = paste0(
      "<jats:abstract>",
      "  <jats:title>Abstract Title</jats:title>",
      "  <jats:p>First paragraph of the abstract.</jats:p>",
      "  <jats:p>Second paragraph with a <jats:xref ref-type=\"fig\">Figure 1</jats:xref> reference.</jats:p>",
      "  <jats:sec>",
      "    <jats:title>Methods</jats:title>",
      "    <jats:p>Methods description.</jats:p>",
      "  </jats:sec>",
      "</jats:abstract>"
    )
  )

  result <- clean_jats_abstracts(test_df)

  expected_text <- paste(
    "Abstract Title First paragraph of the abstract.",
    "Second paragraph with a Figure 1 reference. Methods Methods description."
  )

  testthat::expect_equal(result$abstract_text, expected_text)
})


test_that("clean_jats_abstracts handles invalid XML gracefully", {
  test_df <- tibble::tibble(
    doi = "10.1234/test.16",
    abstract = "<jats:p>This XML is not closed properly"
  )

  testthat::expect_error(clean_jats_abstracts(test_df), NA)
})


test_that("clean_jats_abstracts requires xml2 package", {
  with_mock(
    "requireNamespace" = function(package, ...) return(FALSE),
    testthat::expect_error(
      clean_jats_abstracts(tibble::tibble(abstract = "test")),
      "Please install xml2: install.packages\\('xml2'\\)"
    )
  )
})
