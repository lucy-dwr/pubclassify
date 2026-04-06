test_that("pc_taxonomy() accepts a valid data frame", {
  df  <- data.frame(field = "Ecology", definition = "Study of organisms.")
  tax <- pc_taxonomy(df)
  expect_s3_class(tax, "pc_taxonomy")
  expect_named(tax, c("field", "definition"))
  expect_equal(nrow(tax), 1L)
})

test_that("pc_taxonomy() accepts only field and definition columns", {
  df  <- data.frame(field = "Ecology", definition = "Study of organisms.",
                    extra = "ignored")
  tax <- pc_taxonomy(df)
  expect_named(tax, c("field", "definition"))
})

test_that("pc_taxonomy() rejects missing required columns", {
  expect_error(pc_taxonomy(data.frame(x = "a", y = "b")), "missing required")
  expect_error(pc_taxonomy(data.frame(field = "a")),       "missing required")
})

test_that("pc_taxonomy() rejects duplicate field names", {
  df <- data.frame(field = c("Ecology", "Ecology"),
                   definition = c("def 1", "def 2"))
  expect_error(pc_taxonomy(df), "duplicate")
})

test_that("pc_taxonomy() rejects NA values", {
  expect_error(
    pc_taxonomy(data.frame(field = NA_character_, definition = "def")), "NA"
  )
  expect_error(
    pc_taxonomy(data.frame(field = "Ecology", definition = NA_character_)), "NA"
  )
})

test_that("pc_taxonomy() rejects empty strings", {
  expect_error(
    pc_taxonomy(data.frame(field = "", definition = "def")), "empty"
  )
})

test_that("pc_taxonomy() rejects non-data-frame input", {
  expect_error(pc_taxonomy(list(field = "a", definition = "b")))
})

test_that("pc_taxonomy_example() returns a valid taxonomy", {
  tax <- pc_taxonomy_example()
  expect_s3_class(tax, "pc_taxonomy")
  expect_named(tax, c("field", "definition"))
  expect_gt(nrow(tax), 0L)
  expect_false(anyNA(tax$field))
  expect_false(anyNA(tax$definition))
  expect_equal(anyDuplicated(tax$field), 0L)
})

test_that("print.pc_taxonomy() returns invisibly", {
  tax <- pc_taxonomy_example()
  expect_invisible(print(tax))
})
