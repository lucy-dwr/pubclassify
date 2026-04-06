test_that("pc_classify() rejects non-pc_taxonomy input", {
  results <- .pc_empty_result()
  expect_error(
    pc_classify(results, data.frame(field = "a", definition = "b")),
    "pc_taxonomy"
  )
})

test_that("pc_classify() warns for large taxonomies without embeddings", {
  results  <- .pc_empty_result()
  big_tax  <- pc_taxonomy(data.frame(
    field      = paste0("Field", seq_len(40L)),
    definition = paste0("Definition ", seq_len(40L))
  ))
  expect_warning(
    pc_classify(results, big_tax, use_embeddings = FALSE),
    "40 fields"
  )
})

test_that("pc_classify() does not warn below 40 fields", {
  results  <- .pc_empty_result()
  small_tax <- pc_taxonomy(data.frame(
    field      = paste0("Field", seq_len(39L)),
    definition = paste0("Definition ", seq_len(39L))
  ))
  expect_no_warning(
    pc_classify(results, small_tax, use_embeddings = FALSE)
  )
})

test_that("pc_classify() requires embed_provider when use_embeddings = TRUE", {
  results <- .pc_empty_result()
  tax     <- pc_taxonomy_example()
  expect_error(
    pc_classify(results, tax, use_embeddings = TRUE, embed_provider = NULL),
    "embed_provider"
  )
})

test_that(".pc_build_classify_text() uses title+abstract when available", {
  text <- .pc_build_classify_text("My Title", "My abstract text.")
  expect_match(text, "Title: My Title")
  expect_match(text, "Abstract: My abstract text.")
})

test_that(".pc_build_classify_text() falls back to title when abstract is NA", {
  text <- .pc_build_classify_text("My Title", NA_character_)
  expect_match(text, "Title: My Title")
  expect_no_match(text, "Abstract:")
})

test_that(".pc_build_taxonomy_prompt() includes all field names", {
  tax    <- pc_taxonomy_example()
  prompt <- .pc_build_taxonomy_prompt(tax)
  for (f in tax$field) expect_match(prompt, f, fixed = TRUE)
})
