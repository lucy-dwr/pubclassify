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

test_that(".pc_classify_batch warns (not errors) on an unrecognised category", {
  chat <- list(chat = function(prompt) {
    paste0(
      "[",
      '{"index":1,"field":"hydrology","rationale":"valid"},',
      '{"index":2,"field":"not a real field","rationale":"invalid"}',
      "]"
    )
  })

  expect_warning(
    out <- .pc_classify_batch(
      texts                 = c("Title: A", "Title: B"),
      taxonomy_prompt       = "Taxonomy:\n- hydrology: water movement",
      classify_instructions = NULL,
      chat                  = chat,
      valid_fields          = "hydrology"
    ),
    "unrecognised category"
  )

  expect_identical(out$field, c("hydrology", NA_character_))
  expect_identical(out$rationale, c("valid", NA_character_))
})

test_that(".pc_classify_batch returns all NA when index count mismatches", {
  # Two publications submitted, but the model returns only one object.
  chat <- list(chat = function(prompt) {
    '[{"index":1,"field":"hydrology","rationale":"r"}]'
  })

  expect_warning(
    out <- .pc_classify_batch(
      texts                 = c("Title: A", "Title: B"),
      taxonomy_prompt       = "Taxonomy:\n- hydrology: water movement",
      classify_instructions = NULL,
      chat                  = chat,
      valid_fields          = "hydrology"
    ),
    "do not match"
  )

  expect_identical(out$field, c(NA_character_, NA_character_))
})

test_that(".pc_classify_batch returns all NA when indices are duplicated", {
  # Right count, but index 1 appears twice and index 2 is missing.
  chat <- list(chat = function(prompt) {
    paste0(
      "[",
      '{"index":1,"field":"hydrology","rationale":"a"},',
      '{"index":1,"field":"hydrology","rationale":"b"}',
      "]"
    )
  })

  expect_warning(
    out <- .pc_classify_batch(
      texts                 = c("Title: A", "Title: B"),
      taxonomy_prompt       = "Taxonomy:\n- hydrology: water movement",
      classify_instructions = NULL,
      chat                  = chat,
      valid_fields          = "hydrology"
    ),
    "do not match"
  )

  expect_identical(out$field, c(NA_character_, NA_character_))
})

test_that(".pc_classify_with_retry recovers from a malformed batch response", {
  attempt <- 0L
  chat_fn <- function() {
    list(chat = function(prompt) {
      attempt <<- attempt + 1L
      if (attempt == 1L) {
        # Misaligned: only one object for two publications.
        '[{"index":1,"field":"hydrology","rationale":"r"}]'
      } else {
        paste0(
          "[",
          '{"index":1,"field":"hydrology","rationale":"r1"},',
          '{"index":2,"field":"hydrology","rationale":"r2"}',
          "]"
        )
      }
    })
  }

  withCallingHandlers(
    out <- .pc_classify_with_retry(
      texts                 = c("Title: A", "Title: B"),
      taxonomy_prompt       = "Taxonomy:\n- hydrology: water movement",
      classify_instructions = NULL,
      chat_fn               = chat_fn,
      valid_fields          = "hydrology"
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )

  expect_identical(out$field, c("hydrology", "hydrology"))
  expect_identical(out$rationale, c("r1", "r2"))
  expect_identical(attempt, 2L)
})

test_that(".pc_classify_with_retry resubmits unrecognised rows and succeeds", {
  attempt <- 0L
  chat_fn <- function() {
    list(chat = function(prompt) {
      attempt <<- attempt + 1L
      field <- if (attempt == 1L) "not a real field" else "hydrology"
      paste0('[{"index":1,"field":"', field, '","rationale":"r"}]')
    })
  }

  withCallingHandlers(
    out <- .pc_classify_with_retry(
      texts                 = "Title: A",
      taxonomy_prompt       = "Taxonomy:\n- hydrology: water movement",
      classify_instructions = NULL,
      chat_fn               = chat_fn,
      valid_fields          = "hydrology"
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )

  expect_identical(out$field, "hydrology")
  expect_identical(out$rationale, "r")
  expect_identical(attempt, 2L)
})

test_that(".pc_classify_with_retry gives up and returns NA after max_retries", {
  attempts <- 0L
  chat_fn  <- function() {
    list(chat = function(prompt) {
      attempts <<- attempts + 1L
      '[{"index":1,"field":"not a real field","rationale":"r"}]'
    })
  }

  withCallingHandlers(
    expect_warning(
      out <- .pc_classify_with_retry(
        texts                 = "Title: A",
        taxonomy_prompt       = "Taxonomy:\n- hydrology: water movement",
        classify_instructions = NULL,
        chat_fn               = chat_fn,
        valid_fields          = "hydrology"
      ),
      "could not be classified after 3 attempt"
    ),
    warning = function(w) {
      if (grepl("unrecognised category", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_identical(out$field, NA_character_)
  expect_identical(attempts, 3L)
})

test_that(".pc_classify_with_retry preserves valid rows when some are invalid", {
  # The mock inspects how many publications it is asked about: the full batch
  # gets one valid + one invalid; the lone resubmitted row stays invalid. This
  # mirrors retry only resubmitting the rows that failed.
  chat_fn <- function() {
    list(chat = function(prompt) {
      n_pubs <- length(gregexpr("Title:", prompt, fixed = TRUE)[[1L]])
      if (n_pubs >= 2L) {
        paste0(
          "[",
          '{"index":1,"field":"hydrology","rationale":"valid"},',
          '{"index":2,"field":"not a real field","rationale":"invalid"}',
          "]"
        )
      } else {
        '[{"index":1,"field":"not a real field","rationale":"invalid"}]'
      }
    })
  }

  withCallingHandlers(
    out <- .pc_classify_with_retry(
      texts                 = c("Title: A", "Title: B"),
      taxonomy_prompt       = "Taxonomy:\n- hydrology: water movement",
      classify_instructions = NULL,
      chat_fn               = chat_fn,
      valid_fields          = "hydrology"
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )

  expect_identical(out$field, c("hydrology", NA_character_))
  expect_identical(out$rationale, c("valid", NA_character_))
})
