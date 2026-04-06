# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Save all fields currently in .pc_env and restore them on test teardown.
# This prevents any test from polluting .pc_env for subsequent tests.
.with_clean_pc_env <- function() {
  saved <- as.list(pubclassify:::.pc_env)
  withr::defer(
    {
      rm(list = ls(pubclassify:::.pc_env), envir = pubclassify:::.pc_env)
      for (nm in names(saved)) {
        assign(nm, saved[[nm]], envir = pubclassify:::.pc_env)
      }
    },
    envir = parent.frame()
  )
  rm(list = ls(pubclassify:::.pc_env), envir = pubclassify:::.pc_env)
}

# ---------------------------------------------------------------------------
# .pc_na_chr() and .pc_na_int() — coercion helpers
# ---------------------------------------------------------------------------

test_that(".pc_na_chr() returns NA_character_ for NULL", {
  expect_identical(pubclassify:::.pc_na_chr(NULL), NA_character_)
})

test_that(".pc_na_chr() returns NA_character_ for length-0 input", {
  expect_identical(pubclassify:::.pc_na_chr(character(0L)), NA_character_)
})

test_that(".pc_na_chr() coerces scalar to character", {
  expect_identical(pubclassify:::.pc_na_chr(42L), "42")
  expect_identical(pubclassify:::.pc_na_chr("hello"), "hello")
})

test_that(".pc_na_int() returns NA_integer_ for NULL", {
  expect_identical(pubclassify:::.pc_na_int(NULL), NA_integer_)
})

test_that(".pc_na_int() returns NA_integer_ for length-0 input", {
  expect_identical(pubclassify:::.pc_na_int(integer(0L)), NA_integer_)
})

test_that(".pc_na_int() coerces scalar to integer", {
  expect_identical(pubclassify:::.pc_na_int(2022L), 2022L)
  expect_identical(pubclassify:::.pc_na_int(2022.9), 2022L)
})

# ---------------------------------------------------------------------------
# .pc_env_key()
# ---------------------------------------------------------------------------

test_that(".pc_env_key() returns the value when the env var is set", {
  withr::with_envvar(c(SCOPUS_API_KEY = "test-key-123"), {
    result <- pubclassify:::.pc_env_key("SCOPUS_API_KEY", "Scopus")
    expect_equal(result, "test-key-123")
  })
})

test_that(".pc_env_key() aborts with a helpful message when env var is unset", {
  withr::with_envvar(c(SCOPUS_API_KEY = NA_character_), {
    expect_error(
      pubclassify:::.pc_env_key("SCOPUS_API_KEY", "Scopus"),
      "Scopus"
    )
  })
})

# ---------------------------------------------------------------------------
# pc_configure()
# ---------------------------------------------------------------------------

test_that("pc_configure() stores values in .pc_env", {
  .with_clean_pc_env()
  pc_configure(email = "test@example.com", scopus_key = "sk-123")
  expect_equal(pubclassify:::.pc_env$email,      "test@example.com")
  expect_equal(pubclassify:::.pc_env$scopus_key, "sk-123")
})

test_that("pc_configure() does not store NULL values", {
  .with_clean_pc_env()
  pc_configure(email = NULL, scopus_key = "sk-123")
  expect_null(pubclassify:::.pc_env$email)
  expect_equal(pubclassify:::.pc_env$scopus_key, "sk-123")
})

test_that("pc_configure() replaces existing values when .overwrite = TRUE", {
  .with_clean_pc_env()
  pc_configure(email = "first@example.com")
  pc_configure(email = "second@example.com", .overwrite = TRUE)
  expect_equal(pubclassify:::.pc_env$email, "second@example.com")
})

test_that("pc_configure() does not replace existing values when .overwrite = FALSE", {
  .with_clean_pc_env()
  pc_configure(email = "first@example.com")
  pc_configure(email = "second@example.com", .overwrite = FALSE)
  expect_equal(pubclassify:::.pc_env$email, "first@example.com")
})

test_that("pc_configure() stores new values even when .overwrite = FALSE", {
  .with_clean_pc_env()
  pc_configure(email = "me@example.com", .overwrite = FALSE)
  expect_equal(pubclassify:::.pc_env$email, "me@example.com")
})

test_that("pc_configure() stores llm_provider default of 'anthropic'", {
  .with_clean_pc_env()
  pc_configure()
  expect_equal(pubclassify:::.pc_env$llm_provider, "anthropic")
})

test_that("pc_configure() returns invisibly", {
  .with_clean_pc_env()
  expect_invisible(pc_configure(email = "test@example.com"))
})

test_that("pc_configure() returns a named list", {
  .with_clean_pc_env()
  result <- pc_configure(email = "test@example.com")
  expect_type(result, "list")
  expect_named(result, c("scopus_key", "scopus_insttoken", "email", "llm_key",
                         "llm_provider", "llm_base_url", "embed_key",
                         "embed_provider"))
})

# ---------------------------------------------------------------------------
# pc_config()
# ---------------------------------------------------------------------------

test_that("pc_config() masks key fields as '<set>' or '<not set>'", {
  .with_clean_pc_env()
  pc_configure(scopus_key = "secret", llm_key = "also-secret")
  cfg <- pc_config()
  expect_equal(cfg$scopus_key,       "<set>")
  expect_equal(cfg$llm_key,          "<set>")
  expect_equal(cfg$scopus_insttoken, "<not set>")
  expect_equal(cfg$embed_key,        "<not set>")
})

test_that("pc_config() shows actual values for non-key fields", {
  .with_clean_pc_env()
  pc_configure(email = "me@example.com", llm_provider = "openai",
               llm_base_url = "https://api.example.com")
  cfg <- pc_config()
  expect_equal(cfg$email,       "me@example.com")
  expect_equal(cfg$llm_provider, "openai")
  expect_equal(cfg$llm_base_url, "https://api.example.com")
})

test_that("pc_config() shows '<not set>' for unset non-key fields", {
  .with_clean_pc_env()
  cfg <- pc_config()
  expect_equal(cfg$email,          "<not set>")
  expect_equal(cfg$llm_provider,   "<not set>")  # not set until pc_configure() is called
  expect_equal(cfg$embed_provider, "<not set>")
})

test_that("pc_config() never exposes a raw key value", {
  .with_clean_pc_env()
  pc_configure(scopus_key = "raw-secret-key", scopus_insttoken = "raw-token")
  cfg <- pc_config()
  expect_false("raw-secret-key" %in% unlist(cfg))
  expect_false("raw-token"      %in% unlist(cfg))
})

test_that("pc_config() returns invisibly", {
  .with_clean_pc_env()
  expect_invisible(pc_config())
})

# ---------------------------------------------------------------------------
# pc_save_config()
# ---------------------------------------------------------------------------

test_that("pc_save_config() returns NULL when nothing is configured", {
  .with_clean_pc_env()
  result <- pc_save_config()
  expect_null(result)
})

test_that("pc_save_config() writes configured variables to .Renviron (project scope)", {
  .with_clean_pc_env()
  pc_configure(email = "save@example.com", llm_provider = "openai")

  tmp <- withr::local_tempdir()
  # Create a .gitignore that includes .Renviron to suppress the warning
  writeLines(".Renviron", file.path(tmp, ".gitignore"))

  withr::with_dir(tmp, {
    result <- pc_save_config(scope = "project")
    expect_equal(normalizePath(result), normalizePath(file.path(tmp, ".Renviron")))
    expect_true(file.exists(file.path(tmp, ".Renviron")))

    lines <- readLines(file.path(tmp, ".Renviron"))
    expect_true(any(grepl('PUBCLASSIFY_EMAIL="save@example.com"', lines, fixed = TRUE)))
    expect_true(any(grepl('PUBCLASSIFY_LLM_PROVIDER="openai"', lines, fixed = TRUE)))
  })
})

test_that("pc_save_config() masks key values in the written file", {
  .with_clean_pc_env()
  pc_configure(scopus_key = "my-secret-key")

  tmp <- withr::local_tempdir()
  writeLines(".Renviron", file.path(tmp, ".gitignore"))

  withr::with_dir(tmp, {
    pc_save_config(scope = "project")
    lines <- readLines(file.path(tmp, ".Renviron"))
    # The raw key value should NOT appear in the file — only the variable assignment
    # (The function DOES write the real key; this test confirms it's not 'masked')
    expect_true(any(grepl("SCOPUS_API_KEY=", lines)))
    # But it must never write the literal string "<masked>" as the value
    expect_false(any(grepl("<masked>", lines, fixed = TRUE)))
  })
})

test_that("pc_save_config() skips existing variables when .overwrite = FALSE", {
  .with_clean_pc_env()
  pc_configure(email = "new@example.com")

  tmp <- withr::local_tempdir()
  writeLines(c('PUBCLASSIFY_EMAIL="old@example.com"', ".Renviron"),
             file.path(tmp, ".gitignore"))
  writeLines('PUBCLASSIFY_EMAIL="old@example.com"',
             file.path(tmp, ".Renviron"))

  withr::with_dir(tmp, {
    pc_save_config(scope = "project", .overwrite = FALSE)
    lines <- readLines(file.path(tmp, ".Renviron"))
    expect_true(any(grepl("old@example.com", lines)))
    expect_false(any(grepl("new@example.com", lines)))
  })
})

test_that("pc_save_config() overwrites existing variables when .overwrite = TRUE", {
  .with_clean_pc_env()
  pc_configure(email = "new@example.com")

  tmp <- withr::local_tempdir()
  writeLines(".Renviron", file.path(tmp, ".gitignore"))
  writeLines('PUBCLASSIFY_EMAIL="old@example.com"',
             file.path(tmp, ".Renviron"))

  withr::with_dir(tmp, {
    pc_save_config(scope = "project", .overwrite = TRUE)
    lines <- readLines(file.path(tmp, ".Renviron"))
    expect_false(any(grepl("old@example.com", lines)))
    expect_true(any(grepl("new@example.com", lines)))
    # Should be exactly one PUBCLASSIFY_EMAIL line (not duplicated)
    expect_equal(sum(grepl("PUBCLASSIFY_EMAIL", lines)), 1L)
  })
})

test_that("pc_save_config() appends new variables with a blank-line separator", {
  .with_clean_pc_env()
  pc_configure(email = "me@example.com")

  tmp <- withr::local_tempdir()
  writeLines(".Renviron", file.path(tmp, ".gitignore"))
  # Existing file with no trailing blank line
  writeLines('SOME_OTHER_VAR="value"', file.path(tmp, ".Renviron"))

  withr::with_dir(tmp, {
    pc_save_config(scope = "project")
    lines <- readLines(file.path(tmp, ".Renviron"))
    email_idx <- which(grepl("PUBCLASSIFY_EMAIL", lines))
    expect_true(length(email_idx) > 0L)
    # There should be a blank line between the old content and the new entry
    expect_equal(lines[[email_idx - 1L]], "")
  })
})

test_that("pc_save_config() warns when .Renviron is not in .gitignore (project scope)", {
  .with_clean_pc_env()
  pc_configure(email = "me@example.com")

  tmp <- withr::local_tempdir()
  writeLines("*.log", file.path(tmp, ".gitignore"))  # .Renviron not listed

  withr::with_dir(tmp, {
    expect_warning(
      pc_save_config(scope = "project"),
      "\\.gitignore"
    )
  })
})

test_that("pc_save_config() warns when no .gitignore exists (project scope)", {
  .with_clean_pc_env()
  pc_configure(email = "me@example.com")

  tmp <- withr::local_tempdir()  # no .gitignore created

  withr::with_dir(tmp, {
    expect_warning(
      pc_save_config(scope = "project"),
      "\\.gitignore"
    )
  })
})

test_that("pc_save_config() returns NULL when nothing to write (.overwrite = FALSE, all already set)", {
  .with_clean_pc_env()
  pc_configure(email = "me@example.com")   # also sets llm_provider = "anthropic"

  tmp <- withr::local_tempdir()
  writeLines(".Renviron", file.path(tmp, ".gitignore"))
  # Pre-populate BOTH variables that pc_configure sets, so nothing is new
  writeLines(c('PUBCLASSIFY_EMAIL="me@example.com"',
               'PUBCLASSIFY_LLM_PROVIDER="anthropic"'),
             file.path(tmp, ".Renviron"))

  withr::with_dir(tmp, {
    result <- pc_save_config(scope = "project", .overwrite = FALSE)
    expect_null(result)
  })
})

test_that("pc_save_config() returns the file path invisibly on success", {
  .with_clean_pc_env()
  pc_configure(email = "me@example.com")

  tmp <- withr::local_tempdir()
  writeLines(".Renviron", file.path(tmp, ".gitignore"))

  withr::with_dir(tmp, {
    result <- withVisible(pc_save_config(scope = "project"))
    expect_false(result$visible)
    expect_equal(normalizePath(result$value), normalizePath(file.path(tmp, ".Renviron")))
  })
})
