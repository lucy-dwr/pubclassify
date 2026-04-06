# Internal utilities

# Null coalescing operator
#' @importFrom rlang %||%
NULL

# Internal package environment — holds session config set by pc_configure()
.pc_env <- new.env(parent = emptyenv())

# Standard empty tibble matching the pubclassify result schema
# Every source-specific parser returns rows conforming to this layout
.pc_empty_result <- function() {
  tibble::tibble(
    doi           = character(),
    title         = character(),
    abstract      = character(),
    year          = integer(),
    doc_type      = character(),
    authors       = list(),   # list of character vectors
    affiliations  = list(),   # list of character vectors
    funders       = list(),   # list of character vectors
    grant_numbers = list(),   # list of character vectors
    journal       = character(),
    source        = character()
  )
}

# Return the package version string, falling back to "dev" during development
# This is for User-Agent in API calls where suggested
.pc_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("pubclassify")),
    error = function(e) "dev"
  )
}

# Read a required environment variable, aborting with a helpful message if absent
.pc_env_key <- function(var, label) {
  val <- Sys.getenv(var, unset = NA_character_)
  if (is.na(val)) {
    cli::cli_abort(c(
      "No API key found for {label}.",
      "i" = "Set the {.envvar {var}} environment variable, or call {.fn pc_configure}."
    ))
  }
  val
}

# Coerce a possibly-NULL or length-0 value to a single NA_character_
.pc_na_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1L]])
}

# Coerce a possibly-NULL or length-0 value to a single NA_integer_
.pc_na_int <- function(x) {
  if (is.null(x) || length(x) == 0L) NA_integer_ else as.integer(x[[1L]])
}
