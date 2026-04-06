#' Create a publication taxonomy
#'
#' Validates and wraps a data frame of scholarly fields and their definitions
#' into a `pc_taxonomy` object for use with [pc_classify()]. A taxonomy
#' provides the controlled vocabulary that the LLM assigns publications to.
#'
#' @param x A data frame with exactly two columns:
#'   - `field`: unique field names (character)
#'   - `definition`: free-text descriptions used by the LLM for classification
#'     (character)
#'
#'   Alternatively, a file path (character) pointing to a CSV with those
#'   two columns.
#'
#' @return An object of class `pc_taxonomy` (a validated data frame with
#'   columns `field` and `definition`).
#' @export
#'
#' @examples
#' tax <- pc_taxonomy(data.frame(
#'   field      = c("Ecology", "Economics", "Computer Science"),
#'   definition = c(
#'     "Study of organisms and their interactions with the environment.",
#'     "Study of production, distribution, and consumption of goods.",
#'     "Study of computation, algorithms, and information systems."
#'   )
#' ))
#' print(tax)
pc_taxonomy <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      cli::cli_abort("File not found: {.path {x}}")
    }
    x <- utils::read.csv(x, stringsAsFactors = FALSE)
  }

  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg x} must be a data frame or a path to a CSV file, not {.obj_type_friendly {x}}."
    )
  }

  required_cols <- c("field", "definition")
  missing_cols  <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Taxonomy is missing required column{?s}: {.val {missing_cols}}.",
      "i" = "Expected columns: {.val {required_cols}}."
    ))
  }

  x <- x[, required_cols, drop = FALSE]
  x$field      <- as.character(x$field)
  x$definition <- as.character(x$definition)

  if (anyNA(x$field) || anyNA(x$definition)) {
    cli::cli_abort("Taxonomy columns must not contain {.val NA} values.")
  }
  if (any(!nzchar(x$field)) || any(!nzchar(x$definition))) {
    cli::cli_abort("Taxonomy columns must not contain empty strings.")
  }
  if (anyDuplicated(x$field)) {
    dups <- unique(x$field[duplicated(x$field)])
    cli::cli_abort(
      "Taxonomy contains duplicate field name{?s}: {.val {dups}}."
    )
  }

  structure(x, class = c("pc_taxonomy", "data.frame"))
}

#' @export
print.pc_taxonomy <- function(x, ...) {
  cli::cli_h1("pubclassify taxonomy ({nrow(x)} field{?s})")
  for (i in seq_len(nrow(x))) {
    cli::cli_bullets(c("*" = "{x$field[[i]]}: {x$definition[[i]]}"))
  }
  invisible(x)
}

#' @export
format.pc_taxonomy <- function(x, ...) {
  header <- sprintf("pc_taxonomy [%d field%s]", nrow(x),
                    if (nrow(x) == 1L) "" else "s")
  lines  <- paste0("  ", x$field, ": ", x$definition)
  paste(c(header, lines), collapse = "\n")
}

#' Built-in example taxonomy
#'
#' Returns a small example [pc_taxonomy()] based on a subset of the OECD
#' Fields of Research and Development (FORD) classification. Useful for
#' testing and as a template for building custom taxonomies.
#'
#' @return A [pc_taxonomy()] object with 10 scholarly fields.
#' @export
#'
#' @examples
#' tax <- pc_taxonomy_example()
#' print(tax)
pc_taxonomy_example <- function() {
  pc_taxonomy(data.frame(
    field = c(
      "Mathematics",
      "Computer and Information Sciences",
      "Physical Sciences",
      "Chemical Sciences",
      "Earth and Environmental Sciences",
      "Biological Sciences",
      "Medical and Health Sciences",
      "Agriculture and Veterinary Sciences",
      "Social Sciences",
      "Humanities and the Arts"
    ),
    definition = c(
      "Study of abstract structures including algebra, analysis, geometry, and statistics.",
      "Study of computation, algorithms, software, data systems, and artificial intelligence.",
      "Study of matter and energy including physics, astronomy, and materials science.",
      "Study of the composition, structure, and reactions of chemical substances.",
      "Study of the Earth, atmosphere, oceans, climate, and environmental systems.",
      "Study of living organisms including genetics, ecology, microbiology, and biochemistry.",
      "Study of human health, disease, clinical medicine, and biomedical research.",
      "Study of agricultural systems, food science, animal husbandry, and veterinary medicine.",
      "Study of human society including economics, psychology, sociology, and political science.",
      "Study of human culture, language, literature, philosophy, history, and creative arts."
    )
  ))
}
