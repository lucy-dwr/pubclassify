#' Configure pubclassify API credentials
#'
#' Stores API keys and preferences for the current R session in an internal
#' package environment. For persistence across sessions, use
#' [pc_save_config()] to write values to your `.Renviron` file.
#'
#' @param email Character. Your email address, used as a courtesy identifier
#'   for OpenAlex and Crossref requests (polite pool). If `NULL`, falls back
#'   to `PUBCLASSIFY_EMAIL`.
#' @param scopus_key Character. Elsevier Scopus API key. If `NULL`, falls back
#'   to the `SCOPUS_API_KEY` environment variable.
#' @param scopus_insttoken Character. Elsevier institutional token, used
#'   alongside `scopus_key` to authenticate off-campus COMPLETE view access.
#'   If `NULL`, falls back to the `SCOPUS_INSTTOKEN` environment variable.
#'   Keep this value confidential — do not commit it to version control.
#' @param llm_key Character. API key for the LLM provider used by
#'   [pc_classify()]. If `NULL`, falls back to the provider-specific
#'   environment variable (e.g. `ANTHROPIC_API_KEY`).
#' @param llm_provider Character. LLM provider name passed to `ellmer`
#'   (e.g. `"anthropic"`, `"openai"`). Default `"anthropic"`.
#' @param llm_base_url Character. Base URL for an OpenAI-compatible API
#'   endpoint. Use this when calling a non-standard provider (e.g. a local
#'   model server or a third-party OpenAI-compatible service). If `NULL`,
#'   the provider's default endpoint is used. Falls back to the
#'   `PUBCLASSIFY_LLM_BASE_URL` environment variable.
#' @param embed_key Character. API key for the embedding provider used when
#'   `use_embeddings = TRUE` in [pc_classify()]. If `NULL`, falls back to
#'   the provider-specific environment variable.
#' @param embed_provider Character. Embedding provider name
#'   (e.g. `"openai"`, `"cohere"`).
#' @param .overwrite Logical. If `FALSE`, existing session values are not
#'   replaced. Default `TRUE`.
#'
#' @return Invisibly returns the current configuration as a named list,
#'   with key values masked.
#' @export
#'
#' @examples
#' \dontrun{
#' pc_configure(
#'   scopus_key   = "my-scopus-key",
#'   email        = "me@example.com",
#'   llm_provider = "anthropic"
#' )
#' }
pc_configure <- function(
    email             = NULL,
    scopus_key        = NULL,
    scopus_insttoken  = NULL,
    llm_key           = NULL,
    llm_provider      = "anthropic",
    llm_base_url      = NULL,
    embed_key         = NULL,
    embed_provider    = NULL,
    .overwrite        = TRUE
) {
  args <- list(
    email             = email,
    scopus_key        = scopus_key,
    scopus_insttoken  = scopus_insttoken,
    llm_key           = llm_key,
    llm_provider      = llm_provider,
    llm_base_url      = llm_base_url,
    embed_key         = embed_key,
    embed_provider    = embed_provider
  )

  for (nm in names(args)) {
    val <- args[[nm]]
    if (!is.null(val) && (.overwrite || is.null(.pc_env[[nm]]))) {
      assign(nm, val, envir = .pc_env)
    }
  }

  invisible(pc_config())
}

#' View the current pubclassify configuration
#'
#' Prints the active configuration stored by [pc_configure()]. API key
#' values are masked.
#'
#' @return Invisibly returns the configuration as a named list, with key
#'   values replaced by `"<set>"` or `"<not set>"`.
#' @export
#'
#' @examples
#' pc_config()
pc_config <- function() {
  key_fields <- c("scopus_key", "scopus_insttoken", "llm_key", "embed_key")
  nms <- c("scopus_key", "scopus_insttoken", "email", "llm_key", "llm_provider",
           "llm_base_url", "embed_key", "embed_provider")

  cfg <- lapply(setNames(nms, nms), function(nm) {
    val <- .pc_env[[nm]]
    if (nm %in% key_fields) {
      if (!is.null(val) && nzchar(val)) "<set>" else "<not set>"
    } else {
      val %||% "<not set>"
    }
  })

  cli::cli_h1("pubclassify configuration")
  for (nm in names(cfg)) {
    cli::cli_bullets(c("*" = "{nm}: {cfg[[nm]]}"))
  }

  invisible(cfg)
}

#' Save the current configuration to .Renviron
#'
#' Writes the credentials stored by [pc_configure()] to a `.Renviron` file
#' so they persist across R sessions. Only variables that are currently set
#' in the session are written; nothing else in `.Renviron` is touched.
#'
#' In interactive sessions, the function always shows exactly what it will
#' write (with API key values masked) and asks for confirmation before making
#' any changes. In non-interactive sessions it proceeds without prompting, so
#' use `.overwrite = FALSE` (the default) to avoid unintended overwrites in
#' scripts.
#'
#' @section Security:
#' `.Renviron` stores credentials in plain text. For project-scoped files,
#' ensure `.Renviron` is listed in `.gitignore` before committing. The
#' function checks for this and warns if it is not.
#'
#' @param scope Character. Where to write the `.Renviron` file:
#'   - `"user"`: `~/.Renviron`, applies to all R sessions for this user.
#'   - `"project"`: `./.Renviron` in the current working directory, applies
#'     to this project only. Recommended when different projects use different
#'     API keys.
#' @param .overwrite Logical. If `FALSE` (default), variables that already
#'   exist in the target `.Renviron` are skipped, not replaced. Set to `TRUE`
#'   to update existing entries.
#'
#' @return Invisibly returns the path to the `.Renviron` file that was
#'   written, or `NULL` if nothing was written.
#' @export
#'
#' @examples
#' \dontrun{
#' pc_configure(email = "me@example.com", scopus_key = "my-key")
#' pc_save_config(scope = "project")
#' }
pc_save_config <- function(
    scope      = c("user", "project"),
    .overwrite = FALSE
) {
  scope <- rlang::arg_match(scope)

  # Mapping: .pc_env field name → .Renviron variable name
  # Key fields (values masked in output): scopus_key, scopus_insttoken, llm_key, embed_key
  key_fields  <- c("scopus_key", "scopus_insttoken", "llm_key", "embed_key")
  env_var_map <- c(
    email             = "PUBCLASSIFY_EMAIL",
    scopus_key        = "SCOPUS_API_KEY",
    scopus_insttoken  = "SCOPUS_INSTTOKEN",
    llm_key           = "PUBCLASSIFY_LLM_KEY",
    llm_provider      = "PUBCLASSIFY_LLM_PROVIDER",
    llm_base_url      = "PUBCLASSIFY_LLM_BASE_URL",
    embed_key         = "PUBCLASSIFY_EMBED_KEY",
    embed_provider    = "PUBCLASSIFY_EMBED_PROVIDER"
  )

  # Collect values that are currently set in .pc_env
  to_write <- list()
  for (nm in names(env_var_map)) {
    val <- .pc_env[[nm]]
    if (!is.null(val) && nzchar(val)) {
      to_write[[env_var_map[[nm]]]] <- val
    }
  }

  if (length(to_write) == 0L) {
    cli::cli_inform(c(
      "!" = "No configuration is currently set.",
      "i" = "Call {.fn pc_configure} first, then run {.fn pc_save_config} again."
    ))
    return(invisible(NULL))
  }

  # Resolve the target .Renviron path
  renviron_path <- if (scope == "user") {
    path.expand("~/.Renviron")
  } else {
    file.path(getwd(), ".Renviron")
  }

  # Read the existing file (if any)
  existing_lines <- if (file.exists(renviron_path)) {
    readLines(renviron_path, warn = FALSE)
  } else {
    character(0L)
  }

  # Classify each variable as: add (new), overwrite (exists + .overwrite),
  # or skip (exists + !.overwrite)
  adds       <- character(0L)
  overwrites <- character(0L)
  skips      <- character(0L)

  for (var in names(to_write)) {
    already_set <- any(grepl(paste0("^\\s*", var, "\\s*="), existing_lines))
    if (already_set) {
      if (.overwrite) overwrites <- c(overwrites, var) else skips <- c(skips, var)
    } else {
      adds <- c(adds, var)
    }
  }

  # Verbose pre-flight report
  cli::cli_h1("Saving pubclassify configuration")
  cli::cli_inform("Target file: {.path {renviron_path}}")

  if (length(adds) > 0L) {
    cli::cli_h3("Will add ({length(adds)} variable{?s}):")
    for (var in adds) {
      display <- if (var %in% names(env_var_map[key_fields])) "<masked>" else to_write[[var]]
      # Identify key variables by checking if their .pc_env name is a key field
      pc_nm    <- names(env_var_map)[env_var_map == var]
      display  <- if (pc_nm %in% key_fields) "<masked>" else to_write[[var]]
      cli::cli_bullets(c("+" = "{.envvar {var}} = {display}"))
    }
  }

  if (length(overwrites) > 0L) {
    cli::cli_h3("Will overwrite ({length(overwrites)} variable{?s}):")
    for (var in overwrites) {
      pc_nm   <- names(env_var_map)[env_var_map == var]
      display <- if (pc_nm %in% key_fields) "<masked>" else to_write[[var]]
      cli::cli_bullets(c("!" = "{.envvar {var}} = {display}"))
    }
  }

  if (length(skips) > 0L) {
    cli::cli_h3(
      "Will skip ({length(skips)} variable{?s} already set, {.code .overwrite = FALSE}):"
    )
    for (var in skips) {
      cli::cli_bullets(c("x" = "{.envvar {var}}"))
    }
  }

  actual_changes <- c(adds, overwrites)

  if (length(actual_changes) == 0L) {
    cli::cli_inform(c(
      "i" = "Nothing to write.",
      "i" = "Use {.code .overwrite = TRUE} to update existing entries."
    ))
    return(invisible(NULL))
  }

  # .gitignore check for project scope
  if (scope == "project") {
    gitignore_path <- file.path(getwd(), ".gitignore")
    if (file.exists(gitignore_path)) {
      gitignore_lines <- readLines(gitignore_path, warn = FALSE)
      renviron_ignored <- any(grepl("^\\.Renviron$", trimws(gitignore_lines)))
      if (!renviron_ignored) {
        cli::cli_warn(c(
          "!" = "{.path .Renviron} is not listed in {.path .gitignore}.",
          "i" = "Add {.code .Renviron} to {.path .gitignore} to prevent \\
                 credentials from being committed to version control."
        ))
      }
    } else {
      cli::cli_warn(c(
        "!" = "No {.path .gitignore} found in this project.",
        "i" = "Consider creating one and adding {.code .Renviron} to it \\
               before committing."
      ))
    }
  }

  # Interactive confirmation
  if (interactive()) {
    answer <- readline(
      prompt = paste0(
        "Write ", length(actual_changes), " variable(s) to ",
        renviron_path, "? [y/N] "
      )
    )
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      cli::cli_inform("Cancelled. Nothing was written.")
      return(invisible(NULL))
    }
  }

  # Now write
  updated_lines <- existing_lines

  # Replace lines for overwritten variables in-place
  for (var in overwrites) {
    pattern <- paste0("^\\s*", var, "\\s*=")
    updated_lines[grepl(pattern, updated_lines)] <-
      paste0(var, '="', to_write[[var]], '"')
  }

  # Append new variables, with a blank-line separator if needed
  if (length(adds) > 0L) {
    if (length(updated_lines) > 0L && nzchar(utils::tail(updated_lines, 1L))) {
      updated_lines <- c(updated_lines, "")
    }
    new_lines     <- vapply(adds,
                            function(v) paste0(v, '="', to_write[[v]], '"'),
                            character(1L))
    updated_lines <- c(updated_lines, new_lines)
  }

  writeLines(updated_lines, renviron_path)

  cli::cli_inform(c(
    "v" = "Written to {.path {renviron_path}}.",
    "i" = "Restart R (or run {.code readRenviron(\"{renviron_path}\")}) \\
           for changes to take effect."
  ))

  invisible(renviron_path)
}
