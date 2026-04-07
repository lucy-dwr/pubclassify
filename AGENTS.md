# AGENTS.md — pubclassify

Guidelines for GenAI agents contributing code to this repository.

`pubclassify` retrieves peer-reviewed manuscript metadata from bibliometric
APIs (OpenAlex, Scopus, Crossref) and classifies publications into a
user-defined taxonomy via LLM (`ellmer`).

---

## Before you start

| Area of work                  | Read first                                                    |
|-------------------------------|---------------------------------------------------------------|
| Any code change               | `R/utils.R` — shared helpers, `.pc_env`, `.pc_empty_result()` |
| Search / API integration      | `R/search_openalex.R` (reference implementation)              |
| Result processing / enrichment| `R/utils_results.R`                                           |
| New API parser                | `R/parse.R`                                                   |
| Classification or taxonomy    | `R/taxonomy.R`, `R/classify.R`                                |
| Credentials / configuration   | `R/config.R`                                                  |

---

## Commands

```r
devtools::load_all()    # load for interactive testing
devtools::document()    # regenerate NAMESPACE and man/ — run after any roxygen change
devtools::test()        # run full test suite — run before declaring any change done
devtools::check()       # R CMD CHECK — run before touching exports, DESCRIPTION, NAMESPACE
testthat::test_file("tests/testthat/test-search.R")  # single file during iteration
```

---

## Standard result schema

All `pc_search_*()` and `pc_combine()` must return a tibble with these columns.
The core ten are **stable** — never remove or rename them.

| Column          | Type               | Notes                                         |
|-----------------|--------------------|-----------------------------------------------|
| `doi`           | `character`        | No `https://doi.org/` prefix                  |
| `title`         | `character`        |                                               |
| `abstract`      | `character`        | `NA` when unavailable                         |
| `year`          | `integer`          |                                               |
| `doc_type`      | `character`        | Document type; `NA` for OpenAlex and Crossref |
| `authors`       | `list` of `chr[]`  | Display names in order                        |
| `affiliations`  | `list` of `list[]` | One inner list per author                     |
| `funders`       | `list` of `chr[]`  | Funder display names                          |
| `grant_numbers` | `list` of `chr[]`  | Award/contract identifiers                    |
| `journal`       | `character`        |                                               |
| `source`        | `character`        | `"openalex"`, `"scopus"`, or `"crossref"`     |

Use `.pc_empty_result()` for zero-row early returns. Optional extra columns
(e.g. `award_match`) are acceptable; never add them to `.pc_empty_result()`.

---

## API credentials

**Never print, log, or write a raw API key or token anywhere in code or tests.**

Credential lookup chain (same pattern in every function):

1. Explicit function argument
2. `.pc_env$<field>` (set via `pc_configure()`)
3. `Sys.getenv("<VAR>", unset = NA_character_)`
4. `.pc_env_key()` — aborts with a helpful message if still missing

In `pc_config()` and `pc_save_config()`, always show keys as `"<set>"` /
`"<not set>"`, never their actual value. `SCOPUS_INSTTOKEN` is especially
sensitive — it grants off-campus institutional access.

| Variable                     | Purpose                         |
|------------------------------|---------------------------------|
| `PUBCLASSIFY_EMAIL`          | OpenAlex / Crossref polite pool |
| `SCOPUS_API_KEY`             | Scopus Search API key           |
| `SCOPUS_INSTTOKEN`           | Scopus institutional token      |
| `PUBCLASSIFY_LLM_KEY`        | LLM provider API key            |
| `PUBCLASSIFY_LLM_PROVIDER`   | LLM provider name               |
| `PUBCLASSIFY_EMBED_KEY`      | Embedding API key               |
| `PUBCLASSIFY_EMBED_PROVIDER` | Embedding provider name         |

---

## Style

- American English spellings (e.g. "standardize", "color", "behavior").
- No dplyr, tidyr, or purrr in internals — the package does not depend on them.
- Treat code as CRAN-ready: `R CMD CHECK` must produce no new errors, warnings,
  or notes.
- Wrap all `@examples` in `\dontrun{}` — examples make live API calls.

---

## Testing

- No live API calls in tests — mock with `httr2` or JSON fixtures in
  `tests/testthat/fixtures/`.
- No tests that depend on environment variables being set.
- Always test the zero-results path: `.pc_empty_result()` returned, not an error.

---

## Stub files

Propose an implementation plan and get explicit approval before writing any code
in stub files (`R/classify.R`, `R/search_crossref.R`).

---

## Definition of done

Before declaring any change complete:

1. `devtools::document()` — no errors or warnings
2. `devtools::test()` — no failures
3. `devtools::check()` — no new errors, warnings, or notes
4. New exported functions have complete roxygen docs (`@param`, `@return`,
   `@examples`)
5. No API keys, tokens, or email addresses in any modified file

---

## When to ask vs. proceed

**Ask first** when the change touches more than two files, requires a new
dependency, involves a stub file, or the correct approach is genuinely
ambiguous.

**Proceed directly** for single-function fixes, documentation edits, and new
tests that follow existing patterns.

When in doubt, ask. A brief confirmation is cheaper than implementing the
wrong thing.

---

## Commits

```
<type>: <imperative summary ≤50 chars>

[Optional body: explain why, not what. Wrap at 72 chars.]

Co-Authored-By: Claude <noreply@anthropic.com>
```

Types: `feat`, `fix`, `docs`, `test`, `refactor`, `chore`

---

## Hard limits

- Never auto-commit or push — always ask first.
- Never add a package dependency without explicit approval.
- Never modify `.Renviron` or files outside the project directory.
- Never hardcode an API key, token, or email address in source files or tests.
- Never change the core result schema (remove, rename, or reorder the ten
  standard columns).
