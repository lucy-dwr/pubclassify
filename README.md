# pubclassify

`pubclassify` is an R package for finding peer-reviewed publications and
organizing them into a user-defined taxonomy.

At a high level, the package:

- retrieves publication metadata from OpenAlex, Scopus, and Crossref
- standardizes and combines results across sources
- enriches records with abstracts, acknowledgments, funders, and affiliations
- classifies publications into custom scholarly categories using LLMs

## Typical workflow

1. Securely configure API and model credentials with `pc_configure()`.
2. Search one or more bibliometric sources with `pc_search()`.
3. Combine and deduplicate records with `pc_combine()` and `pc_deduplicate()`.
4. Define a taxonomy with `pc_taxonomy()`.
5. Classify publications with `pc_classify()`.

## Main functions

- `pc_search()`: unified search interface across supported sources
- `pc_search_openalex()`, `pc_search_scopus()`, `pc_search_crossref()`:
  source-specific search helpers
- `pc_fetch_abstracts()` and `pc_fetch_acknowledgments()`: record enrichment
- `pc_find_funder()` and `pc_flag_awards()`: funding-related utilities
- `pc_taxonomy()` and `pc_taxonomy_example()`: taxonomy creation and examples
- `pc_classify()`: LLM-based classification into taxonomy labels

## Status

This package is under active development. The README is intentionally brief for
now; installation, detailed examples, and workflow guidance will be expanded as
the package interface stabilizes.
