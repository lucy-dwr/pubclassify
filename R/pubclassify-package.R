#' pubclassify: Retrieve and Classify Bibliometric Publication Metadata
#'
#' `pubclassify` provides tools to retrieve peer-reviewed publication metadata
#' from bibliometric APIs (OpenAlex, Scopus, Crossref) and classify
#' publications into user-defined scholarly field taxonomies using large
#' language models.
#'
#' @section Main workflow:
#' 1. **Configure** credentials with [pc_configure()]
#' 2. **Search** for publications with [pc_search()]
#' 3. **Combine and deduplicate** across searches with [pc_combine()]
#' 4. **Define a taxonomy** with [pc_taxonomy()]
#' 5. **Classify** publications with [pc_classify()]
#'
#' @section API coverage:
#' | Source   | Abstract | Affiliation | Funder   | Key required |
#' |----------|----------|-------------|----------|--------------|
#' | OpenAlex | Yes      | Yes         | Yes      | No           |
#' | Scopus   | Yes      | Yes         | Yes      | Yes          |
#' | Crossref | Yes      | Yes         | Partial  | No           |
#'
#' @keywords internal
"_PACKAGE"
