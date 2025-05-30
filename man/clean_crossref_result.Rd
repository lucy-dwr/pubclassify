% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_crossref_result.R
\name{clean_crossref_result}
\alias{clean_crossref_result}
\title{Clean and filter Crossref API results}
\usage{
clean_crossref_result(
  df,
  pattern = NULL,
  drop_cols = NULL,
  abstract_col = "abstract",
  abstract_output_col = "abstract_text"
)
}
\arguments{
\item{df}{A data frame containing Crossref API results.}

\item{pattern}{Optional character string or regular expression. When provided,
the function will filter the data frame to retain only rows where this pattern
appears in content columns (excluding references). Default is NULL (no filtering).}

\item{drop_cols}{Optional character vector of column names to remove from the
data frame. Default is NULL (no columns removed).}

\item{abstract_col}{Character string specifying the name of the column containing
abstracts. Default is "abstract".}

\item{abstract_output_col}{Character string specifying the name of the column
where cleaned abstracts will be stored. Default is "abstract_text".}
}
\value{
A tibble with cleaned column names, filtered rows (if pattern is
provided), and cleaned abstracts.
}
\description{
Cleans and filters data frames returned from Crossref API queries. This
function standardizes column names, removes unwanted columns, filters results
based on a search pattern, and cleans JATS-formatted abstracts.
}
\details{
The function performs several cleaning operations:
\enumerate{
\item Standardizes column names using \code{janitor::clean_names()}
\item Removes specified columns if \code{drop_cols} is provided
\item If \code{pattern} is provided, filters rows to those containing the pattern in
content fields (excluding references)
\item Cleans JATS-formatted abstracts using the \code{clean_jats_abstracts()} function
}

The function depends on two helper functions:
\itemize{
\item \code{search_cols()}: Searches columns for a pattern and adds a \code{matching_cols} column
\item \code{clean_jats_abstracts()}: Extracts and cleans text from JATS-formatted XML abstracts
}
}
\examples{
\dontrun{
# get publications from Crossref
crossref_results <- rcrossref::cr_works(query = "machine learning", limit = 20)

# clean the results
cleaned_crossref_results <- clean_crossref_result(
  df = crossref_results$data,
  pattern = "neural network",
  drop_cols = c("reference", "container_title_short"),
  abstract_col = "abstract"
)
}

}
