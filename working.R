devtools::load_all()

pc_configure(
  scopus_key       = Sys.getenv("SCOPUS_API_KEY"),
  scopus_insttoken = Sys.getenv("SCOPUS_INSTTOKEN"),
  email            = Sys.getenv("PUBCLASSIFY_EMAIL")
)

dwr_funded <- pc_search_scopus(
  query         = "California Department of Water Resources",
  field         = "funder",
  doc_type      = "article",
  start_year    = 2020,
  end_year      = 2026,
  max_results   = 5000L,
  award_pattern = "^4600"
) |>
  dplyr::filter(year %in% 2020:2026)

dwr_authored <- pc_search_scopus(
  query       = "California Department of Water Resources",
  field       = "affiliation",
  doc_type.   = "article",
  start_year  = 2020,
  end_year    = 2026,
  max_results = 10000L
) |>
  dplyr::filter(year %in% 2020:2026)