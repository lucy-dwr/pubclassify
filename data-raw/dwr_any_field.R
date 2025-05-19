# set up query
years      <- 2020:2025
query_str  <- "California Department of Water Resources"

# call the API
dwr_any_field_by_year <- lapply(years, function(y) {
  filter_opts <- c(
    from_pub_date  = sprintf("%d-01-01", y),
    until_pub_date = sprintf("%d-12-31", y),
    type           = "journal-article"
  )
  message("=== year ", y, " ===")
  get_pubs(
    query    = query_str,
    filter   = filter_opts,
    total    = 5000L,
    per_page = 1000L
  )
})

# bind to a single object
names(dwr_any_field_by_year) <- as.character(years)
dwr_any_field <- dplyr::bind_rows(dwr_any_field_by_year)

# clean the result
drop_cols <- c("archive_2", "clinical_trial_number", "na", "na_1", "na_2",
               "na_3", "na_4", "subtitle", "subtitle_2")

dwr_any_field <- clean_crossref_result(
  df = dwr_any_field,
  pattern = query_str,
  drop_cols = drop_cols
)

# write out
usethis::use_data(dwr_any_field, overwrite = TRUE)
