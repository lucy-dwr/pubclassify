# identify DWR funder ID
query_str = "California Department of Water Resources"
dwr_funder_info <- get_funder_info(query = query_str)
dwr_funder_id <- dwr_funder_info$id

# call the API
dwr_funded <- get_funded(
  funder_id = dwr_funder_id,
  filter = c(from_pub_date = "2020-01-01", type = "journal-article")
)

# clean the result
drop_cols <- c("archive_2", "clinical_trial_number", "na", "na_1", "na_2",
               "na_3", "na_4", "subtitle", "subtitle_2")

dwr_funded <- clean_crossref_result(
  df = dwr_funded,
  pattern = query_str,
  drop_cols = drop_cols
)

# write out
usethis::use_data(dwr_funded, overwrite = TRUE)
