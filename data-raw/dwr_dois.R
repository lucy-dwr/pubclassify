# read in and clean a list of DWR publication DOIs
dwr_dois_list_raw <- readr::read_lines(file = here::here("data-raw", "dwr_dois_list.csv"))
dwr_dois_list_clean <- clean_doi(doi = dwr_dois_list_raw)

# retrieve publication records for DWR publication DOIs
dwr_dois <- get_pubs(dois = dwr_dois_list_clean, preserve_404 = TRUE)
dwr_dois <- dwr_dois |> tibble::rowid_to_column(var = "index")

# extract unsuccessful call records
dwr_dois_failed <- dwr_dois |>
  dplyr::filter(!doi_retrieved) |>
  dplyr::select(index, doi_retrieved) |>
  dplyr::mutate(original_doi = dwr_dois_list_raw[index])

dwr_dois <- dwr_dois |> dplyr::filter(doi_retrieved)

# clean the result
query_str <- "California Department of Water Resources"

drop_cols <- c("archive_2", "clinical_trial_number", "doi_retrieved", "index",
               "na", "na_1", "na_2", "na_3", "na_4", "original_doi", "subtitle",
               "subtitle_2")

dwr_dois <- clean_crossref_result(
  df = dwr_dois,
  drop_cols = drop_cols
)

# write out
usethis::use_data(dwr_dois, overwrite = TRUE)
usethis::use_data(dwr_dois_failed, overwrite = TRUE)
