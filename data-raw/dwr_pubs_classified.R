dwr_pubs_classified <- classify_pubs_gemini(
  dwr_pubs,
  taxonomy_df = nsf_sed_taxonomy,
  model = "gemini-2.0-flash"
)

dwr_pubs_classified <- dwr_pubs_classified |>
  dplyr::mutate(
    dplyr::across(
      c(first_level, second_level, third_level),
      ~ tolower(as.character(.x))
    )
  )

dwr_pubs_classified <- dplyr::left_join(
  x = dwr_pubs,
  y = dwr_pubs_classified,
  by = "doi"
)

usethis::use_data(dwr_pubs_classified, overwrite = TRUE)
