# create a unified dataset
dwr_pubs <- dplyr::bind_rows(dwr_any_field, dwr_funded, dwr_dois)
dwr_pubs <- dwr_pubs |> dplyr::select(-matching_cols)
dwr_pubs <- search_cols(
  df = dwr_pubs,
  pattern = "California Department of Water Resources"
)

dwr_pubs <- dwr_pubs |>
  dplyr::select(-score, -update_to) |>
  unique()

# add lead author info
dwr_pubs <- dwr_pubs |>
  dplyr::mutate(
    author_info = purrr::map(author, extract_author_info),
    lead_author_first_name = purrr::map_chr(author_info, "lead_author_first_name"),
    lead_author_last_name = purrr::map_chr(author_info, "lead_author_last_name"),
    author_count = purrr::map_int(author_info, "author_count")
  ) |>
  dplyr::select(-author_info)

# add year
dwr_pubs <- dwr_pubs |>
  dplyr::mutate(pub_year = extract_year_vectorized(
    published_print,
    published_online,
    created
  ))

# write out
usethis::use_data(dwr_pubs, overwrite = TRUE)
