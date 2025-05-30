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

# add contribution types; where it's not clear, mark DWR as a funder
dwr_pubs <- dwr_pubs |>
  dplyr::mutate(
    dwr_author = purrr::map_lgl(matching_cols, ~ "author" %in% .x),
    dwr_funder = purrr::map_lgl(matching_cols, ~ "funder" %in% .x)
  ) |>
  dplyr::mutate(
    dwr_funder = dplyr::if_else(
      dwr_author == FALSE & dwr_funder == FALSE,
      TRUE,
      dwr_funder
    )
  )

# define a function to create a full author name
create_full_name <- function(given, family) {
  paste(given, family) |> trimws()
}

# define a function to find the best fuzzy match for a name
find_best_name_match <- function(author_name, employee_names, similarity_threshold = 0.95) {
  if (is.na(author_name) || author_name == "") return(NA_integer_)

  # calculate Jaro-Winkler similarity scores
  similarities <- stringdist::stringdist(author_name, employee_names, method = "jw")

  # find the best match (lowest distance = highest similarity)
  best_idx <- which.min(similarities)
  best_score <- 1 - similarities[best_idx]  # convert distance to similarity

  # return index if similarity meets threshold, otherwise NA
  if (best_score >= similarity_threshold) {
    return(best_idx)
  } else {
    return(NA_integer_)
  }
}

# define a function to process a single publication
match_publication_division <- function(pub_row, employees_df, similarity_threshold = 0.95) {
  pub_year <- pub_row$pub_year
  authors <- pub_row$author[[1]]

  if (is.null(authors) || nrow(authors) == 0) {
    return(NA_character_)
  }

  # filter employees for the publication year
  year_employees <- employees_df |>
    dplyr::filter(year == pub_year)

  if (nrow(year_employees) == 0) {
    return(NA_character_)
  }

  # create full names for authors and add sequence order
  authors <- authors |>
    dplyr::mutate(
      full_name = create_full_name(given, family),
      author_order = dplyr::row_number()
    )

  # find matches for each author
  matches <- authors |>
    dplyr::rowwise() |>
    dplyr::mutate(
      match_idx = find_best_name_match(full_name, year_employees$employee, similarity_threshold),
      has_match = !is.na(match_idx)
    ) |>
    dplyr::ungroup()

  # get the first author (by order) that has a match
  first_match <- matches |>
    dplyr::filter(has_match) |>
    dplyr::arrange(author_order) |>
    dplyr::slice(1)

  if (nrow(first_match) == 0) {
    return(NA_character_)
  }

  # get the division for the matched employee
  matched_employee_idx <- first_match$match_idx
  division <- year_employees$division[matched_employee_idx]

  return(division)
}

# apply the matching function to all publications
dwr_pubs <- dwr_pubs |>
  dplyr::rowwise() |>
  dplyr::mutate(
    division = match_publication_division(
      pub_row = dplyr::tibble(pub_year = pub_year, author = list(author)),
      employees_df = dwr_employees_divisions,
      similarity_threshold = 0.95
    )
  ) |>
  dplyr::ungroup()

# write out
usethis::use_data(dwr_pubs, overwrite = TRUE)
