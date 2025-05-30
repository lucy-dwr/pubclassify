  # read in NSF classification taxonomy from Survey of Earned Doctorates
  # table A-4 here: https://ncses.nsf.gov/surveys/earned-doctorates/2023#technical-tables
  nsf_sed_taxonomy <- readxl::read_xlsx(
    path = here::here("data-raw", "nsf_sed_taxonomy.xlsx"),
    col_names = c("field", "level", "code"),
    col_types = "text",
    skip = 3
  )

  # convert to long format
  nsf_sed_taxonomy <- nsf_sed_taxonomy |>
    dplyr::filter(level != "Area") |>
    dplyr::mutate(
      level = dplyr::case_when(
        level == "Broad"    ~ "first_level",
        level == "Major"    ~ "second_level",
        level == "Detailed" ~ "third_level",
        TRUE                ~ NA_character_
      )
    ) |>
    dplyr::mutate(
      first_level  = dplyr::if_else(level == "first_level",  field, NA_character_),
      second_level = dplyr::if_else(level == "second_level", field, NA_character_),
      third_level  = dplyr::if_else(level == "third_level",  field, NA_character_)
    ) |>
    tidyr::fill(first_level, second_level, .direction = "down") |>
    dplyr::filter(level == "third_level") |>
    dplyr::select(first_level, second_level, third_level, code)

  # clean fields
  nsf_sed_taxonomy <- nsf_sed_taxonomy |>
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.character),
        .fns = ~ gsub("/\\s+", "/", .) |> tolower()
      )
    )

  # write out
  usethis::use_data(nsf_sed_taxonomy, overwrite = TRUE)
