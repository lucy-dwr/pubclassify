# import a file of employees and associated divisions; note that this file is
# not hosted on GitHub for privacy reasons, but the cleaning is displayed here
# for transparency
employees_divisions <- readxl::read_xlsx(
  path = here::here("data-raw", "employee_division_year.xlsx"),
  col_names = c("employee", "division", "year", "division_old"),
  col_types = c("text", "text", "numeric", "text"),
  skip = 1
)

# make a function to clean names in the file
clean_names_case <- function(x) {
  tmp <- snakecase::to_title_case(x)

  # convert any standalone initial (with optional period) to uppercase
  gsub(
    "(?<=^| )([A-Za-z])(?=\\.?($| ))",
    "\\U\\1",
    tmp,
    perl = TRUE
  )
}

# clean the names
employees_divisions <- employees_divisions |>
  dplyr::mutate(employee = clean_names_case(employee))

# write out
usethis::use_data(employees_divisions, overwrite = TRUE)
