library(deeptriangle)
temp_file <- tempfile(fileext = ".xlsx")
download.file("http://www.casact.org/pubs/monographs/meyers/Monograph_Tables_and_Scripts.xlsx",
              temp_file)
dt_group_codes <- readxl::read_xlsx(temp_file, sheet = "Multi Mack Paid", range = "A5:F205") %>%
  dplyr::transmute(lob = dplyr::case_when(
    Line == "CA" ~ "commercial_auto",
    Line == "PA" ~ "private_passenger_auto",
    Line == "WC" ~ "workers_compensation",
    Line == "OL" ~ "other_liability"),
    group_code = as.character(`Group Code`))
usethis::use_data(dt_group_codes, overwrite = TRUE)
