library(tidyverse)

input_folder <- "C:/Users/lenal/Documents/05_R/data_raw"
output_path  <- "C:/Users/lenal/Documents/05_R/data_clean/wgi_fsi_collected.csv"

setwd(input_folder)

# FSI-Dateien (2013-2024)
fsi_files <- list.files(pattern = "fsi_2013-2024.*\\.csv")

fsi_all <- fsi_files %>%
  map_df(function(x) {
    df <- read_csv(x, show_col_types = FALSE)
    colnames(df) <- tolower(colnames(df))
    return(df)
  }) %>%

    mutate(
    year_str = as.character(.data$year),
    year_numeric = as.numeric(substr(year_str, 1, 4))
  ) %>%
  select(country, year = year_numeric, fsi = total) %>%
  filter(!is.na(year))

# WGI-Datei 
wgi_data <- read_csv("wgi_2013_2024.xlsx - pv.csv", show_col_types = FALSE) %>%
  rename_with(tolower) %>%
  rename(wgi = score) %>%
  mutate(year = as.numeric(.data$year)) %>%
  select(country, year, wgi)

# Zusammenführen und nach Land sortieren
  wgi_fsi_collected <- full_join(wgi_data, fsi_all, by = c("country", "year")) %>%
    arrange(country, year) %>%
  select(country, year, wgi, fsi)


print(head(wgi_fsi_collected))

write_csv(wgi_fsi_collected, output_path)
