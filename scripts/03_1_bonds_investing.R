library(tidyverse)
library(lubridate)


raw_path <- "data_raw/"
output_path <- "outputs/"

manual_files <- list.files(raw_path, pattern = "bond_.*\\.csv", full.names = TRUE)

countries <- basename(manual_files) %>%
  str_remove_all("bond_|\\.csv")

manual_bonds <- map2_dfr(manual_files, countries, function(file, country_name) {
  read_csv(file, show_col_types = FALSE) %>%
    select(Date, Price) %>%
    rename(date = Date, yield = Price) %>%
    mutate(
      
      # Datum parsen
      date = parse_date_time(date, orders = c("m/d/Y", "d/m/Y", "Y-m-d")),

            date = as.Date(floor_date(date, "month")),
      yield = as.numeric(yield),
      country = country_name
    ) %>%
    
    # Gruppierung 
    group_by(country, date) %>%
    summarise(yield = mean(yield, na.rm = TRUE), .groups = "drop")
})

manual_bonds %>%
  group_by(country) %>%
  summarise(start = min(date), end = max(date)) %>%
  print()

write_csv(manual_bonds, file.path(output_path, "bond_yields_investing.csv"))