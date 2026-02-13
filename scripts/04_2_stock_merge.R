library(tidyverse)
library(lubridate)

# Basis-Datei (Yahoo) laden
stock_final <- read_csv("stock_data_yahoo.csv")

new_files <- list(
  "stock_CZ.csv" = "CZ",
  "stock_CO.csv" = "CO",
  "stock_PK.csv" = "PK",
  "stock_EG.csv" = "EG"
)

# Einlesen und Formatieren 
new_data <- map2_df(names(new_files), new_files, function(file_path, code) {
  read_csv(file_path) %>%
    mutate(
      country = code,
      date = mdy(Date), 
      close = as.numeric(gsub(",", "", Price))
    ) %>%
    select(country, date, close)
})

# Zusammenfügen und Sortieren
stock_merged <- bind_rows(stock_final, new_data) %>%
  arrange(country, date)

write_csv(stock_merged, "stock_data_merged.csv")

print(head(stock_merged))