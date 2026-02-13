library(tidyverse)
library(quantmod)



# US 10Y Benchmark FRED
us10 <- getSymbols("GS10", src = "FRED", auto.assign = FALSE) %>%
  {tibble(date = as.Date(index(.)), US10Y = as.numeric(.))}



# Daten laden, kombinieren & Spreads berechnen
bind_rows(
  read_csv("bond_yields_fred.csv", show_col_types = FALSE),
  read_csv("bond_yields_investing.csv", show_col_types = FALSE)
) %>%
  
  

  mutate(date = as.Date(date)) %>%
  left_join(us10, by = "date") %>%
  mutate(spread = round(yield - US10Y, 2)) %>%
  select(country, date, yield, US10Y, spread) %>%
  write_csv("outputs/bond_data_merged.csv")

