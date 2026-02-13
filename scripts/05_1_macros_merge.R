
library(tidyverse)
library(lubridate)

macros_data <- read_csv("macros_data.csv", na = c("", "NA", ".."))

macros_data <- macros_data %>%
  filter(!is.na(Country), !grepl("Data from database|Last Updated", Country)) %>%
  mutate(across(starts_with("20"), as.numeric))


macros_filtered <- macros_data %>%
  filter(Series %in% c("ex rate", "reserves", "cpi")) %>%
  select(Country, Series, starts_with("20"))


macros_long <- macros_filtered %>%
  pivot_longer(
    cols = -c(Country, Series),
    names_to = "month",
    values_to = "value"
  ) %>%
  mutate(month = str_extract(month, "^\\d{4}M\\d{2}")) %>%
  filter(!is.na(month))

macros_long <- macros_long %>%
  mutate(
    year = substr(month, 1, 4),
    mon = substr(month, 6, 7),
    date = ymd(paste(year, mon, "01"))
  ) %>%
  select(-year, -mon, -month)

# In wide-Format umwandeln -> Series zu Spalten
macros_wide <- macros_long %>%
  pivot_wider(
    names_from = Series,
    values_from = value
  ) %>%
  rename(
    country = Country,
    ex_rate = `ex rate`,
    cpi = cpi,
    reserves = reserves
  )

macros_wide <- macros_wide %>%
  mutate(country = case_when(
    country == "Switzerland"      ~ "CH",
    country == "Norway"           ~ "NO",
    country == "Canada"           ~ "CA",
    country == "Japan"            ~ "JP",
    country == "Czech Republic"   ~ "CZ",
    country == "Colombia"         ~ "CO",
    country == "Philippines"      ~ "PH",
    country == "Turkey"           ~ "TR",
    country == "Egypt Arab Rep."  ~ "EG",
    country == "Pakistan"         ~ "PK",
    TRUE                          ~ country 
  ))

# Formatierung 
macros_final <- macros_wide %>%
  mutate(
    across(c(ex_rate, reserves, cpi), ~ round(., 2))
  ) %>%
  filter(date >= ymd("2013-01-01") & date <= ymd("2023-12-01")) %>%
  arrange(country, date)

str(macros_final)

write_csv(macros_final, "macros_merged.csv")


