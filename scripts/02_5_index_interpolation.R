setwd("C:/Users/lenal/Documents/05_R")

library(tidyverse)
library(zoo)

input_file  <- "data_raw/pca_scores.csv"
output_file <- "outputs/pc1_monthly.csv"

focus_mapping <- c(
  "Switzerland"    = "CH",
  "Norway"         = "NO",
  "Canada"         = "CA",
  "Japan"          = "JP",
  "Czech Republic" = "CZ",
  "Colombia"       = "CO",
  "Philippines"    = "PH",
  "Türkiye"         = "TR",
  "Egypt, Arab Rep."          = "EG",
  "Pakistan"       = "PK"
)

# laden & Filtern
df_annual <- read_csv(input_file, show_col_types = FALSE) %>%

    rename_with(tolower) %>%
  filter(country %in% names(focus_mapping)) %>%
  mutate(country = focus_mapping[country])

# Interpolation 
df_monthly <- df_annual %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  group_by(country) %>%
  

    complete(date = seq.Date(as.Date("2013-01-01"), as.Date("2024-12-01"), by = "month")) %>%
  
  # Interpolation der PC1-Werte
  mutate(pc1 = na.approx(pc1, na.rm = FALSE)) %>%
  
  fill(pc1, .direction = "downup") %>%
  ungroup() %>%
  select(country, date, pc1)

write_csv(df_monthly, output_file)
