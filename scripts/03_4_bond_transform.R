setwd("C:/Users/lenal/Documents/05_R/data_raw")

library(tidyverse)

bonds <- read_csv("bond_data_merged.csv")

# Transformation und Skalierung Spread
bonds_final <- bonds %>%
  group_by(country) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    
    d_spread = spread - lag(spread),
    
    spread_scaled = as.numeric(scale(d_spread))
  ) %>%
  
  
  filter(!is.na(spread_scaled)) %>%
  ungroup()


write_csv(bonds_final, "bond_data_final.csv")