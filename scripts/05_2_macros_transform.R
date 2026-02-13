library(tidyverse)

macros <- read_csv("macros_merged.csv")

# Transformation und Standardisierung
macros_ready <- macros %>%
  group_by(country) %>%
  
  arrange(date, .by_group = TRUE) %>% 
  mutate(
    
    # (Log-Differenzen) 
    inflation       = log(cpi) - log(lag(cpi)),
    ex_rate_change  = log(ex_rate) - log(lag(ex_rate)),
    reserves_growth = log(reserves) - log(lag(reserves))
  ) %>%
  
  # Ersten Monat  entfernen
  filter(!is.na(inflation)) %>%
  
  # Z-Standardisierung
  mutate(
    inf_scaled      = as.numeric(scale(inflation)),
    ex_scaled       = as.numeric(scale(ex_rate_change)),
    res_scaled      = as.numeric(scale(reserves_growth))
  ) %>%
  ungroup()

summary(macros_ready %>% select(inf_scaled, ex_scaled, res_scaled))

write_csv(macros_ready, "macros_final1.csv")