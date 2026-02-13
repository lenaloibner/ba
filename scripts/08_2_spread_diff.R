
setwd("C:/Users/lenal/Documents/05_R/data_clean")

library(CADFtest)
library(readr)
library(dplyr)
library(tidyr)

data <- read_csv("data_all.csv")

# Differenzen berechnen
data_transformed <- data %>%
  group_by(country) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    d_spread = spread - lag(spread),
    d_stability = stability - lag(stability)
  ) %>%
  filter(!is.na(d_spread)) %>% # Entfernt erste Zeile pro Land -> lag
  ungroup()

# CIPS-Test für die Differenzen
vars_to_retest <- c("d_spread", "d_stability")
countries <- unique(data_transformed$country)
retest_results <- data.frame(Variable = character(), CIPS_Statistik = numeric())

for (var in vars_to_retest) {
  cadf_stats <- c()
  for (country in countries) {
    country_data <- data_transformed %>% filter(country == !!country) %>% pull(!!var)

    
    test_cadf <- CADFtest(na.omit(country_data), type = "drift", max.lag.y = 4, criterion = "AIC")
    cadf_stats <- c(cadf_stats, test_cadf$statistic)
  }
  retest_results <- rbind(retest_results, data.frame(Variable = var, CIPS_Statistik = mean(cadf_stats)))
}

print
print(retest_results)


write_csv(data_transformed, "data_pvar.csv")





