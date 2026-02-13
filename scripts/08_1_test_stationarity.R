
# CIPS Unit Root Test 

setwd("C:/Users/lenal/Documents/05_R/data_clean")

library(readr)
library(dplyr)
library(tidyr)
library(CADFtest)

data <- read_csv("data_all.csv")

vars_to_test <- c("stability", "volatility", "spread", "inflation", "reserves", "ex_rate")
countries <- unique(data$country)

# Cross-sectional Means berechnen
data_augmented <- data %>%
  group_by(date) %>%  
  mutate(across(all_of(vars_to_test), 
                ~mean(., na.rm = TRUE), 
                .names = "mean_{.col}")) %>%
  ungroup()

# Iteration über Variablen & Länder
cips_results <- data.frame(Variable = character(), CIPS_Statistik = numeric())

for (var in vars_to_test) {
  cadf_stats <- c()
  mean_var_name <- paste0("mean_", var) 
  for (country_name in countries) {
    country_data <- data_augmented %>% 
      filter(country == country_name) %>%
      arrange(date)
    
    # Zeitreihen vorbereiten 
    y <- country_data[[var]]
    y_bar <- country_data[[mean_var_name]]
    
    valid_indices <- which(!is.na(y) & !is.na(y_bar))
    y_clean <- y[valid_indices]
    y_bar_clean <- y_bar[valid_indices]
    
    # CADF-Test pro Land durchführen
    # AIC wählt Lags (max. 4)
    test_result <- CADFtest(y_clean, 
                            X = y_bar_clean, 
                            type = "drift", 
                            max.lag.y = 4, 
                            criterion = "AIC")
    
    cadf_stats <- c(cadf_stats, test_result$statistic)
  }
  
  # Mittelwert der Statistiken bilden (= CIPS Statistik)
  cips_stat <- mean(cadf_stats, na.rm = TRUE)
  
  cips_results <- rbind(cips_results, 
                        data.frame(Variable = var, CIPS_Statistik = cips_stat))
}

print("--- Finale CIPS-Testergebnisse ---")
print(cips_results)

