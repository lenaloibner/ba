library(tidyverse)
library(zoo)

stocks <- read_csv("stock_data_merged.csv")

stocks_final <- stocks %>%
  
  mutate(country = ifelse(country == "PA", "PK", country)) %>% 
  group_by(country) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    
    # Monatliche Log-Rendite
    stock_return = log(close) - log(lag(close)),
    
    #  6-Monats-Volatilität 
    volatility = rollapplyr(stock_return, width = 6, FUN = sd, fill = NA),
    
    # Standardisierte Volatilität
    volatility_scaled = as.numeric(scale(volatility))
  ) %>%

    filter(!is.na(volatility)) %>%
  ungroup()

write_csv(stocks_final, "stock_data_final.csv")