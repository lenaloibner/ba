setwd("C:/Users/lenal/Documents/05_R/data_clean")


library(tidyverse)


macros <- read_csv("macros_final.csv", show_col_types = FALSE)
bonds  <- read_csv("bond_data_final.csv", show_col_types = FALSE)
stocks <- read_csv("stock_data_final.csv", show_col_types = FALSE)
pc1    <- read_csv("pc1_monthly.csv", show_col_types = FALSE)

data_all <- macros %>%
  inner_join(bonds, by = c("country", "date")) %>%
  inner_join(stocks, by = c("country", "date")) %>%
  inner_join(pc1,    by = c("country", "date")) %>% 
  select(
    country, 
    date, 
    
    stability  = pc1,

    inflation  = inf_scaled, 
    ex_rate    = ex_scaled, 
    reserves   = res_scaled, 
    
    spread     = spread_scaled, 
    volatility = volatility_scaled
  ) %>%
  
  filter(date >= "2013-09-01" & date <= "2023-12-31") %>%
  arrange(country, date)

print(colnames(data_all))


print(table(data_all$country))

write_csv(data_all, "data_all.csv")