setwd("C:/Users/lenal/Documents/05_R/data_clean")

library(dplyr)

df <- read.csv("data_all.csv")

df$date <- as.Date(df$date)

# Differenzieren und Filtern
df_final <- df %>%
  arrange(country, date) %>%           
  group_by(country) %>%
  mutate(stability = stability - lag(stability)) %>% 
  ungroup() %>%
  filter(date >= "2014-01-01" & date <= "2023-12-31")


head(df_final)


write.csv(df_final, "data_all_final.csv", row.names = FALSE)
