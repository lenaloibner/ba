library(dplyr)
library(readr)

df <- read_csv("data_raw/pca_scores.csv", show_col_types = FALSE)

# Berechnung von Durchschnitt und Standardabweichung pro Land
pc1_summary <- df %>%
  group_by(country) %>%
  summarise(
    average_PC1 = mean(PC1, na.rm = TRUE),
    stdevp_PC1  = sd(PC1, na.rm = TRUE) 
  ) %>%
  arrange(country) #

print(head(pc1_summary))


write_csv(pc1_summary, "data_raw/pc1.csv")

write_csv(pc1_summary, "outputs/pc1_av_stdev.csv")

