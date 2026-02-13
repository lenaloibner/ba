library(tidyverse)
library(WDI)
library(broom) 

source("05_R/scripts/00_theme.R")
theme_set(ba_theme("right"))

input_file <- "data_raw/pc1_av_stdev.csv"
output_dir <- "outputs"
if(!dir.exists(output_dir)) dir.create(output_dir)

pc1 <- read_csv(input_file, show_col_types = FALSE)

colnames(pc1) <- gsub("\\.", "_", colnames(pc1))

# BIP-Daten laden 
gdp_raw <- WDI(country = "all", indicator = "NY.GDP.PCAP.PP.KD", start = 2013, end = 2024)

gdp_avg <- gdp_raw %>%
  rename(gdp_ppp = NY.GDP.PCAP.PP.KD) %>%
  filter(!is.na(gdp_ppp)) %>%
  group_by(country) %>%
  summarise(avg_gdp = mean(gdp_ppp, na.rm = TRUE))

# Merge & Analyse
df_merge <- inner_join(pc1, gdp_avg, by = "country") %>%
  mutate(log_gdp = log(avg_gdp))

# Pearson Korrelation
cor_res <- cor.test(df_merge$average_PC1, df_merge$log_gdp)
print(cor_res)

# Scatterplot
scatter_plot <- ggplot(df_merge, aes(x = average_PC1, y = log_gdp)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed") +
  labs(x = "Politische Stabilität (PC1)", 
       y = "Log(BIP pro Kopf, PPP)")

ggsave(file.path(output_dir, "scatterplot_gdp_global.png"), scatter_plot, width = 7, height = 5)

write_csv(tidy(cor_res), file.path(output_dir, "pearson_results.csv"))

cat("\nSkript erfolgreich ausgeführt. Ergebnisse in 'outputs/' gespeichert.\n")