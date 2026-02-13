library(readr)
library(writexl)

source("scripts/00_ba_theme.R")
theme_set(ba_theme("right"))

df <- read_csv("data_raw/pc1_av_stdev.csv", show_col_types = FALSE)

quartiles <- data.frame(
  variable = c("average_PC1", "stdevp_PC1"),
  Q25 = c(quantile(df$average_PC1, 0.25, na.rm = TRUE), 
          quantile(df$stdevp_PC1, 0.25, na.rm = TRUE)),
  Q50 = c(quantile(df$average_PC1, 0.50, na.rm = TRUE), 
          quantile(df$stdevp_PC1, 0.50, na.rm = TRUE)),
  Q75 = c(quantile(df$average_PC1, 0.75, na.rm = TRUE), 
          quantile(df$stdevp_PC1, 0.75, na.rm = TRUE))
)

output_dir <- "outputs"
if(!dir.exists(output_dir)) dir.create(output_dir)

write_csv(quartiles, file.path(output_dir, "pc1_quartiles.csv"))

quartiles






















# -----------------------------
# Markierte Länder definieren
# -----------------------------
selected_stable   <- c("Norway", "Canada", "Japan")
selected_unstable <- c("India", "Türkiye", "Nigeria")

df$category <- "Andere"
df$category[df$country %in% selected_stable]   <- "Stabil"
df$category[df$country %in% selected_unstable] <- "Instabil"

# -----------------------------
# Farben definieren
# -----------------------------
farben <- c(
  "Andere"  = "grey60",
  "Stabil"  = "darkgreen",
  "Instabil" = "red3"
)

scatter <- ggplot(df, aes(x = `average PC1`, y = `stdevp PC1`)) +
  geom_point(aes(color = category), size = 1.2, alpha = 0.6) +   # kleinere Punkte
  
  # Labels NUR für ausgewählte Länder
  geom_text(
    data = subset(df, category != "Andere"),
    aes(label = country),
    size = 3.0,              # deutlich kleinere Labels
    fontface = "bold",
    hjust = -0.1,
    vjust = 0.4,
    check_overlap = TRUE
  ) +
  
  scale_color_manual(values = farben) +
  
  scale_y_continuous(limits = c(0, 0.4)) +
  
  labs(
    x = "Durchschnitt PC1 (2014–2023)",
    y = "Standardabweichung PC1"
  ) +
  
  theme(
    axis.title = element_text(size = 9, face = "bold"),   # kleinerer Achsentitel
    axis.text  = element_text(size = 7),                  # kleinere Achsenbeschriftung
    legend.title = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8)                  # kleine Legende
  )


ggsave(
  file.path(output_dir, "pc1_scatterplot_markiert_clean.png"),
  plot = scatter,
  width = 9,
  height = 3.15,
  dpi = 300
)
