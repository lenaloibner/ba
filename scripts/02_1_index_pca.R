
data <- read.csv("C:\\Users\\lenal\\Documents\\05_R\\data_clean\\data_all.csv")
data$date <- as.Date(data$date)

# Ländernamen + Gruppen
land_mapping <- c(
  "CA" = "Kanada", "CH" = "Schweiz", "CO" = "Kolumbien", "CZ" = "Tschechien",
  "EG" = "Ägypten", "JP" = "Japan", "NO" = "Norwegen", "PH" = "Philippinen",
  "PK" = "Pakistan", "TR" = "Türkei"
)

data$land <- land_mapping[data$country]
data <- data[data$country %in% c("CA","CH","CO","CZ","EG","JP","NO","PH","PK","TR"), ]
data$gruppe <- ifelse(data$country %in% c("CH","NO","CA","JP","CZ"), "Stabil", "Instabil")

data$land <- factor(data$land, 
                    levels = c("Schweiz", "Norwegen", "Kanada", "Japan", "Tschechien",
                               "Kolumbien", "Türkei", "Pakistan", "Ägypten", "Philippinen"))

library(ggplot2)

ggplot(data, aes(x = date, y = stability, color = land, group = land)) +
  geom_line(linewidth = 0.65, alpha = 0.95) +
  
  scale_color_manual(values = c(
    "Schweiz"    = "#08306b",
    "Norwegen"   = "#2171b5",
    "Kanada"     = "#4292c6",
    "Japan"      = "#6baed6",
    "Tschechien" = "#9ecae1",
    
    "Kolumbien"  = "#99000d",
    "Türkei"     = "#ef3b2c",
    "Pakistan"   = "#fb6a4a",
    "Ägypten"    = "#fc9272",
    "Philippinen"= "#fcbba1"
  )) +
  
  labs(title = "Politische Stabilität",
       x = "Jahr",
       y = "politische Stabilität",
       color = "Land") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))