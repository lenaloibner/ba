library(vars)

setwd("C:/Users/lenal/Documents/05_R/data_clean")

dateien <- list(
  Gesamt = "data_all.csv",
  Stabil = "data_stable.csv",
  Instabil = "data_instable.csv"
)

my_vars <- c("stability", "volatility", "spread", "inflation", "ex_rate", "reserves")

check_lags <- function(dateiname, gruppen_name) {
  cat("\n", paste(rep("=", 40), collapse = ""), "\n")
  cat("LAG-SELEKTION FÜR:", toupper(gruppen_name), "\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  df <- read.csv(dateiname)
  
  df_subset <- df[, my_vars]

  # Lag-Auswahl
  lag_selection <- VARselect(df_clean, lag.max = 5, type = "both")
  
  print(lag_selection$selection)
  print(lag_selection$criteria)
}

# Schleife über alle Gruppen
for (name in names(dateien)) {
  check_lags(dateien[[name]], name)
}


