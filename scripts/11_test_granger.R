
# GRANGER-WALD-MATRIX (Gesamt, Stabil, Instabil) 
library(plm)


# 1. SETUP
base_path <- "C:/Users/lenal/Documents/05_R/data_clean/"
vars <- c("stability", "volatility", "spread", "inflation", "ex_rate", "reserves")
output_file <- paste0(base_path, "granger_ergebnisse_gesamt.txt")


# 2. FUNKTION: BERECHNET UND GIBT MATRIX AUS
run_granger_matrix <- function(file_name, group_name, lag_order) {
  # Daten laden
  df <- read.csv(paste0(base_path, file_name))
  
  df <- df[order(df$country, df$date), ]
  df$time_idx <- as.numeric(ave(df$date, df$country, FUN = seq_along))
  p_df <- pdata.frame(df, index = c("country", "time_idx"), drop.index = TRUE)
  
  p_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars))
  colnames(p_matrix) <- vars
  rownames(p_matrix) <- vars
  
  cat("\n", paste(rep("=", 65), collapse = ""), "\n")
  cat("GRANGER-WALD MATRIX (p-Werte):", toupper(group_name), "(Lags =", lag_order, ")\n")
  cat("Spalte = Ursache  --->  Zeile = Wirkung\n")
  cat(paste(rep("=", 65), collapse = ""), "\n")
  
  for (cause in vars) {
    for (effect in vars) {
      if (cause != effect) {
        
        # Dumitrescu-Hurlin Panel-Test
        test <- pgrangertest(as.formula(paste(effect, "~", cause)), 
                             data = p_df, 
                             order = lag_order)
        p_matrix[effect, cause] <- test$p.value
      }
    }
  }
  
  # Anzeige
  print(round(p_matrix, 4))
  cat("\nInfo: Werte < 0.05 zeigen eine signifikante Kausalität an.\n")
}

# 3. AUSFÜHRUNG & SPEICHERUNG
capture.output({
  cat("=================================================================\n")
  cat("ERGEBNISBERICHT: GRANGER-KAUSALITÄTS-TESTS (WALD)\n")
  cat("Erstellt am:", as.character(Sys.time()), "\n")
  cat("=================================================================\n")
  
  run_granger_matrix("data_all.csv", "Gesamtsample", 3)
  run_granger_matrix("data_stable.csv", "Stabile Gruppe", 3)
  run_granger_matrix("data_instable.csv", "Instabile Gruppe", 3)
  
}, file = output_file)

cat("\nErfolg! Die Granger-Matrizen wurden in folgender Datei gespeichert:\n", output_file)




