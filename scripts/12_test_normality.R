# NORMALVERTEILUNGSTEST DER RESIDUEN

library(panelvar)
library(tseries) 

# 1. SETUP & PFADE
base_path <- "C:/Users/lenal/Documents/05_R/data_clean/"
output_file <- paste0(base_path, "normality_test_results.txt")

file_paths <- list(
  "Gesamtsample" = "pvar_all.rds",
  "Instabile Gruppe" = "pvar_instable.rds",
  "Stabile Gruppe" = "pvar_stable.rds"
)

# 2. FUNKTION FÜR DEN TEST
run_normality_test <- function(model_path, group_name) {
  
  pvar_model <- readRDS(paste0(base_path, model_path))
  
  # Residuen extrahieren
  res <- residuals(pvar_model)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("NORMALVERTEILUNGSTEST (Jarque-Bera):", toupper(group_name), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Test für jede Variable durchführen
  for (i in 1:ncol(res)) {
    var_name <- colnames(res)[i]
    clean_res <- na.omit(res[, i])
    
    jb_test <- jarque.bera.test(clean_res)
    
    cat(sprintf("%-20s: JB-Statistik = %8.2f | p-Wert = %.4f %s\n", 
                var_name, 
                jb_test$statistic, 
                jb_test$p.value,
                ifelse(jb_test$p.value < 0.05, "(!) Nicht normalverteilt", "OK")))
  }
}


# 3. AUSFÜHRUNG & SPEICHERUNG
capture.output({
  cat("=================================================================\n")
  cat("ERGEBNISBERICHT: JARQUE-BERA-TEST AUF NORMALVERTEILUNG\n")
  cat("Basis: Schätzresiduen der PVAR-Modelle\n")
  cat("Erstellt am:", as.character(Sys.time()), "\n")
  cat("=================================================================\n")
  
  for (name in names(file_paths)) {
    run_normality_test(file_paths[[name]], name)
  }
  
  
}, file = output_file)

