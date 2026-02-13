# SCHÄTZUNG & MODELL-DIAGNOSTIK (Gesamt, Stabil, Instabil)
set.seed(123)
library(panelvar)


# 1. SETUP & DATEN LADEN
file_path <- "C:/Users/lenal/Documents/05_R/data_clean/data_all.csv"
data_all <- read.csv(file_path)

# Datum konvertieren und filtern (ab 2014)
data_all$date <- as.Date(data_all$date) 
data_all <- subset(data_all, date >= as.Date("2014-01-01"))
data_all <- data_all[order(data_all$country, data_all$date), ]


# 2. MANUELLE GRUPPENEINTEILUNG
stable_list <- c("CA", "NO", "CH", "CZ", "JP")
unstable_list <- c("EG", "PH", "TR", "PK", "CO")

data_all$group <- NA 
data_all$group[data_all$country %in% stable_list] <- "stabil"
data_all$group[data_all$country %in% unstable_list] <- "instabil"

data_filtered <- subset(data_all, !is.na(group))


# 3. DATEN TEILEN
data_stabil   <- subset(data_filtered, group == "stabil")
data_instabil <- subset(data_filtered, group == "instabil")

dep_vars <- c("stability", "volatility", "spread", "inflation", "reserves", "ex_rate")


# 4. MODELLSCHÄTZUNGEN
cat("\nSchätze Modelle...")

# A) Gesamtsample 
pvar_all <- pvarfeols(dependent_vars = dep_vars, lags = 3, transformation = "demean", 
                      data = data_filtered, panel_identifier = c("country", "date"))

# B) Stabile Gruppe
pvar_stabil <- pvarfeols(dependent_vars = dep_vars, lags = 3, transformation = "demean", 
                         data = data_stabil, panel_identifier = c("country", "date"))

# C) Instabile Gruppe
pvar_instabil <- pvarfeols(dependent_vars = dep_vars, lags = 3, transformation = "demean", 
                           data = data_instabil, panel_identifier = c("country", "date"))

# 5. SPEICHERN DER MODELLE (.rds)
# ------------------------------------------------------------------------------
saveRDS(pvar_all,      "C:/Users/lenal/Documents/05_R/data_clean/pvar_all.rds")
saveRDS(pvar_stabil,   "C:/Users/lenal/Documents/05_R/data_clean/pvar_stable.rds")
saveRDS(pvar_instabil, "C:/Users/lenal/Documents/05_R/data_clean/pvar_instable.rds")


# 6. MODELLNAHE DIAGNOSTIK (Stabilität & Residuen)
diag_path <- "C:/Users/lenal/Documents/05_R/data_clean/diagnose_bericht_ols.txt"

run_quick_diag <- function(model, name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("DIAGNOSE-CHECK:", toupper(name), "\n")
  cat("Spezifikation: Lags =", model$lags, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Eigenwerte (Stabilitätstest)
  cat("\n[1] EIGENWERTE (Stabilität):\n")
  # Muss im Einheitskreis liegen
  print(panelvar::stability(model))
  
  # Residuen-Check (Portmanteau-Test)
  cat("\n[2] RESIDUEN-CHECK (Ljung-Box):\n")
  res <- residuals(model)
  for (i in 1:ncol(res)) {
    clean_res <- na.omit(res[, i])
    if(length(clean_res) > 0) {
      
      # Autokorrelation in den Fehlern
      test_res <- Box.test(clean_res, lag = 10, type = "Ljung-Box")
      cat(sprintf("%-12s: p-Wert = %.4f %s\n", 
                  colnames(res)[i], 
                  test_res$p.value,
                  ifelse(test_res$p.value < 0.05, "(!) ACHTUNG", "OK")))
    }
  }
}

capture.output({
  run_quick_diag(pvar_all, "Gesamtsample")
  run_quick_diag(pvar_stabil, "Stabile Gruppe")
  run_quick_diag(pvar_instabil, "Instabile Gruppe")
}, file = diag_path)
