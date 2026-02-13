# ==============================================================================
# SKRIPT 03: ROBUSTHEITSCHECKS (SENSITIVITÄT) - WINDOWS-VERSION
# 1. Cholesky-Swap (Reihenfolge: Spread vor Volatility)
# ==============================================================================

library(panelvar)
library(ggplot2)

set.seed(123)

input_path  <- "C:/Users/lenal/Documents/05_R/data_clean/"
output_path <- "C:/Users/lenal/Documents/05_R/irfs/"

data_raw <- read.csv(paste0(input_path, "data_all.csv"))

# Panel-Vorbereitung 
data_robust <- as.data.frame(data_raw)
data_robust$country_idx <- as.numeric(as.factor(data_robust$country))
data_robust <- data_robust[order(data_robust$country_idx, data_robust$date), ]
data_robust$time_idx    <- ave(data_robust$country_idx, data_robust$country_idx, FUN = seq_along)


# SENSITIVITÄTSCHECK 1: CHOLESKY-SWAP
# ------------------------------------------------------------------------------
cat("\nStarte Robustheitstest 1: Cholesky-Swap (mc.cores = 1)...")

pvar_swap <- pvarfeols(
  dependent_vars = c("stability", "spread", "volatility", "inflation", "reserves", "ex_rate"), 
  lags = 3,
  transformation = "demean",
  data = data_robust,
  panel_identifier = c("country_idx", "time_idx")
)

irf_swap_boot <- bootstrap_irf(pvar_swap, typeof_irf = "OIRF", n.ahead = 24, 
                               nof_Nstar_draws = 500, mc.cores = 1)
irf_swap_base <- oirf(pvar_swap, n.ahead = 24)

# SENSITIVITÄTSCHECK 2: LAG-CHECK (p=2)
# ------------------------------------------------------------------------------
cat("\nStarte Robustheitstest 2: Lag-Check")

pvar_lag2 <- pvarfeols(
  dependent_vars = c("stability", "volatility", "spread", "inflation", "reserves", "ex_rate"),
  lags = 2, 
  transformation = "demean",
  data = data_robust,
  panel_identifier = c("country_idx", "time_idx")
)

irf_lag2_boot <- bootstrap_irf(pvar_lag2, typeof_irf = "OIRF", n.ahead = 24, 
                               nof_Nstar_draws = 500, mc.cores = 1)
irf_lag2_base <- oirf(pvar_lag2, n.ahead = 24)


# DATENAUFBEREITUNG FÜR PLOT
# ------------------------------------------------------------------------------
prepare_robust_data <- function(base_irf, boot_irf, label) {
  responses <- c("volatility", "spread")
  impulse <- "stability"
  df_list <- list()
  for (resp in responses) {
    len <- length(base_irf[[impulse]][, resp])
    df_list[[resp]] <- data.frame(
      Month = 1:len,
      Response = resp,
      Check = label,
      Value = base_irf[[impulse]][, resp],
      Lower = boot_irf$Lower[[impulse]][, resp],
      Upper = boot_irf$Upper[[impulse]][, resp]
    )
  }
  do.call(rbind, df_list)
}

data_plot_swap <- prepare_robust_data(irf_swap_base, irf_swap_boot, "Swap (spread)")
data_plot_lag2 <- prepare_robust_data(irf_lag2_base, irf_lag2_boot, "Lag-Länge (p=2)")

plot_robust_total <- rbind(data_plot_swap, data_plot_lag2)


# VISUALISIERUNG
# ------------------------------------------------------------------------------
p_robust <- ggplot(plot_robust_total, aes(x = Month, y = Value, color = Check)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Check), alpha = 0.1, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(Response ~ Check, scales = "free_y") + 
  theme_minimal() +
  labs(title = "Robustheitschecks: Sensitivitätsanalyse",
       subtitle = "Vergleich der Ergebnisse bei Änderung der Modell-Spezifikation",
       x = "Monate nach Schock", y = "Reaktion")


# EXPORT
# ------------------------------------------------------------------------------
write.csv(plot_robust_total, paste0(output_path, "03_data_robustness.csv"), row.names = FALSE)

cat("\nRobustheitsanalyse abgeschlossen. Datei: 03_Robustness_Comparison.pdf")