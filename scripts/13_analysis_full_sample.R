# ==============================================================================
# SKRIPT 01: HAUPTANALYSE & SYSTEM-CHECK (H1, H2)
# ==============================================================================

library(panelvar)
library(ggplot2)

set.seed(123)

input_path  <- "C:/Users/lenal/Documents/05_R/irfs/"
output_path <- "C:/Users/lenal/Documents/05_R/irfs/"

pvar_all <- readRDS(paste0(input_path, "01_pvar_all.rds"))

# BERECHNUNG (1.000 DRAWS)
# ------------------------------------------------------------------------------
irf_all_boot <- bootstrap_irf(pvar_all, typeof_irf = "OIRF", n.ahead = 24, 
                              nof_Nstar_draws = 1000, mc.cores = 1)
irf_all_base <- oirf(pvar_all, n.ahead = 24)


# SAVE
# ------------------------------------------------------------------------------
saveRDS(irf_all_boot, paste0(output_path, "01_irf_boot.rds"))
saveRDS(irf_all_base, paste0(output_path, "01_irf_base.rds"))

# DATEN-EXTRAKTION
# ------------------------------------------------------------------------------
prepare_plot_data <- function(base_oirf, boot_irf, impulse, responses) {
  df_list <- list()
  for (resp in responses) {
    df_list[[resp]] <- data.frame(
      Monat    = 1:length(base_oirf[[impulse]][, resp]),
      Response = resp,
      Wert    = base_oirf[[impulse]][, resp],
      Lower    = boot_irf$Lower[[impulse]][, resp],
      Upper    = boot_irf$Upper[[impulse]][, resp]
    )
  }
  do.call(rbind, df_list)
}

# A) Daten für Hypothesen H1 & H2 
plot_data_hyp <- prepare_plot_data(irf_all_base, irf_all_boot, "stability", c("volatility", "spread"))

# B) Daten für den SYSTEM-CHECK 
all_vars <- c("volatility", "spread", "inflation", "reserves", "ex_rate")
plot_data_system <- prepare_plot_data(irf_all_base, irf_all_boot, "stability", all_vars)

# C) Kumuliert
plot_data_cum <- plot_data_hyp
plot_data_cum$Wert <- ave(plot_data_hyp$Wert, plot_data_hyp$Response, FUN = cumsum)
plot_data_cum$Lower <- ave(plot_data_hyp$Lower, plot_data_hyp$Response, FUN = cumsum)
plot_data_cum$Upper <- ave(plot_data_hyp$Upper, plot_data_hyp$Response, FUN = cumsum)

# VISUALISIERUNG
# ------------------------------------------------------------------------------

# PLOT 1: H1 Normal 
p1 <- ggplot(plot_data_hyp, aes(x = Monat, y = Wert)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "steelblue", alpha = 0.3) +
  geom_line(color = "darkblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Response, scales = "free_y") +
  theme_minimal() + 
  
  labs(title = "Marktreaktion auf Stabilitätsschock",
       x = "Monate nach Schock", y = "Reaktion")


# PLOT 2: H2 Kumuliert
p2 <- ggplot(plot_data_cum, aes(x = Monat, y = Wert)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "forestgreen", alpha = 0.3) +
  geom_line(color = "darkgreen", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Response, scales = "free_y") +
  theme_minimal() + labs(title = "Kumulierter Effekt",
                         x = "Monate nach Schock", y = "Kumulierte Reaktion")
                        

# PLOT 3: SYSTEM-CHECK 
p_system <- ggplot(plot_data_system, aes(x = Monat, y = Wert)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.5) +
  geom_line(color = "black", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Response, scales = "free_y") +
  theme_minimal() + 
  labs(title = "System-Plausibilitätscheck", 
       subtitle = "Reaktion aller Systemvariablen auf einen Stabilitätsschock",
       x = "Monate nach Schock", y = "Reaktion")

# EXPORT
# ------------------------------------------------------------------------------
# Grafiken
ggsave(paste0(output_path, "01_OIRF_normal.pdf"), p1, width = 10, height = 6)
ggsave(paste0(output_path, "01_OIRF_kumuliert.pdf"), p2, width = 10, height = 6)
ggsave(paste0(output_path, "01_Anhang_System_Check.pdf"), p_system, width = 10, height = 8)

# CSV-Daten
write.csv(plot_data_hyp,   paste0(output_path, "01_data_normal.csv"), row.names = FALSE)
write.csv(plot_data_cum,    paste0(output_path, "01_data_kumuliert.csv"), row.names = FALSE)
write.csv(plot_data_system, paste0(output_path, "01_data_System_Check.csv"), row.names = FALSE)

# FEVD
fevd_all <- fevd_orthogonal(pvar_all, n.ahead = 24)
saveRDS(fevd_all, paste0(output_path, "01_fevd.rds"))

fevd_df <- as.data.frame(do.call(rbind, fevd_all))
write.csv(fevd_df, paste0(output_path, "01_fevd.csv"), row.names = TRUE)

cat("\nAnalyse abgeschlossen.")