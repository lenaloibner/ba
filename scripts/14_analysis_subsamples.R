# ==============================================================================
# SKRIPT 02: HETEROGENITÄTSANALYSE (H3)
# ==============================================================================
library(panelvar)
library(ggplot2)
set.seed(123)

input_path <- "C:/Users/lenal/Documents/05_R/irfs/"
output_path <- "C:/Users/lenal/Documents/05_R/irfs/"

pvar_stabil <- readRDS(paste0(input_path, "02_pvar_stable.rds"))
pvar_instabil <- readRDS(paste0(input_path, "02_pvar_instable.rds"))


# BERECHNUNG: BOOTSTRAP FÜR BEIDE GRUPPEN (1.000 DRAWS)
# ------------------------------------------------------------------------------
irf_boot_st <- bootstrap_irf(pvar_stabil, typeof_irf = "OIRF", n.ahead = 24,
                             nof_Nstar_draws = 1000, mc.cores = 1)
irf_boot_in <- bootstrap_irf(pvar_instabil, typeof_irf = "OIRF", n.ahead = 24,
                             nof_Nstar_draws = 1000, mc.cores = 1)
irf_base_st <- oirf(pvar_stabil, n.ahead = 24)
irf_base_in <- oirf(pvar_instabil, n.ahead = 24)


# DATEN EXTRAKTION & TRANSFORMATION
# ------------------------------------------------------------------------------
prepare_group_data <- function(base_irf, boot_irf, group_name) {
  responses <- c("volatility", "spread")
  impulse <- "stability"
  df_list <- list()
  
  for (resp in responses) {
    actual_length <- length(base_irf[[impulse]][, resp])
    
    df_list[[resp]] <- data.frame(
      Month = 1:actual_length,
      Response = resp,
      Group = group_name,
      Value = base_irf[[impulse]][, resp],
      Lower = boot_irf$Lower[[impulse]][, resp],
      Upper = boot_irf$Upper[[impulse]][, resp]
    )
    
    # Kumulierung für H3
    df_list[[resp]]$CumValue <- cumsum(df_list[[resp]]$Value)
    df_list[[resp]]$CumLower <- cumsum(df_list[[resp]]$Lower)
    df_list[[resp]]$CumUpper <- cumsum(df_list[[resp]]$Upper)
  }
  do.call(rbind, df_list)
}
data_st <- prepare_group_data(irf_base_st, irf_boot_st, "Stabil")
data_in <- prepare_group_data(irf_base_in, irf_boot_in, "Instabil")
# Zusammenführen und Plotten
plot_compare <- rbind(data_st, data_in)


# VISUALISIERUNG (NORMAL)
# ------------------------------------------------------------------------------
p_h3_normal <- ggplot(plot_compare, aes(x = Month, y = Value, color = Group, fill = Group)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~Response, scales = "free_y") +
  scale_color_manual(values = c("Stabil" = "blue", "Instabil" = "red")) +
  scale_fill_manual(values = c("Stabil" = "blue", "Instabil" = "red")) +
  theme_minimal() +
  labs(title = "Normale Effekte",
       x = "Monate nach Schock", y = "Reaktion")
print(p_h3_normal)


# VISUALISIERUNG (KUMULIERT)
# ------------------------------------------------------------------------------
p_h3_cum <- ggplot(plot_compare, aes(x = Month, y = CumValue, color = Group, fill = Group)) +
  geom_ribbon(aes(ymin = CumLower, ymax = CumUpper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~Response, scales = "free_y") +
  scale_color_manual(values = c("Stabil" = "blue", "Instabil" = "red")) +
  scale_fill_manual(values = c("Stabil" = "blue", "Instabil" = "red")) +
  theme_minimal() +
  labs(title = "Kumulierten Effekte",
       x = "Monate nach Schock", y = "Kumulierte Reaktion")
print(p_h3_cum)


# FEVD
# ------------------------------------------------------------------------------
fevd_st <- fevd_orthogonal(pvar_stabil, n.ahead = 24)
fevd_in <- fevd_orthogonal(pvar_instabil, n.ahead = 24)

all_vars <- colnames(fevd_st$stability)
fevd_comp <- data.frame(
  Horizon = rep(1:24, times = length(all_vars)),
  Variable = rep(all_vars, each = 24),
  FEVD_Stabil = unlist(lapply(all_vars, function(v) fevd_st$stability[, v])),
  FEVD_Instabil = unlist(lapply(all_vars, function(v) fevd_in$stability[, v]))
)
saveRDS(fevd_comp, paste0(output_path, "02_fevd_combined.rds"))
write.csv(fevd_comp, paste0(output_path, "02_fevd_combined.csv"), row.names = FALSE)


# EXPORT
# ------------------------------------------------------------------------------
ggsave(paste0(output_path, "02_H3_Comparison_Normal.pdf"), p_h3_normal, width = 12, height = 7)
ggsave(paste0(output_path, "02_H3_Comparison_Cumulative.pdf"), p_h3_cum, width = 12, height = 7)
write.csv(plot_compare, paste0(output_path, "02_data_H3_comparison.csv"), row.names = FALSE)
write.csv(data_st, paste0(output_path, "02_data_H3_stabil.csv"), row.names = FALSE)
write.csv(data_in, paste0(output_path, "02_data_H3_instabil.csv"), row.names = FALSE)
write.csv(fevd_comp, paste0(output_path, "02_FEVD_H3_comparison.csv"), row.names = FALSE)
cat("\nAnalyse abgeschlossen.")