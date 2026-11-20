# ==============================================================================
# PROYECTO: Auditoría Forense Presupuestaria de Municipios de Bolivia (Gestión 2025)
# SCRIPT: 03_modelos_gmm_lof.R
# OBJETIVO: Modelado no supervisado con Mezclas Gaussianas (GMM / Mclust) y
#           Factor de Valor Atípico Local (LOF / dbscan), análisis de sensibilidad
#           y clasificación en categorías de consenso.
# ==============================================================================

suppressPackageStartupMessages({
  library(mclust)
  library(dbscan)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

cat("====================================================================\n")
cat(" ETAPA 2.2: MODELOS MULTIVARIANTES GMM Y LOF (CONSENSO DE ANOMALIAS)\n")
cat("====================================================================\n\n")

# 1. Cargar datos
df_gam <- readRDS("data/processed/municipios_fiscal_2025.rds")
cat("Municipios cargados:", nrow(df_gam), "\n")

cols_indicadores <- c(
  "tasa_ejecucion_gasto", "tasa_modificacion_gasto", "brecha_pago",
  "ratio_inversion", "ratio_serv_nopers", "balance_caja_real",
  "eficacia_recaudacion", "dependencia_tgn", "log_presupuesto"
)

# Matriz estandarizada (Media 0, Varianza 1)
X <- scale(df_gam[, cols_indicadores])

# ------------------------------------------------------------------------------
# 2. Mezclas Gaussianas (GMM / Mclust)
# ------------------------------------------------------------------------------
cat(">> Ajustando Modelo de Mezclas Gaussianas (Mclust EM)...\n")
set.seed(2025)
mod_gmm <- Mclust(X, G = 1:9)

cat(" - Modelo óptimo:", mod_gmm$modelName, "\n")
cat(" - Componentes óptimos (G):", mod_gmm$G, "\n")
cat(" - Criterio BIC:", mod_gmm$bic, "\n")

# Métricas de anomalía GMM:
# a) Log-densidad total de la observación bajo el modelo de mezcla (densidad global)
log_dens <- dens(data = X, modelName = mod_gmm$modelName, parameters = mod_gmm$parameters, logarithm = TRUE)
# b) Probabilidad posterior máxima (incertidumbre de pertenencia / anomalía contextual)
max_post <- apply(mod_gmm$z, 1, max)

umbral_log_dens <- quantile(log_dens, 0.05)
umbral_max_post <- quantile(max_post, 0.05)

es_anomalo_gmm <- (log_dens < umbral_log_dens)
cat(" - Umbral Log-Densidad (P5):", umbral_log_dens, "\n")
cat(" - Municipios anómalos GMM:", sum(es_anomalo_gmm), "\n")

# ------------------------------------------------------------------------------
# 3. Factor de Valor Atípico Local (LOF) con Sensibilidad en minPts
# ------------------------------------------------------------------------------
cat(">> Calculando LOF y sensibilidad en minPts (10, 15, 20, 30)...\n")
lof_10 <- lof(X, minPts = 10)
lof_15 <- lof(X, minPts = 15)
lof_20 <- lof(X, minPts = 20)  # Valor base
lof_30 <- lof(X, minPts = 30)

umbral_lof_base <- 1.5
es_anomalo_lof <- (lof_20 > umbral_lof_base)
cat(" - Umbral LOF base (minPts = 20):", umbral_lof_base, "\n")
cat(" - Municipios anómalos LOF:", sum(es_anomalo_lof), "\n")

# ------------------------------------------------------------------------------
# 4. Clasificación de Consenso en 4 Categorías Forenses
# ------------------------------------------------------------------------------
cat(">> Asignando categorías de consenso GMM + LOF...\n")

df_modelos <- df_gam %>%
  mutate(
    regimen_gmm     = mod_gmm$classification,
    gmm_log_dens    = log_dens,
    gmm_max_post    = max_post,
    gmm_anomalo     = es_anomalo_gmm,
    lof_score_10    = lof_10,
    lof_score_15    = lof_15,
    lof_score_20    = lof_20,
    lof_score_30    = lof_30,
    lof_score       = lof_20,
    lof_anomalo     = es_anomalo_lof,
    categoria_consenso = case_when(
      gmm_anomalo & lof_anomalo  ~ "Consenso GMM + LOF (Prioridad Alta)",
      gmm_anomalo & !lof_anomalo ~ "Solo GMM (Global/Contextual)",
      !gmm_anomalo & lof_anomalo ~ "Solo LOF (Local)",
      TRUE                       ~ "Normal"
    )
  )

cat("Distribución de Categorías:\n")
print(table(df_modelos$categoria_consenso))

# ------------------------------------------------------------------------------
# 5. Análisis de Componentes Principales (PCA) para Visualización 2D / 3D
# ------------------------------------------------------------------------------
pca_res <- prcomp(X, center = FALSE, scale. = FALSE)
var_exp <- summary(pca_res)$importance[2, 1:3] * 100

df_modelos <- df_modelos %>%
  mutate(
    PCA1 = pca_res$x[, 1],
    PCA2 = pca_res$x[, 2],
    PCA3 = pca_res$x[, 3]
  )

# Guardar dataset enriquecido con modelos para el dashboard y etapas siguientes
saveRDS(df_modelos, "data/processed/municipios_modelados_2025.rds")
arrow::write_parquet(df_modelos, "data/processed/municipios_modelados_2025.parquet")
cat(" - Guardado: data/processed/municipios_modelados_2025.rds y .parquet\n")

# ------------------------------------------------------------------------------
# 6. Generación de Gráficos de Alta Resolución (300 DPI)
# ------------------------------------------------------------------------------
cat(">> Exportando figuras en informe/figuras/...\n")

COLOR_BG        <- "#0D1117"
COLOR_CANVAS    <- "#000000"
COLOR_GRID      <- "#1A222D"
COLOR_TEXT_PRI  <- "#F3F4F6"
COLOR_TEXT_SEC  <- "#9CA3AF"
COLOR_NORMAL    <- "#00FF88"
COLOR_PRIMARY   <- "#4DB8FF"
COLOR_WARN      <- "#FFB020"
COLOR_ALERT     <- "#FF3366"

theme_forense <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = COLOR_CANVAS, color = NA),
      panel.background = element_rect(fill = COLOR_BG, color = "#1F2937", linewidth = 0.5),
      panel.grid.major = element_line(color = COLOR_GRID, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text = element_text(color = COLOR_TEXT_PRI, family = "sans"),
      axis.text = element_text(color = COLOR_TEXT_SEC),
      axis.title = element_text(color = COLOR_TEXT_PRI, face = "bold"),
      plot.title = element_text(color = COLOR_TEXT_PRI, face = "bold", size = 12),
      plot.subtitle = element_text(color = COLOR_TEXT_SEC, size = 9),
      legend.background = element_rect(fill = COLOR_BG, color = NA),
      legend.text = element_text(color = COLOR_TEXT_PRI),
      legend.title = element_text(color = COLOR_TEXT_PRI, face = "bold")
    )
}

# FIGURA 3: Selección de Modelo GMM (Curva BIC)
mat_bic <- unclass(mod_gmm$BIC)
df_bic <- as.data.frame(mat_bic)
df_bic$G <- as.numeric(rownames(df_bic))
df_bic_long <- df_bic %>%
  pivot_longer(cols = -G, names_to = "Modelo", values_to = "BIC") %>%
  filter(!is.na(BIC))

p_bic <- ggplot(df_bic_long, aes(x = G, y = BIC, color = Modelo, group = Modelo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = mod_gmm$G, linetype = "dashed", color = COLOR_ALERT, linewidth = 0.7) +
  annotate("text", x = mod_gmm$G + 0.3, y = min(df_bic_long$BIC, na.rm = TRUE) * 0.95,
           label = sprintf("Óptimo BIC: G = %d (%s)", mod_gmm$G, mod_gmm$modelName),
           color = COLOR_ALERT, fontface = "bold", hjust = 0, size = 3.2) +
  scale_x_continuous(breaks = 1:9) +
  labs(
    title = "Criterio de Información Bayesiano (BIC) para Selección de Regímenes GMM",
    subtitle = "Evaluación de parametrizaciones de covarianza en mclust::Mclust; el valor máximo define la topología óptima",
    x = "Número de Componentes Gaussianos (G)",
    y = "Valor del Criterio BIC"
  ) +
  theme_forense() +
  theme(legend.position = "right")

ggsave("informe/figuras/fig_03_gmm_curva_bic.png", plot = p_bic, width = 10, height = 5.5, dpi = 300)
cat(" - Guardado: informe/figuras/fig_03_gmm_curva_bic.png\n")

# FIGURA 4: Espacio Latente PCA con Regímenes GMM y Consenso de Anomalías
df_plot_pca <- df_modelos %>%
  mutate(
    etiqueta_anomalo = ifelse(categoria_consenso == "Consenso GMM + LOF (Prioridad Alta)",
                              paste0(municipio, " (", departamento, ")"), "")
  )

p_pca <- ggplot(df_plot_pca, aes(x = PCA1, y = PCA2, color = categoria_consenso)) +
  stat_ellipse(aes(group = factor(regimen_gmm)), color = COLOR_PRIMARY, linetype = "dotted", linewidth = 0.6, level = 0.85) +
  geom_point(aes(size = categoria_consenso, alpha = categoria_consenso)) +
  scale_color_manual(values = c(
    "Normal" = COLOR_NORMAL,
    "Solo GMM (Global/Contextual)" = COLOR_PRIMARY,
    "Solo LOF (Local)" = COLOR_WARN,
    "Consenso GMM + LOF (Prioridad Alta)" = COLOR_ALERT
  )) +
  scale_size_manual(values = c(
    "Normal" = 2.0,
    "Solo GMM (Global/Contextual)" = 3.2,
    "Solo LOF (Local)" = 3.5,
    "Consenso GMM + LOF (Prioridad Alta)" = 4.2
  )) +
  scale_alpha_manual(values = c(
    "Normal" = 0.45,
    "Solo GMM (Global/Contextual)" = 0.85,
    "Solo LOF (Local)" = 0.90,
    "Consenso GMM + LOF (Prioridad Alta)" = 1.0
  )) +
  geom_text(aes(label = etiqueta_anomalo), color = "#FFFFFF", size = 2.7, vjust = -0.7, check_overlap = TRUE) +
  labs(
    title = "Plano Principal PCA (Dimensión 1 vs. 2) y Consenso de Detección de Anomalías",
    subtitle = sprintf("PCA1 (%.1f%% var) vs. PCA2 (%.1f%% var). Elipses azules delimitan los %d regímenes GMM. Puntos rojos indican consenso de alto riesgo.",
                       var_exp[1], var_exp[2], mod_gmm$G),
    x = sprintf("Componente Principal 1 (%.1f%% Varianza Explicada)", var_exp[1]),
    y = sprintf("Componente Principal 2 (%.1f%% Varianza Explicada)", var_exp[2]),
    color = "Categoría Forense",
    size = "Categoría Forense",
    alpha = "Categoría Forense"
  ) +
  theme_forense() +
  theme(legend.position = "bottom")

ggsave("informe/figuras/fig_04_pca_consenso_anomalias.png", plot = p_pca, width = 10, height = 6.2, dpi = 300)
cat(" - Guardado: informe/figuras/fig_04_pca_consenso_anomalias.png\n")

# FIGURA 5: Sensibilidad del LOF según minPts (10, 15, 20, 30)
df_sens <- df_modelos %>%
  select(entidad, municipio, lof_score_10, lof_score_15, lof_score_20, lof_score_30, categoria_consenso) %>%
  pivot_longer(cols = starts_with("lof_score_"), names_to = "minPts_param", values_to = "Score_LOF") %>%
  mutate(
    minPts_label = case_when(
      minPts_param == "lof_score_10" ~ "minPts = 10",
      minPts_param == "lof_score_15" ~ "minPts = 15",
      minPts_param == "lof_score_20" ~ "minPts = 20 (Base)",
      minPts_param == "lof_score_30" ~ "minPts = 30"
    )
  )

p_sens <- ggplot(df_sens, aes(x = minPts_label, y = Score_LOF, group = entidad, color = categoria_consenso)) +
  geom_line(alpha = 0.5, linewidth = 0.6) +
  geom_hline(yintercept = 1.5, linetype = "dashed", color = COLOR_ALERT, linewidth = 0.7) +
  scale_color_manual(values = c(
    "Normal" = "#374151",
    "Solo GMM (Global/Contextual)" = COLOR_PRIMARY,
    "Solo LOF (Local)" = COLOR_WARN,
    "Consenso GMM + LOF (Prioridad Alta)" = COLOR_ALERT
  )) +
  labs(
    title = "Estabilidad y Análisis de Sensibilidad del Score LOF ante Variaciones de minPts",
    subtitle = "Línea roja discontinua representa el umbral crítico LOF = 1.5; los casos en consenso mantienen su condición anómala",
    x = "Parámetro de Vecindad Local (minPts)",
    y = "Local Outlier Factor (LOF Score)",
    color = "Categoría Forense"
  ) +
  theme_forense() +
  theme(legend.position = "bottom")

ggsave("informe/figuras/fig_05_sensibilidad_lof.png", plot = p_sens, width = 10, height = 5.5, dpi = 300)
cat(" - Guardado: informe/figuras/fig_05_sensibilidad_lof.png\n")

# ------------------------------------------------------------------------------
# 7. Exportación de Tablas LaTeX para el Informe
# ------------------------------------------------------------------------------
cat(">> Exportando tablas LaTeX en informe/tablas/...\n")

# Tabla Regímenes GMM
df_reg_summary <- df_modelos %>%
  group_by(regimen_gmm) %>%
  summarise(
    n_municipios = n(),
    pct_total = n() / nrow(df_modelos) * 100,
    media_ejecucion = mean(tasa_ejecucion_gasto) * 100,
    media_inversion = mean(ratio_inversion) * 100,
    media_serv_nopers = mean(ratio_serv_nopers) * 100,
    media_dep_tgn = mean(dependencia_tgn) * 100,
    media_ppto_vig = mean(gasto_vigente) / 1e6
  )

lineas_regimenes <- c()
for (i in seq_len(nrow(df_reg_summary))) {
  r <- df_reg_summary[i, ]
  lineas_regimenes <- c(lineas_regimenes, sprintf(
    "R\\'egimen %d & %d (%.1f\\%%) & %.1f\\%% & %.1f\\%% & %.1f\\%% & %.1f\\%% & Bs. %.1fM \\\\",
    r$regimen_gmm, r$n_municipios, r$pct_total,
    r$media_ejecucion, r$media_inversion, r$media_serv_nopers,
    r$media_dep_tgn, r$media_ppto_vig
  ))
}

tex_gmm <- sprintf(
"\\begin{table}[htbp]
\\centering
\\footnotesize
\\caption{Caracterizaci\\'on de los Reg\\'imenes Fiscales Identificados por el Modelo GMM (VVE, $G=%d$)}
\\label{tab:gmm_regimenes}
\\begin{tabular}{lrrrrrr}
\\hline
\\textbf{R\\'egimen} & \\textbf{Tama\\~no} & \\textbf{Ejec. Gasto} & \\textbf{Inversi\\'on} & \\textbf{Serv. No Pers.} & \\textbf{Dep. TGN} & \\textbf{Ppto. Prom.} \\\\
\\hline
%s
\\hline
\\end{tabular}
\\end{table}",
  mod_gmm$G,
  paste(lineas_regimenes, collapse = "\n")
)

writeLines(tex_gmm, "informe/tablas/tabla_gmm_regimenes.tex")
cat(" - Guardado: informe/tablas/tabla_gmm_regimenes.tex\n")

# Tabla Municipios en Consenso
df_consenso_top <- df_modelos %>%
  filter(categoria_consenso == "Consenso GMM + LOF (Prioridad Alta)") %>%
  arrange(desc(lof_score)) %>%
  select(entidad, entidad_desc, departamento, gasto_vigente, tasa_ejecucion_gasto, brecha_pago, balance_caja_real, ratio_inversion, lof_score, gmm_log_dens)

lineas_consenso <- c()
for (i in seq_len(nrow(df_consenso_top))) {
  m <- df_consenso_top[i, ]
  
  # Diagnóstico técnico forense
  diag <- case_when(
    m$tasa_ejecucion_gasto < 0.40 ~ "Parálisis fiscal y acumulación inercial",
    m$brecha_pago > 0.12 ~ "Severa deuda flotante e iliquidez",
    m$balance_caja_real < -0.20 ~ "Déficit crítico de tesorería",
    m$ratio_inversion == 0 ~ "Gasto corriente total (Cero inversión)",
    m$ratio_inversion > 0.50 ~ "Gasto atípico concentrado en inversión",
    TRUE ~ "Distorsión multivariante combinada"
  )
  
  lineas_consenso <- c(lineas_consenso, sprintf(
    "%s & %s & %.1f\\%% & %.2f\\%% & %.2f & %.1f & %s \\\\",
    m$entidad_desc, m$departamento,
    m$tasa_ejecucion_gasto * 100, m$brecha_pago * 100,
    m$lof_score, m$gmm_log_dens, diag
  ))
}

tex_consenso <- sprintf(
"\\begin{table}[htbp]
\\centering
\\scriptsize
\\caption{Municipios Prioritarios en Consenso GMM + LOF (Alta Probabilidad de Anomal\\'ia Forense)}
\\label{tab:consenso_anomalias}
\\begin{tabular}{llrrrrl}
\\hline
\\textbf{Gobierno Municipal} & \\textbf{Depto.} & \\textbf{Ejec.} & \\textbf{Deuda Flot.} & \\textbf{LOF} & \\textbf{Log-Dens.} & \\textbf{Diagn\\'ostico Forense Preliminar} \\\\
\\hline
%s
\\hline
\\end{tabular}
\\end{table}",
  paste(lineas_consenso, collapse = "\n")
)

writeLines(tex_consenso, "informe/tablas/tabla_consenso_anomalias.tex")
cat(" - Guardado: informe/tablas/tabla_consenso_anomalias.tex\n")

cat("\nModelos GMM y LOF ejecutados y exportados con éxito.\n")
cat("====================================================================\n")
