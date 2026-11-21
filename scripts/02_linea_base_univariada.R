# ==============================================================================
# PROYECTO: Auditoría Forense Presupuestaria de Municipios de Bolivia (Gestión 2025)
# SCRIPT: 02_linea_base_univariada.R
# OBJETIVO: Línea base univariada (Tukey 1.5/3 IQR, boxplot.stats, Z-score) y
#           demostración empírica del sesgo de escala en montos brutos vs ratios.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

cat("====================================================================\n")
cat(" ETAPA 2.1: ANALISIS DE LINEA BASE UNIVARIADA Y SESGO DE ESCALA\n")
cat("====================================================================\n\n")

# Cargar dataset procesado
df_gam <- readRDS("data/processed/municipios_fiscal_2025.rds")
cat("Municipios cargados:", nrow(df_gam), "\n")

# Variables a evaluar
vars_montos <- c("gasto_vigente", "gasto_devengado", "ingreso_percibido")
vars_ratios <- c(
  "tasa_ejecucion_gasto", "tasa_modificacion_gasto", "brecha_pago",
  "ratio_inversion", "ratio_serv_nopers", "balance_caja_real",
  "eficacia_recaudacion", "dependencia_tgn"
)
todas_vars <- c(vars_montos, vars_ratios)

# ------------------------------------------------------------------------------
# 1. Función de Detección Univariada (Tukey + Z-Score)
# ------------------------------------------------------------------------------
calcular_univariado <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  
  # Criterios Tukey
  tukey_inf_leve <- q1 - 1.5 * iqr_val
  tukey_sup_leve <- q3 + 1.5 * iqr_val
  tukey_inf_ext  <- q1 - 3.0 * iqr_val
  tukey_sup_ext  <- q3 + 3.0 * iqr_val
  
  es_tukey_leve <- (x < tukey_inf_leve | x > tukey_sup_leve)
  es_tukey_ext  <- (x < tukey_inf_ext  | x > tukey_sup_ext)
  
  # boxplot.stats
  bp_stats <- boxplot.stats(x)
  
  # Z-Score
  media <- mean(x, na.rm = TRUE)
  desv  <- sd(x, na.rm = TRUE)
  z <- if (desv > 0) (x - media) / desv else rep(0, length(x))
  es_z2 <- abs(z) > 2
  es_z3 <- abs(z) > 3
  
  list(
    q1 = q1, q3 = q3, iqr = iqr_val,
    n_tukey_leve = sum(es_tukey_leve),
    n_tukey_ext  = sum(es_tukey_ext),
    n_bp_out     = length(bp_stats$out),
    n_z2         = sum(es_z2),
    n_z3         = sum(es_z3),
    es_tukey_leve = es_tukey_leve,
    es_tukey_ext  = es_tukey_ext,
    es_z2         = es_z2,
    es_z3         = es_z3,
    z             = z
  )
}

# ------------------------------------------------------------------------------
# 2. Computar tabla resumen univariada
# ------------------------------------------------------------------------------
cat(">> Calculando umbrales Tukey y Z-score por variable...\n")

resumen_list <- list()
flags_df <- df_gam %>% select(entidad, entidad_desc, departamento)

for (v in todas_vars) {
  res <- calcular_univariado(df_gam[[v]])
  resumen_list[[v]] <- data.frame(
    Variable = v,
    Media = mean(df_gam[[v]]),
    Mediana = median(df_gam[[v]]),
    Desv_Est = sd(df_gam[[v]]),
    IQR = res$iqr,
    Tukey_1.5_IQR = res$n_tukey_leve,
    Tukey_3.0_IQR = res$n_tukey_ext,
    Z_Score_2 = res$n_z2,
    Z_Score_3 = res$n_z3,
    stringsAsFactors = FALSE
  )
  flags_df[[paste0("out_tukey_", v)]] <- res$es_tukey_leve
  flags_df[[paste0("out_z2_", v)]] <- res$es_z2
}

df_resumen_univariado <- do.call(rbind, resumen_list)
rownames(df_resumen_univariado) <- NULL

# ------------------------------------------------------------------------------
# 3. Demostración Empírica del Sesgo de Escala
# ------------------------------------------------------------------------------
cat("\n--- DEMOSTRACION EMPIRICA DEL SESGO DE ESCALA ---\n")
# Municipios señalados por Tukey en montos brutos (Gasto Vigente)
top_escala <- df_gam %>%
  filter(flags_df$out_tukey_gasto_vigente) %>%
  arrange(desc(gasto_vigente)) %>%
  select(entidad, entidad_desc, departamento, gasto_vigente, tasa_ejecucion_gasto, brecha_pago, ratio_inversion)

cat("Atípicos detectados en Gasto Vigente (Bruto):", nrow(top_escala), "municipios.\n")
cat("Primeros 5 señalados por escala:\n")
print(head(top_escala, 5))

# Comparar con municipios que tienen distorsiones extremas en RATIOS (ej. tasa ejecucion baja o brecha pago alta)
top_anomalias_reales <- df_gam %>%
  filter(flags_df$out_tukey_tasa_ejecucion_gasto | flags_df$out_tukey_brecha_pago | flags_df$out_tukey_balance_caja_real) %>%
  arrange(desc(brecha_pago)) %>%
  select(entidad, entidad_desc, departamento, gasto_vigente, tasa_ejecucion_gasto, brecha_pago, balance_caja_real)

cat("\nAtípicos detectados en Ratios Fiscales (Comportamiento):", nrow(top_anomalias_reales), "municipios.\n")
cat("Primeros 5 municipios con distorsión fiscal severa:\n")
print(head(top_anomalias_reales, 5))

# Guardar resultados univariados
saveRDS(list(
  resumen = df_resumen_univariado,
  flags = flags_df,
  top_escala = top_escala,
  top_anomalias_reales = top_anomalias_reales
), "data/processed/univariado_resultados.rds")

# ------------------------------------------------------------------------------
# 4. Generación de Gráficas de Alta Resolución (300 DPI)
# ------------------------------------------------------------------------------
cat(">> Generando gráficos en informe/figuras/...\n")

# Paleta técnica de DESIGN.md
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
      plot.title = element_text(color = COLOR_TEXT_PRI, face = "bold", size = 13),
      plot.subtitle = element_text(color = COLOR_TEXT_SEC, size = 10),
      legend.background = element_rect(fill = COLOR_BG, color = NA),
      legend.text = element_text(color = COLOR_TEXT_PRI),
      legend.title = element_text(color = COLOR_TEXT_PRI, face = "bold")
    )
}

# FIGURA 1: Demostración empírica de sesgo de escala
# Scatter plot: Gasto Vigente (Log) vs Tasa de Ejecución, coloreando quiénes son señalados por escala vs ratios
df_plot_sesgo <- df_gam %>%
  mutate(
    tipo_atipico = case_when(
      flags_df$out_tukey_gasto_vigente & (flags_df$out_tukey_tasa_ejecucion_gasto | flags_df$out_tukey_brecha_pago) ~ "Ambos (Escala y Ratio)",
      flags_df$out_tukey_gasto_vigente ~ "Solo Escala (Capital/Grande)",
      flags_df$out_tukey_tasa_ejecucion_gasto | flags_df$out_tukey_brecha_pago ~ "Solo Ratio (Anomalía Real Oculta)",
      TRUE ~ "Dentro de Norma"
    ),
    etiqueta = ifelse(tipo_atipico != "Dentro de Norma" & (gasto_vigente > 5e8 | brecha_pago > 0.08 | tasa_ejecucion_gasto < 0.45),
                      municipio, "")
  )

p1 <- ggplot(df_plot_sesgo, aes(x = gasto_vigente / 1e6, y = tasa_ejecucion_gasto * 100, color = tipo_atipico)) +
  geom_point(aes(size = tipo_atipico, alpha = tipo_atipico)) +
  scale_x_log10(labels = label_comma(prefix = "Bs ", suffix = "M")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_color_manual(values = c(
    "Dentro de Norma" = "#374151",
    "Solo Escala (Capital/Grande)" = COLOR_PRIMARY,
    "Solo Ratio (Anomalía Real Oculta)" = COLOR_ALERT,
    "Ambos (Escala y Ratio)" = COLOR_WARN
  )) +
  scale_size_manual(values = c(
    "Dentro de Norma" = 1.8,
    "Solo Escala (Capital/Grande)" = 3.5,
    "Solo Ratio (Anomalía Real Oculta)" = 3.8,
    "Ambos (Escala y Ratio)" = 4.2
  )) +
  scale_alpha_manual(values = c(
    "Dentro de Norma" = 0.35,
    "Solo Escala (Capital/Grande)" = 0.9,
    "Solo Ratio (Anomalía Real Oculta)" = 1.0,
    "Ambos (Escala y Ratio)" = 1.0
  )) +
  geom_text(aes(label = etiqueta), color = "#FFFFFF", size = 2.8, vjust = -0.8, check_overlap = TRUE) +
  labs(
    title = "Evidencia Empírica del Sesgo de Escala en Detección Univariada",
    subtitle = "El criterio univariado sobre montos brutos sólo etiqueta municipios grandes, ignorando distorsiones en municipios medianos/pequeños",
    x = "Presupuesto Vigente (Escala Logarítmica en Millones de Bs.)",
    y = "Tasa de Ejecución Presupuestaria del Gasto (%)",
    color = "Diagnóstico Univariado",
    size = "Diagnóstico Univariado",
    alpha = "Diagnóstico Univariado"
  ) +
  theme_forense() +
  theme(legend.position = "bottom")

ggsave("informe/figuras/fig_01_sesgo_escala_univariado.png", plot = p1, width = 10, height = 6.2, dpi = 300)
cat(" - Guardado: informe/figuras/fig_01_sesgo_escala_univariado.png\n")

# FIGURA 2: Distribución de Ratios Fiscales y Atípicos Tukey
df_ratios_long <- df_gam %>%
  select(entidad, all_of(vars_ratios)) %>%
  pivot_longer(cols = -entidad, names_to = "Ratio", values_to = "Valor") %>%
  mutate(
    Ratio_Nombre = case_when(
      Ratio == "tasa_ejecucion_gasto" ~ "1. Ejecución Gasto",
      Ratio == "tasa_modificacion_gasto" ~ "2. Modificación Ppto",
      Ratio == "brecha_pago" ~ "3. Deuda Flotante",
      Ratio == "ratio_inversion" ~ "4. Inversión Pública",
      Ratio == "ratio_serv_nopers" ~ "5. Serv. No Personales",
      Ratio == "balance_caja_real" ~ "6. Balance Caja",
      Ratio == "eficacia_recaudacion" ~ "7. Eficacia Recaudación",
      Ratio == "dependencia_tgn" ~ "8. Dependencia TGN"
    )
  )

p2 <- ggplot(df_ratios_long, aes(x = Ratio_Nombre, y = Valor)) +
  geom_boxplot(fill = COLOR_BG, color = COLOR_PRIMARY, outlier.color = COLOR_ALERT, outlier.size = 2, outlier.alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Dispersión Univariada y Atípicos en Ratios Fiscales (335 GAMs)",
    subtitle = "Puntos rojos representan municipios con comportamiento extremo según el criterio Tukey (1.5 IQR)",
    x = "",
    y = "Valor del Ratio (Proporción / Variación Relativa)"
  ) +
  theme_forense()

ggsave("informe/figuras/fig_02_boxplot_ratios_tukey.png", plot = p2, width = 10, height = 5.5, dpi = 300)
cat(" - Guardado: informe/figuras/fig_02_boxplot_ratios_tukey.png\n")

# ------------------------------------------------------------------------------
# 5. Exportación de Tablas LaTeX para el Informe
# ------------------------------------------------------------------------------
cat(">> Exportando tablas LaTeX en informe/tablas/...\n")

# Formateo de nombres legibles para la tabla
nombres_legibles <- c(
  "gasto_vigente" = "Presupuesto Vigente (Bs.)",
  "gasto_devengado" = "Gasto Devengado (Bs.)",
  "ingreso_percibido" = "Ingreso Percibido (Bs.)",
  "tasa_ejecucion_gasto" = "Tasa de Ejecución Gasto",
  "tasa_modificacion_gasto" = "Tasa Modificación Presupuestaria",
  "brecha_pago" = "Brecha de Pago (Deuda Flotante)",
  "ratio_inversion" = "Ratio de Inversión Pública",
  "ratio_serv_nopers" = "Ratio Serv. No Personales (Gr. 20000)",
  "balance_caja_real" = "Balance de Caja Real",
  "eficacia_recaudacion" = "Eficacia de Recaudación",
  "dependencia_tgn" = "Dependencia de Transferencias TGN"
)

lineas_tabla <- c()
for (i in seq_len(nrow(df_resumen_univariado))) {
  row <- df_resumen_univariado[i, ]
  v_nom <- nombres_legibles[row$Variable]
  
  if (row$Variable %in% vars_montos) {
    med_str <- sprintf("%.2e", row$Media)
    iqr_str <- sprintf("%.2e", row$IQR)
  } else {
    med_str <- sprintf("%.3f", row$Media)
    iqr_str <- sprintf("%.3f", row$IQR)
  }
  
  lineas_tabla <- c(lineas_tabla, sprintf(
    "%s & %s & %s & %d & %d & %d & %d \\\\",
    v_nom, med_str, iqr_str,
    row$Tukey_1.5_IQR, row$Tukey_3.0_IQR,
    row$Z_Score_2, row$Z_Score_3
  ))
}

tex_univariado <- sprintf(
"\\begin{table}[htbp]
\\centering
\\footnotesize
\\caption{L\\'inea Base Univariada: Comparativa de Umbrales Tukey y Z-Score (335 GAMs)}
\\label{tab:linea_base_univariada}
\\begin{tabular}{lrrrrrr}
\\hline
\\textbf{Variable Presupuestaria} & \\textbf{Media} & \\textbf{IQR} & \\textbf{Tukey 1.5} & \\textbf{Tukey 3.0} & \\textbf{$|z|>2$} & \\textbf{$|z|>3$} \\\\
\\hline
\\multicolumn{7}{l}{\\textit{Panel A: Montos Monetarios Brutos (Sesgo de Escala)}} \\\\
%s
\\hline
\\multicolumn{7}{l}{\\textit{Panel B: Ratios Fiscales Invariantes a la Escala}} \\\\
%s
\\hline
\\end{tabular}
\\end{table}",
  paste(lineas_tabla[1:3], collapse = "\n"),
  paste(lineas_tabla[4:11], collapse = "\n")
)

con_u <- file("informe/tablas/tabla_linea_base_univariada.tex", open = "wt", encoding = "UTF-8")
writeLines(tex_univariado, con_u, useBytes = TRUE)
close(con_u)
cat(" - Guardado: informe/tablas/tabla_linea_base_univariada.tex\n")

# Tabla de Casos Emblemáticos de Sesgo
tex_casos <- sprintf(
"\\begin{table}[htbp]
\\centering
\\footnotesize
\\caption{Demostraci\\'on del Sesgo de Escala: Grandes Ciudades vs. Distorsiones Ocultas}
\\label{tab:top_atipicos_univariados}
\\begin{tabular}{llrrrr}
\\hline
\\textbf{Municipio} & \\textbf{Departamento} & \\textbf{Ppto. Vigente (Bs.)} & \\textbf{Ejec. (\\%%)} & \\textbf{Deuda Flot. (\\%%)} & \\textbf{Diagn\\'ostico Univ.} \\\\
\\hline
\\multicolumn{6}{l}{\\textit{Se\\~nalados por Tukey en Montos Brutos (Falsos Positivos de Escala)}} \\\\
%s \\\\
%s \\\\
%s \\\\
\\hline
\\multicolumn{6}{l}{\\textit{Se\\~nalados por Ratios Fiscales pero Omitidos por Escala (Verdaderas Distorsiones)}} \\\\
%s \\\\
%s \\\\
%s \\\\
\\hline
\\end{tabular}
\\end{table}",
  sprintf("%s & %s & %s & %.1f\\%% & %.2f\\%% & Solo Gran Escala",
          top_escala$entidad_desc[1], top_escala$departamento[1], format(round(top_escala$gasto_vigente[1]), big.mark=".", decimal.mark=","), top_escala$tasa_ejecucion_gasto[1]*100, top_escala$brecha_pago[1]*100),
  sprintf("%s & %s & %s & %.1f\\%% & %.2f\\%% & Solo Gran Escala",
          top_escala$entidad_desc[2], top_escala$departamento[2], format(round(top_escala$gasto_vigente[2]), big.mark=".", decimal.mark=","), top_escala$tasa_ejecucion_gasto[2]*100, top_escala$brecha_pago[2]*100),
  sprintf("%s & %s & %s & %.1f\\%% & %.2f\\%% & Solo Gran Escala",
          top_escala$entidad_desc[3], top_escala$departamento[3], format(round(top_escala$gasto_vigente[3]), big.mark=".", decimal.mark=","), top_escala$tasa_ejecucion_gasto[3]*100, top_escala$brecha_pago[3]*100),
  sprintf("%s & %s & %s & %.1f\\%% & %.2f\\%% & Alta Deuda Flotante",
          top_anomalias_reales$entidad_desc[1], top_anomalias_reales$departamento[1], format(round(top_anomalias_reales$gasto_vigente[1]), big.mark=".", decimal.mark=","), top_anomalias_reales$tasa_ejecucion_gasto[1]*100, top_anomalias_reales$brecha_pago[1]*100),
  sprintf("%s & %s & %s & %.1f\\%% & %.2f\\%% & Alta Deuda Flotante",
          top_anomalias_reales$entidad_desc[2], top_anomalias_reales$departamento[2], format(round(top_anomalias_reales$gasto_vigente[2]), big.mark=".", decimal.mark=","), top_anomalias_reales$tasa_ejecucion_gasto[2]*100, top_anomalias_reales$brecha_pago[2]*100),
  sprintf("%s & %s & %s & %.1f\\%% & %.2f\\%% & Alta Deuda Flotante",
          top_anomalias_reales$entidad_desc[3], top_anomalias_reales$departamento[3], format(round(top_anomalias_reales$gasto_vigente[3]), big.mark=".", decimal.mark=","), top_anomalias_reales$tasa_ejecucion_gasto[3]*100, top_anomalias_reales$brecha_pago[3]*100)
)

con_c <- file("informe/tablas/tabla_top_atipicos_univariados.tex", open = "wt", encoding = "UTF-8")
writeLines(tex_casos, con_c, useBytes = TRUE)
close(con_c)
cat(" - Guardado: informe/tablas/tabla_top_atipicos_univariados.tex\n")

cat("\nLínea base univariada completada con éxito.\n")
cat("====================================================================\n")
