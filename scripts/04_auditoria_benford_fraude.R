# ==============================================================================
# PROYECTO: Auditoría Forense Presupuestaria de Municipios de Bolivia (Gestión 2025)
# SCRIPT: 04_auditoria_benford_fraude.R
# OBJETIVO: Auditoría forense de segundo nivel en registros crudos de gasto de los
#           municipios en Consenso GMM + LOF: Ley de Benford, montos duplicados
#           y descomposición por grupos de objeto de gasto.
# ==============================================================================

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(benford.analysis)
})

cat("====================================================================\n")
cat(" ETAPA 2.3: AUDITORIA FORENSE BENFORD Y DETECCION DE FRAUDE\n")
cat("====================================================================\n\n")

# 1. Cargar municipios modelados y filtrar grupo consenso
df_modelos <- readRDS("data/processed/municipios_modelados_2025.rds")

df_consenso <- df_modelos %>%
  filter(categoria_consenso == "Consenso GMM + LOF (Prioridad Alta)") %>%
  arrange(desc(lof_score))

consenso_ids <- df_consenso$entidad
cat("Municipios prioritarios en Consenso GMM + LOF:", length(consenso_ids), "\n")

# 2. Extracción de partidas devengadas crudas vía DuckDB
cat(">> Extrayendo registros crudos de gasto devengado para los municipios en consenso...\n")
dir_gastos <- "data/raw/gastos_gestion_2025"
gasto_files <- list.files(dir_gastos, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
csv_gastos <- gsub("\\\\", "/", gasto_files[1])

con <- dbConnect(duckdb(shared_home = FALSE))

q_partidas <- sprintf("
SELECT 
  entidad,
  entidad_desc_entidad as entidad_desc,
  desc_departamento as departamento,
  objeto_grupo,
  objeto_desc_grupo,
  objeto_partida,
  objeto_desc_partida,
  inicial,
  vigente,
  devengado,
  pagado
FROM read_csv_auto('%s')
WHERE gestion = 2025 
  AND entidad_sigla_subarea = 'GAM' 
  AND devengado > 0
  AND entidad IN (%s)
", csv_gastos, paste(consenso_ids, collapse = ", "))

df_partidas_consenso <- dbGetQuery(con, q_partidas)
dbDisconnect(con, shutdown = TRUE)

cat("Partidas devengadas extraídas:", nrow(df_partidas_consenso), "\n\n")

# ------------------------------------------------------------------------------
# 3. Prueba de Conformidad con la Ley de Benford (Primer Dígito)
# ------------------------------------------------------------------------------
cat(">> Ejecutando prueba de Benford por municipio...\n")

lista_benford <- list()
digitos_plot_list <- list()

for (ent in consenso_ids) {
  sub_datos <- df_partidas_consenso %>% filter(entidad == ent)
  nom_mun <- sub_datos$entidad_desc[1]
  depto   <- sub_datos$departamento[1]
  n_obs   <- nrow(sub_datos)
  
  bf <- suppressWarnings(benford(sub_datos$devengado, number.of.digits = 1, sign = "positive"))
  
  mad_val     <- bf$MAD
  mad_conf    <- bf$MAD.conformity
  chisq_stat  <- bf$stats$chisq$statistic
  chisq_p     <- bf$stats$chisq$p.value
  
  # Frecuencias observadas vs teóricas del primer dígito
  bf_table <- bf$bfd
  bf_df <- data.frame(
    entidad = ent,
    municipio = nom_mun,
    departamento = depto,
    digito = bf_table$digits,
    observado = bf_table$data.dist,
    teorico = bf_table$benford.dist,
    stringsAsFactors = FALSE
  )
  digitos_plot_list[[as.character(ent)]] <- bf_df
  
  # Análisis de duplicados exactos
  dups <- sub_datos %>%
    group_by(devengado) %>%
    filter(n() > 1) %>%
    summarise(frecuencia = n(), total = sum(devengado)) %>%
    arrange(desc(frecuencia))
  
  n_dups <- sum(dups$frecuencia)
  pct_dups <- (n_dups / n_obs) * 100
  
  lista_benford[[as.character(ent)]] <- data.frame(
    entidad = ent,
    municipio = nom_mun,
    departamento = depto,
    n_partidas = n_obs,
    MAD = mad_val,
    conformidad_benford = mad_conf,
    chi_cuadrado = chisq_stat,
    p_valor = chisq_p,
    partidas_duplicadas = n_dups,
    pct_duplicados = pct_dups,
    stringsAsFactors = FALSE
  )
}

df_resumen_benford <- do.call(rbind, lista_benford)
rownames(df_resumen_benford) <- NULL

df_digitos_todos <- do.call(rbind, digitos_plot_list)

cat("Resultados de Conformidad Benford (Nigrini 2012):\n")
print(df_resumen_benford[, c("municipio", "n_partidas", "MAD", "conformidad_benford", "p_valor", "pct_duplicados")])

# Guardar resultados
saveRDS(list(
  resumen_benford = df_resumen_benford,
  digitos = df_digitos_todos,
  partidas_consenso = df_partidas_consenso
), "data/processed/auditoria_benford_consenso.rds")

# ------------------------------------------------------------------------------
# 4. Generación de Gráficos Forenses de Alta Resolución (300 DPI)
# ------------------------------------------------------------------------------
cat(">> Generando gráficos forenses en informe/figuras/...\n")

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

# FIGURA 6: Caso Emblemático de Violación de Benford (Caquiaviri - p < 0.05) vs Benford Teórico
df_caquiaviri <- df_digitos_todos %>% filter(entidad == 1239)

p_bf_caq <- ggplot(df_caquiaviri, aes(x = factor(digito))) +
  geom_col(aes(y = observado * 100), fill = COLOR_ALERT, alpha = 0.85, width = 0.6) +
  geom_line(aes(y = teorico * 100, group = 1), color = COLOR_PRIMARY, linewidth = 1.2) +
  geom_point(aes(y = teorico * 100), color = "#FFFFFF", size = 2.8) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Prueba de la Ley de Benford: Gobierno Autónomo Municipal de Caquiaviri (La Paz)",
    subtitle = sprintf("Violación estadísticamente significativa (Chi² = %.2f, p = %.4f, MAD = %.4f - Nonconformity). Barras: Observado, Línea: Teórico.",
                       df_resumen_benford$chi_cuadrado[df_resumen_benford$entidad == 1239],
                       df_resumen_benford$p_valor[df_resumen_benford$entidad == 1239],
                       df_resumen_benford$MAD[df_resumen_benford$entidad == 1239]),
    x = "Primer Dígito Significativo (1 a 9)",
    y = "Frecuencia Relativa (%)"
  ) +
  theme_forense()

ggsave("informe/figuras/fig_06_benford_caquiaviri.png", plot = p_bf_caq, width = 9.5, height = 5.2, dpi = 300)
cat(" - Guardado: informe/figuras/fig_06_benford_caquiaviri.png\n")

# FIGURA 7: Composición del Gasto por Objeto en Municipios Prioritarios
df_comp_gasto <- df_partidas_consenso %>%
  mutate(
    grupo_nombre = case_when(
      objeto_grupo == 10000 ~ "10000 Serv. Personales",
      objeto_grupo == 20000 ~ "20000 Serv. No Personales",
      objeto_grupo == 30000 ~ "30000 Materiales y Sum.",
      objeto_grupo == 40000 ~ "40000 Activos Reales",
      TRUE                  ~ "Otros Grupos"
    )
  ) %>%
  group_by(entidad_desc, grupo_nombre) %>%
  summarise(monto = sum(devengado), .groups = "drop") %>%
  group_by(entidad_desc) %>%
  mutate(pct = monto / sum(monto) * 100)

p_comp <- ggplot(df_comp_gasto, aes(x = pct, y = reorder(entidad_desc, pct), fill = grupo_nombre)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c(
    "10000 Serv. Personales" = "#3B82F6",
    "20000 Serv. No Personales" = COLOR_ALERT,
    "30000 Materiales y Sum." = COLOR_WARN,
    "40000 Activos Reales" = COLOR_NORMAL,
    "Otros Grupos" = "#6B7280"
  )) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Estructura Presupuestaria del Gasto Devengado en los 12 GAMs Priorizados",
    subtitle = "Resalta distorsiones estructurales: nula inversión en activos reales (Tahua) o elevado gasto en servicios no personales (grupo 20000)",
    x = "Participación Porcentual en el Total Devengado (%)",
    y = "",
    fill = "Grupo de Objeto de Gasto"
  ) +
  theme_forense() +
  theme(legend.position = "bottom")

ggsave("informe/figuras/fig_07_composicion_gasto_consenso.png", plot = p_comp, width = 11, height = 6.2, dpi = 300)
cat(" - Guardado: informe/figuras/fig_07_composicion_gasto_consenso.png\n")

# ------------------------------------------------------------------------------
# 5. Exportación de Tablas LaTeX para el Informe
# ------------------------------------------------------------------------------
cat(">> Exportando tablas LaTeX en informe/tablas/...\n")

lineas_benford <- c()
for (i in seq_len(nrow(df_resumen_benford))) {
  r <- df_resumen_benford[i, ]
  
  # Formato p-valor
  p_str <- if (r$p_valor < 0.001) "< 0,001" else sprintf("%.4f", r$p_valor)
  
  lineas_benford <- c(lineas_benford, sprintf(
    "%s & %s & %d & %.4f & %s & %.2f & %s & %.1f\\%% \\\\",
    r$municipio, r$departamento, r$n_partidas,
    r$MAD, r$conformidad_benford, r$chi_cuadrado, p_str, r$pct_duplicados
  ))
}

tex_benford <- sprintf(
"\\begin{table}[htbp]
\\centering
\\scriptsize
\\caption{Resultados de la Prueba de Conformidad con la Ley de Benford y Duplicados Exactos}
\\label{tab:auditoria_benford}
\\begin{tabular}{llrrrrrr}
\\hline
\\textbf{Gobierno Municipal} & \\textbf{Depto.} & \\textbf{Partidas} & \\textbf{MAD} & \\textbf{Conformidad (Nigrini)} & \\textbf{Chi-Cuad.} & \\textbf{p-valor} & \\textbf{Duplicados} \\\\
\\hline
%s
\\hline
\\end{tabular}
\\end{table}",
  paste(lineas_benford, collapse = "\n")
)

writeLines(tex_benford, "informe/tablas/tabla_auditoria_benford.tex")
cat(" - Guardado: informe/tablas/tabla_auditoria_benford.tex\n")

# Tabla de Duplicados Sospechosos (Fraccionamiento de Contratos)
dups_sospechosos <- df_partidas_consenso %>%
  group_by(entidad_desc, devengado) %>%
  filter(n() >= 3 & devengado >= 10000) %>%
  summarise(
    departamento = first(departamento),
    repeticiones = n(),
    monto_total = sum(devengado),
    partidas = paste(unique(objeto_desc_partida)[1:min(2, length(unique(objeto_desc_partida)))], collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(repeticiones), desc(monto_total)) %>%
  head(10)

lineas_dups <- c()
for (i in seq_len(nrow(dups_sospechosos))) {
  d <- dups_sospechosos[i, ]
  lineas_dups <- c(lineas_dups, sprintf(
    "%s & %s & Bs. %s & %d & Bs. %s \\\\",
    d$entidad_desc, d$departamento,
    format(round(d$devengado), big.mark = ".", decimal.mark = ","),
    d$repeticiones,
    format(round(d$monto_total), big.mark = ".", decimal.mark = ",")
  ))
}

tex_dups <- sprintf(
"\\begin{table}[htbp]
\\centering
\\footnotesize
\\caption{Top 10 Casos de Montos Id\\'enticos Reincidentes (Alerta Forense de Posible Fraccionamiento)}
\\label{tab:duplicados_sospechosos}
\\begin{tabular}{llrrr}
\\hline
\\textbf{Gobierno Municipal} & \\textbf{Depto.} & \\textbf{Monto Repetido} & \\textbf{Repeticiones} & \\textbf{Monto Total Acum.} \\\\
\\hline
%s
\\hline
\\end{tabular}
\\end{table}",
  paste(lineas_dups, collapse = "\n")
)

writeLines(tex_dups, "informe/tablas/tabla_duplicados_sospechosos.tex")
cat(" - Guardado: informe/tablas/tabla_duplicados_sospechosos.tex\n")

cat("\nAuditoría Benford y detección de duplicados completadas con éxito.\n")
cat("====================================================================\n")
