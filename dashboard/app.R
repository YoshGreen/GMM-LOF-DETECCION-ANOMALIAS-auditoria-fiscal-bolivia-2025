# ==============================================================================
# PROYECTO: Auditoría Forense Presupuestaria de Municipios de Bolivia (Gestión 2025)
# APLICACIÓN: dashboard/app.R
# TECNOLOGÍAS: R Shiny + bslib (Bootstrap 5) + plotly + DT + custom.css
# SISTEMA VISUAL: Dark Telemetry / Obsidian (estricto DESIGN.md & Anti-AI-Slop)
# ==============================================================================

suppressPackageStartupMessages({
  options(sass.cache = tempdir())
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(dplyr)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1. Carga de Datos Procesados
# ------------------------------------------------------------------------------
dir_base <- if (file.exists("data/processed/municipios_modelados_2025.rds")) "." else ".."
df_gam <- readRDS(file.path(dir_base, "data/processed/municipios_modelados_2025.rds"))
benford_data <- readRDS(file.path(dir_base, "data/processed/auditoria_benford_consenso.rds"))
univariado_res <- readRDS(file.path(dir_base, "data/processed/univariado_resultados.rds"))

departamentos_disponibles <- c("TODOS", sort(unique(df_gam$departamento)))

# Opciones limpias para selector forense: nombre amigable con valor = código de entidad
df_consenso_gams <- df_gam %>%
  filter(categoria_consenso == "Consenso GMM + LOF (Prioridad Alta)") %>%
  arrange(desc(lof_score))

opciones_consenso <- setNames(
  df_consenso_gams$entidad,
  paste0(df_consenso_gams$municipio, " (", df_consenso_gams$departamento, ") - LOF: ", sprintf("%.2f", df_consenso_gams$lof_score))
)

# Paleta semántica de DESIGN.md
COLOR_CANVAS   <- "#000000"
COLOR_BG       <- "#0D1117"
COLOR_GRID     <- "#1A222D"
COLOR_TEXT_PRI <- "#F3F4F6"
COLOR_TEXT_SEC <- "#9CA3AF"
COLOR_NORMAL   <- "#00FF88"
COLOR_PRIMARY  <- "#4DB8FF"
COLOR_WARN     <- "#FFB020"
COLOR_ALERT    <- "#FF3366"

# Helper formato moneda boliviana
fmt_bs <- function(x) {
  paste0("Bs. ", format(round(x), big.mark = ".", decimal.mark = ","))
}

# Helper configuración limpia de Plotly (Anti-AI-Slop: sin barra de herramientas ruidosa)
clean_plotly_config <- function(p) {
  config(p, displayModeBar = "hover",
         modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"),
         displaylogo = FALSE)
}

# Diagnóstico forense cualitativo por municipio de consenso
dict_diagnostico <- list(
  "1239" = list(
    titulo = "Violación de la Ley de Benford",
    desc = "Alteración estadísticamente significativa en el primer dígito (Chi² = 16,91, p = 0,031, MAD = 0,0241). Patrón anómalo incompatible con transacciones naturales."
  ),
  "1205" = list(
    titulo = "Parálisis de Inversión Pública (0,00%)",
    desc = "El municipio destinó el 100% de su presupuesto a gasto corriente y servicios. Cero asignación a formación bruta de capital o infraestructura."
  ),
  "1206" = list(
    titulo = "Peor Ejecución Nacional & Alta Deuda Flotante",
    desc = "Tasa de ejecución más baja de Bolivia (31,87%) con un 16,20% de compromisos devengados no pagados. Riesgo de insolvencia y parálisis operativa."
  ),
  "1207" = list(
    titulo = "Fraccionamiento de Compras (19,86% Duplicados)",
    desc = "Concentración crítica de partidas devengadas en montos idénticos repetitivos justo en los límites de contratación menor del Estado."
  ),
  "1208" = list(
    titulo = "Reincidencia de Montos & Sobredependencia",
    desc = "15,32% de pagos en partidas idénticas combinadas con atipicidad local extrema en servicios no personales."
  ),
  "1209" = list(
    titulo = "Superávit de Caja Distorsionado (46,67%)",
    desc = "Saldo aparente positivo originado por subejecución crítica del gasto de inversión en lugar de una recaudación eficiente."
  ),
  "1210" = list(
    titulo = "Aislamiento Multivariante & Deuda Flotante",
    desc = "Brecha de pago del 14,28% y vulnerabilidad estructural en captación de recursos propios respecto a municipios pares."
  ),
  "1211" = list(
    titulo = "Desfase Presupuestario y Modificación Neta",
    desc = "Alta tasa de modificación presupuestaria combinada con dispersión asimétrica de caja percibida."
  ),
  "1212" = list(
    titulo = "Rigidez en Servicios No Personales",
    desc = "Desviación extrema en gasto de grupo 20000 y atipicidad de densidad respecto a la mezcla gaussiana nacional."
  ),
  "1213" = list(
    titulo = "Vulnerabilidad Fiscal & Dispersión Operativa",
    desc = "Dispersión severa en ratios de operación y baja capacidad de cobertura financiera con recursos del TGN."
  ),
  "1214" = list(
    titulo = "Desfase Severo de Caja y Compromisos",
    desc = "Asimetría entre el flujo de ingresos percibidos y los gastos devengados con pasivos flotantes acumulados."
  ),
  "1215" = list(
    titulo = "Atipicidad Multivariante de Escala y Pasivos",
    desc = "Concentración masiva de deuda flotante nominal y desviación multivariante en componentes principales PCA."
  )
)

# ------------------------------------------------------------------------------
# 2. Interfaz de Usuario (UI) con bslib
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 0.75rem;",
    span("AUDITORÍA FISCAL BOLIVIA 2025", style = "color: #00FF88; font-weight: 700; letter-spacing: -0.02em;"),
    span(class = "navbar-badge", span(class = "pulse-dot"), "MEFP 2025 | DUCKDB OLAP")
  ),
  theme = bs_theme(
    version = 5,
    bg = "#000000",
    fg = "#F3F4F6",
    primary = "#00FF88",
    secondary = "#4DB8FF",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  # ============================================================================
  # PESTAÑA 1: PANORAMA FISCAL 2025
  # ============================================================================
  nav_panel(
    title = "Panorama Fiscal 2025",
    
    # Fila de KPIs Principales (Máximo 4 tarjetas según DESIGN.md)
    div(
      class = "kpi-container",
      div(
        class = "kpi-card kpi-normal",
        div(
          class = "kpi-title",
          span("Gasto Vigente Consolidado"),
          span(class = "kpi-tag tag-normal", "335 GAMs")
        ),
        div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_vigente))),
        div(class = "kpi-sub", "Presupuesto municipal total autorizado en Bolivia")
      ),
      div(
        class = "kpi-card kpi-primary",
        div(
          class = "kpi-title",
          span("Gasto Devengado (Ejecución)"),
          span(class = "kpi-tag tag-primary", sprintf("%.1f%% Global", sum(df_gam$gasto_devengado) / sum(df_gam$gasto_vigente) * 100))
        ),
        div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_devengado))),
        div(class = "kpi-sub", "Obligaciones contables efectivamente perfeccionadas")
      ),
      div(
        class = "kpi-card kpi-warn",
        div(
          class = "kpi-title",
          span("Deuda Flotante Exigible"),
          span(class = "kpi-tag tag-warn", "PASIVO EXIGIBLE")
        ),
        div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_devengado) - sum(df_gam$gasto_pagado))),
        div(class = "kpi-sub", "Compromisos devengados pendientes de pago en tesorería")
      ),
      div(
        class = "kpi-card kpi-alert",
        div(
          class = "kpi-title",
          span("Municipios en Alerta Crítica"),
          span(class = "kpi-tag tag-alert", "ALTO RIESGO")
        ),
        div(class = "kpi-value", "12 GAMs"),
        div(class = "kpi-sub", "Consenso GMM + LOF aislado para auditoría forense")
      )
    ),
    
    # Gráficos de Comparativa Departamental y Línea Base Univariada
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          div(
            div("Presupuesto vs. Ejecución por Departamento"),
            div(class = "card-subtitle", "Compara la capacidad autorizada (Vigente) contra la ejecución real (Devengado)")
          )
        ),
        card_body(plotlyOutput("plot_depto_comparativa", height = "360px"))
      ),
      card(
        card_header(
          div(
            div("Línea Base Univariada y Sesgo de Escala"),
            div(class = "card-subtitle", "El análisis univariado sobre montos brutos sólo señala capitales, mientras los ratios revelan fallas de gestión")
          )
        ),
        card_body(
          selectInput("selector_var_univariada", "Seleccionar Métrica de Análisis:",
                      choices = c("Presupuesto Vigente (Monto Bruto)" = "gasto_vigente",
                                  "Tasa de Ejecución de Gasto (%)" = "tasa_ejecucion_gasto",
                                  "Brecha de Pago / Deuda Flotante (%)" = "brecha_pago",
                                  "Ratio de Inversión Pública (%)" = "ratio_inversion",
                                  "Balance de Caja Real (%)" = "balance_caja_real"),
                      selected = "gasto_vigente"),
          plotlyOutput("plot_univariado_interactivo", height = "300px")
        )
      )
    )
  ),
  
  # ============================================================================
  # PESTAÑA 2: REGÍMENES GMM & DENSIDAD LOF
  # ============================================================================
  nav_panel(
    title = "Regímenes GMM & Densidad LOF",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Controles de Sensibilidad",
        selectInput("filtro_depto_gmm", "Departamento:", choices = departamentos_disponibles, selected = "TODOS"),
        sliderInput("slider_minpts", "Parámetro minPts (LOF):", min = 10, max = 30, value = 20, step = 5),
        sliderInput("slider_umbral_lof", "Umbral de Alerta LOF:", min = 1.2, max = 2.2, value = 1.5, step = 0.05),
        hr(style = "border-color: rgba(255, 255, 255, 0.08);"),
        div(
          style = "font-size: 0.78rem; color: #9CA3AF;",
          p(strong("Guía de Interpretación:")),
          p("• GMM ajusta 4 regímenes gaussianos en el espacio normalizado de 9 ratios fiscales."),
          p("• El score LOF mide el grado de aislamiento respecto a la densidad de los k municipios más cercanos."),
          p("• Puntos en carmesí cumplen la condición de doble alerta (GMM global + LOF local).")
        )
      ),
      card(
        card_header(
          div(
            div("Espacio Latente PCA: Regímenes GMM y Factores Locales LOF"),
            div(class = "card-subtitle", "Los puntos anómalos destacan en carmesí (consenso) y ámbar (local). Pase el cursor para ver el diagnóstico.")
          )
        ),
        card_body(plotlyOutput("plot_pca_gmm_interactivo", height = "520px"))
      )
    )
  ),
  
  # ============================================================================
  # PESTAÑA 3: CONSENSO DE ANOMALÍAS
  # ============================================================================
  nav_panel(
    title = "Consenso de Anomalías",
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        title = "Filtros de Auditoría",
        selectInput("filtro_depto_tabla", "Departamento:", choices = departamentos_disponibles, selected = "TODOS"),
        selectInput("filtro_categoria_tabla", "Categoría Forense:",
                    choices = c("TODAS", "Consenso GMM + LOF (Prioridad Alta)",
                                "Solo GMM (Global/Contextual)", "Solo LOF (Local)", "Normal"),
                    selected = "TODAS"),
        hr(style = "border-color: rgba(255, 255, 255, 0.08);"),
        div(
          style = "font-size: 0.75rem; color: #9CA3AF;",
          div(strong("CONTEOS OFICIALES 2025:"), style = "margin-bottom: 0.4rem; color: #F3F4F6;"),
          div(
            class = "stats-pill-grid",
            div(class = "stats-pill", div(class = "stats-pill-count", style = "color: #FF3366;", "12"), div(class = "stats-pill-label", "Consenso")),
            div(class = "stats-pill", div(class = "stats-pill-count", style = "color: #FFB020;", "23"), div(class = "stats-pill-label", "Solo LOF")),
            div(class = "stats-pill", div(class = "stats-pill-count", style = "color: #4DB8FF;", "15"), div(class = "stats-pill-label", "Solo GMM")),
            div(class = "stats-pill", div(class = "stats-pill-count", style = "color: #00FF88;", "285"), div(class = "stats-pill-label", "Normal"))
          )
        )
      ),
      card(
        card_header(
          div(
            div("Matriz Forense de Cuadrantes: Log-Densidad GMM vs. Score LOF"),
            div(class = "card-subtitle", "El cuadrante superior izquierdo (LOF > 1.5 y Log-Densidad < -14.5) delimita la zona de riesgo crítico")
          )
        ),
        card_body(plotlyOutput("plot_cuadrantes_consenso", height = "320px"))
      ),
      card(
        card_header(
          div(
            div("Padrón Municipal Auditado (335 GAMs)"),
            div(class = "card-subtitle", "Tabla interactiva y exportable con los 9 ratios estandarizados y diagnóstico de riesgo")
          )
        ),
        card_body(DTOutput("tabla_consenso_interactiva"))
      )
    )
  ),
  
  # ============================================================================
  # PESTAÑA 4: AUDITORÍA FORENSE (BENFORD & DRILL-DOWN)
  # ============================================================================
  nav_panel(
    title = "Auditoría Forense (Benford & Drill-Down)",
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        title = "Expediente Municipal",
        selectInput("selector_municipio_forense", "Seleccionar GAM Prioritario:",
                    choices = opciones_consenso,
                    selected = opciones_consenso[grep("Caquiaviri", names(opciones_consenso))[1]]),
        hr(style = "border-color: rgba(255, 255, 255, 0.08);"),
        uiOutput("panel_ficha_forense")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(
            div(
              div("Prueba de Ley de Benford (Primer Dígito)"),
              div(class = "card-subtitle", "Distribución observada (carmesí) vs. teórica (azul). Desvíos anómalos señalan alteración humana o fraccionamiento.")
            )
          ),
          card_body(plotlyOutput("plot_benford_drilldown", height = "360px"))
        ),
        card(
          card_header(
            div(
              div("Composición del Gasto Devengado por Objeto"),
              div(class = "card-subtitle", "Desglose por grupos de partida: Serv. Personales (10000), Serv. No Personales (20000), Materiales (30000), Activos (40000)")
            )
          ),
          card_body(plotlyOutput("plot_objeto_drilldown", height = "360px"))
        )
      ),
      card(
        card_header(
          div(
            div("Diagnóstico de Liquidez y Brecha Presupuestaria (Caja Real vs. Deuda)"),
            div(class = "card-subtitle", "Comparativa de Ingresos Percibidos, Gastos Devengados y Gastos Pagados")
          )
        ),
        card_body(plotlyOutput("plot_caja_drilldown", height = "280px"))
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 3. Lógica del Servidor (Server)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- PESTAÑA 1: PANORAMA FISCAL ---
  
  output$plot_depto_comparativa <- renderPlotly({
    df_depto <- df_gam %>%
      group_by(departamento) %>%
      summarise(
        vigente = sum(gasto_vigente) / 1e6,
        devengado = sum(gasto_devengado) / 1e6,
        tasa_ejec = sum(gasto_devengado) / sum(gasto_vigente) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(vigente))
    
    p <- plot_ly(df_depto, y = ~reorder(departamento, vigente)) %>%
      add_bars(x = ~vigente, name = "Presupuesto Vigente",
               marker = list(color = "#161B22", line = list(color = "#374151", width = 1)),
               hovertemplate = "<b>%{y}</b><br>Vigente: Bs. %{x:.1f}M<extra></extra>") %>%
      add_bars(x = ~devengado, name = "Gasto Devengado",
               marker = list(color = COLOR_PRIMARY, line = list(color = "#00FF88", width = 1)),
               hovertemplate = "<b>%{y}</b><br>Devengado: Bs. %{x:.1f}M<extra></extra>") %>%
      layout(
        barmode = "group",
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        xaxis = list(
          title = "Millones de Bolivianos (Bs. M)",
          gridcolor = COLOR_GRID,
          zerolinecolor = COLOR_GRID
        ),
        yaxis = list(title = "", gridcolor = "transparent"),
        legend = list(orientation = "h", x = 0.2, y = 1.15, font = list(color = COLOR_TEXT_PRI)),
        margin = list(t = 40, r = 20, b = 40, l = 100)
      ) %>%
      clean_plotly_config()
    p
  })
  
  output$plot_univariado_interactivo <- renderPlotly({
    var_sel <- input$selector_var_univariada
    df_plot <- df_gam %>%
      mutate(
        val = .data[[var_sel]],
        es_escala = (var_sel == "gasto_vigente")
      )
    
    q1 <- quantile(df_plot$val, 0.25)
    q3 <- quantile(df_plot$val, 0.75)
    iqr_val <- q3 - q1
    u_tukey <- q3 + 1.5 * iqr_val
    l_tukey <- q1 - 1.5 * iqr_val
    
    df_plot <- df_plot %>%
      mutate(
        es_atipico = (val > u_tukey | val < l_tukey),
        color_punto = ifelse(es_atipico, COLOR_ALERT, COLOR_PRIMARY)
      )
    
    p <- plot_ly(df_plot, y = ~departamento, x = ~val, type = "scatter", mode = "markers",
                 color = ~es_atipico,
                 colors = c(COLOR_PRIMARY, COLOR_ALERT),
                 marker = list(size = 7, opacity = 0.75, line = list(width = 0.5, color = "#FFFFFF")),
                 text = ~paste0("<b>", municipio, " (", departamento, ")</b><br>",
                                "Valor: ", ifelse(es_escala, fmt_bs(val), sprintf("%.2f%%", val * 100)), "<br>",
                                "Diagnóstico: ", ifelse(es_atipico, "Atípico Tukey", "Normal")),
                 hoverinfo = "text") %>%
      layout(
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        xaxis = list(
          title = ifelse(var_sel == "gasto_vigente", "Presupuesto (Escala Log)", "Ratio Proporcional"),
          type = ifelse(var_sel == "gasto_vigente", "log", "linear"),
          gridcolor = COLOR_GRID
        ),
        yaxis = list(title = "", gridcolor = COLOR_GRID),
        showlegend = FALSE,
        margin = list(t = 20, r = 20, b = 40, l = 100)
      ) %>%
      clean_plotly_config()
    p
  })
  
  # --- PESTAÑA 2: REGÍMENES GMM & LOF ---
  
  df_reactivo_lof <- reactive({
    minpts_val <- input$slider_minpts
    umbral_lof <- input$slider_umbral_lof
    col_lof <- paste0("lof_score_", minpts_val)
    
    df_temp <- df_gam %>%
      mutate(
        score_lof_actual = .data[[col_lof]],
        anomalo_lof_actual = score_lof_actual > umbral_lof,
        cat_consenso_actual = case_when(
          gmm_anomalo & anomalo_lof_actual  ~ "Consenso GMM + LOF (Prioridad Alta)",
          gmm_anomalo & !anomalo_lof_actual ~ "Solo GMM (Global/Contextual)",
          !gmm_anomalo & anomalo_lof_actual ~ "Solo LOF (Local)",
          TRUE                              ~ "Normal"
        )
      )
    
    if (input$filtro_depto_gmm != "TODOS") {
      df_temp <- df_temp %>% filter(departamento == input$filtro_depto_gmm)
    }
    df_temp
  })
  
  output$plot_pca_gmm_interactivo <- renderPlotly({
    df_p <- df_reactivo_lof()
    
    p <- plot_ly()
    
    # Renderizar puntos normales primero (opacidad 0.45)
    df_norm <- df_p %>% filter(cat_consenso_actual == "Normal")
    if (nrow(df_norm) > 0) {
      p <- p %>% add_trace(
        data = df_norm, x = ~PCA1, y = ~PCA2, type = "scatter", mode = "markers",
        name = "Normal",
        marker = list(color = COLOR_NORMAL, size = 6, opacity = 0.45),
        text = ~paste0("<b>", municipio, " (", departamento, ")</b><br>",
                       "Régimen GMM: ", regimen_gmm, "<br>",
                       "Presupuesto: ", fmt_bs(gasto_vigente), "<br>",
                       "Ejecución: ", sprintf("%.1f%%", tasa_ejecucion_gasto * 100), "<br>",
                       "LOF Score: ", sprintf("%.2f", score_lof_actual), "<br>",
                       "GMM Log-Dens: ", sprintf("%.2f", gmm_log_dens)),
        hoverinfo = "text"
      )
    }
    
    # Renderizar Solo GMM
    df_gmm_only <- df_p %>% filter(cat_consenso_actual == "Solo GMM (Global/Contextual)")
    if (nrow(df_gmm_only) > 0) {
      p <- p %>% add_trace(
        data = df_gmm_only, x = ~PCA1, y = ~PCA2, type = "scatter", mode = "markers",
        name = "Solo GMM",
        marker = list(color = COLOR_PRIMARY, size = 8, opacity = 0.85),
        text = ~paste0("<b>", municipio, " (", departamento, ")</b><br>",
                       "Categoría: Solo GMM (Global)<br>",
                       "LOF Score: ", sprintf("%.2f", score_lof_actual), "<br>",
                       "GMM Log-Dens: ", sprintf("%.2f", gmm_log_dens)),
        hoverinfo = "text"
      )
    }
    
    # Renderizar Solo LOF
    df_lof_only <- df_p %>% filter(cat_consenso_actual == "Solo LOF (Local)")
    if (nrow(df_lof_only) > 0) {
      p <- p %>% add_trace(
        data = df_lof_only, x = ~PCA1, y = ~PCA2, type = "scatter", mode = "markers",
        name = "Solo LOF",
        marker = list(color = COLOR_WARN, size = 8, opacity = 0.9),
        text = ~paste0("<b>", municipio, " (", departamento, ")</b><br>",
                       "Categoría: Solo LOF (Local)<br>",
                       "LOF Score: ", sprintf("%.2f", score_lof_actual), "<br>",
                       "GMM Log-Dens: ", sprintf("%.2f", gmm_log_dens)),
        hoverinfo = "text"
      )
    }
    
    # Renderizar Consenso GMM + LOF (Alta prioridad, opacidad 1.0, borde visible)
    df_cons <- df_p %>% filter(cat_consenso_actual == "Consenso GMM + LOF (Prioridad Alta)")
    if (nrow(df_cons) > 0) {
      p <- p %>% add_trace(
        data = df_cons, x = ~PCA1, y = ~PCA2, type = "scatter", mode = "markers+text",
        name = "Consenso GMM + LOF",
        marker = list(color = COLOR_ALERT, size = 11, opacity = 1.0, line = list(color = "#FFFFFF", width = 1.5)),
        text = ~municipio,
        textposition = "top right",
        textfont = list(color = "#FFFFFF", size = 10),
        hovertext = ~paste0("<b>CRÍTICO: ", municipio, " (", departamento, ")</b><br>",
                            "Presupuesto: ", fmt_bs(gasto_vigente), "<br>",
                            "Ejecución: ", sprintf("%.1f%%", tasa_ejecucion_gasto * 100), "<br>",
                            "Deuda Flotante: ", sprintf("%.1f%%", brecha_pago * 100), "<br>",
                            "LOF Score: ", sprintf("%.2f", score_lof_actual), "<br>",
                            "GMM Log-Dens: ", sprintf("%.2f", gmm_log_dens)),
        hoverinfo = "text"
      )
    }
    
    p <- p %>% layout(
      plot_bgcolor = COLOR_BG,
      paper_bgcolor = COLOR_BG,
      font = list(color = COLOR_TEXT_PRI, family = "Inter"),
      xaxis = list(title = "Componente Principal 1 (PCA 1)", gridcolor = COLOR_GRID),
      yaxis = list(title = "Componente Principal 2 (PCA 2)", gridcolor = COLOR_GRID),
      legend = list(orientation = "h", x = 0.1, y = 1.1, font = list(color = COLOR_TEXT_PRI)),
      margin = list(t = 50, r = 30, b = 50, l = 60)
    ) %>%
      clean_plotly_config()
    p
  })
  
  # --- PESTAÑA 3: CONSENSO DE ANOMALÍAS ---
  
  output$plot_cuadrantes_consenso <- renderPlotly({
    p <- plot_ly(df_gam, x = ~gmm_log_dens, y = ~lof_score,
                 color = ~categoria_consenso,
                 colors = c(COLOR_ALERT, COLOR_NORMAL, COLOR_PRIMARY, COLOR_WARN),
                 type = "scatter", mode = "markers",
                 marker = list(size = 8, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
                 text = ~paste0("<b>", municipio, " (", departamento, ")</b><br>",
                                "Categoría: ", categoria_consenso, "<br>",
                                "GMM Log-Densidad: ", sprintf("%.2f", gmm_log_dens), "<br>",
                                "LOF Score (base 20): ", sprintf("%.2f", lof_score)),
                 hoverinfo = "text") %>%
      add_segments(x = -14.526, xend = -14.526, y = 0.8, yend = 2.8,
                   line = list(color = COLOR_PRIMARY, dash = "dash", width = 1),
                   inherit = FALSE, showlegend = FALSE) %>%
      add_segments(x = -26, xend = 3, y = 1.5, yend = 1.5,
                   line = list(color = COLOR_ALERT, dash = "dash", width = 1),
                   inherit = FALSE, showlegend = FALSE) %>%
      layout(
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        xaxis = list(title = "Log-Densidad GMM (Corte P5 = -14.5)", gridcolor = COLOR_GRID),
        yaxis = list(title = "LOF Score (Corte Crítico = 1.5)", gridcolor = COLOR_GRID),
        legend = list(orientation = "h", x = 0.05, y = 1.15, font = list(color = COLOR_TEXT_PRI)),
        margin = list(t = 40, r = 20, b = 40, l = 60)
      ) %>%
      clean_plotly_config()
    p
  })
  
  output$tabla_consenso_interactiva <- renderDT({
    df_t <- df_gam
    if (input$filtro_depto_tabla != "TODOS") {
      df_t <- df_t %>% filter(departamento == input$filtro_depto_tabla)
    }
    if (input$filtro_categoria_tabla != "TODAS") {
      df_t <- df_t %>% filter(categoria_consenso == input$filtro_categoria_tabla)
    }
    
    df_display <- df_t %>%
      arrange(desc(lof_score)) %>%
      select(
        Código = entidad,
        Municipio = municipio,
        Departamento = departamento,
        `Ppto. Vigente (Bs.)` = gasto_vigente,
        `Ejec. (%)` = tasa_ejecucion_gasto,
        `Deuda Flot. (%)` = brecha_pago,
        `Inversión (%)` = ratio_inversion,
        `LOF Score` = lof_score,
        `Log-Dens GMM` = gmm_log_dens,
        `Categoría Forense` = categoria_consenso
      )
    
    datatable(
      df_display,
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        scrollX = TRUE,
        autoWidth = FALSE
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    ) %>%
      formatCurrency("Ppto. Vigente (Bs.)", currency = "Bs. ", mark = ".", dec.mark = ",", digits = 0) %>%
      formatPercentage(c("Ejec. (%)", "Deuda Flot. (%)", "Inversión (%)"), digits = 1) %>%
      formatRound(c("LOF Score", "Log-Dens GMM"), digits = 2) %>%
      formatStyle(
        "Categoría Forense",
        backgroundColor = styleEqual(
          c("Consenso GMM + LOF (Prioridad Alta)", "Solo LOF (Local)", "Solo GMM (Global/Contextual)", "Normal"),
          c("rgba(255, 51, 102, 0.2)", "rgba(255, 176, 32, 0.2)", "rgba(77, 184, 255, 0.2)", "rgba(0, 255, 136, 0.15)")
        ),
        color = styleEqual(
          c("Consenso GMM + LOF (Prioridad Alta)", "Solo LOF (Local)", "Solo GMM (Global/Contextual)", "Normal"),
          c(COLOR_ALERT, COLOR_WARN, COLOR_PRIMARY, COLOR_NORMAL)
        ),
        fontWeight = "bold"
      )
  })
  
  # --- PESTAÑA 4: AUDITORÍA FORENSE DRILL-DOWN ---
  
  mun_seleccionado <- reactive({
    req(input$selector_municipio_forense)
    df_gam %>% filter(entidad == as.integer(input$selector_municipio_forense))
  })
  
  output$panel_ficha_forense <- renderUI({
    m <- mun_seleccionado()
    req(nrow(m) > 0)
    
    ent_code <- as.character(m$entidad)
    diag <- if (!is.null(dict_diagnostico[[ent_code]])) {
      dict_diagnostico[[ent_code]]
    } else {
      list(
        titulo = "Anomalía Multivariante de Consenso",
        desc = "Combinación simultánea de baja log-densidad probabilística en GMM y alto aislamiento espacial en LOF."
      )
    }
    
    tagList(
      # Banner de Alerta Forense Específica
      div(
        class = "forensic-alert-box",
        div(class = "forensic-alert-title", "ALERTA FORENSE PRINCIPAL"),
        p(strong(diag$titulo), style = "color: #F3F4F6; margin-bottom: 0.25rem; font-size: 0.82rem;"),
        p(class = "forensic-alert-desc", diag$desc)
      ),
      
      # Tarjeta Ficha Técnica Estructurada
      div(
        class = "forensic-spec-card",
        div(
          style = "font-family: var(--font-mono); color: #00FF88; font-weight: 700; font-size: 0.78rem; text-transform: uppercase; margin-bottom: 0.6rem;",
          paste0("GAM ", toupper(m$municipio))
        ),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Código Entidad:"), span(class = "forensic-spec-val", m$entidad)),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Departamento:"), span(class = "forensic-spec-val", m$departamento)),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Presupuesto Vigente:"), span(class = "forensic-spec-val", fmt_bs(m$gasto_vigente))),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Tasa de Ejecución:"), span(class = "forensic-spec-val", sprintf("%.1f%%", m$tasa_ejecucion_gasto * 100))),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Deuda Flotante:"), span(class = "forensic-spec-val", style = "color: #FFB020;", sprintf("%.2f%%", m$brecha_pago * 100))),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Ratio de Inversión:"), span(class = "forensic-spec-val", sprintf("%.1f%%", m$ratio_inversion * 100))),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Balance de Caja Real:"), span(class = "forensic-spec-val", sprintf("%.2f%%", m$balance_caja_real * 100))),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "LOF Score (Local):"), span(class = "forensic-spec-val", style = "color: #FF3366;", sprintf("%.2f", m$lof_score))),
        div(class = "forensic-spec-row", span(class = "forensic-spec-label", "Log-Densidad GMM:"), span(class = "forensic-spec-val", style = "color: #4DB8FF;", sprintf("%.2f", m$gmm_log_dens)))
      )
    )
  })
  
  output$plot_benford_drilldown <- renderPlotly({
    m <- mun_seleccionado()
    req(nrow(m) > 0)
    
    df_b <- benford_data$digitos %>% filter(entidad == m$entidad)
    req(nrow(df_b) > 0)
    
    res_b <- benford_data$resumen_benford %>% filter(entidad == m$entidad)
    p_str <- if (res_b$p_valor < 0.001) "< 0.001" else sprintf("%.4f", res_b$p_valor)
    
    p <- plot_ly(df_b, x = ~factor(digito)) %>%
      add_bars(y = ~observado * 100, name = "Observado",
               marker = list(color = COLOR_ALERT, line = list(color = "#FFFFFF", width = 1)),
               hovertemplate = "Dígito %{x}<br>Observado: %{y:.1f}%<extra></extra>") %>%
      add_lines(y = ~teorico * 100, name = "Curva Benford Teórica",
                line = list(color = COLOR_PRIMARY, width = 3),
                hovertemplate = "Dígito %{x}<br>Teórico: %{y:.1f}%<extra></extra>") %>%
      layout(
        title = list(
          text = sprintf("MAD: %.4f (%s) | Chi²: %.2f (p = %s)",
                         res_b$MAD, res_b$conformidad_benford, res_b$chi_cuadrado, p_str),
          font = list(size = 11, color = COLOR_TEXT_SEC)
        ),
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        xaxis = list(title = "Primer Dígito Significativo (1-9)", gridcolor = COLOR_GRID),
        yaxis = list(title = "Frecuencia Relativa (%)", gridcolor = COLOR_GRID),
        legend = list(orientation = "h", x = 0.2, y = 1.15, font = list(color = COLOR_TEXT_PRI)),
        margin = list(t = 50, r = 20, b = 40, l = 60)
      ) %>%
      clean_plotly_config()
    p
  })
  
  output$plot_objeto_drilldown <- renderPlotly({
    m <- mun_seleccionado()
    req(nrow(m) > 0)
    
    df_p <- benford_data$partidas_consenso %>%
      filter(entidad == m$entidad) %>%
      mutate(
        grupo_nombre = case_when(
          objeto_grupo == 10000 ~ "10000 Serv. Personales",
          objeto_grupo == 20000 ~ "20000 Serv. No Personales",
          objeto_grupo == 30000 ~ "30000 Materiales y Suministros",
          objeto_grupo == 40000 ~ "40000 Activos Reales",
          TRUE                  ~ "Otros Grupos"
        )
      ) %>%
      group_by(grupo_nombre) %>%
      summarise(monto = sum(devengado), .groups = "drop") %>%
      mutate(pct = monto / sum(monto) * 100) %>%
      arrange(desc(monto))
    
    p <- plot_ly(df_p, labels = ~grupo_nombre, values = ~monto, type = "pie",
                 hole = 0.45,
                 textposition = "inside",
                 textinfo = "label+percent",
                 marker = list(
                    colors = c("#3B82F6", COLOR_ALERT, COLOR_WARN, COLOR_NORMAL, "#6B7280"),
                    line = list(color = "#000000", width = 1.5)
                 )) %>%
      layout(
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        legend = list(orientation = "h", x = 0.0, y = -0.15, font = list(color = COLOR_TEXT_PRI, size = 9)),
        margin = list(t = 20, r = 20, b = 40, l = 20)
      ) %>%
      clean_plotly_config()
    p
  })
  
  output$plot_caja_drilldown <- renderPlotly({
    m <- mun_seleccionado()
    req(nrow(m) > 0)
    
    df_caja <- data.frame(
      Concepto = c("Ingresos Percibidos", "Gastos Devengados", "Gastos Pagados"),
      Monto = c(m$ingreso_percibido, m$gasto_devengado, m$gasto_pagado),
      Color = c(COLOR_PRIMARY, COLOR_WARN, COLOR_NORMAL),
      stringsAsFactors = FALSE
    )
    
    p <- plot_ly(df_caja, x = ~Concepto, y = ~Monto / 1e6, type = "bar",
                 marker = list(color = ~Color, line = list(color = "#FFFFFF", width = 1)),
                 hovertemplate = "%{x}<br>Bs. %{y:.2f}M<extra></extra>") %>%
      layout(
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        xaxis = list(title = "", gridcolor = "transparent"),
        yaxis = list(title = "Millones de Bs. (Bs. M)", gridcolor = COLOR_GRID),
        showlegend = FALSE,
        margin = list(t = 20, r = 20, b = 40, l = 60)
      ) %>%
      clean_plotly_config()
    p
  })
}

# ------------------------------------------------------------------------------
# 4. Lanzamiento de la Aplicación
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
