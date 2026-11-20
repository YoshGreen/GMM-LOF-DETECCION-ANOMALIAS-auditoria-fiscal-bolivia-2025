# ==============================================================================
# PROYECTO: Auditoría Forense Presupuestaria de Municipios de Bolivia (Gestión 2025)
# APLICACIÓN: dashboard/app.R
# TECNOLOGÍAS: R Shiny + bslib (Bootstrap 5) + plotly + DT + custom.css
# SISTEMA VISUAL: Dark Telemetry / Obsidian (estricto DESIGN.md)
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

# ------------------------------------------------------------------------------
# 2. Interfaz de Usuario (UI) con bslib
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = div(
    span("AUDITORÍA FISCAL BOLIVIA 2025", style = "color: #00FF88; font-weight: 700;"),
    span("| TELEMETRÍA FORENSE GAM", style = "color: #9CA3AF; font-size: 0.82rem; margin-left: 0.5rem;")
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
        div(class = "kpi-title", "Gasto Vigente Consolidado"),
        div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_vigente))),
        div(class = "kpi-sub", "335 Gobiernos Autónomos Municipales")
      ),
      div(
        class = "kpi-card kpi-primary",
        div(class = "kpi-title", "Gasto Devengado (Ejecución)"),
        div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_devengado))),
        div(class = "kpi-sub", sprintf("Tasa Global: %.2f%% del presupuesto", sum(df_gam$gasto_devengado) / sum(df_gam$gasto_vigente) * 100))
      ),
      div(
        class = "kpi-card kpi-warn",
        div(class = "kpi-title", "Deuda Flotante Exigible"),
        div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_devengado) - sum(df_gam$gasto_pagado))),
        div(class = "kpi-sub", sprintf("Compromisos pendientes de desembolso"))
      ),
      div(
        class = "kpi-card kpi-alert",
        div(class = "kpi-title", "Municipios en Alerta Crítica"),
        div(class = "kpi-value", "12 GAMs"),
        div(class = "kpi-sub", "Consenso GMM + LOF de alta prioridad")
      )
    ),
    
    # Gráficos de Comparativa Departamental y Línea Base Univariada
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          div(
            div("Presupuesto vs. Ejecución por Departamento"),
            div(class = "card-subtitle", "Compara el gasto vigente (capacidad asignada) contra el devengado real por departamento")
          )
        ),
        card_body(plotlyOutput("plot_depto_comparativa", height = "360px"))
      ),
      card(
        card_header(
          div(
            div("Línea Base Univariada y Sesgo de Escala"),
            div(class = "card-subtitle", "Demuestra cómo evaluar montos brutos sólo señala grandes capitales, mientras que los ratios revelan fallas de ejecución")
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
        hr(style = "border-color: #1F2937;"),
        div(
          style = "font-size: 0.78rem; color: #9CA3AF;",
          p(strong("Guía del Auditor:")),
          p("• GMM ajusta 4 regímenes gaussianos multivariantes en el espacio de 9 ratios."),
          p("• Elipses azules delimitan el 85% de densidad de cada régimen."),
          p("• Ajuste el slider minPts para verificar la estabilidad de los atípicos locales.")
        )
      ),
      card(
        card_header(
          div(
            div("Espacio Latente PCA: Regímenes GMM y Factores Locales LOF"),
            div(class = "card-subtitle", "Los puntos anómalos destacan en carmesí (consenso) y ámbar (local). Pase el cursor para inspeccionar el diagnóstico detallado.")
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
        hr(style = "border-color: #1F2937;"),
        div(
          style = "font-size: 0.76rem; color: #9CA3AF;",
          p(strong("Criterio de Priorización:")),
          p("• ", span("Consenso GMM + LOF:", style = "color: #FF3366; font-weight: 600;"), " Municipios atípicos a nivel de densidad global y vecindad local."),
          p("• ", span("Solo LOF:", style = "color: #FFB020; font-weight: 600;"), " Distorsión respecto a sus pares similares."),
          p("• ", span("Solo GMM:", style = "color: #4DB8FF; font-weight: 600;"), " Combinación de ratios atípica en la mezcla global.")
        )
      ),
      card(
        card_header(
          div(
            div("Matriz Forense de Cuadrantes: Log-Densidad GMM vs. Score LOF"),
            div(class = "card-subtitle", "Cuadrante superior izquierdo (LOF > 1.5 y Log-Densidad < -14.5) concentra el consenso de riesgo crítico")
          )
        ),
        card_body(plotlyOutput("plot_cuadrantes_consenso", height = "320px"))
      ),
      card(
        card_header(
          div(
            div("Padrón Municipal Auditado (335 GAMs)"),
            div(class = "card-subtitle", "Tabla interactiva, buscable y exportable con ratios estandarizados y clasificación forense")
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
        width = 300,
        title = "Expediente Municipal",
        selectInput("selector_municipio_forense", "Seleccionar GAM Prioritario:",
                    choices = opciones_consenso,
                    selected = opciones_consenso[grep("Caquiaviri", names(opciones_consenso))[1]]),
        hr(style = "border-color: #1F2937;"),
        uiOutput("panel_ficha_forense")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(
            div(
              div("Prueba de Ley de Benford (Primer Dígito)"),
              div(class = "card-subtitle", "Distribución de dígitos observada (carmesí) vs. teórica de Benford (azul). Desviaciones anómalas indican alteración humana o fraccionamiento.")
            )
          ),
          card_body(plotlyOutput("plot_benford_drilldown", height = "360px"))
        ),
        card(
          card_header(
            div(
              div("Composición del Gasto Devengado por Objeto"),
              div(class = "card-subtitle", "Desglose por grupos de partida (Servicios Personales 10000, No Personales 20000, Materiales 30000, Activos 40000)")
            )
          ),
          card_body(plotlyOutput("plot_objeto_drilldown", height = "360px"))
        )
      ),
      card(
        card_header(
          div(
            div("Diagnóstico de Liquidez y Brecha Presupuestaria (Caja Real vs. Deuda)"),
            div(class = "card-subtitle", "Comparativa de Ingresos Percibidos, Gastos Devengados y Gastos Efectivamente Pagados")
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
               marker = list(color = "#1F2937", line = list(color = "#374151", width = 1)),
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
      )
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
      )
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
    )
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
      )
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
    
    tagList(
      div(
        style = "background-color: #161B22; border: 1px solid #1F2937; padding: 0.85rem; border-radius: 6px; font-size: 0.8rem;",
        p(strong("FICHA TÉCNICA FORENSE"), style = "color: #00FF88; font-size: 0.85rem; margin-bottom: 0.5rem;"),
        p(strong("Código Entidad: "), span(m$entidad, class = "num-cell")),
        p(strong("Departamento: "), m$departamento),
        p(strong("Presupuesto: "), span(fmt_bs(m$gasto_vigente), class = "num-cell")),
        p(strong("Tasa Ejecución: "), span(sprintf("%.1f%%", m$tasa_ejecucion_gasto * 100), class = "num-cell")),
        p(strong("Deuda Flotante: "), span(sprintf("%.2f%%", m$brecha_pago * 100), class = "num-cell")),
        p(strong("Balance Caja: "), span(sprintf("%.2f%%", m$balance_caja_real * 100), class = "num-cell")),
        p(strong("LOF Score: "), span(sprintf("%.2f", m$lof_score), class = "num-cell")),
        p(strong("Log-Densidad: "), span(sprintf("%.2f", m$gmm_log_dens), class = "num-cell"))
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
      )
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
      )
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
      )
    p
  })
}

# ------------------------------------------------------------------------------
# 4. Lanzamiento de la Aplicación
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
