# ==============================================================================
# PROYECTO: Auditoría Forense Presupuestaria de Municipios de Bolivia (Gestión 2025)
# APLICACIÓN: dashboard/app.R
# TECNOLOGÍAS: R Shiny + bslib (Bootstrap 5) + plotly + DT + custom.css + ideas.js
# SISTEMA VISUAL: Forensic Telemetry / Evidence Dossier (estricto DESIGN.md)
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

# 12 Municipios de Consenso de Alto Riesgo ordenados por LOF
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

# Grupos y notas forenses para el Tablero de Corcho
dict_corcho <- list(
  "1256" = list(grupo = "paralisis", label = "PARÁLISIS FISCAL", note = "peor ejecución nacional (31,9%) y 16,2% deuda", rot = -2.5),
  "1524" = list(grupo = "tesoreria", label = "DÉFICIT TESORERÍA", note = "superávit artificial (46,7%) por subejecución", rot = 1.8),
  "1536" = list(grupo = "inversion", label = "CERO INVERSIÓN REAL", note = "0,00% asignado a inversión pública", rot = -1.2),
  "1701" = list(grupo = "deuda", label = "DEUDA E ILIQUIDEZ", note = "concentración masiva de deuda flotante (17,8%)", rot = 2.1),
  "1239" = list(grupo = "deuda", label = "DEUDA & BENFORD", note = "violación ley benford (p=0,031) y 16,0% deuda", rot = -3.0),
  "1284" = list(grupo = "inversion", label = "INVERSIÓN ATÍPICA", note = "aislamiento severo en vecindad de gasto", rot = 1.5),
  "1752" = list(grupo = "distorsion", label = "DISTORSIÓN COMBINADA", note = "14,2% partidas duplicadas y desvío de caja", rot = -1.8),
  "1410" = list(grupo = "tesoreria", label = "DÉFICIT TESORERÍA", note = "rigidez extrema en servicios no personales", rot = 2.4),
  "1254" = list(grupo = "inversion", label = "INVERSIÓN ATÍPICA", note = "alta dispersión operativa respecto a pares", rot = -2.0),
  "1756" = list(grupo = "distorsion", label = "DISTORSIÓN COMBINADA", note = "desfase severo entre flujo de caja y devengado", rot = 1.2),
  "1724" = list(grupo = "distorsion", label = "FRACCIONAMIENTO COMPRAS", note = "19,86% de partidas en montos repetidos", rot = -2.7),
  "1521" = list(grupo = "distorsion", label = "FRACCIONAMIENTO COMPRAS", note = "15,32% montos idénticos reiterativos", rot = 2.0)
)

# ------------------------------------------------------------------------------
# 2. Interfaz de Usuario (UI) con Menú Lateral Responsivo
# ------------------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bg = "#000000",
    fg = "#F3F4F6",
    primary = "#00FF88",
    secondary = "#4DB8FF"
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "ideas.js")
  ),
  
  # Cabecera Superior Fija Telemetría
  div(
    class = "top-header-banner",
    div(
      class = "brand-title-wrap",
      div(
        class = "brand-main",
        span("AUDITORÍA FORENSE"),
        span(" GAM 2025", class = "brand-accent")
      ),
      span(class = "brand-sub", "335 municipios auditados · 12 casos sospechosos")
    ),
    div(
      class = "header-actions",
      tags$button(
        id = "btnDrawerOpen",
        class = "action-btn-console btn-menu-drawer",
        onclick = "toggleForensicDrawer()",
        title = "Abrir/Cerrar menú lateral de expedientes",
        "☰ Menú de Expedientes"
      ),
      tags$button(
        id = "btnThemeToggle",
        class = "action-btn-console btn-theme",
        onclick = "toggleDashboardTheme()",
        title = "Alternar entre modo oscuro y modo investigador (corcho)",
        "☀️ Modo Investigador"
      ),
      span(id = "currentSectionBadge", class = "navbar-badge current-section-badge", "1. Panorama Fiscal 2025"),
      span(class = "navbar-badge d-none-mobile", span(class = "pulse-dot"), "MEFP 2025 · DUCKDB"),
      span(class = "navbar-badge d-none-mobile", style = "border-color: rgba(77, 184, 255, 0.35); color: #4DB8FF;", "GMM + LOF")
    )
  ),
  
  # Cortina semitransparente para cerrar el menú desplegable al hacer clic fuera
  div(class = "drawer-backdrop", onclick = "closeForensicDrawer()"),
  
  # Contenedor Principal con Menú Izquierdo Desplegable (Off-Canvas Forensic Drawer)
  div(
    class = "main-wrapper",
    navset_pill_list(
      widths = c(3, 9),
      well = FALSE,
      
      # Cabecera dentro del Menú Desplegable (Off-Canvas)
      nav_item(
        div(
          class = "drawer-header-box",
          div(
            div(class = "drawer-header-title", "EXPEDIENTES FORENSES"),
            div(class = "drawer-header-sub", "335 Municipios · MEFP Bolivia 2025")
          ),
          tags$button(
            type = "button",
            class = "drawer-close-btn",
            onclick = "closeForensicDrawer()",
            title = "Cerrar menú lateral",
            "✕"
          )
        )
      ),
      nav_item(
        div(
          class = "drawer-actions-bar",
          tags$button(
            id = "btnPinDrawer",
            type = "button",
            class = "action-btn-console btn-pin-drawer",
            onclick = "togglePinDrawer()",
            title = "Fijar menú lateral en pantallas de escritorio (>1200px)",
            "📍 Fijar en Pantalla"
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 1: PANORAMA FISCAL 2025
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("1. Panorama Fiscal 2025"), span(class = "pill-badge", "335 GAMs")),
        
        # Tarjetas KPI Principales
        div(
          class = "kpi-container",
          div(
            class = "kpi-card kpi-normal",
            div(class = "kpi-title", span("Gasto Vigente Total"), span(class = "kpi-tag tag-normal", "AUTORIZADO")),
            div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_vigente))),
            div(class = "kpi-sub", "Presupuesto municipal consolidado en Bolivia")
          ),
          div(
            class = "kpi-card kpi-primary",
            div(class = "kpi-title", span("Gasto Devengado"), span(class = "kpi-tag tag-primary", sprintf("%.1f%% Global", sum(df_gam$gasto_devengado) / sum(df_gam$gasto_vigente) * 100))),
            div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_devengado))),
            div(class = "kpi-sub", "Obligaciones contables formalmente ejecutadas")
          ),
          div(
            class = "kpi-card kpi-warn",
            div(class = "kpi-title", span("Deuda Flotante Exigible"), span(class = "kpi-tag tag-warn", "PASIVO")),
            div(class = "kpi-value", fmt_bs(sum(df_gam$gasto_devengado) - sum(df_gam$gasto_pagado))),
            div(class = "kpi-sub", "Devengado pendiente de desembolso en tesorería")
          ),
          div(
            class = "kpi-card kpi-alert",
            div(class = "kpi-title", span("Alerta Forense Crítica"), span(class = "kpi-tag tag-alert", "CONSENSO")),
            div(class = "kpi-value", "12 GAMs"),
            div(class = "kpi-sub", "Doble alerta simultánea (GMM global + LOF local)")
          )
        ),
        
        # Gráficas Panorama
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(
              div(
                div("Presupuesto Vigente vs. Ejecución por Departamento"),
                div(class = "card-subtitle", "Compara el techo autorizado contra el devengado real por departamento")
              )
            ),
            card_body(plotlyOutput("plot_depto_comparativa", height = "360px"))
          ),
          card(
            card_header(
              div(
                div("Línea Base Univariada: Demostración del Sesgo de Escala"),
                div(class = "card-subtitle", "Los montos brutos sólo señalan capitales; los ratios revelan crisis reales de gestión")
              )
            ),
            card_body(
              selectInput("selector_var_univariada", "Seleccionar Métrica de Inspección:",
                          choices = c("Presupuesto Vigente (Monto Bruto)" = "gasto_vigente",
                                      "Tasa de Ejecución de Gasto (%)" = "tasa_ejecucion_gasto",
                                      "Brecha de Pago / Deuda Flotante (%)" = "brecha_pago",
                                      "Ratio de Inversión Pública (%)" = "ratio_inversion",
                                      "Balance de Caja Real (%)" = "balance_caja_real"),
                          selected = "gasto_vigente"),
              plotlyOutput("plot_univariado_interactivo", height = "290px")
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 2: REGÍMENES GMM & DENSIDAD LOF
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("2. Regímenes GMM & LOF"), span(class = "pill-badge", "PCA 2D")),
        layout_sidebar(
          sidebar = sidebar(
            width = 290,
            title = "Controles de Sensibilidad",
            selectInput("filtro_depto_gmm", "Departamento:", choices = departamentos_disponibles, selected = "TODOS"),
            sliderInput("slider_minpts", "Parámetro minPts (LOF):", min = 10, max = 30, value = 20, step = 5),
            sliderInput("slider_umbral_lof", "Umbral de Alerta LOF:", min = 1.2, max = 2.2, value = 1.5, step = 0.05),
            hr(style = "border-color: rgba(255, 255, 255, 0.08);"),
            div(
              style = "font-size: 0.77rem; color: #9CA3AF;",
              p(strong("Guía de Telemetría:")),
              p("• GMM clasifica 4 regímenes gaussianos multivariantes en 9 dimensiones fiscales."),
              p("• LOF calcula el factor de densidad local frente a los k vecinos más próximos."),
              p("• Los puntos carmesí cumplen la doble condición de baja densidad y aislamiento.")
            )
          ),
          card(
            card_header(
              div(
                div("Espacio Latente PCA: Regímenes GMM y Densidad LOF"),
                div(class = "card-subtitle", "Los casos críticos destacan en carmesí. Pase el cursor para ver el diagnóstico pericial.")
              )
            ),
            card_body(plotlyOutput("plot_pca_gmm_interactivo", height = "520px"))
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 3: CONSENSO DE ANOMALÍAS & PADRÓN
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("3. Consenso de Anomalías"), span(class = "pill-badge", "Padrón DT")),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            title = "Filtros del Padrón",
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
                div(class = "card-subtitle", "El cuadrante superior izquierdo (LOF > 1.5 y Log-Densidad < -14.5) delimita la zona de alta prioridad")
              )
            ),
            card_body(plotlyOutput("plot_cuadrantes_consenso", height = "320px"))
          ),
          card(
            card_header(
              div(
                div("Padrón Municipal Auditado (335 GAMs)"),
                div(class = "card-subtitle", "Tabla interactiva y exportable con los 9 ratios estandarizados y clasificación de riesgo")
              )
            ),
            card_body(DTOutput("tabla_consenso_interactiva"))
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 4: EXPEDIENTE DE EVIDENCIAS (DRILL-DOWN)
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("4. Expediente de Evidencias"), span(class = "pill-badge", "Benford")),
        layout_sidebar(
          sidebar = sidebar(
            width = 310,
            title = "Expediente Pericial",
            selectInput("selector_municipio_forense", "Seleccionar Caso Prioritario:",
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
                  div(class = "card-subtitle", "Barras observadas (carmesí) vs. teórica (azul). Desvíos anómalos indican manipulación o fraccionamiento.")
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
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 5: TABLERO DE INVESTIGACIÓN (CORCHO CRIMINALÍSTICO)
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("5. Tablero de Investigación"), span(class = "pill-badge", style = "background: rgba(196, 22, 28, 0.2); color: #FF4A3D;", "Corcho")),
        div(
          style = "margin-bottom: 0.85rem;",
          h4("TABLERO CRIMINALÍSTICO DE CASOS CONECTADOS", style = "font-family: var(--font-oswald); letter-spacing: 0.04em; margin-bottom: 0.2rem;"),
          p(style = "color: #9CA3AF; font-size: 0.82rem; margin: 0;",
            "Fichas periciales de los 12 municipios de alto riesgo. Los hilos rojos conectan expedientes con el mismo patrón forense. Pasa el cursor sobre una tarjeta para aislar su red de implicación.")
        ),
        uiOutput("cork_board_ui")
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 6: TABLERO SPLIT-FLAP & CINTA DE SUMADORA
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("6. Telemetría Split-Flap"), span(class = "pill-badge", "Flap & Cinta")),
        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header(
              div(
                style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 0.5rem;",
                div(
                  div("Tablero de Alertas Split-Flap (Caracteres Mecánicos)"),
                  div(class = "card-subtitle", "Visualización de telemetría aeroportuaria/forense con los 12 municipios priorizados")
                ),
                tags$button(
                  class = "receipt-btn",
                  style = "background: #E8B400; color: #000; margin: 0; padding: 6px 14px; font-size: 0.75rem; white-space: nowrap;",
                  onclick = "runFlapAnimation()",
                  "⟲ ACTUALIZAR CARACTERES FLAP"
                )
              )
            ),
            card_body(
              uiOutput("split_flap_ui")
            )
          ),
          card(
            card_header(
              div(
                style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 0.5rem;",
                div(
                  div("Cinta de Sumadora Contable"),
                  div(class = "card-subtitle", "Impresión mecánica secuencial de la auditoría final")
                ),
                tags$button(
                  class = "receipt-btn",
                  style = "margin: 0; padding: 6px 14px; font-size: 0.75rem; white-space: nowrap;",
                  onclick = "printReceiptSequence()",
                  "⟲ REIMPRIMIR CINTA"
                )
              )
            ),
            card_body(
              div(
                class = "receipt-wrapper",
                div(class = "receipt-slot"),
                div(id = "receiptContent", class = "receipt-paper")
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 7: GRÁFICAS COMPARATIVAS AVANZADAS
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("7. Gráficas Comparativas"), span(class = "pill-badge", "3 Modelos")),
        div(
          style = "margin-bottom: 1rem;",
          h4("ANÁLISIS COMPARATIVO MULTIDIMENSIONAL DE RIESGO", style = "font-family: var(--font-oswald); letter-spacing: 0.04em; margin-bottom: 0.2rem;"),
          p(style = "color: #9CA3AF; font-size: 0.82rem; margin: 0;",
            "Gráficas diagnósticas que confrontan directamente variables de comportamiento contable: Inversión vs. Deuda, No Conformidad de Benford y Fraccionamiento de Compras.")
        ),
        layout_columns(
          col_widths = c(12),
          card(
            card_header(
              div(
                div("Comparativa 1: Cuadrantes de Inversión Pública (%) vs. Deuda Flotante (%)"),
                div(class = "card-subtitle", "Los 335 municipios divididos en cuadrantes de riesgo. El cuadrante superior izquierdo concentra los casos de parálisis e insolvencia.")
              )
            ),
            card_body(plotlyOutput("plot_comparativa_inversion_deuda", height = "420px"))
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(
              div(
                div("Comparativa 2: Desviación Benford (MAD) vs. Umbrales de Nigrini"),
                div(class = "card-subtitle", "Los 12 municipios evaluados frente al umbral crítico de No Conformidad (MAD > 0.015)")
              )
            ),
            card_body(plotlyOutput("plot_comparativa_benford_mad", height = "360px"))
          ),
          card(
            card_header(
              div(
                div("Comparativa 3: Tasa de Partidas en Montos Duplicados (%)"),
                div(class = "card-subtitle", "Reincidencia de pagos en importes idénticos. Resalta señales de compras fraccionadas en Boyuibe y Colcha K.")
              )
            ),
            card_body(plotlyOutput("plot_comparativa_duplicados", height = "360px"))
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # PESTAÑA 8: BITÁCORA KDD & EVIDENCIAS PERICIALES
      # ------------------------------------------------------------------------
      nav_panel(
        title = div(span("8. Bitácora KDD & Evidencias"), span(class = "pill-badge", style = "background: rgba(0, 255, 136, 0.2); color: #00FF88;", "KDD")),
        div(
          style = "margin-bottom: 1.25rem;",
          h4("EXPEDIENTE PERICIAL KDD: PROGRESO Y EVIDENCIAS GRÁFICAS", style = "font-family: var(--font-oswald); letter-spacing: 0.04em; margin-bottom: 0.2rem;"),
          p(style = "color: #9CA3AF; font-size: 0.85rem; margin: 0;",
            "Registro visual exhaustivo del proceso de Minería de Datos: desde la depuración microcontable en DuckDB hasta la comprobación empírica del sesgo de escala, modelos multivariantes y pruebas forenses de fraude.")
        ),
        
        # Etapa 1
        div(
          class = "bitacora-step",
          div(
            class = "bitacora-header",
            div(span(class = "bitacora-number", "FASE 1"), span(class = "bitacora-title", "Ingesta y Poda Microcontable en Disco con DuckDB")),
            span(class = "stamp", "0 NULOS · DEPURADO")
          ),
          div(
            class = "bitacora-body",
            p("Se procesaron ", strong("429.753 partidas de gasto"), " y ", strong("14.644 rubros de ingreso"), " oficiales de la gestión 2025 del Ministerio de Economía. Utilizando el motor columnar ", code("DuckDB"), " directamente en disco, se filtraron los ", strong("335 Gobiernos Autónomos Municipales"), " sin colapso de memoria RAM."),
            div(class = "hw", "«DuckDB procesó 408 MB en disco en 1,4 s y transformó 444 mil filas en 9 ratios normalizados.»"),
            div(
              style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem; margin-top: 1rem;",
              div(class = "kpi-card kpi-normal", div(class = "kpi-title", "Registros Iniciales"), div(class = "kpi-value", "444.397"), div(class = "kpi-sub", "Gastos + Ingresos crudos")),
              div(class = "kpi-card kpi-primary", div(class = "kpi-title", "Universo Municipal"), div(class = "kpi-value", "335 GAMs"), div(class = "kpi-sub", "Cobertura 100% de Bolivia")),
              div(class = "kpi-card kpi-warn", div(class = "kpi-title", "Ratios Normalizados"), div(class = "kpi-value", "9 Métricas"), div(class = "kpi-sub", "Invariantes a escala"))
            )
          )
        ),
        
        # Etapa 2
        div(
          class = "bitacora-step",
          div(
            class = "bitacora-header",
            div(span(class = "bitacora-number", "FASE 2"), span(class = "bitacora-title", "Demostración Científica del Sesgo de Escala (Tukey 1.5 IQR)")),
            span(class = "stamp", "SESGO DEMOSTRADO")
          ),
          div(
            class = "bitacora-body",
            p("Comprobación empírica del sesgo que afecta a las auditorías tradicionales: al evaluar el presupuesto bruto, únicamente las capitales aparecen como 'atípicas' debido a su volumen poblacional. Al contrastar con la tasa de ejecución y ratios relativos, la realidad se invierte."),
            div(class = "hw", "«El monto bruto solo señala capitales; los ratios relativos destapan las verdaderas parálisis municipales.»"),
            div(
              class = "bitacora-gallery",
              div(
                class = "bitacora-card-img",
                tags$img(src = "figuras/fig_01_sesgo_escala_univariado.png", alt = "Sesgo de Escala"),
                div(class = "bitacora-card-caption", strong("Figura 1: Sesgo de Escala."), " En la izquierda (monto bruto), sólo Santa Cruz, La Paz y Cochabamba son atípicos. En la derecha (tasa de ejecución), las capitales son normales y se evidencian las fallas de gestión en Chulumani y Mojinete.")
              ),
              div(
                class = "bitacora-card-img",
                tags$img(src = "figuras/fig_02_boxplot_ratios_tukey.png", alt = "Boxplots Tukey Ratios"),
                div(class = "bitacora-card-caption", strong("Figura 2: Boxplots de Tukey (1.5 IQR)."), " Distribución empírica de los ratios fiscales normalizados, permitiendo aislar atípicos genuinos sin contaminación por tamaño.")
              )
            )
          )
        ),
        
        # Etapa 3
        div(
          class = "bitacora-step",
          div(
            class = "bitacora-header",
            div(span(class = "bitacora-number", "FASE 3"), span(class = "bitacora-title", "Modelado No Supervisado Multivariante: GMM (EM + BIC) y LOF")),
            span(class = "stamp", "CONSENSO GMM + LOF")
          ),
          div(
            class = "bitacora-body",
            p("Ajuste probabilístico de Mezclas Gaussianas (GMM) con optimización bayesiana BIC y Factor de Valor Atípico Local (LOF) con matriz de sensibilidad multiescala."),
            div(class = "hw", "«GMM aisló anomalías de densidad global mientras LOF detectó atipicidad local en vecindad de 20 municipios.»"),
            div(
              class = "bitacora-gallery",
              div(
                class = "bitacora-card-img",
                tags$img(src = "figuras/fig_03_gmm_curva_bic.png", alt = "Curva BIC GMM"),
                div(class = "bitacora-card-caption", strong("Figura 3: Curvas BIC de Selección GMM."), " Evaluación de 14 familias paramétricas y G = 1..9 componentes. El modelo VVE con G = 4 componentes maximizó el criterio BIC (-5.284,80).")
              ),
              div(
                class = "bitacora-card-img",
                tags$img(src = "figuras/fig_05_sensibilidad_lof.png", alt = "Sensibilidad LOF"),
                div(class = "bitacora-card-caption", strong("Figura 5: Matriz de Sensibilidad LOF (k in {10, 15, 20, 30})."), " Correlaciones r > 0.94 que demuestran la robustez matemática e invariancia de los 12 municipios priorizados.")
              )
            )
          )
        ),
        
        # Etapa 4
        div(
          class = "bitacora-step",
          div(
            class = "bitacora-header",
            div(span(class = "bitacora-number", "FASE 4"), span(class = "bitacora-title", "Auditoría Forense de Partidas Crudas: Ley de Benford y Duplicados")),
            span(class = "stamp", "VIOLACIÓN CONFIRMADA")
          ),
          div(
            class = "bitacora-body",
            p("Inspección pericial de las 6.387 transacciones individuales de los 12 municipios en consenso. Se contrastó la ley natural del primer dígito y se rastrearon patrones de compras fraccionadas repetidas."),
            div(class = "hw", "«Caquiaviri violó la Ley de Benford (p = 0,031) y Boyuibe concentró 19,86% de sus pagos en montos idénticos.»"),
            div(
              class = "bitacora-gallery",
              div(
                class = "bitacora-card-img",
                tags$img(src = "figuras/fig_06_benford_caquiaviri.png", alt = "Benford Caquiaviri"),
                div(class = "bitacora-card-caption", strong("Figura 6: Desvío de Benford en Caquiaviri."), " Déficit anómalo en dígito 1 y picos artificiales en dígitos 3, 4 y 5 (Chi² = 16,91, p = 0,031, MAD = 0,0241).")
              ),
              div(
                class = "bitacora-card-img",
                tags$img(src = "figuras/fig_07_composicion_gasto_consenso.png", alt = "Composición Gasto Consenso"),
                div(class = "bitacora-card-caption", strong("Figura 7: Composición de Gasto por Objeto."), " Evidencia de 0% de inversión en Tahua y anomalías severas de concentración en servicios no personales.")
              )
            )
          )
        )
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
    
    # Normales
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
    
    # Solo GMM
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
    
    # Solo LOF
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
    
    # Consenso GMM + LOF
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
    diag <- if (!is.null(dict_corcho[[ent_code]])) {
      dict_corcho[[ent_code]]
    } else {
      list(label = "ANOMALÍA MULTIVARIANTE", note = "aislamiento conjunto en densidad GMM y vecindad LOF")
    }
    
    tagList(
      div(
        class = "forensic-alert-box",
        div(class = "forensic-alert-title", "ALERTA FORENSE PRINCIPAL"),
        p(strong(diag$label), style = "color: #F3F4F6; margin-bottom: 0.25rem; font-size: 0.82rem;"),
        p(class = "forensic-alert-desc", diag$note)
      ),
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
  
  # --- PESTAÑA 5: TABLERO DE CORCHO CRIMINALÍSTICO (Ideas/) ---
  
  output$cork_board_ui <- renderUI({
    # Fichas generadas a partir de df_consenso_gams
    cards_ui <- lapply(seq_len(nrow(df_consenso_gams)), function(i) {
      r <- df_consenso_gams[i, ]
      code_str <- as.character(r$entidad)
      meta <- if (!is.null(dict_corcho[[code_str]])) dict_corcho[[code_str]] else list(grupo = "general", label = "ATÍPICO", note = "anomalía multivariante", rot = 0)
      
      div(
        class = "cork-card",
        `data-group` = meta$grupo,
        style = sprintf("--r: %.1fdeg;", meta$rot),
        span(class = "card-lof", sprintf("%.2f", r$lof_score)),
        tags$h3(r$municipio),
        tags$small(toupper(r$departamento)),
        div(class = "handwritten-note", meta$note),
        tags$small(sprintf("ejec %.1f%% · deuda %.1f%%", r$tasa_ejecucion_gasto * 100, r$brecha_pago * 100))
      )
    })
    
    tagList(
      div(
        class = "cork-container",
        id = "corkBoard",
        div(
          class = "cork-pins-header",
          span(class = "cork-sticky-note", style = "--r: -2deg;", "Deuda e Iliquidez"),
          span(class = "cork-sticky-note", style = "--r: 1.5deg;", "Déficit de Tesorería"),
          span(class = "cork-sticky-note", style = "--r: -1deg;", "Cero Inversión Real"),
          span(class = "cork-sticky-note", style = "--r: 2deg;", "Fraccionamiento de Compras"),
          span(class = "cork-sticky-note", style = "--r: -1.5deg;", "Parálisis Fiscal")
        ),
        tags$svg(id = "corkSvg", class = "cork-strings"),
        div(class = "cork-grid", cards_ui)
      ),
      div(
        style = "margin-top: 0.75rem; text-align: right;",
        tags$button(
          class = "receipt-btn",
          style = "margin: 0 0 0 auto; display: inline-block; background: #5B3A1E;",
          onclick = "renderCorkStrings(); initCorkEvents();",
          "⟲ REAJUSTAR HILOS DEL CORCHO"
        )
      )
    )
  })
  
  # --- PESTAÑA 6: SPLIT-FLAP BOARD (Ideas/) ---
  
  output$split_flap_ui <- renderUI({
    # Función auxiliar para empaquetar casillas mecánicas dentro de una columna alineada
    render_flap_col <- function(text, col_class, cell_class = "") {
      chars <- strsplit(text, "")[[1]]
      cells <- lapply(chars, function(ch) {
        extra_cls <- if (ch != " ") cell_class else ""
        tags$span(
          class = paste("flap-cell", extra_cls),
          `data-char` = ch,
          ch
        )
      })
      tags$div(class = paste("flap-col", col_class), cells)
    }
    
    rows_ui <- lapply(seq_len(nrow(df_consenso_gams)), function(i) {
      r <- df_consenso_gams[i, ]
      
      # 1. Nombre de Municipio limpio a 13 caracteres exactos
      clean_mun <- toupper(r$municipio)
      clean_mun <- gsub('["“”]', '', clean_mun)
      clean_mun <- gsub(" \\(.*\\)", "", clean_mun)
      clean_mun <- substr(trimws(clean_mun), 1, 13)
      mun_padded <- sprintf("%-13s", clean_mun)
      
      # 2. Código oficial de Departamento (3 letras + 1 espacio = 4 caracteres)
      dep_raw <- toupper(r$departamento)
      dep_code <- if (grepl("PAZ", dep_raw)) {
        "LPZ "
      } else if (grepl("POTO", dep_raw)) {
        "POT "
      } else if (grepl("CRUZ", dep_raw)) {
        "SCZ "
      } else if (grepl("ORU", dep_raw)) {
        "ORU "
      } else if (grepl("COCH", dep_raw)) {
        "CBB "
      } else if (grepl("CHU", dep_raw)) {
        "CHQ "
      } else if (grepl("TAR", dep_raw)) {
        "TRJ "
      } else if (grepl("BEN", dep_raw)) {
        "BEN "
      } else if (grepl("PAN", dep_raw)) {
        "PAN "
      } else {
        sprintf("%-4s", substr(dep_raw, 1, 4))
      }
      
      # 3. Factor LOF (4 caracteres)
      lof_padded <- sprintf("%4.2f", r$lof_score)
      
      # 4. Tasa de Ejecución de Gasto (5 caracteres)
      ejec_padded <- sprintf("%5.1f", r$tasa_ejecucion_gasto * 100)
      
      # 5. Brecha de Pago / Deuda Flotante (5 caracteres)
      deuda_padded <- sprintf("%5.1f", r$brecha_pago * 100)
      
      # 6. Dictamen de Auditoría Forense (9 caracteres)
      estado_padded <- if (r$lof_score >= 1.9) "CRÍTICO  " else "OBSERVADO"
      estado_cls <- if (r$lof_score >= 1.9) "cell-red" else "cell-yellow"
      
      tags$div(
        class = "flap-row",
        `data-row-idx` = i - 1,
        onclick = sprintf("runFlapAnimation(%d)", i - 1),
        title = "Haz clic para reanimar este municipio",
        render_flap_col(mun_padded, "col-gam", ""),
        render_flap_col(dep_code, "col-depto", ""),
        render_flap_col(lof_padded, "col-lof", "cell-yellow"),
        render_flap_col(ejec_padded, "col-ejec", ""),
        render_flap_col(deuda_padded, "col-deuda", ""),
        render_flap_col(estado_padded, "col-estado", estado_cls)
      )
    })
    
    tags$div(
      class = "flap-board",
      tags$div(
        class = "flap-mobile-hint d-md-none",
        tags$span("⇄ Desliza horizontalmente para ver todas las métricas en móviles")
      ),
      tags$div(
        class = "flap-header",
        tags$div(class = "flap-col col-gam", "MUNICIPIO (GAM)"),
        tags$div(class = "flap-col col-depto", "DEPTO"),
        tags$div(class = "flap-col col-lof", "LOF"),
        tags$div(class = "flap-col col-ejec", "EJEC %"),
        tags$div(class = "flap-col col-deuda", "DEUDA %"),
        tags$div(class = "flap-col col-estado", "DICTAMEN AUDITORÍA")
      ),
      rows_ui
    )
  })
  # --- PESTAÑA 7: GRÁFICAS COMPARATIVAS AVANZADAS ---
  
  output$plot_comparativa_inversion_deuda <- renderPlotly({
    p <- plot_ly() %>%
      # Puntos de los 335 GAMs
      add_trace(
        data = df_gam,
        x = ~ratio_inversion * 100,
        y = ~brecha_pago * 100,
        type = "scatter",
        mode = "markers",
        color = ~categoria_consenso,
        colors = c(COLOR_ALERT, COLOR_NORMAL, COLOR_PRIMARY, COLOR_WARN),
        marker = list(size = 8, opacity = 0.75),
        text = ~paste0("<b>", municipio, " (", departamento, ")</b><br>",
                       "Inversión Pública: ", sprintf("%.1f%%", ratio_inversion * 100), "<br>",
                       "Deuda Flotante: ", sprintf("%.1f%%", brecha_pago * 100), "<br>",
                       "LOF Score: ", sprintf("%.2f", lof_score)),
        hoverinfo = "text"
      ) %>%
      # Línea umbral deuda flotante 10%
      add_segments(x = 0, xend = 85, y = 10, yend = 10,
                   line = list(color = COLOR_WARN, dash = "dash", width = 1),
                   showlegend = FALSE, inherit = FALSE) %>%
      # Línea umbral inversión 15%
      add_segments(x = 15, xend = 15, y = -5, yend = 35,
                   line = list(color = COLOR_PRIMARY, dash = "dash", width = 1),
                   showlegend = FALSE, inherit = FALSE) %>%
      # Anotación Zona Crítica
      add_annotations(
        x = 5, y = 25,
        text = "🚨 ZONA CRÍTICA<br>(Baja Inversión & Alta Deuda)",
        showarrow = FALSE,
        font = list(color = COLOR_ALERT, size = 11, family = "Inter")
      ) %>%
      # Anotación Zona Eficiente
      add_annotations(
        x = 60, y = 2,
        text = "✓ ZONA DE SALUD FISCAL<br>(Alta Inversión & Baja Deuda)",
        showarrow = FALSE,
        font = list(color = COLOR_NORMAL, size = 11, family = "Inter")
      ) %>%
      layout(
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        xaxis = list(title = "Ratio de Inversión Pública (% del Gasto Devengado)", gridcolor = COLOR_GRID),
        yaxis = list(title = "Brecha de Pago / Deuda Flotante (% Exigible)", gridcolor = COLOR_GRID),
        legend = list(orientation = "h", x = 0.1, y = 1.15, font = list(color = COLOR_TEXT_PRI)),
        margin = list(t = 40, r = 20, b = 40, l = 60)
      ) %>%
      clean_plotly_config()
    p
  })
  
  output$plot_comparativa_benford_mad <- renderPlotly({
    df_bf <- benford_data$resumen_benford %>%
      arrange(desc(MAD))
    
    p <- plot_ly(df_bf, y = ~reorder(municipio, MAD), x = ~MAD, type = "bar", orientation = "h",
                 marker = list(
                   color = ~ifelse(MAD > 0.015, COLOR_ALERT, ifelse(MAD > 0.012, COLOR_WARN, COLOR_NORMAL)),
                   line = list(color = "#FFFFFF", width = 0.8)
                 ),
                 hovertemplate = "<b>%{y}</b><br>MAD: %{x:.4f}<extra></extra>") %>%
      add_annotations(
        x = 0.0155, y = 10,
        text = "Umbral Crítico Nigrini (0,015)",
        showarrow = FALSE,
        font = list(color = COLOR_ALERT, size = 10)
      ) %>%
      layout(
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        shapes = list(
          list(type = "line", x0 = 0.015, x1 = 0.015, y0 = 0, y1 = 1, yref = "paper",
               line = list(color = COLOR_ALERT, dash = "dash", width = 1.5))
        ),
        xaxis = list(title = "Mean Absolute Deviation (MAD)", gridcolor = COLOR_GRID),
        yaxis = list(title = "", gridcolor = "transparent"),
        margin = list(t = 20, r = 20, b = 40, l = 160)
      ) %>%
      clean_plotly_config()
    p
  })
  
  output$plot_comparativa_duplicados <- renderPlotly({
    df_dup <- benford_data$resumen_benford %>%
      arrange(desc(pct_duplicados))
    
    p <- plot_ly(df_dup, y = ~reorder(municipio, pct_duplicados), x = ~pct_duplicados, type = "bar", orientation = "h",
                 marker = list(
                   color = ~ifelse(pct_duplicados > 15, COLOR_ALERT, ifelse(pct_duplicados > 10, COLOR_WARN, COLOR_PRIMARY)),
                   line = list(color = "#FFFFFF", width = 0.8)
                 ),
                 hovertemplate = "<b>%{y}</b><br>Duplicados: %{x:.1f}%<extra></extra>") %>%
      layout(
        plot_bgcolor = COLOR_BG,
        paper_bgcolor = COLOR_BG,
        font = list(color = COLOR_TEXT_PRI, family = "Inter"),
        shapes = list(
          list(type = "line", x0 = 10, x1 = 10, y0 = 0, y1 = 1, yref = "paper",
               line = list(color = COLOR_WARN, dash = "dash", width = 1.2))
        ),
        xaxis = list(title = "% de Partidas Devengadas en Montos Duplicados", gridcolor = COLOR_GRID),
        yaxis = list(title = "", gridcolor = "transparent"),
        margin = list(t = 20, r = 20, b = 40, l = 160)
      ) %>%
      clean_plotly_config()
    p
  })
}

# ------------------------------------------------------------------------------
# 4. Lanzamiento de la Aplicación
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
