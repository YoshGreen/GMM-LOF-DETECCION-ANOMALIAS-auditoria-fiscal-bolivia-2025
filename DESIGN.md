# DESIGN.md — Sistema Visual Forense (Dashboard & Gráficas)

## 1. Dirección Estética (Anti-AI-Slop)
- **Concepto**: Terminal de Auditoría Forense Fiscal ("Dark Telemetry"). Sobrio, técnico, alta densidad informativa, cero adornos decorativos innecesarios.
- **Prohibiciones estrictas**:
  - NO usar degradados morados/índigo genéricos ni sombras difusas excesivas (`box-shadow` pesados).
  - NO usar paletas arcoíris por defecto de ggplot2 o Plotly.
  - NO sobrecargar la vista inicial con más de 4 tarjetas KPI en una fila.
  - NO mezclar fuentes redondeadas informales.

## 2. Paleta de Colores Semántica
### Para Dashboard Interactivo (Dark Mode Forense)
- **Fondo principal (Canvas)**: `#000000` (Negro puro)
- **Superficie de tarjetas y paneles**: `#0D1117` (Obsidiana)
- **Bordes sutiles (1px)**: `#1F2937`
- **Texto primario**: `#F3F4F6`
- **Texto secundario / ejes / etiquetas**: `#9CA3AF`
- **Normal / Salud fiscal / Base**: `#00FF88` (Verde neón de alto contraste)
- **Regímenes GMM / Tendencia secundaria / Ingresos**: `#4DB8FF` (Azul eléctrico)
- **Alerta Local (Solo LOF) / Advertencia**: `#FFB020` (Ámbar técnico)
- **Anomalía Crítica (Consenso GMM + LOF / Violación Benford)**: `#FF3366` (Carmesí alerta)

### Para Figuras Exportadas a LaTeX (`informe/figuras/`)
- Mantener **identidad cromática dual**:
  - Por defecto para el informe PDF: fondo `#000000` con rejilla mínima `#1A222D` y los mismos colores semánticos (`#00FF88`, `#4DB8FF`, `#FFB020`, `#FF3366`), incluyendo una bandera `THEME_MODE <- "dark"` / `"print"` en R por si se necesita exportar una versión con fondo blanco para impresión.

## 3. Tipografía y Números
- **Títulos e interfaz**: `'Inter', 'Segoe UI', system-ui, sans-serif` (peso 600 en encabezados, `letter-spacing: -0.02em`).
- **Cifras monetarias (Bs.), códigos de entidad, scores LOF y tablas**: `'JetBrains Mono', 'IBM Plex Mono', monospace` con `font-variant-numeric: tabular-nums` obligatorio para alineación vertical perfecta.
- **Formato numérico boliviano**: separar miles de forma consistente en todo el dashboard (ej. `Bs 12.450.320`) y porcentajes con 1 o 2 decimales.

## 4. Reglas para Gráficas Interactivas (`plotly`) y Estáticas (`ggplot2`)
- **Tooltips informativos**: En `plotly`, cada punto debe mostrar al pasar el cursor: Nombre del GAM, Departamento, Presupuesto Vigente (Bs.), Tasa de Ejecución (%), Score LOF, Log-Densidad GMM y Categoría de Consenso.
- **Jerarquía de opacidad**: Los puntos `Normal` deben renderizarse con opacidad `0.45` y tamaño menor (`5px`) para que las anomalías (`Consenso GMM + LOF`, opacidad `1.0`, tamaño `9px`, borde claro) destaquen de inmediato sin ruido visual.
- **Rejillas**: Líneas guía muy tenues (`#1A222D`), sin caja completa cerrada innecesaria, márgenes respirables (`margin = list(t = 50, r = 30, b = 50, l = 60)`).
- **Tablas (`DT`)**: Fila compacta, cabecera fija oscura, resaltado condicional solo en la columna de categoría de anomalía (badge tipo píldora con borde de 1px y fondo translúcido al 15%).

## 5. Layout del Dashboard (`bslib`)
- Usar `page_navbar()` con barra superior fija en `#05070A` y borde inferior `#00FF88` de `1px`.
- Barra lateral (`sidebar`) colapsable a la izquierda (`width = 300px`) únicamente en las pestañas que requieren controles interactivos (`minPts`, umbral `LOF`, selector de departamento, selector de municipio).
- Cada gráfica debe incluir un subtítulo de 1 línea explicando qué buscar en el gráfico (guía de lectura para el auditor).