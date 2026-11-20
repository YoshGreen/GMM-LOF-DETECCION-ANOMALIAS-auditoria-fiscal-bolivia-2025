# AGENTS.md — Auditoría Forense Presupuestaria de Municipios de Bolivia (Gestión 2025)

## 1. Regla Visual Obligatoria
- **Lee `DESIGN.md` siempre** antes de crear o modificar cualquier archivo dentro de `dashboard/`, estilos CSS o gráficas `ggplot2` / `plotly`.

## 2. Objetivo y Alcance del Proyecto
Proyecto académico de Minería de Datos en **R** orientado a detectar anomalías presupuestarias y riesgo de fraude en los **Gobiernos Autónomos Municipales (GAM)** de Bolivia durante la **gestión 2025**, cruzando dos archivos CSV crudos oficiales del Ministerio de Economía y Finanzas Públicas (MEFP): **Gastos** e **Ingresos**.

## 3. Estructura Obligatoria de Directorios
```text
.
├── AGENTS.md
├── DESIGN.md
├── data/
│   ├── raw/
│   │   ├── gastos_gestion_2025/      # Contiene el CSV crudo de Gastos 2025 y su diccionario (NO modificar)
│   │   └── ingresos_gestion_2025/    # Contiene el CSV crudo de Ingresos 2025 y su diccionario (NO modificar)
│   └── processed/                    # Tablas limpias .rds y .parquet ligeros (< 5 MB)
├── scripts/
│   ├── 01_limpieza_kdd_duckdb.R
│   ├── 02_linea_base_univariada.R
│   ├── 03_modelos_gmm_lof.R
│   └── 04_auditoria_benford_fraude.R
├── dashboard/
│   ├── app.R                         # Aplicación interactiva R Shiny (bslib + plotly + DT)
│   └── www/
│       └── custom.css                # Sistema visual basado estrictamente en DESIGN.md
└── informe/
    ├── informe.tex                   # Informe académico completo compilable en pdflatex / MiKTeX
    ├── referencias.bib               # Bibliografía BibTeX
    ├── figuras/                      # Gráficos de alta resolución (300 DPI) generados desde R
    └── tablas/                       # Tablas .tex exportadas automáticamente desde R
```

## 4. Convenciones de Código en R
- Escribir código **explícito, modular y paso a paso** (evitar one-liners crípticos o encadenamientos excesivamente compactos).
- Comentar el propósito matemático o contable de cada transformación.
- Detectar los archivos de entrada dinámicamente usando `list.files("data/raw/gastos_gestion_2025", pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)` y `list.files("data/raw/ingresos_gestion_2025", pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)`, ignorando los archivos de documentación/diccionario presentes en esas carpetas.
- Nunca cargar los CSV crudos pesados directamente con `read.csv()` o `read_csv()` en RAM: **usar obligatoriamente `duckdb` + `DBI` + `dplyr`** para filtrar y agregar en disco antes de ejecutar `collect()`.
- Antes de construir consultas definitivas, inspeccionar nombres reales de columnas, delimitador y codificación UTF-8 de ambos CSV mediante una consulta `LIMIT 5` en DuckDB.

## 5. Diccionario Oficial (MEFP) y Metodología de Limpieza (KDD Fiscal)
### Fase 1 — Poda estructural en DuckDB
- Filtrar en ambos CSV: `gestion == 2025` y `entidad_sigla_subarea == 'GAM'` (subárea `1.1.7.3.`, Gobiernos Autónomos Municipales).
- Conservar códigos clave y descartar columnas de texto largo redundante durante el procesamiento masivo.
- Llave de cruce entre Gastos e Ingresos: `entidad` (junto con `entidad_desc_entidad` y `desc_departamento` de Gastos).

### Fase 2 — Auditoría de integridad contable
- Tratar `NA` monetarios como `0` mediante `coalesce(col, 0)`.
- Documentar (sin borrar silenciosamente) registros con montos negativos (reversiones contables) y violaciones de consistencia (`pagado > devengado`, `devengado > vigente`, o `vigente == 0` con `devengado > 0`).
- Guardar un log de auditoría de limpieza (`data/processed/auditoria_limpieza_2025.rds`) con filas iniciales, filtradas, nulos, duplicados exactos y reversiones para reportarlo en `informe/informe.tex`.

### Fase 3 — Ingeniería de características invariantes a la escala (nivel Entidad GAM)
Construir una matriz limpia por municipio con ratios relativos que eliminen el sesgo de tamaño entre capitales y municipios rurales:
1. `tasa_ejecucion_gasto` = `gasto_devengado / gasto_vigente`
2. `tasa_modificacion_gasto` = `(gasto_vigente - gasto_inicial) / gasto_inicial`
3. `brecha_pago` (deuda flotante) = `(gasto_devengado - gasto_pagado) / gasto_devengado`
4. `ratio_inversion` = `devengado_inversion / gasto_devengado` (donde `actividad == 0` indica proyecto de inversión y `proyecto == '0'` indica actividad/gasto corriente)
5. `ratio_serv_nopers` = `devengado_grupo_20000 / gasto_devengado` (Servicios No Personales, `objeto_grupo == 20000`)
6. `balance_caja_real` = `(ingreso_percibido - gasto_pagado) / ingreso_percibido`
7. `eficacia_recaudacion` = `ingreso_percibido / ingreso_vigente`
8. `dependencia_tgn` = `percibido_fuente_41 / ingreso_percibido` (Transferencias T.G.N., `fuente == 41`)
9. `log_presupuesto` = `log1p(gasto_vigente)`

### Fase 4 — Control de colas y estandarización
- Reemplazar divisiones por cero (`Inf`/`NaN`) por `NA` y documentar municipios excluidos por falta de ejecución.
- Aplicar winsorización suave (percentiles 1% y 99%) solo cuando existan distorsiones extremas documentadas, o comparar versión cruda vs. transformada.
- Estandarizar todas las variables numéricas con `scale()` antes de ejecutar GMM y LOF.

## 6. Requisitos de Modelado (Cobertura de la Materia)
1. **Línea base univariada (`02_linea_base_univariada.R`)**:
   - Calcular `boxplot.stats()`, criterio de Tukey (atípico leve: `1.5 * IQR`; extremo: `3 * IQR`) y Z-score (`|z| > 2`).
   - Demostrar empíricamente cómo el análisis univariado sobre montos brutos solo señala municipios grandes por escala y omite anomalías multivariantes.
2. **Mezclas Gaussianas — GMM (`03_modelos_gmm_lof.R`)**:
   - Usar `mclust::Mclust()` (algoritmo EM) seleccionando `G` mediante criterio BIC.
   - Evaluar tanto la probabilidad posterior máxima (`apply(pred$z, 1, max) < percentil 5%`) como la **log-densidad de probabilidad total** (`dens()` / `logLik` por observación) para identificar anomalías globales y contextuales verdaderas.
3. **Local Outlier Factor — LOF (`03_modelos_gmm_lof.R`)**:
   - Usar `dbscan::lof()` con análisis de sensibilidad en `minPts` (`10`, `15`, `20`, `30`).
   - Marcar anomalías locales con umbral base `LOF > 1.5` y evaluar estabilidad entre valores de `minPts`.
4. **Consenso y Auditoría de Fraude (`04_auditoria_benford_fraude.R`)**:
   - Clasificar cada municipio en 4 categorías: `Normal`, `Solo GMM (Global/Contextual)`, `Solo LOF (Local)`, `Consenso GMM + LOF (Prioridad Alta)`.
   - Sobre los registros crudos de gasto (`devengado > 0`) de los municipios en `Consenso GMM + LOF`, ejecutar prueba de **Ley de Benford** (primer dígito, prueba Chi-cuadrado y MAD) y detección de montos duplicados exactos o brechas en secuencias.

## 7. Dashboard Interactivo (`dashboard/app.R`)
- Construir en **R Shiny** con `bslib` (Bootstrap 5), `plotly` (gráficas interactivas), `DT` (tablas buscables y exportables) y `dashboard/www/custom.css`.
- Pestañas obligatorias:
  1. **Panorama Fiscal 2025**: KPIs ejecutivos, comparativa Gastos vs. Ingresos por departamento y línea base univariada interactiva (Tukey/Z-score).
  2. **Regímenes GMM & Densidad LOF**: Gráfica interactiva PCA/2D y 3D con elipses de regímenes GMM, slider dinámico de `minPts` y umbral `LOF`, y filtro por departamento.
  3. **Consenso de Anomalías**: Matriz comparativa GMM vs. LOF y tabla interactiva (`DT`) de municipios priorizados.
  4. **Auditoría Forense (Benford & Drill-Down)**: Al seleccionar un municipio anómalo, mostrar su distribución observada vs. teórica de la Ley de Benford, su estructura de gasto por `objeto_grupo` y su brecha Ingresos vs. Gastos.

## 8. Informe Académico en LaTeX (`informe/informe.tex`)
- Redactar en español académico formal, estructurado en: Resumen, Introducción y Problemática en Bolivia, Marco Teórico (Anomalías, Tukey/Z, GMM/EM, LOF, Benford), Metodología KDD y Auditoría de Datos 2025, Resultados Experimentales (con figuras en `informe/figuras/` y tablas en `informe/tablas/`), Análisis Forense de Casos Prioritarios y Conclusiones.
- Todas las cifras citadas en el `.tex` deben provenir directamente de los scripts de R (cero números inventados).

## 9. Verificación Obligatoria del Agente
- Ejecutar cada script con `Rscript scripts/0X_...R` y verificar código de salida `0`.
- Levantar `dashboard/app.R` localmente y verificar con **Playwright / Chrome DevTools MCP** que no haya errores en consola JS, que los gráficos de `plotly` rendericen correctamente y que el contraste visual cumpla `DESIGN.md`.