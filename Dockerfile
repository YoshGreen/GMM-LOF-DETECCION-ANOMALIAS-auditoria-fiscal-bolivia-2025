# ==============================================================================
# DOCKERFILE PARA DESPLIEGUE EN RENDER.COM / CLOUD PAAS
# APLICACIÓN: Auditoría Fiscal Municipal Bolivia 2025 (R Shiny Dashboard)
# ==============================================================================

FROM rocker/shiny:4.3.3

# Instalar dependencias del sistema operativo
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    && rm -rf /var/lib/apt/lists/*

# Instalar librerías de R utilizadas en el Dashboard
RUN R -e "install.packages(c('bslib', 'plotly', 'DT', 'dplyr', 'scales', 'arrow'), repos='https://cloud.r-project.org/')"

# Directorio de trabajo en el contenedor
WORKDIR /app

# Copiar únicamente el código del Dashboard y los datos procesados ligeros (< 300 KB)
# Se ignoran intencionalmente los 408 MB de CSVs crudos mediante .dockerignore
COPY dashboard/ /app/dashboard/
COPY data/processed/ /app/data/processed/

# Render asigna dinámicamente el puerto en la variable $PORT (por defecto 3838)
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('dashboard', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]
