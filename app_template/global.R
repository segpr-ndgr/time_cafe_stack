cat("📦 Iniciando carregamento: global.R\n")

# preparação ----
## bibliotecas ----

# banco
library(DBI)
library(RPostgres)
library(dbplyr)
library(pool)

# etl
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(zoo)

# gráficos
library(DT)
library(ggplot2)
library(gtExtras)
library(plotly)

# mapas
library(htmltools)
library(htmlwidgets)
library(mapgl)
library(sf)

# render
library(bsicons)
library(bslib)
library(shiny)
library(shinybusy)
library(shinyjs)
library(waiter)

# utils
library(billboarder)
library(cachem)
library(glue)
library(purrr)
library(qs)
library(readr)
library(safer)
library(scales)
library(shinycssloaders)
library(shinymanager)
library(shinyWidgets)
library(sodium)
library(spsComps)
library(vialactea)

Sys.setlocale(locale = "pt_BR.utf8")

# YAML ----

# Auth ----

# ETL ----
## Listas ----

## Mapas ----
shp_limites_simplificados <- readRDS('mapas/mapa_pe_municipios.rds')

shp_pe_limites <- readRDS('mapas/pe_limits.rds')

# Leitura dos arquivos ----
safe_ls <- function(paths) {
  files <- fs::dir_ls(paths, regexp = "\\.R$", invert = FALSE)
  files[!grepl("DONOTLOAD", basename(files), fixed = TRUE)]
}

helpers <- safe_ls("helpers")

modules <- safe_ls("modules")

file_paths <- c(helpers, modules)

for (f in file_paths) {
  tryCatch({
    sys.source(f, envir = globalenv(), keep.source = FALSE)
  }, error = function(e) {
    warning("⚠️ Falha ao carregar ", ": ", e$message, call. = FALSE)
  })
}
