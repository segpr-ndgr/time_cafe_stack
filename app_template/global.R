# biblios ----
library(bsicons)            # Ícones para Bootstrap
library(bslib)              # Temas Bootstrap
library(DBI)                # Interface com bancos
library(DT)
library(dbplyr)             # dplyr com bancos
library(dplyr)              # Manipulação de dados
library(glue)               # Strings com variáveis
library(htmltools)          # HTML para Shiny
library(lubridate)          # Datas e horários
library(plotly)             # Gráficos interativos
library(pool)               # Gerenciar conexões
library(purrr)
library(qs)
library(reactable)          # Tabelas reativas
library(rlang)              # Metaprogramação tidyverse
library(RPostgres)
library(safer)              # Senhas seguras
library(scales)             # Escalas gráficas
library(shiny)              # Aplicativos web
library(shinyFeedback)      # Feedback de validação
library(shinyjs)            # JS no Shiny
library(shinymanager)       # Autenticação no Shiny
library(spsComps)           # Componentes Shiny extras
library(sodium)
library(stringi)            # Strings (internacional)
library(stringr)            # Manipular strings
library(tidyr)              # Transformações tidy
library(vialactea)          # Estilo SEPLAG
library(waiter)             # Carregamento visual

# YAML ----
config_path <- file.path("data", "config.yml")

status <- config::get("status", file = config_path)
config_banco <- config::get(paste0("sepedatalake_", status), file = config_path)
app_nome <- config::get("app_name", file = config_path)

# setup ----

# auth ----
set_labels(
  language = "pt-BR",
  "Please authenticate" = "",
  "Username:" = "Nome",
  "Password:" = "Senha"
)

db_config <- list(
  dbname = config_banco$dbname,
  host = config_banco$host,
  port = config_banco$port,
  user = config_banco$user,
  password = config_banco$password
)

# ETL ----
## pool ----
if (status == "adm") {
  con_base <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = db_config$dbname,
    host = db_config$host,
    user = db_config$user,
    password = db_config$password
  )

  message("Pool de conexões criado.")

  onStop(function() {
    message("Antes?", DBI::dbIsValid(con_base))
    pool::poolClose(con_base)
    message("Depois?", DBI::dbIsValid(con_base))
  })
}

## main ----
### sad - ses ----
dados_centralizados_sad <- qs::qread('data/dados_centralizados_sad.qs') %>%
  filter(
    status_cod >= 1
  )

### sad - central ----
dados_geral_sad <- qs::qread('data/dados_geral_sad.qs') %>%
  filter(
    status_cod >= 1
  )

# dados_processos <- qs::qread('data/dados_processos.qs')

dados_unificados_ses <-
  qs::qread('data/dados_unificados_ses.qs')

df_safety_supply <- qs::qread('data/df_safety_supply.qs')

df_consumo_ceos <- qs::qread("data/df_consumo_ceos.qs")

df_liquidacao_ses <- qs::qread("data/df_liquidacao_ses.qs")

## Mapas ----


## listas ----
#data_min <- min(dados_unificados_ses$data_de_inicio, na.rm = T)
data_min <- min(dados_geral_sad$data_de_inicio, na.rm = T)

#data_max <- max(dados_processos$data_de_inicio, na.rm = T)

#data_max <- '2025-08-19'
# data_max <- '2025-09-24'

data_max <- max(dados_geral_sad$data_de_inicio, na.rm = T)

lista_status <- qs::qread('data/lista_status.qs')

lista_demandante <- qs::qread('data/lista_demandante.qs')

# Módulos ---

file_paths <- fs::dir_ls(c("modules", "helpers"), invert = TRUE, regexp = ".*DONOTLOAD.*") # Lista todos os arquivos nos diretórios 'modules' e 'helpers', que não contenham a expressão DONOTLOAD

map(file_paths, function(x) {
  source(x) # Carrega cada arquivo individualmente.
})


