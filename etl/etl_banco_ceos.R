library(DBI)
library(dplyr)
library(glue)
library(RPostgres)

config_path <- file.path("data", "config.yml")

config_ceos <- config::get('ceos', file = config_path)

# Conexão ao banco de dados
con <- dbConnect(
  RPostgres::Postgres(),
  host = "172.31.45.87",
  port = 5432,
  dbname = "ceos",
  user = config_ceos$usr,
  password = config_ceos$pwd
)

# compras ----
tictoc::tic('infos de compras')
query_info_view_compras <- "
SELECT
  column_name,
  data_type,
  character_maximum_length,
  is_nullable
FROM information_schema.columns
WHERE table_schema = 'ceos'
  AND table_name = 'ses_rel_hist_compras';
"
tictoc::toc()

# Executa a consulta
dbGetQuery(con, query_info_view_compras)

tictoc::tic('dados compras')
dados_compras <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_HIST_COMPRAS
  "
)
tictoc::toc()

dados_compras %>% glimpse()

dados_compras %>%
  qs::qsave('data/df_compras_ceos.qs')

# consumo ----
query_info_view_consumo <- "
SELECT
  column_name,
  data_type,
  character_maximum_length,
  is_nullable
FROM information_schema.columns
WHERE table_schema = 'ceos'
  AND table_name = 'ses_rel_hist_consumo';
"

# Executa a consulta
dbGetQuery(con, query_info_view_consumo)

tictoc::tic('dados de consumo')
dados_consumo <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_HIST_CONSUMO
  "
)
tictoc::toc()

dados_consumo %>% glimpse()

dados_consumo %>%
  qs::qsave('data/df_consumo_ceos.qs')

tictoc::tic('unidades de consumo')
unidades_req <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_HIST_CONSUMO
  "
)
tictoc::toc()

unidades_req %>%
  qs::qsave('data/unidades_req.qs')

# expedição ----
tictoc::tic('infos de expedição')
query_info_view_exped <- "
SELECT
  column_name,
  data_type,
  character_maximum_length,
  is_nullable
FROM information_schema.columns
WHERE table_schema = 'ceos'
  AND table_name = 'ses_rel_hist_exped';
"
tictoc::toc()

# Executa a consulta
dbGetQuery(con, query_info_view_exped)

tictoc::tic('dados exped')
dados_exped <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_HIST_EXPED
  "
)
tictoc::toc()

dados_exped %>% glimpse()

dados_exped %>%
  qs::qsave('data/df_expedicao_ceos.qs')

# recebimento ----
tictoc::tic('infos de recebimento')
query_info_view_recebimento <- "
SELECT
  column_name,
  data_type,
  character_maximum_length,
  is_nullable
FROM information_schema.columns
WHERE table_schema = 'ceos'
  AND table_name = 'ses_rel_hist_receb';
"
tictoc::toc()

# Executa a consulta
view_info_recebimento <- dbGetQuery(con, query_info_view_recebimento)

# Exibe resultado
print(view_info_recebimento)

tictoc::tic('dados exped')
dados_receb <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_HIST_RECEB
  "
)
tictoc::toc()

dados_receb %>% glimpse()

dados_receb %>%
  qs::qsave('data/df_recebimento_ceos.qs')

# estoque ----
tictoc::tic('dados exped')
dados_estoque <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_POSICAO_ESTOQUE_FISICO
  "
)
tictoc::toc()

dados_estoque %>% glimpse()

dados_estoque %>%
  qs::qsave('data/df_estoque_ceos.qs')

# estoque ----
## simples ----
tictoc::tic('dados exped')
dados_estoque <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_POSICAO_ESTOQUE_FISICO
  "
)
tictoc::toc()

dados_estoque %>% glimpse()

dados_estoque %>%
  qs::qsave('data/df_estoque_ceos.qs')

## detahado
dados_estoque_det <- dbGetQuery(
  con,
  "
  SELECT *
  FROM ceos.SES_REL_POSICAO_ESTOQUE_CONT_DETAL
  "
)
tictoc::toc()

dados_estoque_det %>% glimpse()

dados_estoque_det %>%
  qs::qsave('data/df_estoque_det_ceos.qs')
