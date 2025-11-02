library(DBI)
library(dplyr)
library(glue)
library(lubridate)
library(RPostgres)

# Conexão ao banco de dados
con <- dbConnect(
  RPostgres::Postgres(),
  host = "10.238.107.182",
  port = 5432,
  dbname = "sepe",
  user = "sepe_adm",
  password = "!sepe_adm!"
)

# itens ----
query_info_itens <- "
SELECT
  column_name,
  data_type,
  character_maximum_length,
  is_nullable
FROM information_schema.columns
WHERE table_name = 'efisco_itens';
"

dbGetQuery(con, query_info_itens)

tictoc::tic('dados itens')
dados_itens <- dbGetQuery(
  con,
  "
  SELECT *
  FROM efisco_itens
  "
)
tictoc::toc()

dados_itens %>% glimpse()

dados_itens %>%
  count(nome_grupo_material_servico)

dados_itens_hospitalar <- dados_itens %>%
  filter(
    nome_grupo_material_servico %in% c('EQUIPAMENTOS E ARTIGOS DE USO MEDICO, ODONTOLOGICO , HOSPITALAR E LABORATORIAL', '')
  )

# liquidação ----
query_info_liquidacao <- "
SELECT
  column_name,
  data_type,
  character_maximum_length,
  is_nullable
FROM information_schema.columns
WHERE table_name = 'efisco_liquidacao';
"

dbGetQuery(con, query_info_liquidacao)

tictoc::tic('dados liquidação')
dados_liquidacao <- dbGetQuery(
  con,
  "
  SELECT *
  FROM efisco_liquidacao
  "
)
tictoc::toc()

dados_liquidacao %>% glimpse()

dados_liquidacao2 <- dados_liquidacao %>%
  mutate(
    data_lancamento_empenho = as.Date(data_de_lancamento_do_empenho),
    ano_extraido = as.integer(substr(codigo_empenho, 1, 4))
  )

dados_liquidacao2 %>%
  distinct(codigo_liquidacao) %>%
  nrow()

dados_liquidacao2 %>% nrow()

dados_liquidacao_ses <- dados_liquidacao2 %>%
  filter(
    item_de_gasto %in% c("2 - LOCAÇÃO DE EQUIPAMENTOS MÉDICO-HOSPITALARES", "6 - MEDICAMENTOS", "8 - APARELHOS, EQUIPAMENTOS, UTENSÍLIOS MÉDICO-ODONTOLÓGICO, LABORATORIAL E HOSPITALAR", "36 - MATERIAL HOSPITALAR", "50 - SERVIÇOS MÉDICO-HOSPITALARES, ODONTOLÓGICOS E LABORATORIAIS")
  )

dados_liquidacao_ses %>%
  count(
    #year(data_de_inicio_da_vigencia_do_contrato),
    ano_da_liquidacao
    )

dados_liquidacao_ses %>%
  writexl::write_xlsx('data/dados_liquidacao_hospitalares.xlsx')

