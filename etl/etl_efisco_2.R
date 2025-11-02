# biblios ----
library(BigDataPE)
library(data.table)
library(DBI)
library(dplyr)
library(glue)
library(janitor)
library(lubridate)
library(qs)
library(RPostgres)

# liquidacao_2023 <- fread('data/efisco/Liquidacao_2023.csv') %>%
#   clean_names()
#
# liquidacao_2024 <- fread('data/efisco/Liquidacao_2024.csv') %>%
#   clean_names()
#
# liquidacao_23_24 <- liquidacao_2024 %>%
#   dplyr::bind_rows(liquidacao_2023)
#
# liquidacao_23_24 %>% glimpse()
#
# liquidacao_23_24 %>%
#   qsave('data/efisco/liquidacao_23_24.qs')

config_path <- file.path("data", "config.yml")

config_tokens <- config::get('tokens', file = config_path)

bdpe_store_token(
  'liquidacao_2025',
  config_tokens$liquidacao_2025)

liquidacao_23_24 <-
  qread('data/efisco/liquidacao_23_24.qs')

liquidacao_23_24 %>% glimpse()

liquidacao_2025 <-BigDataPE::bdpe_fetch_data(
  base_name = "liquidacao_2025",
  verbosity = 1)  %>%
  clean_names()

liquidacao_2025 %>% glimpse()

df_liquidacao <- liquidacao_2025 %>%
  mutate(

    # inteiros
    ano_da_liquidacao = as.integer(ano_da_liquidacao),
    mes_da_liquidacao = as.integer(mes_da_liquidacao),
    uge_emp = as.integer(uge_emp),
    codigo_grupo_da_despesa = as.integer(codigo_grupo_da_despesa),
    natureza_da_despesa_codigo = as.integer(natureza_da_despesa_codigo),
    unidade_fornecimento = as.integer(unidade_fornecimento),
    codigo_da_classe_do_material_de_servico = as.integer(codigo_da_classe_do_material_de_servico),
    codigo_do_grupo_de_material_de_servico = as.integer(codigo_do_grupo_de_material_de_servico),
    codigo_da_licitacao = as.integer(codigo_da_licitacao),

    # datas
    data_de_lancamento_do_empenho = as.Date(data_de_lancamento_do_empenho),
    data_do_doc_liq = as.IDate(data_do_doc_liq),
  ) %>%
  bind_rows(liquidacao_23_24) %>%
  mutate(
    ano = ano_da_liquidacao,
    qtd_de_itens_liquidados   = as.numeric(
      gsub(",", ".", qtd_de_itens_liquidados)),
    valor_unitario_do_item_liquidado   = as.numeric(
      gsub(",", ".", valor_unitario_do_item_liquidado)),
    valor_total_liquidado   = as.numeric(
      gsub(",", ".", valor_total_liquidado)),
    valor_liquidado_final   = as.numeric(
      gsub(",", ".", valor_liquidado_final)),
    valor_descontado        = as.numeric(
      gsub(",", ".", valor_descontado)),
    valor_estornado         = as.numeric(
      gsub(",", ".", valor_estornado)),

    # uge

    uge_nome_emp = case_when(
      uge_nome_emp == "FUNDO ESTADUAL DE SAÚDE"                ~ "FES",
      uge_nome_emp == "HOSPITAL UNIVERSITÁRIO OSWALDO CRUZ"    ~ "HUOC",
      uge_nome_emp == "HOSPITAL DA RESTAURAÇÃO"                ~ "HR",
      uge_nome_emp == "HOSPITAL AGAMENON MAGALHAES"            ~ "HAM",
      uge_nome_emp == "HOSPITAL GETULIO VARGAS"                ~ "HGV",
      uge_nome_emp == "HOSPITAL OTAVIO DE FREITAS"             ~ "HOF",
      uge_nome_emp == "HOSPITAL BARAO DE LUCENA"               ~ "HBL",
      uge_nome_emp == "HOSPITAL REGIONAL DO AGRESTE DR.WALDEMIRO FERREIRA" ~ "HRA",
      TRUE                                                     ~ uge_nome_emp
    )

  )

df_liquidacao %>% glimpse()

df_liquidacao %>%
  count(ano_da_liquidacao)

df_liquidacao %>%
  count(grupo_do_material_de_servico) %>%
  View()

# df_liquidacao_hospitalar <- df_liquidacao %>%
#   filter(
#     grupo_do_material_de_servico %in% c('EQUIPAMENTOS E ARTIGOS DE USO MEDICO, ODONTOLOGICO , HOSPITALAR E LABORATORIAL')
#   )
#
# df_liquidacao_hospitalar %>%
#   glimpse()
#
# df_liquidacao_hospitalar %>%
#   count(ugc_emp_sigla) %>%
#   print(n=1000)

df_liquidacao_ses <- df_liquidacao %>%
  filter(
    item_de_gasto %in% c(
      "2 - LOCAÇÃO DE EQUIPAMENTOS MÉDICO-HOSPITALARES",
      "6 - MEDICAMENTOS",
      "8 - APARELHOS, EQUIPAMENTOS, UTENSÍLIOS MÉDICO-ODONTOLÓGICO, LABORATORIAL E HOSPITALAR",
      "36 - MATERIAL HOSPITALAR",
      "50 - SERVIÇOS MÉDICO-HOSPITALARES, ODONTOLÓGICOS E LABORATORIAIS")
  )

df_liquidacao_ses %>%
  count(grupo_do_material_de_servico, item_de_gasto) %>%
  View()

df_liquidacao_ses %>%
  count(ano_da_liquidacao)

lst_uge_nome_emp <- df_liquidacao_ses %>%
  distinct(uge_nome_emp) %>%
  arrange(desc(uge_nome_emp)) %>%
  pull(uge_nome_emp)

lst_ano_da_liquidacao <- df_liquidacao_ses %>%
  distinct(ano_da_liquidacao) %>%
  arrange(desc(ano_da_liquidacao)) %>%
  pull(ano_da_liquidacao)

lst_item_liquidado_nome <- df_liquidacao_ses %>%
  distinct(item_liquidado_nome) %>%
  arrange(item_liquidado_nome) %>%
  pull(item_liquidado_nome)

qs::qsave(
  list(
    df_liquidacao_ses = df_liquidacao_ses,
    lst_uge_nome_emp = lst_uge_nome_emp,
    lst_ano_da_liquidacao = lst_ano_da_liquidacao,
    lst_item_liquidado_nome = lst_item_liquidado_nome
  ),
  file = "app_suprimentos_ses/data/df_liquidacao_ses.qs"
)
