
library(dplyr)
library(janitor)
library(qs)

df_consumo_ceos <- qread('data/df_consumo_ceos.qs')

df_ceos_programa_conversao <- readxl::read_excel(
  'data/ceos_programa_conversao.xlsx'
)

df_consumo_ceos2 <- df_consumo_ceos %>%
  left_join(
    df_ceos_programa_conversao,
    by = 'nome_programa'
  )

# df_consumo_ceos2 %>%
#   qsave('data/df_consumo_ceos2.qs')

lst_nome_programa <- df_consumo_ceos2 %>%
  distinct(nome_programa2) %>%
  arrange(desc(nome_programa2)) %>%
  pull(nome_programa2)

lst_ano <- df_consumo_ceos2 %>%
  distinct(ano) %>%
  arrange(desc(ano)) %>%
  pull(ano)

lst_descricao_completa <- df_consumo_ceos2 %>%
  distinct(descricao_produto) %>%
  arrange(descricao_produto) %>%
  pull(descricao_produto)

qs::qsave(
  list(
    df_consumo_ceos = df_consumo_ceos2,
    lst_ano = lst_ano,
    lst_descricao_completa = lst_descricao_completa,
    lst_nome_programa = lst_nome_programa
  ),
  file = "app_suprimentos_ses/data/df_consumo_ceos.qs"
)

df_consumo_ceos %>% glimpse()

df_consumo_ceos %>% count(ano)

df_consumo_ceos %>%
  count(
    id_programa,
    nome_programa,
    nome_unidade_requisitante,
    nome_unidade_abastecedora
    ) %>%
  View()

