# pacotes ----
library(dplyr)
library(googledrive)
library(lubridate)
library(janitor)
library(purrr)
library(qs)
library(readxl)
library(stringi)
library(stringr)

# SES - prioritários ----
url_safety_supply <- "https://docs.google.com/spreadsheets/d/1f2HjSbYWYGrqEaaNg5MAezd6FXQDepgv/edit"

drive_auth(email = "hugoavmedeiros@gmail.com")

arquivo_safety_supply <- drive_download(
  as_id(url_safety_supply),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_safety_supply <- excel_sheets(arquivo_safety_supply$local_path)

plan_safety_supply <- map2(
  abas_safety_supply,
  seq_along(abas_safety_supply),
  ~ if (.y == 1) {
    read_excel(arquivo_safety_supply$local_path, sheet = .x, skip = 7) %>%
      select(-1)  # remove a primeira coluna
  } else {
    read_excel(arquivo_safety_supply$local_path, sheet = .x)
  }
)

# Nomeia a lista com os nomes das abas
names(plan_safety_supply) <- abas_safety_supply

dados_safety_supply <- plan_safety_supply$STATUS %>%
  clean_names() %>%
  mutate(
    data_geracao_irp = as.Date(as.numeric(str_replace(data_geracao_irp, ",", ".")), origin = "1899-12-30"),
    ordem = case_when(
      status_detalhado == "Aguardando posicionamento quanto à obrigatoriedade do DFD/PCA na elaboração da Solicitação de Compras" ~ 99,
      TRUE ~ as.numeric(str_extract(status_detalhado, "^\\d+"))
    ),
    ordem_status = if_else(is.na(ordem), 0, ordem),
    alerta_status = if_else(ordem_status %in% c(3, 99), "⚠️", ""),
    sei2 = str_remove(sei2, "^SEI\\.")
  )

dados_safety_supply %>% View()

dados_safety_supply %>%
  qs::qsave('app_suprimentos_ses/data/df_safety_supply.qs')

# SES - unificados ----
url_processos_itens <- "https://docs.google.com/spreadsheets/d/1VWisqlNTRsZXbWP4llZ4bQZ0tN88xBj3Fx-nAgv091k/edit"

arquivo_processos_itens <- drive_download(
  as_id(url_processos_itens),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_processos_itens <- excel_sheets(arquivo_processos_itens$local_path)

plan_processos_itens <- map2(
  abas_processos_itens,
  seq_along(abas_processos_itens),
  ~ if (.y == 1) {
    read_excel(arquivo_processos_itens$local_path, sheet = .x)
  } else {
    read_excel(arquivo_processos_itens$local_path, sheet = .x)
  }
)

# Nomeia a lista com os nomes das abas
names(plan_processos_itens) <- abas_processos_itens

dados_processos <- plan_processos_itens$STATUS %>%
  #slice(-1) %>%  # remove a primeira linha (cabeçalho repetido)
  #setNames(as.character(unlist(plan_processos_itens$STATUS[1, ]))) %>%
  clean_names() %>%
  mutate(
    #data_de_inicio = as.Date(as.numeric(data_de_inicio), origin = "1899-12-30"),
    #data_de_inicio = coalesce(data_de_inicio, data_geracao_irp),
    status_cod = str_extract(status, "^[0-9]+"),
    status_cod = case_when(
      status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      status == "Arquivado (Reagentes)" ~ "0.1",
      status == "Execução de Compra Direta" ~ "0.2",
      is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
      TRUE ~ as.character(status_cod)
    ),
    status_cod = as.numeric(status_cod)
    # status_cod = ifelse(
    #   is.na(status_cod),
    #   00,
    #   status_cod)
  ) %>%
  left_join(
    dados_safety_supply,
    by = c('sei' = 'sei2')
  )

dados_processos %>% glimpse()

dados_processos %>% View()

dados_processos %>%
  qs::qsave('app_suprimentos_ses/data/dados_processos.qs')

# SAD - Gerais ----
url_centralizados_sad <- "https://docs.google.com/spreadsheets/d/1tvNcspcU2bM4JYwWWJk7evTeOcdH6mfX3iCmz9yEJ0M/edit?gid=349470604#gid=349470604"

arquivo_centralizados_sad <- drive_download(
  as_id(url_centralizados_sad),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_centralizados_sad <- excel_sheets(arquivo_centralizados_sad$local_path)

plan_centralizados_sad <- map2(
  abas_centralizados_sad,
  seq_along(abas_centralizados_sad),
  ~ if (.y == 1) {
    read_excel(arquivo_centralizados_sad$local_path, sheet = .x, skip = 1)
  } else {
    read_excel(arquivo_centralizados_sad$local_path, sheet = .x)
  }
)

names(plan_centralizados_sad) <- abas_centralizados_sad

dados_centralizados_sad <- plan_centralizados_sad$STATUS %>%
  clean_names() %>%
  mutate(
    status_cod = str_extract(status, "^[0-9]+"),
    status_cod = case_when(
      status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      status == "Arquivado (Reagentes)" ~ "0.1",
      status == "Execução de Compra Direta" ~ "0.2",
      is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
      TRUE ~ as.character(status_cod)
    ),
    status_cod = as.numeric(status_cod)
  )

dados_centralizados_sad <- dados_centralizados_sad %>%
  mutate(
    is_ses_unificadas = ifelse(sei %in% dados_processos$sei, "Sim", "Não"),
    progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 17 * 100)), 2),
    dias_corridos = pmax(0, as.integer(Sys.Date() - as.Date(data_de_inicio)))
    )

sla_tbl <- tribble(
  ~status_cod, ~fluxo,                               ~sla_dias,
  0, "Cancelado",                                     0,
  1, "01. Intenção de Registro de Preços",           14,
  2, "02. Formação de Mapa de Preços",               10,
  3, "03. Elaboração de ETP e Termo de Referência",  10,
  4, "04. Análise Inicial SAD",                      23,
  5, "05. Resposta Cota SAD",                        20,
  6, "06. Elaboração de Edital",                      5,
  7, "07. Análise Jurídica do Edital",                5,
  8, "08. Resposta Cota Jurídico",                    5,
  9, "09. Parecer da Procuradoria",                  10,
  10, "10. Resposta Cota Procuradoria",                5,
  11, "11. Sessão de Abertura",                       12,
  12, "12. Fase de Proposta/Amostra",                 15,
  13, "13. Fase de Habilitação",                       5,
  14, "14. Sessão de Resultado/Recurso",              15,
  15, "15. Homologação",                               5,
  16, "16. Confecção da Ata de Registro de Preços",   15,
  17, "17. Ata Disponível",                            0
)

# 2) SLA cumulativo por fase
sla_cum <- sla_tbl %>%
  arrange(status_cod) %>%
  mutate(cum_sla = cumsum(sla_dias)) %>%
  select(status_cod, cum_sla)

# 3) Junte ao seu data frame original (precisa ter status_cod e data_de_inicio)
dados_centralizados_sad_2 <- dados_centralizados_sad %>%
  # garanta que status_cod é numérico
  mutate(status_cod = as.numeric(status_cod)) %>%
  left_join(sla_cum, by = "status_cod") %>%
  mutate(
    deadline_date   = as_date(data_de_inicio) + days(coalesce(cum_sla, 0)),
    atraso          = Sys.Date() > deadline_date,
    dias_em_atraso  = pmax(0, as.integer(Sys.Date() - deadline_date))
  )

dados_centralizados_sad_2 %>% glimpse()

dados_centralizados_sad_2 %>% View()

dados_centralizados_sad_2 %>%
  qs::qsave('app_suprimentos_ses/data/dados_centralizados_sad.qs')
