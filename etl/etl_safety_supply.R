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

drive_auth(email = "hugoavmedeiros@gmail.com")

# SES - prioritários ----
source('etl/etl_ses_prioritarios.R')

# SES - unificados ----
#url_processos_itens <- "https://docs.google.com/spreadsheets/d/1VWisqlNTRsZXbWP4llZ4bQZ0tN88xBj3Fx-nAgv091k/edit"

url_processos_itens <- "https://docs.google.com/spreadsheets/d/1Sbs4qB44exuiVWFuldqDAwYPHRDeJ_lu-RwHUu2Ts6M/edit?gid=231167667#gid=231167667"

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

#dados_processos <- plan_processos_itens$STATUS %>%
dados_processos <- plan_processos_itens$Suprimentos %>%
  #slice(-1) %>%  # remove a primeira linha (cabeçalho repetido)
  #setNames(as.character(unlist(plan_processos_itens$STATUS[1, ]))) %>%
  clean_names() %>%
  filter(
    !is.na(sei)
  ) %>%
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

# sla ----
url_sla <- "https://docs.google.com/spreadsheets/d/1VWisqlNTRsZXbWP4llZ4bQZ0tN88xBj3Fx-nAgv091k/edit"

arquivo_sla <- drive_download(
  as_id(url_sla),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_sla <- excel_sheets(arquivo_sla$local_path)

plan_sla <- map2(
  abas_sla,
  seq_along(abas_sla),
  ~ if (.y == 1) {
    read_excel(arquivo_sla$local_path, sheet = .x)
  } else {
    read_excel(arquivo_sla$local_path, sheet = .x)
  }
)

# Nomeia a lista com os nomes das abas
names(plan_sla) <- abas_sla

sla_tbl <- plan_sla$`Lead Time SS` %>%
  clean_names() %>%
  rename(
    status_cod = fase,
    status = fluxo,
    sla_dias = dead_line_dias
  ) %>%
  mutate(
    cor_barras = case_when(
      cor == "Cinza"     ~ "#6c757d",
      cor == "Azul Escuro"     ~ "#005372",
      cor == "Azul Claro"    ~ "#66a5ad",
      cor == "Rosa" ~ "#c94f7c",
      cor == "Verde"  ~ "#3c8d63",
      TRUE ~ NA_character_
    )
  )

# 2) SLA cumulativo por fase
sla_cum <- sla_tbl %>%
  arrange(status_cod) %>%
  mutate(cum_sla = cumsum(sla_dias)) %>%
  select(status_cod, cum_sla) %>%
  left_join(
    sla_tbl,
    by = 'status_cod'
  ) %>%
  filter(
    !is.na(status_cod)
  )

dados_processos %>% glimpse()

dados_processos %>%
  qs::qsave('app_suprimentos_ses/data/dados_processos.qs')

# dados_unificados_ses <- plan_processos_itens$STATUS %>%
#   dados_processos <- plan_processos_itens$Suprimentos %>%
#   clean_names() %>%
#   select(
#     sei,
#     grupo,
#     #qtd_de_itens = x3,
#     tempo_na_fase = tempo_na_fase,
#     #itens_unicos = x4,
#     data_de_inicio,
#     setor = x6,
#     status,
#     observacoes = obs
#   ) %>%
#   mutate(
#     status_cod = str_extract(status, "^[0-9]+"),
#     status_cod = case_when(
#       status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
#       status == "Arquivado (Reagentes)" ~ "0.1",
#       status == "Execução de Compra Direta" ~ "0.2",
#       is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
#       TRUE ~ as.character(status_cod)
#     ),
#     status_cod = as.numeric(status_cod),
#     is_ses_unificadas = "Sim",
#     base_origem = 'SES',
#     demandante = 'SES - SEAF'
#   ) %>%
#   left_join(
#     sla_cum %>%
#       select(
#         -c(status)
#       ), by = "status_cod")

dados_unificados_ses <- plan_processos_itens$Suprimentos %>%
  clean_names() %>%
  filter(
    !is.na(sei)
  ) %>%
  select(
    sei,
    grupo = objeto,
    #qtd_de_itens = x3,
    #tempo_na_fase = tempo_na_fase,
    data_do_status,
    demandante = demandante,
    #itens_unicos = x4,
    data_de_inicio,
    setor = tema,
    status,
    observacoes = observacoes
  ) %>%
  mutate(
    #data_do_status = as_date(data_do_status),

    data_do_status = case_when(
      suppressWarnings(!is.na(as.numeric(data_do_status))) ~ as.Date(as.numeric(data_do_status), origin = "1899-12-30"),
      TRUE ~ suppressWarnings(dmy(data_do_status))
    ),

    tempo_na_fase = if_else(
      is.na(data_do_status),
      0L,
      as.integer(Sys.Date() - data_do_status)
    ),

    data_de_inicio = case_when(
      suppressWarnings(!is.na(as.numeric(data_de_inicio))) ~ as.Date(as.numeric(data_de_inicio), origin = "1899-12-30"),
      TRUE ~ suppressWarnings(dmy(data_de_inicio))
    ),

    tempo_na_fase = pmax(0L, tempo_na_fase),
    status_cod = str_extract(status, "^[0-9]+"),
    status_cod = case_when(
      status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      status == "Arquivado (Reagentes)" ~ "0.1",
      status == "Execução de Compra Direta" ~ "0.2",
      is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
      TRUE ~ as.character(status_cod)
    ),
    status_cod = as.numeric(status_cod),
    is_ses_unificadas = "Sim",
    base_origem = 'SES' #,
    #demandante = 'SES - SEAF'
  ) %>%
  left_join(
    sla_cum %>%
      select(
        -c(status)
      ), by = "status_cod")

dados_unificados_ses %>% glimpse()

## join com ggafp ----

# SES - prioritários ----
# source('etl/etl_ggafp_unificados.R')
#
# dados_unificados_ses_ggafp <- dados_unificados_ses %>%
#   #rbind(dados_ggafp_unificados)
#   rbind(dados_ggafp_unificados_2)
#
# dados_unificados_ses_ggafp %>% glimpse()

dados_unificados_ses_ggafp <- dados_unificados_ses

dados_unificados_ses_2 <- dados_unificados_ses %>%
  filter(
    !is.na(sei)
  ) %>%
  mutate(
    progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2),
    dias_corridos = pmax(0, as.integer(Sys.Date() - as.Date(data_de_inicio))),
    deadline_date   = as_date(data_de_inicio) + days(coalesce(cum_sla, 0)),
    atraso          = Sys.Date() > deadline_date,
    dias_em_atraso  = pmax(0, as.integer(Sys.Date() - deadline_date))
  )

dados_unificados_ses_2 %>%
  qs::qsave('app_suprimentos_ses/data/dados_unificados_ses.qs')

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
  filter(
    !is.na(sei)
  ) %>%
  mutate(
    data_do_status = as_date(data_do_status),

    tempo_na_fase = if_else(
      is.na(data_do_status),
      0L,
      as.integer(Sys.Date() - data_do_status)
    ),

    status_cod = str_extract(status, "^[0-9]+"),
    status_cod = case_when(
      status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      status == "Arquivado (Reagentes)" ~ "0.1",
      status == "Execução de Compra Direta" ~ "0.2",
      is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
      TRUE ~ as.character(status_cod)
    ),
    status_cod = as.numeric(status_cod),
    is_ses_unificadas = ifelse(sei %in% dados_unificados_ses$sei, "Sim", "Não"),
    base_origem = 'SAD',
    #demandante = itens_unicos
    demandante = demandante
  ) %>%
  left_join(
    sla_cum %>%
      select(-c(status)),
    by = "status_cod")

dados_centralizados_sad %>% glimpse()

dados_centralizados_sad %>% count()

dados_centralizados_sad %>% count(demandante)

dados_centralizados_sad %>% count(is_ses_unificadas)

dados_combinados <- bind_rows(
  dados_centralizados_sad %>%
    mutate(
      #tempo_na_fase = as.character(tempo_na_fase)

      # data_de_inicio = case_when(
      #   suppressWarnings(!is.na(as.numeric(data_de_inicio))) ~ as.Date(as.numeric(data_de_inicio), origin = "1899-12-30"),
      #   TRUE ~ suppressWarnings(dmy(data_de_inicio))
      # )

    ),
  dados_unificados_ses_ggafp
  ) %>%
  mutate(
    #preferida antigo #
    # preferida = case_when(
    #   is_ses_unificadas == "Sim" & responsavel == "SES" & base_origem == "SES" ~ 2L,
    #   is_ses_unificadas == "Sim" & responsavel != "SAD" & base_origem == "SAD" ~ 2L,
    #   base_origem == "SES" ~ 1L,
    #   TRUE ~ 0L
    # )

    # preferida novo #

    preferida = case_when(
      base_origem == "SAD" ~ 2L,   # agora sempre prefere SAD
      base_origem == "SES" ~ 1L,
      TRUE ~ 0L
    )

  )

dados_combinados %>% count()

dados_combinados_2 <- dados_combinados %>%
  arrange(desc(preferida)) %>%
  distinct(across(all_of('sei')), .keep_all = TRUE) %>%
  select(-preferida)

dados_combinados_2 %>% count()

status_ses <- dados_unificados_ses_ggafp %>%
  select(sei, status) %>%
  filter(!is.na(sei)) %>%
  distinct(sei, .keep_all = TRUE) %>%
  rename(status_secundario = status)

dados_combinados_2 <- dados_combinados_2 %>%
  left_join(status_ses, by = "sei") %>%
  relocate(
    status_secundario,
    .after = status)

dados_combinados_3 <- dados_combinados_2 %>%
  mutate(
    progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2),
    dias_corridos = pmax(0, as.integer(Sys.Date() - as.Date(data_de_inicio)))
    )

dados_combinados_3 %>% count()

# 3) Junte ao seu data frame original (precisa ter status_cod e data_de_inicio)
dados_combinados_4 <- dados_combinados_3 %>%
  mutate(
    deadline_date   = as_date(data_de_inicio) + days(coalesce(cum_sla, 0)),
    atraso          = Sys.Date() > deadline_date,
    dias_em_atraso  = pmax(0, as.integer(Sys.Date() - deadline_date)),

    demandante = case_when(
      is.na(demandante) ~ "Outros",
      demandante == "Não se aplica" ~ "Outros",
      TRUE ~ demandante
    ),

    tempo_na_fase = tempo_na_fase,

    status = str_replace(status, "05\\. Resposta cota SAD", "05. Resposta Cota SAD"),

    status_secundario = str_replace(status_secundario, "05\\. Resposta cota SAD", "05. Resposta Cota SAD")
  )

dados_combinados_4 %>% count()

dados_combinados_4 %>% glimpse()

lista_max_data <- dados_combinados_4 %>% pull(data_de_inicio) %>% max(na.rm = TRUE)

lista_max_data

dados_combinados_4 %>%
  filter(
    data_de_inicio >= '2025-08-19' | is.na(demandante)
  ) %>%
  count(data_de_inicio, demandante)

count(dados_combinados_4)

dados_combinados_4 %>%
  filter(
    !is.na(sei)
  ) %>%
  qs::qsave('app_suprimentos_ses/data/dados_centralizados_sad.qs')

# SAD CENTRAL ----
source('etl/etl_sad_geral.R')

# listas ----
## status ----
lista_status <- dados_combinados_4 %>%
  select(status_cod, status) %>%
  bind_rows(
    sla_tbl %>% select(status_cod, status)
  ) %>%
  arrange(status_cod) %>%
  distinct(status) %>%
  filter(!is.na(status)) %>%
  pull(status)

lista_status

lista_status %>% qs::qsave('app_suprimentos_ses/data/lista_status.qs')

## demandantes ----
lista_demandante_1 <- dados_combinados_4 %>%
  arrange(demandante) %>%
  distinct(demandante) %>%
  filter(!is.na(demandante)) %>%
  pull(demandante)

lista_demandante_2 <- dados_geral_sad_3 %>%
  arrange(demandante) %>%
  distinct(demandante) %>%
  filter(!is.na(demandante)) %>%
  pull(demandante)

lista_demandante <- c(
  lista_demandante_1, lista_demandante_2
  ) %>%
  sort(decreasing = TRUE) %>%
  unique()

lista_demandante

lista_demandante %>% qs::qsave('app_suprimentos_ses/data/lista_demandante.qs')

# qualidade ----

dados_centralizados_sad %>%
  filter(
    is_ses_unificadas == 'Sim'
  ) %>%
  count()

teste_qualidade <- dados_centralizados_sad %>%
  filter(
    is_ses_unificadas == 'Sim'
  ) %>%
  select(
    sei, status_cod, status
  ) %>%
  left_join(
    dados_unificados_ses %>%
      select(
        sei, status_cod, status
      ),
    by = 'sei',
    suffix = c("_SAD", "_SES")
  ) %>%
  mutate(
    check_quali = ifelse(status_cod_SAD == status_cod_SES, 1, -1)
  )

teste_qualidade %>% group_by(check_quali) %>% count()

teste_qualidade %>% writexl::write_xlsx('data/teste_consistencia.xlsx')

dados_combinados_4 %>%
  filter(
    data_de_inicio > '2025-08-19'
  ) %>%
  count()

dados_combinados_4 %>%
  filter(
    status_cod < 1
  ) %>%
  count()
