# lista ----
#### GGAFP  ##
#### HAM    ##
#### HOF    ##

# ggafp ----
url_ggafp_unificados <- "https://docs.google.com/spreadsheets/d/1D2bZ6Unk-mi9yAHVPmdgfxY9r32GssoItsSa-JSsGaY/edit?gid=0#gid=0"

drive_auth(email = "hugoavmedeiros@gmail.com")

arquivo_ggafp_unificados <- drive_download(
  as_id(url_ggafp_unificados),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_ggafp_unificados <- excel_sheets(arquivo_ggafp_unificados$local_path)

plan_ggafp_unificados <- map2(
  abas_ggafp_unificados,
  seq_along(abas_ggafp_unificados),
  ~ if (.y == 1) {
    read_excel(arquivo_ggafp_unificados$local_path, sheet = .x)
  } else {
    read_excel(arquivo_ggafp_unificados$local_path, sheet = .x)
  }
)

# Nomeia a lista com os nomes das abas
names(plan_ggafp_unificados) <- abas_ggafp_unificados

dados_ggafp_unificados <- plan_ggafp_unificados$STATUS %>%
  janitor::row_to_names(row_number = 1, remove_row = TRUE) %>%
  #slice(-1) %>%  # remove a primeira linha (cabeçalho repetido)
  #setNames(as.character(unlist(plan_processos_itens$STATUS[1, ]))) %>%
  clean_names() %>%
  select(
    sei,
    grupo,
    #qtd_de_itens = x3,
    tempo_na_fase = tempo_na_etapa,
    demandante = demandante,
    data_de_inicio,
    setor = setor,
    status,
    observacoes = observacoes
  ) %>%
  mutate(
    status_cod = str_extract(status, "^[0-9]+"),
    status_cod = case_when(
      status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      status == "Arquivado (Reagentes)" ~ "0.1",
      status == "Execução de Compra Direta" ~ "0.2",
      is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
      TRUE ~ as.character(status_cod)
    ),
    status_cod = as.numeric(status_cod),
    is_ses_unificadas = "Não",
    base_origem = 'SES',
    #demandante = 'SES - SEAF',
    data_de_inicio = as.Date(as.numeric(data_de_inicio), origin = "1899-12-30")
  ) %>%
  left_join(
    sla_cum %>%
      select(
        -c(status)
      ), by = "status_cod") %>%
  filter(
    !is.na(sei)
  )

dados_ggafp_unificados %>% glimpse()

# HAM ----
url_ham_unificados <- "https://docs.google.com/spreadsheets/d/1fAgzbpR6RUVDk_Hk1dFl3xyRAzpqCoeQPrQY1O1CIJY/edit?gid=349470604#gid=349470604"

drive_auth(email = "hugoavmedeiros@gmail.com")

arquivo_ham_unificados <- drive_download(
  as_id(url_ham_unificados),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_ham_unificados <- excel_sheets(arquivo_ham_unificados$local_path)

plan_ham_unificados <- map2(
  abas_ham_unificados,
  seq_along(abas_ham_unificados),
  ~ if (.y == 1) {
    read_excel(arquivo_ham_unificados$local_path, sheet = .x)
  } else {
    read_excel(arquivo_ham_unificados$local_path, sheet = .x)
  }
)

# Nomeia a lista com os nomes das abas
names(plan_ham_unificados) <- abas_ham_unificados

dados_ham_unificados <- plan_ham_unificados$STATUS %>%
  janitor::row_to_names(row_number = 1, remove_row = TRUE) %>%
  clean_names() %>%
  select(
    sei,
    grupo,
    #qtd_de_itens = x3,
    tempo_na_fase = tempo_na_fase,
    demandante = demandante,
    data_de_inicio,
    setor = setor,
    status,
    observacoes = observacoes
  ) %>%
  mutate(
    status_cod = str_extract(status, "^[0-9]+"),
    status_cod = case_when(
      status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      status == "Arquivado (Reagentes)" ~ "0.1",
      status == "Execução de Compra Direta" ~ "0.2",
      is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
      TRUE ~ as.character(status_cod)
    ),
    status_cod = as.numeric(status_cod),
    is_ses_unificadas = "Não",
    base_origem = 'SES',
    #demandante = 'SES - SEAF',
    data_de_inicio = as.Date(as.numeric(data_de_inicio), origin = "1899-12-30")
  ) %>%
  left_join(
    sla_cum %>%
      select(
        -c(status)
      ), by = "status_cod") %>%
  filter(
    !is.na(sei)
  )

dados_ham_unificados %>% glimpse()

# HOF ----
url_hof_unificados <- "https://docs.google.com/spreadsheets/d/1FuUmkqS2a9QtJ5PxRVn_x_ZGfpUx0JSum321cBv3IuE/edit?gid=349470604#gid=349470604"

drive_auth(email = "hugoavmedeiros@gmail.com")

arquivo_hof_unificados <- drive_download(
  as_id(url_hof_unificados),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_hof_unificados <- excel_sheets(arquivo_hof_unificados$local_path)

plan_hof_unificados <- map2(
  abas_hof_unificados,
  seq_along(abas_hof_unificados),
  ~ if (.y == 1) {
    read_excel(arquivo_hof_unificados$local_path, sheet = .x)
  } else {
    read_excel(arquivo_hof_unificados$local_path, sheet = .x)
  }
)

# Nomeia a lista com os nomes das abas
names(plan_hof_unificados) <- abas_hof_unificados

dados_hof_unificados <- plan_hof_unificados$STATUS %>%
  janitor::row_to_names(row_number = 1, remove_row = TRUE) %>%
  clean_names() %>%
  select(
    sei,
    grupo,
    #qtd_de_itens = x3,
    tempo_na_fase = tempo_na_fase,
    demandante = demandante,
    data_de_inicio,
    setor = setor,
    status,
    observacoes = observacoes
  ) %>%
  mutate(
    status_cod = str_extract(status, "^[0-9]+"),
    status_cod = case_when(
      status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      status == "Arquivado (Reagentes)" ~ "0.1",
      status == "Execução de Compra Direta" ~ "0.2",
      is.na(status_cod) ~ "00",  # ou "0", dependendo do que deseja
      TRUE ~ as.character(status_cod)
    ),
    status_cod = as.numeric(status_cod),
    is_ses_unificadas = "Não",
    base_origem = 'SES',
    #demandante = 'SES - SEAF',
    data_de_inicio = as.Date(as.numeric(data_de_inicio), origin = "1899-12-30")
  ) %>%
  left_join(
    sla_cum %>%
      select(
        -c(status)
      ), by = "status_cod") %>%
  filter(
    !is.na(sei)
  )

dados_hof_unificados %>% glimpse()

# unir ----
dados_ggafp_unificados_2 <- dados_ggafp_unificados %>%
  bind_rows(
    dados_hof_unificados,
    dados_ham_unificados
  )

dados_ggafp_unificados %>% count()

dados_hof_unificados %>% count()

dados_ham_unificados %>% count()

dados_ggafp_unificados_2 %>% count()

dados_ggafp_unificados_2 %>% distinct(sei) %>% count()

dados_ggafp_unificados_2 %>% glimpse()
