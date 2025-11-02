# SAD - geral ----
url_geral_sad <- "https://docs.google.com/spreadsheets/d/1tvNcspcU2bM4JYwWWJk7evTeOcdH6mfX3iCmz9yEJ0M/edit?gid=349470604#gid=349470604"

arquivo_geral_sad <- drive_download(
  as_id(url_geral_sad),
  path = tempfile(fileext = ".xlsx"),
  overwrite = TRUE)

abas_geral_sad <- excel_sheets(arquivo_geral_sad$local_path)

plan_geral_sad <- map2(
  abas_geral_sad,
  seq_along(abas_geral_sad),
  ~ if (.y == 1) {
    read_excel(arquivo_geral_sad$local_path, sheet = .x, skip = 1)
  } else {
    read_excel(arquivo_geral_sad$local_path, sheet = .x)
  }
)

names(plan_geral_sad) <- abas_geral_sad

dados_geral_sad <- plan_geral_sad$CENTRAL %>%
  janitor::row_to_names(row_number = 1, remove_row = TRUE) %>%
  clean_names() %>%
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
    data_de_inicio = as.Date(as.numeric(data_de_inicio), origin = "1899-12-30"),
    demandante = demandante
  ) %>%
  left_join(
    sla_cum %>%
      select(-c(status)),
    by = "status_cod") %>%
  mutate(
    tempo_na_fase = as.integer(tempo_na_fase),
    tempo_na_fase = tidyr::replace_na(tempo_na_fase, 0),
    demandante    = tidyr::replace_na(demandante, "Outros")
  )

dados_geral_sad %>% glimpse()

dados_geral_sad %>% count()

dados_geral_sad %>% count(demandante)

dados_geral_sad %>% count(is_ses_unificadas)

dados_geral_sad_2 <- dados_geral_sad %>%
  mutate(
    progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2),
    dias_corridos = pmax(0, as.integer(Sys.Date() - as.Date(data_de_inicio)))
  )

dados_geral_sad_2 %>% count()

# 3) Junte ao seu data frame original (precisa ter status_cod e data_de_inicio)
dados_geral_sad_3 <- dados_geral_sad_2 %>%
  mutate(
    deadline_date   = as_date(data_de_inicio) + days(coalesce(cum_sla, 0)),
    atraso          = Sys.Date() > deadline_date,
    dias_em_atraso  = pmax(0, as.integer(Sys.Date() - deadline_date)),

    demandante = case_when(
      is.na(demandante) ~ "Outros",
      demandante == "Não se aplica" ~ "Outros",
      TRUE ~ demandante
    ),

    status = str_replace(status, "05\\. Resposta cota SAD", "05. Resposta Cota SAD")
  )

dados_geral_sad_3 %>%
  filter(
    !is.na(sei)
  ) %>%
  qs::qsave('app_suprimentos_ses/data/dados_geral_sad.qs')
