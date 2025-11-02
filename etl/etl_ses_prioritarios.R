# SES - prioritários ----
url_safety_supply <- "https://docs.google.com/spreadsheets/d/1f2HjSbYWYGrqEaaNg5MAezd6FXQDepgv/edit"

#drive_auth(email = "hugoavmedeiros@gmail.com")

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

dados_safety_supply %>%
  qs::qsave('app_suprimentos_ses/data/df_safety_supply.qs')
