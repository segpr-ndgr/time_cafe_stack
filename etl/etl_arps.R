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

config_path <- file.path("data", "config.yml")

config_tokens <- config::get('tokens', file = config_path)

bdpe_store_token(
  'peintegrado_consumo_adesao_arp',
  config_tokens$peintegrado_consumo_adesao_arp)

bdpe_store_token(
  'peintegrado_todas_atas_registro_preco_v2',
  config_tokens$peintegrado_todas_atas_registro_preco_v2)

peintegrado_consumo_adesao_arp <-BigDataPE::bdpe_fetch_data(
  base_name = "peintegrado_consumo_adesao_arp",
  verbosity = 1)  %>%
  clean_names()

peintegrado_consumo_adesao_arp %>%
  writexl::write_xlsx('data/peintegrado_consumo_adesao_arp.xlsx')

peintegrado_todas_atas_registro_preco_v2 <-BigDataPE::bdpe_fetch_data(
  base_name = "peintegrado_todas_atas_registro_preco_v2",
  verbosity = 1)  %>%
  clean_names()

peintegrado_todas_atas_registro_preco_v2 %>%
  writexl::write_xlsx('data/peintegrado_todas_atas_registro_preco_v2.xlsx')
