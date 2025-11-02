library(DBI)
library(dplyr)
library(glue)
library(odbc)
library(RPostgres)

odbc::odbcListDrivers()

config_path <- file.path("data", "config.yml")

config_mv <- config::get('mv', file = config_path)

con <- dbConnect(
  odbc(),
  Driver = "Oracle in instantclient_19_22",
  DBQ = "10.18.56.10:1521/PRD",
  UID = config_mv$usr,
  PWD = config_mv$pwd
)


