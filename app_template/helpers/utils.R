gerar_senha <- function() {
  # Gera caracteres aleatórios de cada tipo separadamente
  minusculas <- stringi::stri_rand_strings(1, 2, "[a-z]")   # 2 letras minúsculas
  maiusculas <- stringi::stri_rand_strings(1, 2, "[A-Z]")   # 2 letras maiúsculas
  numeros <- stringi::stri_rand_strings(1, 2, "[0-9]")      # 2 números
  extras <- stringi::stri_rand_strings(1, 2, "[a-zA-Z0-9]") # 2 caracteres variados
  caractere_especial <- sample(strsplit("!@#$%^&*+", "")[[1]], 1)  # 1 caractere especial aleatório
  
  # Junta todos os caracteres e embaralha a sequência
  senha <- paste0(sample(c(minusculas, maiusculas, numeros, extras, caractere_especial)), collapse = "")
  
  return(senha)
}

# check_creds_secure <- function(db_conf, tabela = "tbl_usuarios") {
#   function(user, password) {
#     
#     app_name <- config::get("app_name")
#     
#     con <- dbConnect(
#       Postgres(),
#       dbname = db_conf$dbname,
#       host = db_conf$host,
#       port = db_conf$port,
#       user = db_conf$user,
#       password = db_conf$password
#     )
#     
#     # Buscar usuário
#     query <- glue_sql("SELECT * FROM {`tabela`} WHERE nome = {user}", .con = con)
#     user_data <- tryCatch({
#       dbGetQuery(con, query)
#     }, error = function(e) {
#       dbDisconnect(con)
#       return(tibble())
#     })
#     
#     # Validar senha e se app está entre os autorizados
#     valid <- FALSE
#     if (nrow(user_data) == 1 && password_verify(user_data$senha[1], password)) {
#       # Verifica se o app está na lista de apps do usuário
#       lista_apps <- strsplit(user_data$app[1], ";")[[1]] |> trimws()
#       valid <- app_name %in% lista_apps
#     }
#     
#     # Dados do log
#     secretaria <- if (nrow(user_data) > 0) user_data$secretaria[1] else NA
#     url_protocol <- tryCatch(session$clientData$url_protocol, error = function(e) NA)
#     url_hostname <- tryCatch(session$clientData$url_hostname, error = function(e) NA)
#     url_port     <- tryCatch(session$clientData$url_port, error = function(e) NA)
#     user_agent   <- tryCatch(session$clientData$HTTP_USER_AGENT, error = function(e) NA)
#     ip_address   <- tryCatch(session$request$REMOTE_ADDR, error = function(e) NA)
#     
#     log_entry <- tibble::tibble(
#       nome = user,
#       app = app_name,
#       secretaria = secretaria,
#       login_timestamp = Sys.time(),
#       login_success = valid,
#       url_protocol = url_protocol,
#       url_hostname = url_hostname,
#       url_port = url_port,
#       user_agent = user_agent,
#       ip_address = ip_address
#     )
#     
#     # Escreve o log no banco
#     tryCatch({
#       dbWriteTable(con, "tbl_logs", log_entry, append = TRUE)
#     }, error = function(e) {
#       message("Erro ao registrar log: ", e$message)
#     })
#     
#     # Fechar conexão
#     dbDisconnect(con)
#     
#     # Retorno para o shiny::secure_server
#     if (valid) {
#       return(list(result = TRUE, user_info = list(user = user)))
#     } else {
#       return(list(result = FALSE))
#     }
#   }
# }

# check_creds_secure <- function(tabela = "tbl_usuarios") {
#   function(user, password) {
#     
#     app_name <- config::get("app_name")
#     
#     # Buscar usuário usando o con_base existente
#     query <- glue::glue_sql("SELECT * FROM {`tabela`} WHERE nome = {user}", .con = con_base)
#     user_data <- tryCatch({
#       DBI::dbGetQuery(con_base, query)
#     }, error = function(e) {
#       message("Erro ao buscar usuário: ", e$message)
#       return(tibble())
#     })
#     
#     # Validar senha e se app está entre os autorizados
#     valid <- FALSE
#     if (nrow(user_data) == 1 && password_verify(user_data$senha[1], password)) {
#       lista_apps <- strsplit(user_data$app[1], ";")[[1]] |> trimws()
#       valid <- app_name %in% lista_apps
#     }
#     
#     # Dados do log
#     secretaria <- if (nrow(user_data) > 0) user_data$secretaria[1] else NA
#     url_protocol <- tryCatch(session$clientData$url_protocol, error = function(e) NA)
#     url_hostname <- tryCatch(session$clientData$url_hostname, error = function(e) NA)
#     url_port     <- tryCatch(session$clientData$url_port, error = function(e) NA)
#     user_agent   <- tryCatch(session$clientData$HTTP_USER_AGENT, error = function(e) NA)
#     ip_address   <- tryCatch(session$request$REMOTE_ADDR, error = function(e) NA)
#     
#     log_entry <- tibble::tibble(
#       nome = user,
#       app = app_name,
#       secretaria = secretaria,
#       login_timestamp = Sys.time(),
#       login_success = valid,
#       url_protocol = url_protocol,
#       url_hostname = url_hostname,
#       url_port = url_port,
#       user_agent = user_agent,
#       ip_address = ip_address
#     )
#     
#     # Escreve o log no banco
#     tryCatch({
#       DBI::dbWriteTable(con_base, "tbl_logs", log_entry, append = TRUE)
#     }, error = function(e) {
#       message("Erro ao registrar log: ", e$message)
#     })
#     
#     # NÃO precisa desconectar pool (o pool gerencia isso)
#     
#     # Retorno para o shiny::secure_server
#     if (valid) {
#       return(list(result = TRUE, user_info = list(user = user)))
#     } else {
#       return(list(result = FALSE))
#     }
#   }
# }

check_creds_secure <- function(tabela = "tbl_usuarios", app_name) {
  function(user, password) {
    
    # Buscar usuário
    query <- glue::glue_sql("SELECT * FROM {`tabela`} WHERE nome = {user}", .con = con_base)
    user_data <- tryCatch({
      DBI::dbGetQuery(con_base, query)
    }, error = function(e) {
      message("Erro ao buscar usuário: ", e$message)
      return(tibble())
    })
    
    # Validar senha e se app está entre os autorizados
    valid <- FALSE
    if (nrow(user_data) == 1 && password_verify(user_data$senha[1], password)) {
      lista_apps <- strsplit(user_data$app[1], ";")[[1]] |> trimws()
      valid <- app_name %in% lista_apps
    }
    
    # Dados do log
    secretaria <- if (nrow(user_data) > 0) user_data$secretaria[1] else NA
    url_protocol <- tryCatch(session$clientData$url_protocol, error = function(e) NA)
    url_hostname <- tryCatch(session$clientData$url_hostname, error = function(e) NA)
    url_port     <- tryCatch(session$clientData$url_port, error = function(e) NA)
    user_agent   <- tryCatch(session$clientData$HTTP_USER_AGENT, error = function(e) NA)
    ip_address   <- tryCatch(session$request$REMOTE_ADDR, error = function(e) NA)
    
    log_entry <- tibble::tibble(
      nome = user,
      app = app_name,
      secretaria = secretaria,
      login_timestamp = Sys.time(),
      login_success = valid,
      url_protocol = url_protocol,
      url_hostname = url_hostname,
      url_port = url_port,
      user_agent = user_agent,
      ip_address = ip_address
    )
    
    tryCatch({
      DBI::dbWriteTable(con_base, "tbl_logs", log_entry, append = TRUE)
    }, error = function(e) {
      message("Erro ao registrar log: ", e$message)
    })
    
    if (valid) {
      return(list(result = TRUE, user_info = list(user = user)))
    } else {
      return(list(result = FALSE))
    }
  }
}

find_owncloud_path <- function() {
  home <- normalizePath("~") # Obtém o diretório home do usuário
  
  # Possíveis locais onde o OwnCloud pode estar instalado
  possible_paths <- c(
    file.path(home, "ownCloud"),
    file.path(home, "Owncloud"),
    file.path(home, "OwnCloud"),
    file.path("C:/Users", Sys.getenv("USERNAME"), "ownCloud"),
    file.path("C:/Users", Sys.getenv("USERNAME"), "OwnCloud"),
    file.path("/mnt/owncloud"),
    file.path("/Volumes/ownCloud")
  )
  
  # Verifica qual diretório existe e retorna o primeiro encontrado
  owncloud_path <- possible_paths[file.exists(possible_paths)]
  
  if (length(owncloud_path) > 0) {
    return(owncloud_path[1])  # Retorna o primeiro caminho encontrado
  } else {
    stop("OwnCloud não encontrado! Verifique se está instalado e sincronizado.")
  }
}

