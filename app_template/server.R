# Lógica do Servidor para Aplicativo Shiny
# Este script organiza e define a lógica do servidor para um aplicativo Shiny,
# incluindo autenticação do usuário e carregamento de módulos para diferentes funcionalidades.

# Definição da função do servidor
function(input, output, session) {
  #

  if (status == "adm") {
    auth <- secure_server(
      #check_credentials = check_creds_secure(db_config)
      check_credentials = check_creds_secure(app_name = app_nome),
    )
    
    auth %>% glimpse()
    
  } else {
    message("Modo desenvolvimento: sem autenticação")
    auth <- list(local = "SEPLAG", org = "SEPLAG", user = "Secretaria de Planejamento")
  }
  
  #### forçar troca de senha se senha_temp == TRUE ----
  observe({
    req(input$shinymanager_where == "application")
    
    usuario_logado <- auth$user
    
    query_temp <- glue::glue_sql("
      SELECT senha_temp FROM tbl_usuarios
      WHERE nome = {usuario_logado}
      LIMIT 1", .con = con_base)
    
    result <- DBI::dbGetQuery(con_base, query_temp)
    
    if (nrow(result) == 1 && isTRUE(result$senha_temp)) {
      showModal(modalDialog(
        title = "Trocar senha",
        easyClose = FALSE,
        fade = TRUE,
        passwordInput("nova_senha", "Nova senha", placeholder = "Digite a nova senha"),
        passwordInput("confirmar_senha", "Confirme a nova senha", placeholder = "Confirme a senha"),
        footer = tagList(
          actionButton("confirmar_troca", "Trocar senha", class = "btn-success")
        )
      ))
    }
  })
  
  #### lógica para troca de senha ----
  observeEvent(input$confirmar_troca, {
    req(input$nova_senha, input$confirmar_senha)
    
    if (input$nova_senha != input$confirmar_senha) {
      showNotification("As senhas não coincidem.", type = "error")
      return()
    }
    
    senha_hash <- password_store(input$nova_senha)
    
    query_update <- glue::glue_sql("
      UPDATE tbl_usuarios
      SET senha = {senha_hash}, 
      senha_temp = FALSE, 
      updated_at = CURRENT_TIMESTAMP
      WHERE nome = {auth$user}
    ", .con = con_base)
    
    result <- tryCatch({
      DBI::dbExecute(con_base, query_update)
      TRUE
    }, error = function(e) {
      showNotification(paste("Erro ao atualizar senha:", e$message), type = "error")
      FALSE
    })
    
    if (result) {
      removeModal()
      showNotification("Senha atualizada com sucesso!", type = "message")
    }
  })
  
  #### opções gerais ----
  shinyOptions(reconnect = TRUE)
  
  #### logout ----
  observeEvent(input$logout, {
    updateQueryString("?", mode = "replace")
    session$reload()
    showNotification("Você foi desconectado.", type = "message")
  })
  
  session$onSessionEnded(function() {
    message("Sessão encerrada.")
  })
  
  output$applocal <- renderUI({
    #message("Gerando applocal ...")
    tagList(
      tooltip(
        trigger = tagBadge(
          auth$local, paste0(
            "https://www.", str_to_lower(auth$local), ".pe.gov.br"), status = "bg-white", style = "padding:2px"),
        paste0(auth$user, " – ", auth$local)
      )
    )
  })

  server_Safety("safety")
  
  server_Ceos("ceos")
  
  server_Liquidacao("liquidacao")

}
