ui_Safety <- function(id) {
  ns <- NS(id)

  tagList(

    accordion(
      class = "accordion-filtros",
      style = "border: none; padding: 0px;",
      open = FALSE,
      accordion_panel(
        style = "border: none;",
        title = HTML("<strong>Filtros</strong>"),
        icon = bs_icon("filter-square"),
        card_body(
          style = "border: none; padding: 0px; font-size: 12px;",
          layout_column_wrap(
            width = 1/5,
            heights_equal = "all",
            
            dateRangeInput(
              ns("filtro_periodo"), 
              "Selecione um período",
              start = data_min,
              end = data_max,
              min = data_min,
              max = data_max,
              format = "dd/mm/yyyy",
              separator = 'a',
              language = "pt-BR"),
            
            selectInput(
              ns("filtro_status"),
              label = tags$span(
                "Status", 
                class = "label-custom"),
              choices = lista_status,
              selectize = TRUE,
              multiple = TRUE
            ),
            
            selectInput(
              ns("filtro_demandante"),
              label = tags$span(
                "Demandante", 
                class = "label-custom"),
              choices = lista_demandante,
              selectize = TRUE,
              multiple = TRUE
            ),
            
            numericInput(
              inputId = ns("filtro_tempo_fase"),
              label = "Máximo de dias na fase",
              value = 365,
              min = 0,
              max = 365,
              step = 1
            )
            
          )
        ),

        div(style = "height: 10px;"),
        
        card_footer(
          layout_columns(
            col_widths = c(-3, 3, 3, -3) ,
            gap = "10px",
            style = "text-align: center;", 
            actionButton(
              inputId = ns("btn_limpar"),
              label = "Limpar filtros",
              icon = icon("eraser"),
              class = "btn-limpar"
            ),
            actionButton(
              inputId = ns("btn_aplicar"),
              label = "Aplicar filtros",
              icon = icon("square-check"),
              class = "btn-aplicar",
              style = "margin-right: 10px;"
            )
          ) 
        ) 
      )
    ),

    navset_card_pill(
      id = ns("aba"),
      
      nav_panel(
        "SAD - SES",
        card(
          full_screen = TRUE,
          
          card_body(
            layout_column_wrap(
              width = 1/4, gap = "10px", style = "text-align: center;",
              uiOutput(ns("kpi_demandantes")),
              uiOutput(ns("kpi_processos")),
              uiOutput(ns("kpi_progresso_medio")),
              uiOutput(ns("kpi_progresso_medio_2")),
              
              uiOutput(ns("kpi_sad")),
              uiOutput(ns("kpi_outros")),
              uiOutput(ns("kpi_pge")),
              uiOutput(ns("kpi_homologados"))
            ),
            br(),
            plotlyOutput(ns("plot_centralizadas_sad")),
            br(),
            plotlyOutput(ns("plot_demandantes_progresso")),
            br(),
            DT::DTOutput(ns("tbl_centralizadas_sad")),
            br()
          )
        )
      ),
      
      nav_panel(
        "SAD - Central",
        card(
          full_screen = TRUE,
          
          card_body(
            layout_column_wrap(
              width = 1/4, gap = "10px", style = "text-align: center;",
              uiOutput(ns("kpi_demandantes_central")),
              uiOutput(ns("kpi_processos_central")),
              uiOutput(ns("kpi_progresso_medio_central")),
              uiOutput(ns("kpi_progresso_medio_2_central")),
              
              uiOutput(ns("kpi_sad_central")),
              uiOutput(ns("kpi_outros_central")),
              uiOutput(ns("kpi_pge_central")),
              uiOutput(ns("kpi_homologados_central"))
            ),
            br(),
            plotlyOutput(ns("plot_centralizadas_central")),
            br(),
            plotlyOutput(ns("plot_demandantes_progresso_central")),
            br(),
            DT::DTOutput(ns("tbl_centralizadas_sad_central")),
            br()
          )
        )
      ),
      
      nav_panel(
        "SES - Unificadas",
        card(
          full_screen = TRUE,
          
          card_body(
            
            layout_column_wrap(
              width = 1/4, 
              gap = "10px", 
              style = "text-align: center;",
              
              uiOutput(ns("kpi_demandantes_ses")),
              uiOutput(ns("kpi_processos_ses")),
              uiOutput(ns("kpi_homologados_ses")),
              uiOutput(ns("kpi_progresso_medio_ses"))
            ),
            
            plotlyOutput(ns("plot_unificadas_ses")),
            reactableOutput(ns("tbl_status_geral"))
          )
        )
      ),
      
      nav_panel(
        "SES - Prioritários",
        card(
          style = "border:none;",
          
          card_body(
            layout_columns(
              fill = TRUE,
              kpi_card(title = "Total",       icon_src = "images/product.svg",          output_id = "box_0b", ns = ns, bg_header_color = "#004080", bg_aux_color = "#0077b6"),
              kpi_card(title = "Concluídos",  icon_src = "images/product_green.svg",    output_id = "box_1b", ns = ns, bg_header_color = "#006400", bg_aux_color = "#006400"),
              kpi_card(title = "Em ação",     icon_src = "images/product_blue.svg",     output_id = "box_2b", ns = ns, bg_header_color = "#0000FF", bg_aux_color = "#0000FF"),
              kpi_card(title = "Pendente",    icon_src = "images/product_orange.svg",   output_id = "box_3b", ns = ns, bg_header_color = "#FFA500", bg_aux_color = "#FFA500"),
              kpi_card(title = "Cancelados",  icon_src = "images/product_grey.svg",     output_id = "box_4b", ns = ns, bg_header_color = "#6c757d", bg_aux_color = "#495057")
            )
          )
        ),
        card(
          full_screen = TRUE,
          card_body(reactableOutput(ns("tbl_status_detalhado")))
        )
      )
    )

  )
}

server_Safety <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      badge_spacing_style <- "margin:2px; padding: 2px; display:inline-block;"

      # eventos ----
      ## filtros ----
      valores_filtros <- reactiveValues(
        filtro_periodo = c(data_min, data_max),
        filtro_status = NULL,
        filtro_demandante = NULL,
        filtro_tempo_fase = 365
      )
      
      ## aplicar ----
      observeEvent(input$btn_aplicar, {
        valores_filtros$filtro_periodo <- input$filtro_periodo
        valores_filtros$filtro_status <- input$filtro_status
        valores_filtros$filtro_demandante <- input$filtro_demandante
        valores_filtros$filtro_tempo_fase <- input$filtro_tempo_fase
      })
      
      ## limpar ----
      observeEvent(input$btn_limpar, {
      
        valores_filtros$filtro_periodo <- c(data_min, data_max)
        valores_filtros$filtro_status <- NULL
        valores_filtros$filtro_demandante <- NULL
        valores_filtros$filtro_tempo_fase <- 365
        
      updateDateRangeInput(session, "filtro_periodo", start = data_min, end = data_max)
      updateSelectInput(session, "filtro_status", selected = "")
      updateSelectInput(session, "filtro_demandante", selected = "")
      updateNumericInput(session, "filtro_tempo_fase", value = 365)
      })
      
      # reativos ----
      ## ses - unificadas ----
      rct_unificadas_ses <- reactive({
        filtro_periodo <- valores_filtros$filtro_periodo
        
        dados_unificados_ses %>%
          filter(
            
            status_cod >= 1, 
            
            is.null(filtro_periodo) |
              ( (data_de_inicio >= filtro_periodo[1] & data_de_inicio <= filtro_periodo[2]) | is.na(data_de_inicio) ),
            
            is.null(valores_filtros$filtro_status) | status %in% valores_filtros$filtro_status
          )
      })
      
      ## sad - centralizadas ----
      rct_centralizados_sad <- reactive({
        
        filtro_periodo <- valores_filtros$filtro_periodo
        
        dados_centralizados_sad %>%
          filter(
            
            is.na(tempo_na_fase) | tempo_na_fase <= valores_filtros$filtro_tempo_fase,
            
            is.null(filtro_periodo) |
              ( (data_de_inicio >= filtro_periodo[1] & data_de_inicio <= filtro_periodo[2]) | is.na(data_de_inicio) ),
            
            is.null(valores_filtros$filtro_demandante) | demandante %in% valores_filtros$filtro_demandante,
            
            is.null(valores_filtros$filtro_status) | status %in% valores_filtros$filtro_status
            
          )
        
      })
      
      rct_geral_sad <- reactive({
        req(dados_geral_sad)
        
        filtro_periodo <- valores_filtros$filtro_periodo
        
        dados_geral_sad %>%
          dplyr::filter(
            is.na(tempo_na_fase) | tempo_na_fase <= valores_filtros$filtro_tempo_fase,
            
            is.null(filtro_periodo) |
              ((data_de_inicio >= filtro_periodo[1] & data_de_inicio <= filtro_periodo[2]) | is.na(data_de_inicio)),
            
            is.null(valores_filtros$filtro_demandante) | demandante %in% valores_filtros$filtro_demandante,
            
            is.null(valores_filtros$filtro_status) | status %in% valores_filtros$filtro_status
          )
      })
      
      supply_totais <- reactive({
        df <- df_safety_supply
        tibble::tibble(
          total     = nrow(df),
          concluido = sum(df$status == "CONCLUÍDO", na.rm = TRUE),
          em_acao   = sum(df$status == "EM AÇÃO",   na.rm = TRUE),
          pendente  = sum(df$status == "PENDENTE",  na.rm = TRUE),
          arquivcan = sum(df$status %in% c("ARQUIVADO","CANCELADO"), na.rm = TRUE)
        )
      })
      
      rct_resumo_demandante <- reactive({
        df <- rct_centralizados_sad()
        req(nrow(df))
        
        if (!"progress_pct" %in% names(df)) {
          df <- dplyr::mutate(
            df,
            status_num   = suppressWarnings(as.numeric(status_cod)),
            progress_pct = round(pmin(100, pmax(0, status_num/17*100)), 2)
          )
        }
        
        df %>%
          dplyr::filter(!is.na(demandante)) %>%
          dplyr::summarise(
            .by = demandante,
            processos = dplyr::n_distinct(sei),
            progresso_medio = mean(progress_pct, na.rm = TRUE)
          ) %>%
          dplyr::mutate(
            progresso_medio = round(progresso_medio, 2),
            progresso_fmt   = sprintf('%.2f%%', progresso_medio)
          ) %>%
          dplyr::arrange(dplyr::desc(processos), dplyr::desc(progresso_medio))
      })
      
      # objetos ----
      ## caixas ----
      output$box_0b <- renderUI({
        s <- supply_totais()
        valor_formatado <- format(s$total, big.mark = ".", decimal.mark = ",", scientific = FALSE)
        
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar' style='visibility: hidden;'>-</div>
  "))
      })
      
      output$box_1b <- renderUI({
        s <- supply_totais()
        valor <- s$concluido
        total <- s$total
        HTML(glue::glue("
    <div class='kpi-value'>{format(valor, big.mark='.', decimal.mark=',', scientific=FALSE)}</div>
    <div class='kpi-auxiliar'>{scales::percent(valor/total, accuracy=0.1)}</div>
    do Total
  "))
      })
      
      output$box_2b <- renderUI({
        s <- supply_totais()
        
        valor  <- s$em_acao
        total  <- s$total
        pct    <- if (total > 0) valor / total else NA_real_
        
        valor_formatado <- format(valor, big.mark = ".", decimal.mark = ",", scientific = FALSE)
        percentual_formatado <- if (is.na(pct)) "--" else scales::percent(pct, accuracy = 0.1)
        
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar'>{percentual_formatado}</div>
    do Total
  "))
      })
      
      output$box_3b <- renderUI({
        s <- supply_totais()   # total, concluido, em_acao, pendente, arquivcan
        valor <- s$pendente
        total <- s$total
        pct   <- if (total > 0) valor / total else NA_real_
        
        valor_formatado      <- format(valor, big.mark = ".", decimal.mark = ",", scientific = FALSE)
        percentual_formatado <- if (is.na(pct)) "--" else scales::percent(pct, accuracy = 0.1)
        
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar'>{percentual_formatado}</div>
    do Total
  "))
      })
      
      output$box_4b <- renderUI({
        s <- supply_totais()   # total, concluido, em_acao, pendente, arquivcan
        valor <- s$arquivcan
        total <- s$total
        pct   <- if (total > 0) valor / total else NA_real_
        
        valor_formatado      <- format(valor, big.mark = ".", decimal.mark = ",", scientific = FALSE)
        percentual_formatado <- if (is.na(pct)) "--" else scales::percent(pct, accuracy = 0.1)
        
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar'>{percentual_formatado}</div>
    do Total
  "))
      })
      
      ### sad centralizadas % ----
    #   output$kpi_demandantes <- renderUI({
    #     df_info <- rct_centralizados_sad()
    #     
    #     # total <- df_info %>%
    #     #   distinct(demandante) %>%
    #     #   nrow()
    #     
    #     total <- dplyr::n_distinct(df_info$demandante)
    #     
    #     htmltools::div(
    #       style = "
    #   background: #fff;
    #   border-radius: 12px;
    #   box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    #   padding: 20px;
    #   text-align: center;
    #   width: 220px;
    # ",
    #       htmltools::h4("Demandantes", style = "margin:0; font-size:16px; color:#555;"),
    #       htmltools::h2(
    #         format(total, big.mark = ".", decimal.mark = ","), 
    #         style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#000000;"  # verde sugerido
    #       ),
    #       htmltools::div(
    #         "Total de demandantes",
    #         style = "margin-top:2px; font-size:11px; color:#999;"
    #       )
    #     )
    #   })
      
      output$kpi_demandantes <- renderUI({
        df_info <- rct_centralizados_sad()
        total <- dplyr::n_distinct(df_info$demandante)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Demandantes", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(total, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#000000;"
          ),
          htmltools::div(
            "Total de demandantes",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
    #   output$kpi_processos <- renderUI({
    #     df_info <- rct_centralizados_sad()
    #     
    #     # total <- df_info %>%
    #     #   distinct(sei) %>%
    #     #   nrow()
    #     
    #     total <- dplyr::n_distinct(df_info$sei)
    #     
    #     htmltools::div(
    #       style = "
    #   background: #fff;
    #   border-radius: 12px;
    #   box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    #   padding: 20px;
    #   text-align: center;
    #   width: 220px;
    # ",
    #       htmltools::h4("Processos", style = "margin:0; font-size:16px; color:#555;"),
    #       htmltools::h2(
    #         format(total, big.mark = ".", decimal.mark = ","), 
    #         style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#000000;"  # verde sugerido
    #       ),
    #       htmltools::div(
    #         "Total de SEIs",
    #         style = "margin-top:2px; font-size:11px; color:#999;"
    #       )
    #     )
    #   })
      
      output$kpi_processos <- renderUI({
        df_info <- rct_centralizados_sad()
        total <- dplyr::n_distinct(df_info$sei)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(total, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#000000;"
          ),
          htmltools::div(
            "Total de SEIs",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
    #   output$kpi_homologados <- renderUI({
    #     df_info <- rct_centralizados_sad() %>%
    #       mutate(
    #         # Coerção segura para numérico mesmo se vier como factor/char
    #         status_num = suppressWarnings(as.numeric(as.character(status_cod)))
    #       )
    #     
    #     total_homologados <- sum(df_info$status_num >= 15, na.rm = TRUE)
    #     
    #     htmltools::div(
    #       style = "
    #   background: #fff;
    #   border-radius: 12px;
    #   box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    #   padding: 20px;
    #   text-align: center;
    #   width: 220px;
    # ",
    #       htmltools::h4("Homologados", style = "margin:0; font-size:16px; color:#555;"),
    #       htmltools::h2(
    #         format(total_homologados, big.mark = ".", decimal.mark = ","), 
    #         style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#4f9d69;"  # verde sugerido
    #       ),
    #       htmltools::div(
    #         "Fase \u2265 15",
    #         style = "margin-top:2px; font-size:11px; color:#999;"
    #       )
    #     )
    #   })
      
      output$kpi_sad <- renderUI({
        tot_sad <- rct_centralizados_sad() %>%
          dplyr::filter(!is.na(status)) %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado, responsavel == "SAD") %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(tot_sad) == 0) tot_sad <- 0L
        
        total_geral <- rct_geral_sad() %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(total_geral) == 0) total_geral <- 0L
        
        pct_txt <- if (total_geral > 0)
          scales::percent(tot_sad / total_geral, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos na SAD", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(tot_sad, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#66a5ad;"
          ),
          htmltools::div(
            paste(pct_txt, "do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_outros <- renderUI({
        tot_ses <- rct_centralizados_sad() %>%
          dplyr::filter(!is.na(status)) %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado, responsavel == "SES") %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(tot_ses) == 0) tot_ses <- 0L
        
        total_geral <- rct_geral_sad() %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(total_geral) == 0) total_geral <- 0L
        
        pct_txt <- if (total_geral > 0)
          scales::percent(tot_ses / total_geral, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos na SES", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(tot_ses, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#005372;"
          ),
          htmltools::div(
            paste(pct_txt, "do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_pge <- renderUI({
        tot_pge <- rct_centralizados_sad() %>%
          dplyr::filter(!is.na(status)) %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado, responsavel == "PGE") %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(tot_pge) == 0) tot_pge <- 0L
        
        total_geral <- rct_geral_sad() %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(total_geral) == 0) total_geral <- 0L
        
        pct_txt <- if (total_geral > 0)
          scales::percent(tot_pge / total_geral, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos na PGE", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(tot_pge, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#c94f7c;"
          ),
          htmltools::div(
            paste(pct_txt, "do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_homologados <- renderUI({
        df_info <- rct_centralizados_sad() %>%
          dplyr::mutate(status_num = suppressWarnings(as.numeric(as.character(status_cod))))
        
        total <- dplyr::n_distinct(df_info$sei)
        total_homologados <- sum(df_info$status_num >= 15, na.rm = TRUE)
        pct_txt <- if (total > 0)
          scales::percent(total_homologados / total, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos Homologados", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(total_homologados, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#4f9d69;"
          ),
          htmltools::div(
            paste0("Fase \u2265 15 \u2022 ", pct_txt, " do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
    #   output$kpi_progresso_medio <- renderUI({
    #     df_info <- rct_centralizados_sad() %>%
    #       mutate(progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2))
    #     
    #     media <- mean(df_info$progress_pct, na.rm = TRUE)
    #     
    #     htmltools::div(
    #       style = "
    #   background: #fff;
    #   border-radius: 12px;
    #   box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    #   padding: 20px;
    #   text-align: center;
    #   width: 220px;
    # ",
    #       htmltools::h4("Progresso Médio", style = "margin:0; font-size:16px; color:#555;"),
    #       htmltools::h2(
    #         sprintf('%.2f%%', media), 
    #         style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#005372;"
    #       ),
    #       htmltools::div(
    #         "considerando 15 fases de status",
    #         style = "margin-top:6px; font-size:12px; color:#777;"
    #       )
    #     )
    #   })
      
      output$kpi_progresso_medio <- renderUI({
        df_info <- rct_centralizados_sad() %>%
          dplyr::mutate(
            progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2)
          )
        
        media <- mean(df_info$progress_pct, na.rm = TRUE)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Progresso Médio", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(sprintf('%.2f%%', media),
                        style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#005372;"),
          htmltools::div(
            "Todos os processos",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
    #   output$kpi_progresso_medio_2 <- renderUI({
    #     df_info <- rct_centralizados_sad() %>%
    #       mutate(
    #         #data_de_inicio  = as.Date(data_de_inicio),
    #         progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2)
    #       ) %>%
    #       filter(!is.na(data_de_inicio), data_de_inicio <= as.Date("2025-08-19"))
    #     
    #     media <- mean(df_info$progress_pct, na.rm = TRUE)
    #     
    #     htmltools::div(
    #       style = "
    #   background: #fff;
    #   border-radius: 12px;
    #   box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    #   padding: 20px;
    #   text-align: center;
    #   width: 220px;
    # ",
    #       htmltools::h4("Progresso Médio", style = "margin:0; font-size:16px; color:#555;"),
    #       htmltools::h2(
    #         sprintf('%.2f%%', media),
    #         style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#005372;"
    #       ),
    #       htmltools::div(
    #         "iniciados até 19/08/2025",
    #         style = "margin-top:6px; font-size:12px; color:#777;"
    #       )
    #     )
    #   })
      
      output$kpi_progresso_medio_2 <- renderUI({
        df_info <- rct_centralizados_sad() %>%
          dplyr::mutate(
            progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2)
          ) %>%
          dplyr::filter(!is.na(data_de_inicio), data_de_inicio <= as.Date("2025-08-19"))
        
        media <- mean(df_info$progress_pct, na.rm = TRUE)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Progresso Médio", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(sprintf('%.2f%%', media),
                        style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#005372;"),
          htmltools::div(
            "iniciados até 19/08/2025",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      ### sad centralizadas no prazo ----
      output$kpi_no_prazo <- renderUI({
        
        df <- rct_centralizados_sad()
        
        n_total   <- sum(!is.na(df$atraso))
        n_ok      <- sum(df$atraso == FALSE, na.rm = TRUE)
        pct_ok    <- if (n_total > 0) round(n_ok / n_total * 100, 2) else NA_real_
        
        htmltools::div(
          style = "
      background:#fff; border-radius:12px; box-shadow:0 2px 6px rgba(0,0,0,.15);
      padding:20px; text-align:center; width:220px;
    ",
          htmltools::h4("% no Prazo", style = "margin:0; font-size:16px; color:#555;"),
          htmltools::h2(
            if (is.na(pct_ok)) "--" else sprintf('%.2f%%', pct_ok),
            style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#28a745;"
          ),
          htmltools::div(
            sprintf("%d de %d itens em dia", n_ok, n_total),
            style = "margin-top:6px; font-size:12px; color:#777;"
          )
        )
      })
      
      ### ses unificadas % ----
      output$kpi_demandantes_ses <- renderUI({
        df_info <- rct_unificadas_ses()
        
        total <- dplyr::n_distinct(df_info$demandante)
       
        htmltools::div(
          style = "
      background: #fff;
      border-radius: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      padding: 20px;
      text-align: center;
      width: 220px;
    ",
          htmltools::h4("Demandantes", style = "margin:0; font-size:16px; color:#555;"),
          htmltools::h2(
            format(total, big.mark = ".", decimal.mark = ","), 
            style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#000000;"  # verde sugerido
          ),
          htmltools::div(
            "Total de demandantes",
            style = "margin-top:2px; font-size:11px; color:#999;"
          )
        )
      })
      
      output$kpi_processos_ses <- renderUI({
        df_info <- rct_unificadas_ses()
        
        total <- dplyr::n_distinct(df_info$sei)
        
        htmltools::div(
          style = "
      background: #fff;
      border-radius: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      padding: 20px;
      text-align: center;
      width: 220px;
    ",
          htmltools::h4("Processos", style = "margin:0; font-size:16px; color:#555;"),
          htmltools::h2(
            format(total, big.mark = ".", decimal.mark = ","), 
            style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#000000;"  # verde sugerido
          ),
          htmltools::div(
            "Total de SEIs",
            style = "margin-top:2px; font-size:11px; color:#999;"
          )
        )
      })
      
      output$kpi_homologados_ses <- renderUI({
        df_info <- rct_unificadas_ses() %>%
          mutate(
            # Coerção segura para numérico mesmo se vier como factor/char
            status_num = suppressWarnings(as.numeric(as.character(status_cod)))
          )
        
        total_homologados <- sum(df_info$status_num >= 15, na.rm = TRUE)
        
        htmltools::div(
          style = "
      background: #fff;
      border-radius: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      padding: 20px;
      text-align: center;
      width: 220px;
    ",
          htmltools::h4("Homologados", style = "margin:0; font-size:16px; color:#555;"),
          htmltools::h2(
            format(total_homologados, big.mark = ".", decimal.mark = ","), 
            style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#4f9d69;"  # verde sugerido
          ),
          htmltools::div(
            "Fase \u2265 15",
            style = "margin-top:2px; font-size:11px; color:#999;"
          )
        )
      })
      
      output$kpi_progresso_medio_ses <- renderUI({
        df_info <- rct_unificadas_ses() %>%
          mutate(progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 17 * 100)), 2))
        
        media <- mean(df_info$progress_pct, na.rm = TRUE)
        
        htmltools::div(
          style = "
      background: #fff;
      border-radius: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      padding: 20px;
      text-align: center;
      width: 220px;
    ",
          htmltools::h4("Progresso Médio", style = "margin:0; font-size:16px; color:#555;"),
          htmltools::h2(
            sprintf('%.2f%%', media), 
            style = "margin:10px 0 0; font-size:32px; font-weight:bold; color:#005372;"
          ),
          htmltools::div(
            "considerando 15 fases de status",
            style = "margin-top:6px; font-size:12px; color:#777;"
          )
        )
      })
      
      ### KPIs (versões *_central) ----
      output$kpi_demandantes_central <- renderUI({
        df_info <- rct_geral_sad()
        total <- dplyr::n_distinct(df_info$demandante)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Demandantes", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(total, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#000000;"
          ),
          htmltools::div(
            "Total de demandantes",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_processos_central <- renderUI({
        df_info <- rct_geral_sad()
        total <- dplyr::n_distinct(df_info$sei)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(total, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#000000;"
          ),
          htmltools::div(
            "Total de SEIs",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_progresso_medio_central <- renderUI({
        df_info <- rct_geral_sad() %>%
          dplyr::mutate(
            progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2)
          )
        
        media <- mean(df_info$progress_pct, na.rm = TRUE)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Progresso Médio", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(sprintf('%.2f%%', media),
                        style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#005372;"),
          htmltools::div(
            "Todos os processos",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_progresso_medio_2_central <- renderUI({
        df_info <- rct_geral_sad() %>%
          dplyr::mutate(
            progress_pct = round(pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100)), 2)
          ) %>%
          dplyr::filter(!is.na(data_de_inicio), data_de_inicio <= as.Date("2025-08-19"))
        
        media <- mean(df_info$progress_pct, na.rm = TRUE)
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Progresso Médio", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(sprintf('%.2f%%', media),
                        style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#005372;"),
          htmltools::div(
            "iniciados até 19/08/2025",
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_sad_central <- renderUI({
        tot_sad <- rct_geral_sad() %>%
          dplyr::filter(!is.na(status)) %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado, responsavel == "SAD") %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(tot_sad) == 0) tot_sad <- 0L
        
        total_geral <- rct_geral_sad() %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(total_geral) == 0) total_geral <- 0L
        
        pct_txt <- if (total_geral > 0)
          scales::percent(tot_sad / total_geral, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos na SAD", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(tot_sad, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#66a5ad;"
          ),
          htmltools::div(
            paste(pct_txt, "do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_outros_central <- renderUI({
        tot_ses <- rct_geral_sad() %>%
          dplyr::filter(!is.na(status)) %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado, responsavel == "SES") %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(tot_ses) == 0) tot_ses <- 0L
        
        total_geral <- rct_geral_sad() %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(total_geral) == 0) total_geral <- 0L
        
        pct_txt <- if (total_geral > 0)
          scales::percent(tot_ses / total_geral, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos nos Órgãos", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(tot_ses, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#005372;"
          ),
          htmltools::div(
            paste(pct_txt, "do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_pge_central <- renderUI({
        tot_pge <- rct_geral_sad() %>%
          dplyr::filter(!is.na(status)) %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado, responsavel == "PGE") %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(tot_pge) == 0) tot_pge <- 0L
        
        total_geral <- rct_geral_sad() %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          dplyr::filter(!is_arquivado) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)
        if (length(total_geral) == 0) total_geral <- 0L
        
        pct_txt <- if (total_geral > 0)
          scales::percent(tot_pge / total_geral, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos na PGE", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(tot_pge, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#c94f7c;"
          ),
          htmltools::div(
            paste(pct_txt, "do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      output$kpi_homologados_central <- renderUI({
        df_info <- rct_geral_sad() %>%
          dplyr::mutate(status_num = suppressWarnings(as.numeric(as.character(status_cod))))
        
        total <- dplyr::n_distinct(df_info$sei)
        total_homologados <- sum(df_info$status_num >= 15, na.rm = TRUE)
        pct_txt <- if (total > 0)
          scales::percent(total_homologados / total, accuracy = 0.1, decimal.mark = ",")
        else "0%"
        
        htmltools::div(
          style = "
      background:#fff;border-radius:12px;box-shadow:0 2px 6px rgba(0,0,0,0.15);
      padding:20px;text-align:center;width:220px;",
          htmltools::h4("Processos Homologados", style = "margin:0;font-size:16px;color:#555;"),
          htmltools::h2(
            format(total_homologados, big.mark = ".", decimal.mark = ","),
            style = "margin:10px 0 0;font-size:32px;font-weight:bold;color:#4f9d69;"
          ),
          htmltools::div(
            paste0("Fase \u2265 15 \u2022 ", pct_txt, " do total"),
            style = "margin-top:4px;font-size:0.95rem;color:#000;"
          )
        )
      })
      
      # tbls ----
      ## sad - centralizadas ----
      output$tbl_centralizadas_sad <- DT::renderDT({
        df_info <- rct_centralizados_sad() |>
          mutate(
            status = case_when(
              status != status_secundario ~ paste0(status, " | ", status_secundario),
              TRUE ~ status
            )
          ) |>
          dplyr::select(
            is_ses_unificadas, sei, grupo, data_de_inicio, status, observacoes,
            atraso, status_cod, progress_pct, tempo_na_fase
          )
        
        # Ícones + barra (sobrescrevendo o conteúdo, mas SEM renomear as colunas)
        icon_yes  <- "<span style='color:green;font-size:18px;'>&#10004;</span>"
        icon_no   <- "<span style='color:red;font-size:18px;'>&#10006;</span>"
        icon_ok   <- "<span style='color:#28a745;font-size:18px;'>&#10004;</span>"
        icon_late <- "<span style='color:#d9534f;font-size:18px;'>&#10006;</span>"
        
        df_view <- df_info |>
          dplyr::mutate(
            is_ses_unificadas = ifelse(is_ses_unificadas == "Sim", icon_yes, icon_no),
            # “no Prazo?” -> usamos a própria coluna atraso, mas vamos escondê-la
            atraso = ifelse(isTRUE(atraso), icon_late, icon_ok),
            progress_pct = dplyr::if_else(
              is.na(progress_pct), "-",
              sprintf(
                "<div style='display:flex;align-items:center;gap:8px;'>
             <div style='flex:1;background:#e9ecef;border-radius:9999px;height:12px;overflow:hidden;'>
               <div style='height:100%%;width:%1$.2f%%;background:#66a5ad;'></div>
             </div>
             <span style=\"min-width:64px;text-align:right;font-size:13px;color:#333;\" data-order='%1$.4f'>%1$.2f%%</span>
           </div>", progress_pct
              )
            )
          ) |>
          # Ordem final das colunas (sem renomear)
          dplyr::select(
            is_ses_unificadas, sei, grupo, data_de_inicio, status, observacoes,
            atraso, status_cod, progress_pct, tempo_na_fase
          )
        
        # Índice da coluna que será escondida (atraso = “no Prazo?”)
        idx_hide <- which(names(df_view) == "atraso") - 1L
        # Ordenar inicialmente por “tempo_na_fase” desc
        ord_ini  <- list(list(which(names(df_view) == "tempo_na_fase") - 1L, "desc"))
        
        DT::datatable(
          df_view,
          rownames = FALSE,
          escape   = FALSE,
          filter   = "none",
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            pageLength = 10, lengthMenu = c(10,25,50,100),
            order = ord_ini,
            deferRender = TRUE,
            processing  = TRUE,
            columnDefs = list(
              list(visible = FALSE, targets = idx_hide),
              list(visible = FALSE, targets = which(names(df_view) == "status_cod") - 1L),
              list(className = "dt-center",
                   targets = which(names(df_view) %in% c("is_ses_unificadas","atraso")) - 1L)
            ),
            language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json"),
            # Botões nativos (filtrado vs tudo)
            buttons = list(
              list(extend="csv",   text="CSV (filtrado)",
                   title="dados_filtrados",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="applied", order="applied", page="all"))),
              list(extend="excel", text="Excel (filtrado)",
                   title="dados_filtrados",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="applied", order="applied", page="all"))),
              list(extend="csv",   text="CSV (tudo)",
                   title="dados_completos",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="none", order="index", page="all"))),
              list(extend="excel", text="Excel (tudo)",
                   title="dados_completos",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="none", order="index", page="all")))
            )
          ),
          # >>> AQUI: títulos sem renomear no R (ordem = ordem das colunas)
          colnames = c(
            "SES Unificada?", "SEI", "Grupo", "Data de Início", "Status", "Obs.",
            "no Prazo?", "status_cod", "Progresso", "Dias na Fase"
          ),
          callback = DT::JS("
      // placeholder + botão limpar (opc.)
      var $f = $(table.table().container()).find('div.dataTables_filter');
      var $input = $f.find('input[type=search]');
      $input.attr('placeholder', 'Find doers & offers');
      var $btn = $('<div class=\"dt-clear-btn\" aria-label=\"Limpar busca\">×</div>');
      $f.css('position','relative').append($btn);
      function toggle(){ $btn.css('display', $input.val() ? 'flex' : 'none'); }
      $input.on('input', toggle); toggle();
      $btn.on('click', function(){ $input.val('').trigger('input'); table.search('').draw(); $input.focus(); toggle(); });
    ")
        ) |>
          DT::formatDate("data_de_inicio", method="toLocaleDateString", params=list("pt-BR")) |>
          DT::formatStyle(
            columns = c("sei","grupo","data_de_inicio","status","observacoes","tempo_na_fase","progress_pct"),
            `font-size` = "15px"
          )
      },
      server = FALSE)
      
      ## sad - geral ----
      output$tbl_centralizadas_sad_central <- DT::renderDT({
        df_info <- rct_geral_sad() |>
          dplyr::select(
            is_ses_unificadas, sei, grupo, data_de_inicio, status, observacoes,
            atraso, status_cod, progress_pct, tempo_na_fase
          )
        
        # Ícones + barra (sobrescrevendo o conteúdo, mas SEM renomear as colunas)
        icon_yes  <- "<span style='color:green;font-size:18px;'>&#10004;</span>"
        icon_no   <- "<span style='color:red;font-size:18px;'>&#10006;</span>"
        icon_ok   <- "<span style='color:#28a745;font-size:18px;'>&#10004;</span>"
        icon_late <- "<span style='color:#d9534f;font-size:18px;'>&#10006;</span>"
        
        df_view <- df_info |>
          dplyr::mutate(
            is_ses_unificadas = ifelse(is_ses_unificadas == "Sim", icon_yes, icon_no),
            # “no Prazo?” -> usamos a própria coluna atraso, mas vamos escondê-la
            atraso = ifelse(isTRUE(atraso), icon_late, icon_ok),
            progress_pct = dplyr::if_else(
              is.na(progress_pct), "-",
              sprintf(
                "<div style='display:flex;align-items:center;gap:8px;'>
             <div style='flex:1;background:#e9ecef;border-radius:9999px;height:12px;overflow:hidden;'>
               <div style='height:100%%;width:%1$.2f%%;background:#66a5ad;'></div>
             </div>
             <span style=\"min-width:64px;text-align:right;font-size:13px;color:#333;\" data-order='%1$.4f'>%1$.2f%%</span>
           </div>", progress_pct
              )
            )
          ) |>
          # Ordem final das colunas (sem renomear)
          dplyr::select(
            is_ses_unificadas, sei, grupo, data_de_inicio, status, observacoes,
            atraso, status_cod, progress_pct, tempo_na_fase
          )
        
        # Índice da coluna que será escondida (atraso = “no Prazo?”)
        idx_hide <- which(names(df_view) == "atraso") - 1L
        # Ordenar inicialmente por “tempo_na_fase” desc
        ord_ini  <- list(list(which(names(df_view) == "tempo_na_fase") - 1L, "desc"))
        
        DT::datatable(
          df_view,
          rownames = FALSE,
          escape   = FALSE,
          filter   = "none",
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            pageLength = 10, lengthMenu = c(10,25,50,100),
            order = ord_ini,
            deferRender = TRUE,
            processing  = TRUE,
            columnDefs = list(
              list(visible = FALSE, targets = idx_hide),
              list(visible = FALSE, targets = which(names(df_view) == "status_cod") - 1L),
              list(className = "dt-center",
                   targets = which(names(df_view) %in% c("is_ses_unificadas","atraso")) - 1L)
            ),
            language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json"),
            # Botões nativos (filtrado vs tudo)
            buttons = list(
              list(extend="csv",   text="CSV (filtrado)",
                   title="dados_filtrados",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="applied", order="applied", page="all"))),
              list(extend="excel", text="Excel (filtrado)",
                   title="dados_filtrados",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="applied", order="applied", page="all"))),
              list(extend="csv",   text="CSV (tudo)",
                   title="dados_completos",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="none", order="index", page="all"))),
              list(extend="excel", text="Excel (tudo)",
                   title="dados_completos",
                   exportOptions=list(columns=":visible",
                                      modifier=list(search="none", order="index", page="all")))
            )
          ),
          # >>> AQUI: títulos sem renomear no R (ordem = ordem das colunas)
          colnames = c(
            "SES Unificada?", "SEI", "Grupo", "Data de Início", "Status", "Obs.",
            "no Prazo?", "status_cod", "Progresso", "Dias na Fase"
          ),
          callback = DT::JS("
      // placeholder + botão limpar (opc.)
      var $f = $(table.table().container()).find('div.dataTables_filter');
      var $input = $f.find('input[type=search]');
      $input.attr('placeholder', 'Find doers & offers');
      var $btn = $('<div class=\"dt-clear-btn\" aria-label=\"Limpar busca\">×</div>');
      $f.css('position','relative').append($btn);
      function toggle(){ $btn.css('display', $input.val() ? 'flex' : 'none'); }
      $input.on('input', toggle); toggle();
      $btn.on('click', function(){ $input.val('').trigger('input'); table.search('').draw(); $input.focus(); toggle(); });
    ")
        ) |>
          DT::formatDate("data_de_inicio", method="toLocaleDateString", params=list("pt-BR")) |>
          DT::formatStyle(
            columns = c("sei","grupo","data_de_inicio","status","observacoes","tempo_na_fase","progress_pct"),
            `font-size` = "15px"
          )
      },
      server = FALSE)
      
      ## ses - unificadas ----
      output$tbl_status_geral <- renderReactable({
        
        df_info <- rct_unificadas_ses() %>%
          #select(sei, data_de_inicio, tema, status, observacoes)
          select(sei, data_de_inicio, setor, status, observacoes)
        
        df_info %>%
          reactable(
            wrap = TRUE,
            showPageSizeOptions = TRUE,
            defaultPageSize = 10,
            defaultSorted = c("data_de_inicio"),
            defaultSortOrder = "desc",
            striped = TRUE,
            highlight = TRUE,
            showSortable = TRUE,
            height = "auto",
            columns = list(
              sei = colDef(
                name = "SEI",
                maxWidth = 240,
                style = list(fontSize = "15px")
              ),
              data_de_inicio = colDef(
                name = 'Data de Início',
                align = "left",
                maxWidth = 120,
                format = colFormat(
                  date = TRUE, locales = "pt-BR"),
                style = list(fontSize = "15px")
              ),
              # tema = colDef(
              #   name = 'Tema',
              #   align = "left",
              #   maxWidth = 120,
              #   style = list(fontSize = "15px")
              # ),
              setor = colDef(
                name = 'Setor',
                align = "left",
                maxWidth = 120,
                style = list(fontSize = "15px")
              ),
              status = colDef(
                name = 'Status',
                align = "left",
                maxWidth = 210,
                style = list(fontSize = "15px")
              ),
              observacoes = colDef(
                name = 'Observações',
                align = "left",
                maxWidth = 320,
                style = list(fontSize = "15px")
              )
            ),
            language = br_react
          )
      })
      
      ## ses - prioritárias ----
      output$tbl_status_detalhado <- renderReactable({

        df_info <- df_safety_supply %>%
          count(alerta_status, ordem_status, status_detalhado) %>%
          mutate(
            percentual = n / sum(n),
            percentual_fmt = percent(percentual, accuracy = 0.01, decimal.mark = ",")
          )

        df_info %>%
          reactable(
            wrap = TRUE,
            showPageSizeOptions = TRUE,
            defaultPageSize = 10,
            defaultSorted = c("ordem_status"),
            defaultSortOrder = "desc",
            striped = TRUE,
            highlight = TRUE,
            showSortable = TRUE,
            height = "auto",
            columns = list(
              alerta_status = colDef(
                name = "",
                maxWidth = 50,
                style = list(fontSize = "20px")
              ),
              n = colDef(
                name = 'Total',
                align = "right",
                style = list(textAlign = "right")
              ),
              ordem_status = colDef(
                show = FALSE
              ),
              percentual = colDef(
                show = FALSE
              ),
              percentual_fmt = colDef(
                name = "%",
                align = "right",
                style = list(fontWeight = "bold")
              ),
              status_detalhado = colDef(
                name = 'Status',
                align = "left",
                style = list(textAlign = "left", whiteSpace = "normal")
              )
            ),
            language = br_react
          )
      })

      # plots ----
      ## ses - unificadas ----
      output$plot_unificadas_ses <- renderPlotly({
        
        # --- Paleta para as caixas de anotação por responsável (ajuste se quiser) ---
        cores_resp <- c(
          "SES"        = "#005372",  # azul escuro
          "SAD"        = "#66a5ad",  # azul claro
          "PGE"        = "#c94f7c",  # rosa
          "Fase Final" = "#4f9d69"   # verde sugerido
        )
        
        # --- Base agregada PARA AS BARRAS (leva a cor da própria base) ---
        df_status <- rct_unificadas_ses() %>%
          filter(!is.na(status)) %>%
          count(status, status_cod, cor_barras, name = "total") %>%  # <- mantém cor_barras
          mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2"),
            is_pge = status_cod_chr %in% c("9"),
            grupo = case_when(
              as.numeric(status_cod_chr) == 9 ~ "PGE",
              as.numeric(status_cod_chr) <= 4.1 ~ "Em Andamento",
              TRUE ~ "Fase Final"
            )
          )
        
        # --- Ordenação automática no eixo X ---
        ordem_codigos <- df_status %>%
          pull(status_cod_chr) %>%
          unique() %>%
          as.numeric() %>%
          sort(na.last = TRUE) %>%
          as.character()
        
        df_status <- df_status %>%
          mutate(status_cod_factor = factor(status_cod_chr, levels = ordem_codigos)) %>%
          arrange(status_cod_factor)
        
        # --- Totais gerais (excluindo arquivados) ---
        total_processos <- sum(df_status$total[!df_status$is_arquivado])
        
        # --- Totais por RESPONSÁVEL (SES, SAD, PGE, Fase Final), excluindo arquivados ---
        #     Observação: calculamos a partir da base "linha a linha" para não perder 'responsavel'
        df_resp_totais <- rct_unificadas_ses() %>%
          filter(!is.na(status)) %>%
          mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
          ) %>%
          filter(!is_arquivado) %>%
          group_by(responsavel) %>%
          summarise(total = n(), .groups = "drop")
        
        # Garante a presença das 4 categorias, mesmo que alguma venha ausente
        todos_resp <- tibble::tibble(
          responsavel = c("SES", "SAD", "PGE", "Fase Final")
        )
        df_resp_totais <- todos_resp %>%
          left_join(df_resp_totais, by = "responsavel") %>%
          mutate(total = dplyr::coalesce(total, 0L))
        
        # --- Gráfico de barras (cor vem da coluna cor_barras) ---
        fig <- plot_ly(
          data = df_status,
          x = ~status_cod_factor,
          y = ~total,
          type = "bar",
          marker = list(color = ~cor_barras),
          text = ~total,
          textposition = "outside",
          hovertemplate = paste(
            "<b>Status:</b> %{customdata}<br>",
            "<b>Total:</b> %{y}<extra></extra>"
          ),
          customdata = ~status
        )
        
        # --- Anotações por responsável + total geral ---
        y_top <- max(df_status$total) + 14
        anotacoes <- list(
          # Usando xref='paper' para posicionar de forma responsiva no eixo X
          list(
            xref = "paper", yref = "y",
            x = 0.15, y = y_top,
            text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "SES"], " (SES)</b>"),
            showarrow = FALSE,
            bgcolor = cores_resp["SES"], bordercolor = "#ffffff", borderwidth = 1,
            font = list(size = 15, color = "#ffffff")
          ),
          list(
            xref = "paper", yref = "y",
            x = 0.40, y = y_top,
            text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "SAD"], " (SAD)</b>"),
            showarrow = FALSE,
            bgcolor = cores_resp["SAD"], bordercolor = "#ffffff", borderwidth = 1,
            font = list(size = 15, color = "#ffffff")
          ),
          list(
            xref = "paper", yref = "y",
            x = 0.65, y = y_top,
            text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "PGE"], " (PGE)</b>"),
            showarrow = FALSE,
            bgcolor = cores_resp["PGE"], bordercolor = "#ffffff", borderwidth = 1,
            font = list(size = 15, color = "#ffffff")
          ),
          list(
            xref = "paper", yref = "y",
            x = 0.90, y = y_top,
            text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "Fase Final"], " (Fase Final)</b>"),
            showarrow = FALSE,
            bgcolor = cores_resp["Fase Final"], bordercolor = "#ffffff", borderwidth = 1,
            font = list(size = 15, color = "#ffffff")
          ),
          list(
            xref = "paper", yref = "y",
            x = 0.02, y = y_top + 6,
            text = paste0("<b>", total_processos, "<br>PROCESSOS</b>"),
            showarrow = FALSE,
            font = list(size = 16, color = "black")
          )
        )
        
        fig <- fig %>%
          layout(
            xaxis = list(
              title = "",
              type = "category",
              tickmode = "array",
              tickvals = ordem_codigos,
              ticktext = ordem_codigos,
              tickangle = 0
            ),
            yaxis = list(
              title = "",
              showticklabels = FALSE,
              showgrid = FALSE
            ),
            barmode = "group",
            margin = list(t = 20, b = 60, l = 20, r = 20),
            annotations = anotacoes,
            legend = list(orientation = "h", x = 0.3, y = -0.2),
            bargap = 0.15
          )
        
        fig
      })
      
      ## sad - centralizadas ----
      # output$plot_centralizadas_sad <- renderPlotly({
      #   
      #   # --- Paleta para as caixas de anotação por responsável (ajuste se quiser) ---
      #   cores_resp <- c(
      #     "SES"        = "#005372",  # azul escuro
      #     "SAD"        = "#66a5ad",  # azul claro
      #     "PGE"        = "#c94f7c",  # rosa
      #     "Fase Final" = "#4f9d69"   # verde sugerido
      #   )
      #   
      #   # --- Base agregada PARA AS BARRAS (leva a cor da própria base) ---
      #   df_status <- rct_centralizados_sad() %>%
      #     filter(!is.na(status)) %>%
      #     count(status, status_cod, cor_barras, name = "total") %>%  # <- mantém cor_barras
      #     mutate(
      #       status_cod_chr = as.character(status_cod),
      #       status_cod_chr = case_when(
      #         status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      #         status == "Arquivado (Reagentes)" ~ "0.1",
      #         status == "Execução de Compra Direta" ~ "0.2",
      #         is.na(status_cod_chr) ~ "0",
      #         TRUE ~ status_cod_chr
      #       ),
      #       is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2"),
      #       is_pge = status_cod_chr %in% c("9"),
      #       grupo = case_when(
      #         as.numeric(status_cod_chr) == 9 ~ "PGE",
      #         as.numeric(status_cod_chr) <= 4.1 ~ "Em Andamento",
      #         TRUE ~ "Fase Final"
      #       )
      #     )
      #   
      #   # --- Ordenação automática no eixo X ---
      #   ordem_codigos <- df_status %>%
      #     pull(status_cod_chr) %>%
      #     unique() %>%
      #     as.numeric() %>%
      #     sort(na.last = TRUE) %>%
      #     as.character()
      #   
      #   df_status <- df_status %>%
      #     mutate(status_cod_factor = factor(status_cod_chr, levels = ordem_codigos)) %>%
      #     arrange(status_cod_factor)
      #   
      #   # --- Totais gerais (excluindo arquivados) ---
      #   total_processos <- sum(df_status$total[!df_status$is_arquivado])
      #   
      #   # --- Totais por RESPONSÁVEL (SES, SAD, PGE, Fase Final), excluindo arquivados ---
      #   #     Observação: calculamos a partir da base "linha a linha" para não perder 'responsavel'
      #   df_resp_totais <- rct_centralizados_sad() %>%
      #     filter(!is.na(status)) %>%
      #     mutate(
      #       status_cod_chr = as.character(status_cod),
      #       status_cod_chr = case_when(
      #         status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
      #         status == "Arquivado (Reagentes)" ~ "0.1",
      #         status == "Execução de Compra Direta" ~ "0.2",
      #         is.na(status_cod_chr) ~ "0",
      #         TRUE ~ status_cod_chr
      #       ),
      #       is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2")
      #     ) %>%
      #     filter(!is_arquivado) %>%
      #     group_by(responsavel) %>%
      #     summarise(total = n(), .groups = "drop")
      #   
      #   # Garante a presença das 4 categorias, mesmo que alguma venha ausente
      #   todos_resp <- tibble::tibble(
      #     responsavel = c("SES", "SAD", "PGE", "Fase Final")
      #   )
      #   df_resp_totais <- todos_resp %>%
      #     left_join(df_resp_totais, by = "responsavel") %>%
      #     mutate(total = dplyr::coalesce(total, 0L))
      #   
      #   # --- Gráfico de barras (cor vem da coluna cor_barras) ---
      #   fig <- plot_ly(
      #     data = df_status,
      #     x = ~status_cod_factor,
      #     y = ~total,
      #     type = "bar",
      #     marker = list(color = ~cor_barras),
      #     text = ~total,
      #     textposition = "outside",
      #     hovertemplate = paste(
      #       "<b>Status:</b> %{customdata}<br>",
      #       "<b>Total:</b> %{y}<extra></extra>"
      #     ),
      #     customdata = ~status
      #   )
      #   
      #   # --- Anotações por responsável + total geral ---
      #   y_top <- max(df_status$total) + 14
      #   anotacoes <- list(
      #     # Usando xref='paper' para posicionar de forma responsiva no eixo X
      #     list(
      #       xref = "paper", yref = "y",
      #       x = 0.15, y = y_top,
      #       text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "SES"], " (SES)</b>"),
      #       showarrow = FALSE,
      #       bgcolor = cores_resp["SES"], bordercolor = "#ffffff", borderwidth = 1,
      #       font = list(size = 15, color = "#ffffff")
      #     ),
      #     list(
      #       xref = "paper", yref = "y",
      #       x = 0.40, y = y_top,
      #       text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "SAD"], " (SAD)</b>"),
      #       showarrow = FALSE,
      #       bgcolor = cores_resp["SAD"], bordercolor = "#ffffff", borderwidth = 1,
      #       font = list(size = 15, color = "#ffffff")
      #     ),
      #     list(
      #       xref = "paper", yref = "y",
      #       x = 0.65, y = y_top,
      #       text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "PGE"], " (PGE)</b>"),
      #       showarrow = FALSE,
      #       bgcolor = cores_resp["PGE"], bordercolor = "#ffffff", borderwidth = 1,
      #       font = list(size = 15, color = "#ffffff")
      #     ),
      #     list(
      #       xref = "paper", yref = "y",
      #       x = 0.90, y = y_top,
      #       text = paste0("<b>", df_resp_totais$total[df_resp_totais$responsavel == "Fase Final"], " (Fase Final)</b>"),
      #       showarrow = FALSE,
      #       bgcolor = cores_resp["Fase Final"], bordercolor = "#ffffff", borderwidth = 1,
      #       font = list(size = 15, color = "#ffffff")
      #     ),
      #     list(
      #       xref = "paper", yref = "y",
      #       x = 0.02, y = y_top + 6,
      #       text = paste0("<b>", total_processos, "<br>PROCESSOS</b>"),
      #       showarrow = FALSE,
      #       font = list(size = 16, color = "black")
      #     )
      #   )
      #   
      #   fig <- fig %>%
      #     layout(
      #       xaxis = list(
      #         title = "",
      #         type = "category",
      #         tickmode = "array",
      #         tickvals = ordem_codigos,
      #         ticktext = ordem_codigos,
      #         tickangle = 0
      #       ),
      #       yaxis = list(
      #         title = "",
      #         showticklabels = FALSE,
      #         showgrid = FALSE
      #       ),
      #       barmode = "group",
      #       margin = list(t = 20, b = 60, l = 20, r = 20),
      #       annotations = anotacoes,
      #       legend = list(orientation = "h", x = 0.3, y = -0.2),
      #       bargap = 0.15
      #     )
      #   
      #   fig
      # })
      
      output$plot_centralizadas_sad <- renderPlotly({
        
        # --- Paleta para as caixas de anotação por responsável (mantida caso queira reaproveitar) ---
        cores_resp <- c(
          "SES"        = "#005372",  # azul escuro
          "SAD"        = "#66a5ad",  # azul claro
          "PGE"        = "#c94f7c",  # rosa
          "Fase Final" = "#4f9d69"   # verde sugerido
        )
        
        # --- Base agregada PARA AS BARRAS (leva a cor da própria base) ---
        df_status <- rct_centralizados_sad() %>%
          filter(!is.na(status)) %>%
          count(status, status_cod, cor_barras, name = "total") %>%
          mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2"),
            is_pge = status_cod_chr %in% c("9"),
            grupo = case_when(
              as.numeric(status_cod_chr) == 9 ~ "PGE",
              as.numeric(status_cod_chr) <= 4.1 ~ "Em Andamento",
              TRUE ~ "Fase Final"
            )
          )
        
        # --- Ordenação automática no eixo X ---
        ordem_codigos <- df_status %>%
          pull(status_cod_chr) %>%
          unique() %>%
          as.numeric() %>%
          sort(na.last = TRUE) %>%
          as.character()
        
        df_status <- df_status %>%
          mutate(status_cod_factor = factor(status_cod_chr, levels = ordem_codigos)) %>%
          arrange(status_cod_factor)
        
        # --- Gráfico de barras (cor vem da coluna cor_barras) ---
        fig <- plot_ly(
          data = df_status,
          x = ~status_cod_factor,
          y = ~total,
          type = "bar",
          marker = list(color = ~cor_barras),
          text = ~total,
          textposition = "outside",
          hovertemplate = paste(
            "<b>Status:</b> %{customdata}<br>",
            "<b>Total:</b> %{y}<extra></extra>"
          ),
          customdata = ~status
        )
        
        # --- Layout sem anotações ---
        fig <- fig %>%
          layout(
            xaxis = list(
              title = "",
              type = "category",
              tickmode = "array",
              tickvals = ordem_codigos,
              ticktext = ordem_codigos,
              tickangle = 0
            ),
            yaxis = list(
              title = "",
              showticklabels = FALSE,
              showgrid = FALSE
            ),
            barmode = "group",
            margin = list(t = 20, b = 60, l = 20, r = 20),
            legend = list(orientation = "h", x = 0.3, y = -0.2),
            bargap = 0.15
          )
        
        fig
      })
      
      ## demandantes ----
      output$plot_demandantes_progresso <- renderPlotly({
        df_sum <- rct_resumo_demandante()
        
        ord <- df_sum %>%
          arrange(desc(processos), desc(progresso_medio)) %>%
          pull(demandante)
        
        df_sum <- df_sum %>%
          mutate(
            demandante_ord = factor(demandante, levels = rev(ord)),
            progresso_fmt  = scales::number(progresso_medio, accuracy = 0.01, decimal.mark = ","),
            label_txt      = paste0(progresso_fmt, "% · ", processos, " processos"),
            hover_txt      = glue::glue(
              "<b>Demandante:</b> {demandante}<br>",
              "<b>Progresso médio:</b> {progresso_fmt}%<br>",
              "<b>Total de processos:</b> {processos}"
            )
          )
        
        # folga no eixo X para caber o "outside" (ex.: +20%)
        x_max <- max(df_sum$progresso_medio, na.rm = TRUE)
        x_lim <- max(100, x_max) * 1.20   # 100 -> 120, por ex.
        
        plot_ly(
          data = df_sum,
          x = ~progresso_medio,
          y = ~demandante_ord,
          type = "bar",
          orientation = "h",
          marker = list(
            color = ~progresso_medio,
            colorscale = list(
              c(0,   "#d73027"),  # vermelho
              c(0.5, "#bdbdbd"),  # cinza
              c(1,   "#4575b4")   # azul
            ),
            cmin = 0, cmax = 100,
            showscale = FALSE,
            line = list(color = "#000000", width = 1.5)
          ),
          text = ~label_txt,
          textposition = "outside",
          textfont = list(size = 12),
          hovertext = ~hover_txt,
          hovertemplate = "%{hovertext}<extra></extra>",
          cliponaxis = FALSE  # evita “clip” mesmo se texto passar do eixo
        ) %>%
          layout(
            xaxis = list(
              title = "Progresso médio (%)",
              rangemode = "tozero",
              range = c(0, x_lim)  # <<< folga no plot
            ),
            yaxis = list(
              title = "",
              automargin = TRUE     # <<< comprime margem conforme os rótulos
            ),
            # margens enxutas — como o texto cabe no range, não precisa margem grande
            margin = list(l = 10, r = 10, t = 10, b = 40)
          ) %>%
          config(locale = "pt-BR")
      })
      
      ## sad geral ----
      output$plot_centralizadas_central <- plotly::renderPlotly({
        
        df_status <- rct_geral_sad() %>%
          dplyr::filter(!is.na(status)) %>%
          dplyr::count(status, status_cod, cor_barras, name = "total") %>%
          dplyr::mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = dplyr::case_when(
              status == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status == "Arquivado (Reagentes)" ~ "0.1",
              status == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2"),
            is_pge = status_cod_chr %in% c("9"),
            grupo = dplyr::case_when(
              as.numeric(status_cod_chr) == 9 ~ "PGE",
              as.numeric(status_cod_chr) <= 4.1 ~ "Em Andamento",
              TRUE ~ "Fase Final"
            )
          )
        
        # Ordenação automática no eixo X
        ordem_codigos <- df_status %>%
          dplyr::pull(status_cod_chr) %>%
          unique() %>%
          as.numeric() %>%
          sort(na.last = TRUE) %>%
          as.character()
        
        df_status <- df_status %>%
          dplyr::mutate(status_cod_factor = factor(status_cod_chr, levels = ordem_codigos)) %>%
          dplyr::arrange(status_cod_factor)
        
        # Gráfico de barras (cor vinda da coluna cor_barras), sem anotações extras
        fig <- plotly::plot_ly(
          data = df_status,
          x = ~status_cod_factor,
          y = ~total,
          type = "bar",
          marker = list(color = ~cor_barras),
          text = ~total,
          textposition = "outside",
          hovertemplate = paste(
            "<b>Status:</b> %{customdata}<br>",
            "<b>Total:</b> %{y}<extra></extra>"
          ),
          customdata = ~status
        ) %>%
          plotly::layout(
            xaxis = list(
              title = "",
              type = "category",
              tickmode = "array",
              tickvals = ordem_codigos,
              ticktext = ordem_codigos,
              tickangle = 0
            ),
            yaxis = list(
              title = "",
              showticklabels = FALSE,
              showgrid = FALSE
            ),
            margin = list(t = 20, b = 60, l = 20, r = 20),
            bargap = 0.15
          )
        
        fig
      })
      
      ## sad geral - demandantes ----
      output$plot_demandantes_progresso_central <- plotly::renderPlotly({
        df_sum <- rct_geral_sad() %>%
          dplyr::mutate(
            progress_pct = pmin(100, pmax(0, as.numeric(status_cod) / 15 * 100))
          ) %>%
          dplyr::group_by(demandante) %>%
          dplyr::summarise(
            progresso_medio = mean(progress_pct, na.rm = TRUE),
            processos       = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::filter(!is.na(demandante))  # por segurança; seu ETL já troca NA por "Não informado"
        
        # ordenação: mais processos primeiro, depois maior progresso
        ord <- df_sum %>%
          dplyr::arrange(dplyr::desc(processos), dplyr::desc(progresso_medio)) %>%
          dplyr::pull(demandante)
        
        df_sum <- df_sum %>%
          dplyr::mutate(
            demandante_ord = factor(demandante, levels = rev(ord)),
            progresso_fmt  = scales::number(progresso_medio, accuracy = 0.01, decimal.mark = ","),
            label_txt      = paste0(progresso_fmt, "% · ", processos, " processos"),
            hover_txt      = glue::glue(
              "<b>Demandante:</b> {demandante}<br>",
              "<b>Progresso médio:</b> {progresso_fmt}%<br>",
              "<b>Total de processos:</b> {processos}"
            )
          )
        
        # folga no eixo X para caber o texto "outside"
        x_max <- max(df_sum$progresso_medio, na.rm = TRUE)
        x_lim <- max(100, x_max) * 1.20
        
        plotly::plot_ly(
          data = df_sum,
          x = ~progresso_medio,
          y = ~demandante_ord,
          type = "bar",
          orientation = "h",
          marker = list(
            color = ~progresso_medio,
            colorscale = list(
              c(0,   "#d73027"),  # vermelho
              c(0.5, "#bdbdbd"),  # cinza
              c(1,   "#4575b4")   # azul
            ),
            cmin = 0, cmax = 100,
            showscale = FALSE,
            line = list(color = "#000000", width = 1.5)
          ),
          text = ~label_txt,
          textposition = "outside",
          textfont = list(size = 12),
          hovertext = ~hover_txt,
          hovertemplate = "%{hovertext}<extra></extra>",
          cliponaxis = FALSE
        ) %>%
          plotly::layout(
            xaxis = list(
              title = "Progresso médio (%)",
              rangemode = "tozero",
              range = c(0, x_lim)
            ),
            yaxis = list(
              title = "",
              automargin = TRUE
            ),
            margin = list(l = 10, r = 10, t = 10, b = 40)
          ) %>%
          plotly::config(locale = "pt-BR")
      })
      
    }
  )
}
