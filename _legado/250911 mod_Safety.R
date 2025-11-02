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
        "SAD - Centralizadas",
        card(
          full_screen = TRUE,
          
          card_body(
            layout_column_wrap(
              width = 1/4, gap = "10px", style = "text-align: center;",
              uiOutput(ns("kpi_demandantes")),
              uiOutput(ns("kpi_processos")),
              uiOutput(ns("kpi_homologados")),
              uiOutput(ns("kpi_progresso_medio"))
            ),
            br(),
            plotlyOutput(ns("plot_centralizadas_sad")),
            br(),
            plotlyOutput(ns("plot_demandantes_progresso")),
            br(),
            #reactableOutput(ns("tbl_centralizadas_sad"))
            DTOutput(ns("tbl_centralizadas_sad")),
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
      
      rct_resumo_demandante <- reactive({
        df <- rct_centralizados_sad()
        
        req(df)
        
        if (!"progress_pct" %in% names(df)) {
          df <- df %>%
            mutate(
              status_num = suppressWarnings(as.numeric(as.character(status_cod))),
              progress_pct = round(pmin(100, pmax(0, status_num / 17 * 100)), 2)
            )
        }
        
        df %>%
          filter(!is.na(demandante)) %>%
          group_by(demandante) %>%
          summarise(
            processos = n_distinct(sei),
            progresso_medio = mean(progress_pct, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            progresso_medio = round(progresso_medio, 2),
            progresso_fmt = sprintf('%.2f%%', progresso_medio)
          ) %>%
          arrange(desc(processos), desc(progresso_medio))
      })
      
      # objetos ----
      ## caixas ----
      output$box_0b <- renderUI({
        # Agregação dos dados
        df_info <- df_safety_supply %>%
          summarise(
            total = n()
          ) %>%
          mutate(
            valor_formatado = format(total, big.mark = ".", scientific = FALSE)
          )

        # Renderização HTML no padrão solicitado
        HTML(glue::glue("
    <div class='kpi-value'>{df_info$valor_formatado}</div>
    <div class='kpi-auxiliar' style='visibility: hidden;'>-</div>
  "))
      })

      output$box_1b <- renderUI({
        # Total geral
        total <- df_safety_supply %>%
          summarise(n_total = n(), .groups = "drop") %>%
          pull(n_total)

        # Total filtrado (status == "CONCLUÍDO")
        concluido <- df_safety_supply %>%
          filter(status == "CONCLUÍDO") %>%
          summarise(n_concluido = n(), .groups = "drop") %>%
          pull(n_concluido)

        # Formatações
        valor_formatado <- concluido %>%
          format(big.mark = ".", decimal.mark = ",", scientific = FALSE) %>%
          trimws()

        percentual_formatado <- scales::percent(concluido / total, accuracy = 0.1)

        # HTML no padrão kpi
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar'>{percentual_formatado}</div>
    do Total
  "))
      })

      output$box_2b <- renderUI({
        # Total geral
        total <- df_safety_supply %>%
          summarise(n_total = n(), .groups = "drop") %>%
          pull(n_total)

        # Total filtrado (status == "EM AÇÃO")
        concluido <- df_safety_supply %>%
          filter(status == "EM AÇÃO") %>%
          summarise(n_concluido = n(), .groups = "drop") %>%
          pull(n_concluido)

        # Formatações
        valor_formatado <- concluido %>%
          format(big.mark = ".", decimal.mark = ",", scientific = FALSE) %>%
          trimws()

        percentual_formatado <- scales::percent(concluido / total, accuracy = 0.1)

        # HTML no padrão kpi
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar'>{percentual_formatado}</div>
    do Total
  "))
      })

      output$box_3b <- renderUI({
        # Total geral
        total <- df_safety_supply %>%
          summarise(n_total = n(), .groups = "drop") %>%
          pull(n_total)

        # Total filtrado (status == "PENDENTE")
        valor_info <- df_safety_supply %>%
          filter(status == "PENDENTE") %>%
          summarise(n_concluido = n(), .groups = "drop") %>%
          pull(n_concluido)

        # Formatações
        valor_formatado <- valor_info %>%
          format(big.mark = ".", decimal.mark = ",", scientific = FALSE) %>%
          trimws()

        percentual_formatado <- scales::percent(valor_info / total, accuracy = 0.1)

        # HTML no padrão kpi
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar'>{percentual_formatado}</div>
    do Total
  "))
      })
      
      output$box_4b <- renderUI({
        # Total geral
        total <- df_safety_supply %>%
          summarise(n_total = n(), .groups = "drop") %>%
          pull(n_total)
        
        # Total filtrado (status == "PENDENTE")
        valor_info <- df_safety_supply %>%
          filter(
            status %in% c("ARQUIVADO", 'CANCELADO')
            ) %>%
          summarise(n_concluido = n(), .groups = "drop") %>%
          pull(n_concluido)
        
        # Formatações
        valor_formatado <- valor_info %>%
          format(big.mark = ".", decimal.mark = ",", scientific = FALSE) %>%
          trimws()
        
        percentual_formatado <- scales::percent(valor_info / total, accuracy = 0.1)
        
        # HTML no padrão kpi
        HTML(glue::glue("
    <div class='kpi-value'>{valor_formatado}</div>
    <div class='kpi-auxiliar'>{percentual_formatado}</div>
    do Total
  "))
      })
      
      ### sad centralizadas % ----
      output$kpi_demandantes <- renderUI({
        df_info <- rct_centralizados_sad()
        
        total <- df_info %>%
          distinct(demandante) %>%
          nrow()
        
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
      
      output$kpi_processos <- renderUI({
        df_info <- rct_centralizados_sad()
        
        total <- df_info %>%
          distinct(sei) %>%
          nrow()
        
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
      
      output$kpi_homologados <- renderUI({
        df_info <- rct_centralizados_sad() %>%
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
      
      output$kpi_progresso_medio <- renderUI({
        df_info <- rct_centralizados_sad() %>%
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
        
        total <- df_info %>%
          distinct(demandante) %>%
          nrow()
        
        total %>% print()
        
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
        
        total <- df_info %>%
          distinct(sei) %>%
          nrow()
        
        total %>% print()
        
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
      
      # tbls ----
      ## sad - centralizadas ----
      # output$tbl_centralizadas_sad <- renderReactable({
      #   
      #   df_info <- rct_centralizados_sad() %>%
      #     select(is_ses_unificadas, sei, grupo, data_de_inicio, status, observacoes, atraso, status_cod, progress_pct, tempo_na_fase)
      #   
      #   df_info %>%
      #     reactable(
      #       wrap = TRUE,
      #       showPageSizeOptions = TRUE,
      #       defaultPageSize = 10,
      #       defaultSorted = c("tempo_na_fase"),
      #       defaultSortOrder = "desc",
      #       striped = TRUE,
      #       highlight = TRUE,
      #       showSortable = TRUE,
      #       searchable = TRUE, 
      #       height = "auto",
      #       columns = list(
      #         is_ses_unificadas = colDef(
      #           name = "SES Unificada?",
      #           maxWidth = 120,
      #           align = "center",
      #           cell = function(value) {
      #             if (value == "Sim") {
      #               htmltools::tags$span("\u2714", style = "color: green; font-size: 18px;") # check verde
      #             } else {
      #               htmltools::tags$span("\u2716", style = "color: red; font-size: 18px;")  # X vermelho
      #             }
      #           }
      #         ),
      #         
      #         sei = colDef(
      #           name = "SEI",
      #           maxWidth = 180,
      #           style = list(fontSize = "15px")
      #         ),
      #         
      #         data_de_inicio = colDef(
      #           name = 'Data de Início',
      #           align = "left",
      #           maxWidth = 120,
      #           format = colFormat(
      #             date = TRUE, locales = "pt-BR"),
      #           style = list(fontSize = "15px")
      #         ),
      #         
      #         grupo = colDef(
      #           name = 'Grupo',
      #           align = "left",
      #           maxWidth = 180,
      #           style = list(fontSize = "15px")
      #         ),
      #         
      #         tempo_na_fase = colDef(
      #           name = 'Dias na Fase',
      #           align = "left",
      #           maxWidth = 120,
      #           style = list(fontSize = "15px")
      #         ),
      #         
      #         status = colDef(
      #           name = 'Status',
      #           align = "left",
      #           maxWidth = 180,
      #           style = list(fontSize = "15px")
      #         ),
      #         
      #         observacoes = colDef(
      #           name = 'Obs.',
      #           align = "left",
      #           maxWidth = 220,
      #           style = list(fontSize = "15px")
      #         ),
      #         
      #         status_cod = colDef(
      #           show = FALSE
      #         ),
      #         
      #         atraso = colDef(
      #           name = "no Prazo?",
      #           show = FALSE, 
      #           align = "center",
      #           maxWidth = 100,
      #           cell = function(value) {
      #             if (isTRUE(value)) {
      #               # Atrasado => X vermelho
      #               htmltools::tags$span("\u2716", style = "color:#d9534f; font-size:18px;")
      #             } else {
      #               # Em dia => check verde
      #               htmltools::tags$span("\u2714", style = "color:#28a745; font-size:18px;")
      #             }
      #           }
      #         ),
      #         
      #         progress_pct = colDef(
      #           name = "Progresso",
      #           align = "left",
      #           maxWidth = 180,
      #           cell = function(value) {
      #             if (is.na(value)) return(htmltools::tags$span("-"))
      #             label <- sprintf("%.2f%%", value)  # ex.: 52.94%
      #             # barra (trilha cinza) + preenchimento com a sua paleta
      #             htmltools::div(
      #               style = "display:flex; align-items:center; gap:8px;",
      #               htmltools::div(
      #                 style = "flex:1; background:#e9ecef; border-radius:9999px; height:12px; overflow:hidden;",
      #                 htmltools::div(
      #                   style = paste0(
      #                     "height:100%; width:", value, 
      #                     "%; background:#66a5ad;"  # pode trocar para #005372 se quiser mais contraste
      #                   )
      #                 )
      #               ),
      #               htmltools::tags$span(label, style = "min-width:64px; text-align:right; font-size:13px; color:#333;")
      #             )
      #           }
      #         )
      #       ),
      #       language = br_react
      #     )
      # })
      
  #     output$tbl_centralizadas_sad <- renderReactable({
  #       df_info <- rct_centralizados_sad() %>%
  #         dplyr::select(
  #           is_ses_unificadas, sei, grupo, data_de_inicio, status, observacoes,
  #           atraso, status_cod, progress_pct, tempo_na_fase
  #         )
  #       
  #       # CSS da barra de busca (igual ao que mandei antes)
  #       css_search <- htmltools::tags$style(htmltools::HTML("
  #   .reactable .rt-search { position: relative; padding: 4px 0; }
  #   .reactable .rt-search input[type='text']{
  #     width: 360px !important; height: 38px;
  #     padding-left: 36px !important; padding-right: 36px !important;
  #     border-radius: 9999px !important; border: 1px solid #D1D5DB !important;
  #     background: rgba(255,255,255,.9);
  #     box-shadow: 0 1px 2px rgba(0,0,0,.04) inset, 0 1px 1px rgba(0,0,0,.06);
  #     backdrop-filter: blur(2px);
  #     transition: box-shadow .15s ease, border-color .15s ease;
  #   }
  #   .reactable .rt-search::before{
  #     content: ''; position: absolute; left: 12px; top: 50%; transform: translateY(-50%);
  #     width: 16px; height: 16px; opacity: .55; background-repeat: no-repeat; background-size: 16px 16px;
  #     background-image: url('data:image/svg+xml;utf8,<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"%23999\"><path d=\"M15.5 14h-.79l-.28-.27A6.5 6.5 0 1 0 14 15.5l.27.28v.79L20 21.5 21.5 20l-6-6zm-6 0A4.5 4.5 0 1 1 14 9.5 4.5 4.5 0 0 1 9.5 14z\"/></svg>');
  #   }
  #   .reactable .rt-search input[type='text']:focus{
  #     outline: none !important; border-color: #93C5FD !important;
  #     box-shadow: 0 0 0 3px rgba(59,130,246,.25) !important;
  #   }
  #   .reactable .rt-search .rt-clear-btn{
  #     position: absolute; right: 8px; top: 50%; transform: translateY(-50%);
  #     width: 22px; height: 22px; border-radius: 9999px;
  #     background: #E5E7EB; border: 1px solid #D1D5DB;
  #     display: none; align-items: center; justify-content: center;
  #     font-weight: 700; font-size: 12px; color: #6B7280; cursor: pointer;
  #     user-select: none;
  #   }
  #   .reactable .rt-search .rt-clear-btn:hover{ filter: brightness(0.98); }
  # "))
  #       
  #       tbl <- reactable(
  #         df_info,
  #         wrap = TRUE,
  #         showPageSizeOptions = TRUE,
  #         defaultPageSize = 10,
  #         defaultSorted = "tempo_na_fase",
  #         defaultSortOrder = "desc",
  #         striped = TRUE,
  #         highlight = TRUE,
  #         showSortable = TRUE,
  #         searchable = TRUE,
  #         height = "auto",
  #         theme = reactableTheme(
  #           searchInputStyle = list(
  #             borderRadius = "9999px", height = "38px", width = "360px",
  #             paddingLeft = "36px", paddingRight = "36px"
  #           )
  #         ),
  #         columns = list(
  #           is_ses_unificadas = colDef(
  #             name = "SES Unificada?", maxWidth = 120, align = "center",
  #             cell = function(value) if (value == "Sim")
  #               htmltools::tags$span("\u2714", style = "color: green; font-size: 18px;")
  #             else
  #               htmltools::tags$span("\u2716", style = "color: red; font-size: 18px;")
  #           ),
  #           sei = colDef(name = "SEI", maxWidth = 180, style = list(fontSize = "15px")),
  #           data_de_inicio = colDef(
  #             name = "Data de Início", align = "left", maxWidth = 120,
  #             format = colFormat(date = TRUE, locales = "pt-BR"),
  #             style = list(fontSize = "15px")
  #           ),
  #           grupo = colDef(name = "Grupo", align = "left", maxWidth = 180, style = list(fontSize = "15px")),
  #           tempo_na_fase = colDef(name = "Dias na Fase", align = "left", maxWidth = 120, style = list(fontSize = "15px")),
  #           status = colDef(name = "Status", align = "left", maxWidth = 180, style = list(fontSize = "15px")),
  #           observacoes = colDef(name = "Obs.", align = "left", maxWidth = 220, style = list(fontSize = "15px")),
  #           status_cod = colDef(show = FALSE),
  #           atraso = colDef(
  #             name = "no Prazo?", show = FALSE, align = "center", maxWidth = 100,
  #             cell = function(value) if (isTRUE(value))
  #               htmltools::tags$span("\u2716", style = "color:#d9534f; font-size:18px;")
  #             else
  #               htmltools::tags$span("\u2714", style = "color:#28a745; font-size:18px;")
  #           ),
  #           progress_pct = colDef(
  #             name = "Progresso", align = "left", maxWidth = 180,
  #             cell = function(value){
  #               if (is.na(value)) return(htmltools::tags$span("-"))
  #               label <- sprintf("%.2f%%", value)
  #               htmltools::div(
  #                 style = "display:flex; align-items:center; gap:8px;",
  #                 htmltools::div(
  #                   style = "flex:1; background:#e9ecef; border-radius:9999px; height:12px; overflow:hidden;",
  #                   htmltools::div(style = paste0("height:100%; width:", value, "%; background:#66a5ad;"))
  #                 ),
  #                 htmltools::tags$span(label, style = "min-width:64px; text-align:right; font-size:13px; color:#333;")
  #               )
  #             }
  #           )
  #         ),
  #         language = br_react
  #       )
  #       
  #       # Anexa CSS e JS ao WIDGET e retorna só o widget
  #       tbl <- htmlwidgets::prependContent(tbl, css_search)
  #       
  #       tbl <- htmlwidgets::onRender(tbl, "
  #   function(el, x){
  #     var box = el.querySelector('.rt-search'); if(!box) return;
  #     var input = box.querySelector('input[type=\"text\"]'); if(!input) return;
  # 
  #     input.placeholder = 'Find doers & offers';
  # 
  #     var btn = box.querySelector('.rt-clear-btn');
  #     if(!btn){
  #       btn = document.createElement('div');
  #       btn.className = 'rt-clear-btn';
  #       btn.setAttribute('aria-label', 'Limpar busca');
  #       btn.innerText = '×';
  #       box.appendChild(btn);
  # 
  #       btn.addEventListener('click', function(){
  #         input.value = '';
  #         input.dispatchEvent(new Event('input', { bubbles: true }));
  #         input.focus();
  #         toggle();
  #       });
  #     }
  #     function toggle(){ btn.style.display = input.value ? 'flex' : 'none'; }
  #     input.addEventListener('input', toggle);
  #     toggle();
  #   }
  # ")
  #       
  #       tbl
  #     })
      
      # trecho enxuto focado nos 3 pontos
      output$tbl_centralizadas_sad <- DT::renderDT({
        df_info <- rct_centralizados_sad() |>
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
            # >>> AQUI: esconder “no Prazo?” sem excluir a coluna
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
          select(sei, data_de_inicio, tema, status, observacoes)
        
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
              tema = colDef(
                name = 'Tema',
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
      output$plot_centralizadas_sad <- renderPlotly({
        
        # --- Paleta para as caixas de anotação por responsável (ajuste se quiser) ---
        cores_resp <- c(
          "SES"        = "#005372",  # azul escuro
          "SAD"        = "#66a5ad",  # azul claro
          "PGE"        = "#c94f7c",  # rosa
          "Fase Final" = "#4f9d69"   # verde sugerido
        )
        
        # --- Base agregada PARA AS BARRAS (leva a cor da própria base) ---
        df_status <- rct_centralizados_sad() %>%
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
        df_resp_totais <- rct_centralizados_sad() %>%
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
      
    }
  )
}
