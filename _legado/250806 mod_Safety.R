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

    # sad - centralizadas ----
    card(
      full_screen = TRUE,
      card_header(
        "SAD - Centralizadas",
        class = "bg-info d-flex justify-content-between"
      ),
      card_body(
        layout_column_wrap(
          width = 1/4,
          gap = "10px",
          style = "text-align: center;", 
          uiOutput(ns("kpi_demandantes")),
          uiOutput(ns("kpi_processos")),
          uiOutput(ns("kpi_homologados")),
          uiOutput(ns("kpi_progresso_medio"))
          # uiOutput(ns("kpi_no_prazo"))
        ),
        br(),
        plotlyOutput(ns("plot_centralizadas_sad")),
        br(),
        plotlyOutput(ns("plot_demandantes_progresso")),
        br(),
        reactableOutput(ns("tbl_centralizadas_sad"))
      )
    ),
    
    # ses - unificadas ----
    card(
      full_screen = TRUE,
      card_header(
        "SES - Unificadas",
        class = "bg-info d-flex justify-content-between"
      ),
      card_body(
        #plotOutput(ns("plot_processos"))
        plotlyOutput(ns("plot_processos")),
        reactableOutput(ns("tbl_status_geral"))
      )
    ),

    # ses - prioritárias ----
    card(
      style = "border: none;",
      
      card_header(
        'SES - Prioritários',
        class = "bg-info",
        uiOutput(outputId = ns("atualizado_em"))
      ),
    
    card(
      style = "border: none;",

      card_body(
        layout_columns(
          fill = T,
        kpi_card(
          title = "Total",
          icon_src = "images/product.svg",
          output_id = "box_0b",
          ns = ns,
          bg_header_color = "#004080",
          bg_aux_color = "#0077b6"
        ),
        kpi_card(
          title = "Concluídos",
          icon_src = "images/product_green.svg",
          output_id = "box_1b",
          ns = ns,
          bg_header_color = "#006400",
          bg_aux_color = "#006400"
        ),
        kpi_card(
          title = "Em ação",
          icon_src = "images/product_blue.svg",
          output_id = "box_2b",
          ns = ns,
          bg_header_color = "#0000FF",
          bg_aux_color = "#0000FF"
        ),
        kpi_card(
          title = "Pendente",
          icon_src = "images/product_orange.svg",
          output_id = "box_3b",
          ns = ns,
          bg_header_color = "#FFA500",
          bg_aux_color = "#FFA500"
        ),
        kpi_card(
          title = "Cancelados",
          icon_src = "images/product_grey.svg",
          output_id = "box_4b",
          ns = ns,
          bg_header_color = "#6c757d",
          bg_aux_color = "#495057"
        )
      )
      )
      ),

    card(
      full_screen = TRUE,
      
      card_body(
        reactableOutput(ns("tbl_status_detalhado"))
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

      observe({
        df0 <- dados_centralizados_sad
        df1 <- rct_centralizados_sad()
        
        message("orig: ", nrow(df0), " | rct: ", nrow(df1))
        
        # 1) checar classes
        message("class(data_de_inicio): ", paste(class(df0$data_de_inicio), collapse = ", "))
        
        # 2) quantas fora do período atual
        fp <- valores_filtros$filtro_periodo
        if (!is.null(fp)) {
          fora <- df0 %>%
            filter(
              !is.na(data_de_inicio) &
                !(data_de_inicio >= fp[1] & data_de_inicio <= fp[2])
            )
          message("fora do período: ", nrow(fora))
        }
        
        # 3) strings vazias/ruins
        ruins <- df0 %>% filter(!is.na(data_de_inicio) & is.character(data_de_inicio))
        vazias <- df0 %>% filter(identical(data_de_inicio, ""))  # se for coluna character inteira
        
        # 4) limites de datas da base
        sup <- suppressWarnings(as.Date(df0$data_de_inicio))
        message("min/max (as.Date): ", min(sup, na.rm = TRUE), " / ", max(sup, na.rm = TRUE))
      })
      
      
      # eventos ----
      ## filtros ----
      valores_filtros <- reactiveValues(
        filtro_periodo = c(data_min, data_max),
        filtro_status = NULL,
        filtro_demandante = NULL
      )
      
      ## aplicar ----
      observeEvent(input$btn_aplicar, {
        valores_filtros$filtro_periodo <- input$filtro_periodo
        valores_filtros$filtro_status <- input$filtro_status
        valores_filtros$filtro_demandante <- input$filtro_demandante
      })
      
      ## limpar ----
      observeEvent(input$btn_limpar, {
      
        valores_filtros$filtro_periodo <- c(data_min, data_max)
        valores_filtros$filtro_status <- NULL
        valores_filtros$filtro_demandante <- NULL
        
      updateDateRangeInput(session, "filtro_periodo", start = data_min, end = data_max)
      updateSelectInput(session, "filtro_status", selected = "")
      updateSelectInput(session, "filtro_demandante", selected = "")
      })
      
      # reativos ----
      ## ses - unificadas ----
      rct_processos <- reactive({
        filtro_periodo <- valores_filtros$filtro_periodo
        
        dados_processos %>%
          filter(
            is.null(filtro_periodo) |
              ( (data_de_inicio >= filtro_periodo[1] & data_de_inicio <= filtro_periodo[2]) | is.na(data_de_inicio) ),
            
            is.null(valores_filtros$filtro_status) | status.x %in% valores_filtros$filtro_status
          )
      })
      
      ## sad - centralizadas ----
      rct_centralizados_sad <- reactive({
        filtro_periodo <- valores_filtros$filtro_periodo
        
        dados_centralizados_sad %>%
          filter(
            
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
      
      output$kpi_processos <- renderUI({
        df_info <- rct_centralizados_sad()
        
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
      
      # tbls ----
      ## sad - centralizadas ----
      output$tbl_centralizadas_sad <- renderReactable({
        
        df_info <- rct_centralizados_sad() %>%
          select(is_ses_unificadas, sei, grupo, data_de_inicio, status, observacoes, atraso, status_cod, progress_pct)
        
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
              is_ses_unificadas = colDef(
                name = "SES Unificada?",
                maxWidth = 120,
                align = "center",
                cell = function(value) {
                  if (value == "Sim") {
                    htmltools::tags$span("\u2714", style = "color: green; font-size: 18px;") # check verde
                  } else {
                    htmltools::tags$span("\u2716", style = "color: red; font-size: 18px;")  # X vermelho
                  }
                }
              ),
              sei = colDef(
                name = "SEI",
                maxWidth = 180,
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
              grupo = colDef(
                name = 'Grupo',
                align = "left",
                maxWidth = 180,
                style = list(fontSize = "15px")
              ),
              
              status = colDef(
                name = 'Status',
                align = "left",
                maxWidth = 180,
                style = list(fontSize = "15px")
              ),
              
              observacoes = colDef(
                name = 'Obs.',
                align = "left",
                maxWidth = 220,
                style = list(fontSize = "15px")
              ),
              
              status_cod = colDef(
                show = FALSE
              ),
              
              atraso = colDef(
                name = "no Prazo?",
                show = FALSE, 
                align = "center",
                maxWidth = 100,
                cell = function(value) {
                  if (isTRUE(value)) {
                    # Atrasado => X vermelho
                    htmltools::tags$span("\u2716", style = "color:#d9534f; font-size:18px;")
                  } else {
                    # Em dia => check verde
                    htmltools::tags$span("\u2714", style = "color:#28a745; font-size:18px;")
                  }
                }
              ),
              
              progress_pct = colDef(
                name = "Progresso",
                align = "left",
                maxWidth = 180,
                cell = function(value) {
                  if (is.na(value)) return(htmltools::tags$span("-"))
                  label <- sprintf("%.2f%%", value)  # ex.: 52.94%
                  # barra (trilha cinza) + preenchimento com a sua paleta
                  htmltools::div(
                    style = "display:flex; align-items:center; gap:8px;",
                    htmltools::div(
                      style = "flex:1; background:#e9ecef; border-radius:9999px; height:12px; overflow:hidden;",
                      htmltools::div(
                        style = paste0(
                          "height:100%; width:", value, 
                          "%; background:#66a5ad;"  # pode trocar para #005372 se quiser mais contraste
                        )
                      )
                    ),
                    htmltools::tags$span(label, style = "min-width:64px; text-align:right; font-size:13px; color:#333;")
                  )
                }
              )
            ),
            language = br_react
          )
      })
      
      ## ses - unificadas ----
      output$tbl_status_geral <- renderReactable({
        
        df_info <- rct_processos() %>%
          select(sei, data_de_inicio, tema, status.x, obs)
        
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
              status.x = colDef(
                name = 'Status',
                align = "left",
                maxWidth = 210,
                style = list(fontSize = "15px")
              ),
              obs = colDef(
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
      
      output$plot_processos <- renderPlotly({
        
        df_status <- rct_processos() %>%
          count(status.x, status_cod, name = "total") %>%
          filter(!is.na(status.x)) %>%
          mutate(
            status_cod_chr = as.character(status_cod),
            status_cod_chr = case_when(
              status.x == "04.1 Elaboração de Termo de Referência /ETP (SAD)" ~ "4.1",
              status.x == "Arquivado (Reagentes)" ~ "0.1",
              status.x == "Execução de Compra Direta" ~ "0.2",
              is.na(status_cod_chr) ~ "0",
              TRUE ~ status_cod_chr
            ),
            is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2"),
            is_pge = status_cod_chr %in% c("9"),
            grupo = ifelse(as.numeric(status_cod_chr) <= 4.1, "Em Andamento", "Fase Final")
          )
        
        # Ordenação automática:
        ordem_codigos <- df_status %>%
          pull(status_cod_chr) %>%
          unique() %>%
          as.numeric() %>%
          sort(na.last = TRUE) %>%
          as.character()
        
        df_status <- df_status %>%
          mutate(
            status_cod_factor = factor(status_cod_chr, levels = ordem_codigos)
          ) %>%
          arrange(status_cod_factor)
        
        # Totais excluindo status laranja:
        total_processos <- sum(df_status$total[!df_status$is_arquivado])
        
        totais_grupo <- df_status %>%
          filter(!is_arquivado) %>%
          group_by(grupo) %>%
          summarise(total_grupo = sum(total), .groups = "drop")
        
        # Definição de cores:
        # df_status$cor_barras <- ifelse(
        #   #df_status$is_arquivado, "#F39200",
        #   df_status$is_arquivado, "#6c757d",
        #   ifelse(
        #     df_status$grupo == "Em Andamento", 
        #     "#005372", "#66a5ad"))
        
        df_status <- df_status %>%
          mutate(cor_barras = case_when(
            is_arquivado ~ "#6c757d",        # Cinza
            is_pge ~ "#c94f7c",              # Rosa
            grupo == "Em Andamento" ~ "#005372",  # Azul petróleo
            TRUE ~ "#66a5ad"                 # Azul claro esverdeado (default)
          ))
        
        # Gráfico:
        fig <- plot_ly(
          data = df_status,
          x = ~status_cod_factor,
          y = ~total,
          type = 'bar',
          marker = list(color = df_status$cor_barras),
          text = ~total,
          textposition = 'outside',
          hovertemplate = paste(
            "<b>Status:</b> %{customdata}<br>",
            "<b>Total:</b> %{y}<extra></extra>"
          ),
          customdata = ~status.x
        )
        
        anotacoes <- list(
          list(
            x = 3,
            y = max(df_status$total) + 14,
            text = paste0("<b>", totais_grupo$total_grupo[totais_grupo$grupo == "Em Andamento"], " em andamento</b>"),
            showarrow = FALSE,
            bgcolor = "#005372",
            bordercolor = "#ffffff",
            borderwidth = 1,
            font = list(size = 15, color = "#ffffff")
          ),
          list(
            x = 9,
            y = max(df_status$total) + 14,
            text = paste0("<b>", totais_grupo$total_grupo[totais_grupo$grupo == "Fase Final"], " fase final</b>"),
            showarrow = FALSE,
            bgcolor = "#66a5ad",
            bordercolor = "#ffffff",
            borderwidth = 1,
            font = list(size = 15, color = "#ffffff")
          ),
          list(
            x = 0,
            y = max(df_status$total) + 20,
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
              showgrid = FALSE),
            barmode = 'group',
            margin = list(t = 20, b = 60, l = 20, r = 20),
            annotations = anotacoes,
            legend = list(orientation = 'h', x = 0.3, y = -0.2),
            bargap = 0.15
          )
        
        fig
        
      })
      
      ## sad - centralizadas ----
      
      # output$plot_centralizadas_sad <- renderPlotly({
      #   
      #   df_status <- rct_centralizados_sad() %>%
      #     count(status, status_cod, name = "total") %>%
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
      #       is_arquivado = status_cod_chr %in% c("0", "0.1", "0.2"),
      #       is_pge = status_cod_chr %in% c("9"),
      #       #grupo = ifelse(as.numeric(status_cod_chr) <= 4.1, "Em Andamento", "Fase Final")
      #       grupo = case_when(
      #         as.numeric(status_cod_chr) == 9 ~ "PGE",
      #         as.numeric(status_cod_chr) <= 4.1 ~ "Em Andamento",
      #         TRUE ~ "Fase Final"
      #       )
      #     )
      #   
      #   
      #   # Ordenação automática:
      #   ordem_codigos <- df_status %>%
      #     pull(status_cod_chr) %>%
      #     unique() %>%
      #     as.numeric() %>%
      #     sort(na.last = TRUE) %>%
      #     as.character()
      #   
      #   df_status <- df_status %>%
      #     mutate(
      #       status_cod_factor = factor(status_cod_chr, levels = ordem_codigos)
      #     ) %>%
      #     arrange(status_cod_factor)
      #   
      #   # Totais excluindo status laranja:
      #   total_processos <- sum(df_status$total[!df_status$is_arquivado])
      #   
      #   totais_grupo <- df_status %>%
      #     filter(!is_arquivado) %>%
      #     group_by(grupo) %>%
      #     summarise(total_grupo = sum(total), .groups = "drop")
      #   
      #   # df_status <- df_status %>%
      #   #   mutate(cor_barras = case_when(
      #   #     is_arquivado ~ "#6c757d",        # Cinza
      #   #     is_pge ~ "#c94f7c",              # Rosa
      #   #     grupo == "Em Andamento" ~ "#005372",  # Azul petróleo
      #   #     TRUE ~ "#66a5ad"                 # Azul claro esverdeado (default)
      #   #   ))
      #   
      #   # Gráfico:
      #   fig <- plot_ly(
      #     data = df_status,
      #     x = ~status_cod_factor,
      #     y = ~total,
      #     type = 'bar',
      #     marker = list(color = df_status$cor_barras),
      #     text = ~total,
      #     textposition = 'outside',
      #     hovertemplate = paste(
      #       "<b>Status:</b> %{customdata}<br>",
      #       "<b>Total:</b> %{y}<extra></extra>"
      #     ),
      #     customdata = ~status
      #   )
      #   
      #   anotacoes <- list(
      #     list(
      #       x = 3,
      #       y = max(df_status$total) + 14,
      #       text = paste0("<b>", totais_grupo$total_grupo[totais_grupo$grupo == "Em Andamento"], " em andamento</b>"),
      #       showarrow = FALSE,
      #       bgcolor = "#005372",
      #       bordercolor = "#ffffff",
      #       borderwidth = 1,
      #       font = list(size = 15, color = "#ffffff")
      #     ),
      #     list(
      #       x = 5,
      #       y = max(df_status$total) + 14,
      #       text = paste0("<b>", totais_grupo$total_grupo[totais_grupo$grupo == "PGE"], " na PGE</b>"),
      #       showarrow = FALSE,
      #       bgcolor = "#c94f7c",
      #       bordercolor = "#ffffff",
      #       borderwidth = 1,
      #       font = list(size = 15, color = "#ffffff")
      #     ),
      #     list(
      #       x = 7,
      #       y = max(df_status$total) + 14,
      #       text = paste0("<b>", totais_grupo$total_grupo[totais_grupo$grupo == "Fase Final"], " fase final</b>"),
      #       showarrow = FALSE,
      #       bgcolor = "#66a5ad",
      #       bordercolor = "#ffffff",
      #       borderwidth = 1,
      #       font = list(size = 15, color = "#ffffff")
      #     ),
      #     list(
      #       x = 0,
      #       y = max(df_status$total) + 20,
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
      #         showgrid = FALSE),
      #       barmode = 'group',
      #       margin = list(t = 20, b = 60, l = 20, r = 20),
      #       annotations = anotacoes,
      #       legend = list(orientation = 'h', x = 0.3, y = -0.2),
      #       bargap = 0.15
      #     )
      #   
      #   fig
      #   
      # })
      
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
      
      # output$plot_demandantes_progresso <- renderPlotly({
      #   df_sum <- rct_resumo_demandante()
      #   
      #   # ordenar por TOTAL (desc) e, em empate, por progresso (desc)
      #   ord <- df_sum %>%
      #     arrange(desc(processos), desc(progresso_medio)) %>%
      #     pull(demandante)
      #   
      #   df_sum <- df_sum %>%
      #     mutate(
      #       demandante_ord = factor(demandante, levels = rev(ord)),
      #       # formatos pt-BR
      #       progresso_fmt = scales::number(progresso_medio, accuracy = 0.01, decimal.mark = ","),
      #       # rótulo na barra: progresso + total
      #       label_txt = paste0(progresso_fmt, "% · ", processos, " processos"),
      #       # hover com vírgula e campos explícitos
      #       hover_txt = glue::glue(
      #         "<b>Demandante:</b> {demandante}<br>",
      #         "<b>Progresso médio:</b> {progresso_fmt}%<br>",
      #         "<b>Total de processos:</b> {processos}"
      #       )
      #     )
      #   
      #   # gradiente laranja -> verde pela % de progresso
      #   fig <- plot_ly(
      #     data = df_sum,
      #     x = ~progresso_medio,
      #     y = ~demandante_ord,
      #     type = "bar",
      #     orientation = "h",
      #     marker = list(
      #       color = ~progresso_medio,
      #       colorscale = list(
      #         c(0,   "#d73027"),  # vermelho
      #         c(0.5, "#bdbdbd"),  # cinza neutro
      #         c(1,   "#4575b4")   # azul
      #       ),
      #       cmin = 0, cmax = 100,
      #       showscale = FALSE,
      #       line = list(color = "#000000", width = 1.5)
      #     ),
      #     text = ~label_txt,
      #     textposition = "outside",
      #     textfont = list(size = 12),
      #     hovertext = ~hover_txt,
      #     hovertemplate = "%{hovertext}<extra></extra>"
      #   ) %>%
      #     layout(
      #       xaxis = list(title = "Progresso médio (%)", rangemode = "tozero"),
      #       yaxis = list(title = ""),
      #       margin = list(l = 140, r = 20, t = 10, b = 40)
      #     ) %>%
      #     config(locale = "pt-BR")
      #   
      #   fig
      # })
      
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
