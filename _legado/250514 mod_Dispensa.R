ui_Dispensa <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Campo de busca + fuzzy
    # fluidRow(
    #   column(3, selectInput(
    #     ns("unidade_filtro"), 
    #     "Unidade de medida:",
    #     choices = c(
    #       "Todas", sort(unique(suprimentos_hof$df$unidade_de_medida))),
    #     selected = "Todas")),
    #   column(3, textInput(ns("busca_txt"), "Buscar produto:", width = "100%")),
    #   column(3, radioButtons(ns("modo_busca"), "Tipo:", choices = c("Exata" = "exata", "Fuzzy" = "fuzzy"), inline = TRUE)),
    #   column(3, uiOutput(ns("filtro_descricao_completa")))
    # ),
    # 
    # fluidRow(
    #   column(3, checkboxInput(ns("remover_zeros"), "Remover zeros das médias", value = TRUE))
    # ),
    
    fluidRow(
      column(3, selectInput(
        ns("unidade_filtro"), "Unidade de medida:",
        choices = c("Todas", sort(unique(suprimentos_hof$df$unidade_de_medida))),
        selected = "Todas")
      ),
      column(3, textInput(ns("busca_txt"), "Buscar produto:", width = "100%")),
      column(3, radioButtons(ns("modo_busca"), "Tipo:", choices = c("Exata" = "exata", "Fuzzy" = "fuzzy"), inline = TRUE)),
      column(3, uiOutput(ns("filtro_descricao_completa")))
    ),
    
    fluidRow(
      column(3, radioButtons(ns("remover_zeros"), "Zeros na média:", 
                             choices = c("Manter" = FALSE, "Remover" = TRUE), 
                             selected = TRUE, inline = TRUE)),
      column(3, actionButton(ns("btn_aplicar_filtro"), "Aplicar filtro", icon = icon("filter")))
    ),
    
    fluidRow(
      column(3, kpi_card(
        title = "Total 2023",
        icon_src = "images/product.svg",
        output_id = "box_total_2023",
        ns = ns,
        bg_header_color = "#004080",
        bg_aux_color = "#0077b6"
      )),
      column(3, kpi_card(
        title = "Média 2023",
        icon_src = "images/product.svg",
        output_id = "box_media_2023",
        ns = ns,
        bg_header_color = "#004080",
        bg_aux_color = "#0077b6"
      )),
      column(3, kpi_card(
        title = "Total 2024",
        icon_src = "images/product.svg",
        output_id = "box_total_2024",
        ns = ns,
        bg_header_color = "#004080",
        bg_aux_color = "#0077b6"
      )),
      column(3, kpi_card(
        title = "Média 2024",
        icon_src = "images/product.svg",
        output_id = "box_media_2024",
        ns = ns,
        bg_header_color = "#004080",
        bg_aux_color = "#0077b6"
      ))
    ),
    
    fluidRow(
      column(12, plotly::plotlyOutput(ns("plot_serie_historica")))
    ),
    
    card(
      full_screen = TRUE,
      card_header("Dados filtrados"),
      card_body(reactableOutput(ns("tbl_busca")))
    )
  )
}

server_Dispensa <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # base_busca <- reactive({
      #   req(input$busca_txt)
      #   
      #   termo <- input$busca_txt
      #   tipo  <- input$modo_busca
      #   unidade <- input$unidade_filtro
      #   
      #   dados <- suprimentos_hof$df
      #   
      #   # Filtra por descrição
      #   dados_filtrados <- if (tipo == "fuzzy") {
      #     matches <- agrep(termo, suprimentos_hof$lst_descricao_completa, value = TRUE, max.distance = 0.2, ignore.case = TRUE)
      #     filter(dados, descricao_completa %in% matches)
      #   } else {
      #     filter(dados, str_detect(descricao_completa, regex(termo, ignore_case = TRUE)))
      #   }
      #   
      #   # Filtra por unidade se selecionada
      #   if (!is.null(unidade) && unidade != "Todas") {
      #     dados_filtrados <- filter(dados_filtrados, unidade_de_medida == unidade)
      #   }
      #   
      #   dados_filtrados
      # })
      
      # base_busca <- reactive({
      #   req(input$busca_txt)
      #   
      #   termo <- input$busca_txt
      #   tipo <- input$modo_busca
      #   unidade <- input$unidade_filtro
      #   dados <- suprimentos_hof$df
      #   
      #   dados_filtrados <- if (tipo == "fuzzy") {
      #     matches <- agrep(termo, suprimentos_hof$lst_descricao_completa, value = TRUE, max.distance = 0.2, ignore.case = TRUE)
      #     filter(dados, descricao_completa %in% matches)
      #   } else {
      #     filter(dados, str_detect(descricao_completa, regex(termo, ignore_case = TRUE)))
      #   }
      #   
      #   if (!is.null(unidade) && unidade != "Todas") {
      #     dados_filtrados <- filter(dados_filtrados, unidade_de_medida == unidade)
      #   }
      #   
      #   dados_filtrados
      # })
      
      base_A <- reactive({
        req(input$busca_txt)
        
        dados <- suprimentos_hof$df
        termo <- input$busca_txt
        tipo <- input$modo_busca
        unidade <- input$unidade_filtro
        
        dados_filtrados <- if (tipo == "fuzzy") {
          matches <- agrep(termo, suprimentos_hof$lst_descricao_completa, value = TRUE, max.distance = 0.2, ignore.case = TRUE)
          filter(dados, descricao_completa %in% matches)
        } else {
          filter(dados, str_detect(descricao_completa, regex(termo, ignore_case = TRUE)))
        }
        
        if (!is.null(unidade) && unidade != "Todas") {
          dados_filtrados <- filter(dados_filtrados, unidade_de_medida == unidade)
        }
        
        dados_filtrados
      })
      
      output$filtro_descricao_completa <- renderUI({
        descricoes <- sort(unique(base_A()$descricao_completa))
        
        selectInput(
          inputId = ns("descricao_filtro"),
          label = "Descrição específica:",
          choices = c("Todas", descricoes),
          selected = "Todas"
        )
      })
      
      descricao_aplicada <- reactiveVal(NULL)
      
      observeEvent(input$btn_aplicar_filtro, {
        descricao_aplicada(input$descricao_filtro)
      })
      
      base_B <- reactive({
        df <- base_A()
        
        desc <- descricao_aplicada()
        
        if (!is.null(desc) && desc != "Todas") {
          df <- filter(df, descricao_completa == desc)
        }
        
        df
      })
      
      # KPIs renderText
      # output$box_total_2023 <- renderText({
      #   df <- base_busca()
      #   total <- nrow(filter(df, ano == 2023))
      #   perc <- round(100 * total / nrow(df), 1)
      #   glue::glue("{total}<br><small>{perc}%</small>")
      # })
      
      output$box_total_2023 <- renderText({
        df <- filter(
          base_B(),
          #base_busca(), 
          ano == 2023)
        meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
        total_2023 <- df %>% select(all_of(meses)) %>% rowSums(na.rm = TRUE) %>% sum()
        
        total_geral <- base_busca() %>%
          select(all_of(meses)) %>%
          rowSums(na.rm = TRUE) %>%
          sum()
        
        perc <- round(100 * total_2023 / total_geral, 1)
        glue::glue("{scales::comma(total_2023)}<br><small>{perc}%</small>")
      })
      
      output$box_total_2024 <- renderText({
        df <- filter(
          base_B(),
          #base_busca(), 
          ano == 2024)
        meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
        total_2024 <- df %>% select(all_of(meses)) %>% rowSums(na.rm = TRUE) %>% sum()
        
        total_geral <- base_busca() %>%
          select(all_of(meses)) %>%
          rowSums(na.rm = TRUE) %>%
          sum()
        
        perc <- round(100 * total_2024 / total_geral, 1)
        glue::glue("{scales::comma(total_2024)}<br><small>{perc}%</small>")
      })
      
      output$box_media_2023 <- renderText({
        df <- filter(
          base_B(),
          #base_busca(), 
          ano == 2023)
        meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
        valores <- df %>% select(all_of(meses)) %>% unlist()
        if (input$remover_zeros) valores <- valores[valores != 0]
        media <- round(mean(valores, na.rm = TRUE), 2)
        glue::glue("{media}")
      })
      
      # output$box_total_2024 <- renderText({
      #   df <- base_busca()
      #   total <- nrow(filter(df, ano == 2024))
      #   perc <- round(100 * total / nrow(df), 1)
      #   glue::glue("{total}<br><small>{perc}%</small>")
      # })
      
      output$box_media_2024 <- renderText({
        df <- filter(
          base_B(),
          #base_busca(), 
          ano == 2024)
        meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
        valores <- df %>% select(all_of(meses)) %>% unlist()
        if (input$remover_zeros) valores <- valores[valores != 0]
        media <- round(mean(valores, na.rm = TRUE), 2)
        glue::glue("{media}")
      })
      
      #### plot ----
      
      output$plot_serie_historica <- plotly::renderPlotly({
        #df <- base_busca()
        
        df <- base_B()
        
        meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
        
        df_long <- df %>%
          select(ano, all_of(meses)) %>%
          pivot_longer(cols = all_of(meses), names_to = "mes", values_to = "valor") %>%
          group_by(ano, mes) %>%
          summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")
        
        plotly::plot_ly(
          df_long,
          x = ~mes,
          y = ~total,
          color = ~as.factor(ano),
          type = 'bar'
        ) %>%
          plotly::layout(
            barmode = 'group',
            title = 'Consumo por mês',
            xaxis = list(title = 'Mês'),
            yaxis = list(title = 'Total')
          )
      })
      
      #### tbl ----
      
      output$tbl_busca <- renderReactable({
        reactable(
          
          base_B(),
          
          #base_busca(), 
          
          searchable = FALSE, 
          wrap = TRUE,
          showPageSizeOptions = TRUE,
          striped = TRUE,
          highlight = TRUE,
          showSortable = TRUE,
          height = "auto",
          language = br_react,
          
          defaultColDef = colDef(
            #header = function(value) gsub(".", " ", value, fixed = TRUE),
            #cell = function(value) format(value, nsmall = 1),
            #align = "center",
            #minWidth = 70,
            headerStyle = list(
              background = "#F2F2F2",
              fontSize = "0.75rem")
          ),
          
          columns = list(
            ano = colDef(
              maxWidth = 55,
              style = list(fontSize = "0.8rem")
              ),
            cod_prod = colDef(
              name = 'Cód.',
              maxWidth = 65,
              style = list(fontSize = "0.75rem")
            ),
            conta_contabil = colDef(
              name = 'Conta',
              maxWidth = 180,
              style = list(fontSize = "0.75rem")
            ),
            descricao_completa = colDef(
              name = 'Descrição',
              maxWidth = 200,
              style = list(fontSize = "0.75rem")
            ),
            descricao_produto = colDef(
              show = FALSE
            ),
            unidade_de_medida = colDef(
              name = "Un.",
              maxWidth = 200,
              style = list(fontSize = "0.75rem")
            ),
            jan = colDef(
              name = "Jan",
              format = colFormat(
                locales = "pt-BR",
                separators = TRUE
              ),
              maxWidth = 120,
              style = list(fontSize = "0.75rem")
            ),
            fev = colDef(
              name = "Fev",
              format = colFormat(
                locales = "pt-BR",
                separators = TRUE
                )
            ),
            mar = colDef(
              name = "Mar",
              format = colFormat(
                locales = "pt-BR",
                separators = TRUE
              )
            ),
            abr = colDef(
              name = "Abr",
              format = colFormat(
                locales = "pt-BR",
                separators = TRUE
              )
            )
            )
          )
      })
      
    }
  )
} 
