ui_Ceos <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    card(
      card_header(
       "Estimativa" 
      ),
    card(
      layout_columns(
        width = c(3, 6, 3),
        gap = "8px",
        
        uiOutput(ns("filtro_programa")),
        
        textInput(
          ns("busca_txt"),
          "Termo de busca",
          width = "100%",
          placeholder = "Digite um termo de busca. Exemplos: midazolam, máscara..."
        ),
        
        radioButtons(
          ns("modo_busca"),
          "Tipo de busca",
          width = "80%",
          choices = c("Exata" = "exata", "Aproximada" = "fuzzy"),
          inline = TRUE
        )
      )
    ),
    
    card(
      
      layout_column_wrap(
        uiOutput(ns("filtro_descricao_completa"))
      )
      
    ),
    
    card(
      layout_columns(
        width = c(6, 3, 3),
        gap = "8px",
        
        #uiOutput(ns("filtro_unidade")),
        
        selectInput(
          inputId = ns("ano_filtro"),
          label = "Ano:",
          choices = df_consumo_ceos$lst_ano,
          selected = head(df_consumo_ceos$lst_ano, 1),
          multiple = FALSE
          ),
        
        radioButtons(
            ns("remover_zeros"), 
            "Remover zeros?",
            choices = c("Não" = FALSE, "Sim" = TRUE), 
            selected = TRUE,
            inline = TRUE
          )
        )
      ),
    
    
        card(
          layout_columns(
            width = c(-3, 3, 3, -3),
            gap = "2px",
        div(
          class = "d-flex align-items-center h-100",
          style = "height: 100%;",
          actionButton(
            ns("btn_aplicar_filtro"), 
            "Aplicar filtro", 
            icon = icon("filter"),
            class = "btn-aplicar",
            style = "margin-right: 10px;"
          )
        ),
        
        div(
          class = "d-flex align-items-center h-100",
          style = "height: 100%;",
          actionButton(
            ns("btn_limpar_filtro"), 
            "Limpar filtro", 
            icon = icon("eraser"),
            class = "btn-limpar",
            style = "margin-right: 10px;"
          )
        )
        
      )
    ),
    
    card(
      layout_column_wrap(
        width = 1/3,
        gap = "16px",
        
        kpi_card(
          textOutput(ns("titulo_total_m2")), 
          "images/product.svg", 
          "box_total_m2", 
          ns, 
          "#004080", 
          "#0077b6"),
        kpi_card(
          textOutput(ns("titulo_total_m1")),
          "images/product.svg", "box_total_m1", ns, "#004080", "#0077b6"),
        kpi_card(
          textOutput(ns("titulo_total")),
          "images/product.svg", "box_total",    ns, "#004080", "#0077b6"),
        
        kpi_card(
          textOutput(ns("titulo_media_m2")),
          "images/product.svg", "box_media_m2", ns, "#004080", "#0077b6"),
        kpi_card(
          textOutput(ns("titulo_media_m1")),
          "images/product.svg", "box_media_m1", ns, "#004080", "#0077b6"),
        kpi_card(
          textOutput(ns("titulo_media")),
          "images/product.svg", "box_media",    ns, "#004080", "#0077b6")
      )
    ),
    
    card(
      column(12, plotly::plotlyOutput(ns("plot_serie_historica")))
    ),
    
    card(
      column(12, plotly::plotlyOutput(ns("plot_total_uge")))
    ),
    
    card(
      full_screen = TRUE,
      card_header("Dados filtrados"),
      card_body(reactableOutput(ns("tbl_busca")))
    )
    ),
    
    card(
      card_header("Relatório"),
      
      layout_column_wrap(
        width = 1/3, 
        gap = "16px",
        textInput(ns("numero_processo"), label = "Processo SEI", placeholder = "Ex: 12345678.123456/2024-12"),
        textInput(ns("numero_ipr"), label = "IPR", placeholder = "Ex: 0001.2024.SES"),
        textInput(ns("objeto_irp"), label = "Objeto", placeholder = "Informe o objeto da solicitação"),
        radioButtons(ns("tipo_item"), label = "Tipo de Item", choices = c("Medicamento" = "medicamento", "MMH" = "mmh"), inline = TRUE),
        textInput(ns("nome_solicitante"), label = "Nome do responsável pela solicitação"), 
        textInput(ns("cargo_solicitante"), label = "Cargo do responsável")
        ),
      
      checkboxGroupInput(
        inputId = ns("filtro_justificativas"),
        label = "Selecione os motivos aplicáveis:",
        inline = TRUE,
        choices = c(
          "DESABASTECIMENTO HISTÓRICO" = "desabastecimento",
          "AQUISIÇÃO ATRAVÉS DE EMPRÉSTIMOS" = "emprestimo",
          "HABILITAÇÃO DE NOVA ESPECIALIDADE" = "nova_especialidade",
          "AUMENTO DE LEITO" = "aumento_leito",
          "AUMENTO DE PROCEDIMENTO" = "aumento_procedimento",
          "SAZIONALIDADE" = "sazonalidade",
          "FRACASSO/DESERÇÃO DO ITEM EM OUTROS PROCESSOS LICITATÓRIOS" = "fracasso_licitacao",
          "NECESSIDADE DE CORREÇÃO HISTÓRICA DE AQUISIÇÃO DO ITEM" = "correcao_historica"
        ),
        selected = NULL
      ),
      
      uiOutput(ns("ui_justificativas_complementares")),
      
      downloadButton(
        ns("baixar_relatorio"), "Baixar Relatório")
    )
    
  )
}

server_Ceos <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # observar ----
      valores_filtros <- reactiveValues(
        descricao_filtro = NULL
      )
      
      observeEvent(input$btn_aplicar_filtro, {
        valores_filtros$descricao_filtro <- input$descricao_filtro
      })
      
      observeEvent(input$btn_limpar_filtro, {
        valores_filtros$descricao_filtro <- NULL
        updateSelectInput(session, "descricao_filtro", selected = character(0))
      })
      
      descricao_aplicada <- reactiveVal(NULL)
      
      observeEvent(input$btn_aplicar_filtro, {
        descricao_aplicada(input$descricao_filtro)
      })
      
      # reativos ----
      ## títulos ----
      # código novo
      titulos_kpi <- reactive({
        anos <- df_kpis()$anos
        list(
          total_m2  = glue::glue("Total ({anos[length(anos)-2]})"),
          total_m1  = glue::glue("Total ({anos[length(anos)-1]})"),
          total     = glue::glue("Total ({anos[length(anos)]})"),
          media_m2  = glue::glue("Média ({anos[length(anos)-2]})"),
          media_m1  = glue::glue("Média ({anos[length(anos)-1]})"),
          media     = glue::glue("Média ({anos[length(anos)]})")
        )
      })
      
      ## base A ----
      # base_A <- reactive({
      #   req(input$busca_txt)
      #   
      #   dados <- df_consumo_ceos$df_consumo_ceos
      #   termo <- input$busca_txt
      #   tipo <- input$modo_busca
      #   #unidade <- input$unidade_filtro
      #   programa <- input$programa_filtro
      #   
      #   dados_filtrados <- if (tipo == "fuzzy") {
      #     matches <- agrep(termo, df_consumo_ceos$lst_descricao_completa, value = TRUE, max.distance = 0.2, ignore.case = TRUE)
      #     filter(dados, descricao_produto %in% matches)
      #   } else {
      #     filter(dados, str_detect(descricao_produto, regex(termo, ignore_case = TRUE)))
      #   }
      #   
      #   if (!is.null(programa) && nzchar(programa)) {
      #     dados_filtrados <- filter(dados_filtrados, nome_programa2 == programa)
      #   }
      #   
      #   dados_filtrados
      # })
      
      base_A <- reactive({
        req(nzchar(input$busca_txt %||% ""))  # garante não-vazio
        
        dados    <- df_consumo_ceos$df_consumo_ceos
        termo    <- input$busca_txt
        tipo     <- input$modo_busca
        programa <- input$programa_filtro
        
        # filtro por termo
        dados_filtrados <- if (identical(tipo, "fuzzy")) {
          matches <- agrep(
            termo,
            df_consumo_ceos$lst_descricao_completa,
            value = TRUE, max.distance = 0.2, ignore.case = TRUE
          )
          dplyr::filter(dados, descricao_produto %in% matches)
        } else {
          dplyr::filter(dados, stringr::str_detect(descricao_produto, stringr::regex(termo, ignore_case = TRUE)))
        }
        
        # normaliza seleção (remove vazios) e aplica filtro por múltiplos
        programas_sel <- setdiff(programa %||% character(0), "")
        if (length(programas_sel) > 0) {
          dados_filtrados <- dplyr::filter(dados_filtrados, nome_programa2 %in% programas_sel)
        }
        
        dados_filtrados
      })
      
      ## base B ----
      
      base_B <- reactive({
        
        req(valores_filtros$descricao_filtro)
        
        df <- base_A()
        
        desc <- descricao_aplicada()
        
        if (!is.null(desc) && length(desc) > 0) {
          
          df <- filter(df, descricao_produto %in% desc)
        }
        
        df
      })
      
      tbl_mes_a_mes <- reactive({
        base_B() %>%
          mutate(
            mes_nome = factor(
              mes,
              levels = 1:12,
              labels = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
              ordered = TRUE
            )
          ) %>%
          group_by(ano, nome_programa, descricao_produto, nome_unidade_medida, mes_nome) %>%
          summarise(quantidade = sum(quantidade_total, na.rm = TRUE), .groups = "drop") %>%
          arrange(mes_nome) %>%
          tidyr::pivot_wider(
            names_from = mes_nome,
            values_from = quantidade
          ) %>%
          arrange(desc(ano))
      })
      
      ## kpis ----
      # Reactive com últimos 3 anos disponíveis
      df_kpis <- reactive({
        df <- base_B()
        
        # Ordena anos disponíveis
        anos_disponiveis <- sort(unique(df$ano))
        
        # Garante que só pegue no máximo 3 últimos anos
        anos_selecionados <- tail(anos_disponiveis, 3)
        
        # Ano de referência é sempre o mais recente
        ano_ref <- max(anos_selecionados)
        
        # Filtra os dados para apenas esses anos
        df_filtrado <- df %>%
          filter(ano %in% anos_selecionados)
        
        list(
          df = df_filtrado,
          ano = ano_ref,
          anos = anos_selecionados
        )
      })
      
      # textos ----
      output$titulo_total_m2 <- renderText({ titulos_kpi()$total_m2 })
      output$titulo_total_m1 <- renderText({ titulos_kpi()$total_m1 })
      output$titulo_total    <- renderText({ titulos_kpi()$total })
      output$titulo_media_m2 <- renderText({ titulos_kpi()$media_m2 })
      output$titulo_media_m1 <- renderText({ titulos_kpi()$media_m1 })
      output$titulo_media    <- renderText({ titulos_kpi()$media })
      
      # box ----
      ## totais ----
      output$box_total_m2 <- renderText({
        kpis <- df_kpis()
        total <- kpis$df %>% filter(ano == (kpis$ano - 2)) %>% summarise(soma = sum(quantidade_total, na.rm = TRUE)) %>% pull(soma)
        total_geral <- kpis$df %>% summarise(soma = sum(quantidade_total, na.rm = TRUE)) %>% pull(soma)
        perc <- if (total_geral > 0) round(100 * total / total_geral, 2) else 0
        
        glue::glue(
          "{scales::number(total, big.mark = '.', decimal.mark = ',')}<br>",
          "<small>{scales::number(perc, big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%</small>"
        )
      })
      
      output$box_total_m1 <- renderText({
        kpis <- df_kpis()
        total <- kpis$df %>% filter(ano == (kpis$ano - 1)) %>% summarise(soma = sum(quantidade_total, na.rm = TRUE)) %>% pull(soma)
        total_geral <- kpis$df %>% summarise(soma = sum(quantidade_total, na.rm = TRUE)) %>% pull(soma)
        perc <- if (total_geral > 0) round(100 * total / total_geral, 2) else 0
        
        glue::glue(
          "{scales::number(total, big.mark = '.', decimal.mark = ',')}<br>",
          "<small>{scales::number(perc, big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%</small>"
        )
      })
      
      output$box_total <- renderText({
        kpis <- df_kpis()
        total <- kpis$df %>% filter(ano == kpis$ano) %>% summarise(soma = sum(quantidade_total, na.rm = TRUE)) %>% pull(soma)
        total_geral <- kpis$df %>% summarise(soma = sum(quantidade_total, na.rm = TRUE)) %>% pull(soma)
        perc <- if (total_geral > 0) round(100 * total / total_geral, 2) else 0
        
        glue::glue(
          "{scales::number(total, big.mark = '.', decimal.mark = ',')}<br>",
          "<small>{scales::number(perc, big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%</small>"
        )
      })
      
      ## médias ----
      # código novo
      
      output$box_media_m2 <- renderText({
        kpis <- df_kpis()
        anos <- kpis$anos
        if (length(anos) < 3) return("0")
        ano_alvo <- anos[length(anos) - 2]
        
        df <- kpis$df %>% filter(ano == ano_alvo)
        
        df_mes <- df %>%
          group_by(mes) %>%
          summarise(total_mes = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
        
        if (isTRUE(input$remover_zeros)) {
          df_mes <- df_mes %>% filter(total_mes > 0)
        }
        
        media_final <- if (nrow(df_mes) > 0) mean(df_mes$total_mes, na.rm = TRUE) else 0
        
        glue::glue("{scales::number(media_final, big.mark = '.', decimal.mark = ',', accuracy = 0.01)}")
      })
      
      output$box_media_m1 <- renderText({
        kpis <- df_kpis()
        anos <- kpis$anos
        if (length(anos) < 2) return("0")
        ano_alvo <- anos[length(anos) - 1]
        
        df <- kpis$df %>% filter(ano == ano_alvo)
        
        df_mes <- df %>%
          group_by(mes) %>%
          summarise(total_mes = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
        
        if (isTRUE(input$remover_zeros)) {
          df_mes <- df_mes %>% filter(total_mes > 0)
        }
        
        media_final <- if (nrow(df_mes) > 0) mean(df_mes$total_mes, na.rm = TRUE) else 0
        
        glue::glue("{scales::number(media_final, big.mark = '.', decimal.mark = ',', accuracy = 0.01)}")
      })
      
      output$box_media <- renderText({
        kpis <- df_kpis()
        anos <- kpis$anos
        if (length(anos) < 1) return("0")
        ano_alvo <- anos[length(anos)]
        
        df <- kpis$df %>% filter(ano == ano_alvo)
        
        df_mes <- df %>%
          group_by(mes) %>%
          summarise(total_mes = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
        
        if (isTRUE(input$remover_zeros)) {
          df_mes <- df_mes %>% filter(total_mes > 0)
        }
        
        media_final <- if (nrow(df_mes) > 0) mean(df_mes$total_mes, na.rm = TRUE) else 0
        
        glue::glue("{scales::number(media_final, big.mark = '.', decimal.mark = ',', accuracy = 0.01)}")
      })
      
      # plot ----
      ## série ----
      # código novo
      
      output$plot_serie_historica <- plotly::renderPlotly({
        kpis <- df_kpis()
        anos_plot <- kpis$anos
        
        df_long <- kpis$df %>%
          filter(ano %in% anos_plot) %>%
          mutate(
            mes = factor(
              mes,
              levels = 1:12,
              labels = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
              ordered = TRUE
            )
          ) %>%
          group_by(ano, mes) %>%
          summarise(total = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
        
        anos_presentes <- sort(unique(df_long$ano))
        
        cores_anos <- setNames(
          RColorBrewer::brewer.pal(n = max(3, length(anos_presentes)), name = "Set2"),
          anos_presentes
        )
        
        plotly::plot_ly(
          df_long,
          x = ~mes,
          y = ~total,
          color = ~as.factor(ano),
          colors = cores_anos,
          type = "bar",
          marker = list(
            line = list(color = 'black', width = 1) 
          )
        ) %>%
          plotly::layout(
            barmode = "group",
            title = "Consumo por mês",
            xaxis = list(title = ''),
            yaxis = list(title = ''),
            plot_bgcolor = '#F2F2F2',
            paper_bgcolor = '#F2F2F2',
            font = list(family = "Jost")
          )
      })
      
      ## setor ----
      output$plot_total_uge <- plotly::renderPlotly({
        
        df_unidade <-
          base_B() %>%
          filter(
            nome_programa2 %in% c('FES', 'HUOC', 'HR', 'HOF', 'HAM', 'HGV', 'HBL', 'HRA')
          ) %>%
          group_by(nome_programa2) %>%  # Nome da unidade
          summarise(qtd_total = sum(quantidade_total, na.rm = TRUE)) %>%
          arrange(desc(qtd_total)) %>%
          mutate(
            perc = round(100 * qtd_total / sum(qtd_total), 1),
            valor_formatado = number(qtd_total, big.mark = ".", decimal.mark = ","),
            perc_formatado = number(perc, big.mark = ".", decimal.mark = ","),
            hover_label = paste0(
              nome_programa2, "<br>",
              "Qtd: ", valor_formatado, "<br>",
              "(", perc_formatado, "%)"
            )
          ) %>%
          mutate(
            unidade_sigla = factor(nome_programa2, levels = nome_programa2),
            valor_formatado = scales::number(qtd_total, big.mark = ".", decimal.mark = ","),
            hover_label = paste0(
              unidade_sigla, "<br>",
              "Total: ", valor_formatado, "<br>",
              "Participação: ", perc, "%"
            )
          ) 
        
        df_unidade %>%
          plot_ly(
            x = ~qtd_total,
            y = ~unidade_sigla,
            type = 'bar',
            orientation = 'h',
            name = "Unidades Liquidadas",
            text = ~valor_formatado,
            textposition = 'outside',
            hovertext = ~hover_label,
            hoverinfo = 'text',
            marker = list(
              color = RColorBrewer::brewer.pal(
                n = max(3, nrow(df_unidade)), name = "Pastel1"),
              line = list(color = 'black', width = 1)
            )
          ) %>%
          layout(
            title = "Consumo por Unidade",
            xaxis = list(title = ''),
            yaxis = list(title = ''),
            legend = list(
              orientation = "h",
              x = 0.7, y = 1.15,
              font = list(family = "Jost", size = 10)
            ),
            font = list(family = "Jost"), 
            margin = list(l = 100),
            plot_bgcolor = '#F2F2F2',
            paper_bgcolor = '#F2F2F2'
          )
        
      })
      
      # tbl ----
      output$tbl_busca <- renderReactable({
        
        reactable(
          tbl_mes_a_mes(),
          searchable = FALSE,
          wrap = TRUE,
          showPageSizeOptions = TRUE,
          striped = TRUE,
          highlight = TRUE,
          showSortable = TRUE,
          height = "auto",
          language = br_react,
          
          defaultColDef = colDef(
            headerStyle = list(background = "#F2F2F2", fontSize = "0.75rem"),
            style = list(fontSize = "0.75rem"),
            maxWidth = 55
          ),
          
          columns = list(
            ano = colDef(name = "Ano", maxWidth = 55),
            nome_programa = colDef(name = "Unidade", maxWidth = 120),
            descricao_produto = colDef(name = "Descrição", maxWidth = 150),
            nome_unidade_medida = colDef(name = "Un.", maxWidth = 120),
            jan = colDef(name = "Jan", format = colFormat(separators = TRUE, locales = "pt-BR")),
            fev = colDef(name = "Fev", format = colFormat(separators = TRUE, locales = "pt-BR")),
            mar = colDef(name = "Mar", format = colFormat(separators = TRUE, locales = "pt-BR")),
            abr = colDef(name = "Abr", format = colFormat(separators = TRUE, locales = "pt-BR")),
            mai = colDef(name = "Maio", format = colFormat(separators = TRUE, locales = "pt-BR")),
            jun = colDef(name = "Jun", format = colFormat(separators = TRUE, locales = "pt-BR")),
            jul = colDef(name = "Jul", format = colFormat(separators = TRUE, locales = "pt-BR")),
            ago = colDef(name = "Ago", format = colFormat(separators = TRUE, locales = "pt-BR")),
            set = colDef(name = "Set", format = colFormat(separators = TRUE, locales = "pt-BR")),
            out = colDef(name = "Out", format = colFormat(separators = TRUE, locales = "pt-BR")),
            nov = colDef(name = "Nov", format = colFormat(separators = TRUE, locales = "pt-BR")),
            dez = colDef(name = "Dez", format = colFormat(separators = TRUE, locales = "pt-BR"))
          )
        )
      })
      
      # ui ----
      # output$filtro_descricao_completa <- renderUI({
      #   descricoes <- sort(unique(base_A()$descricao_produto))
      #   
      #   selectizeInput(
      #     inputId = ns("descricao_filtro"),
      #     label = "Descrição específica:",
      #     choices = descricoes,
      #     selected = NULL,
      #     multiple = TRUE,
      #     options = list(
      #       maxOptions = 1000,
      #       placeholder = 'Selecione uma ou mais descrições...',
      #       dropdownParent = 'body',
      #       size = 5
      #     )
      #   )
      #   
      # })
      
      output$filtro_descricao_completa <- renderUI({
        dados <- base_A()
        
        descricoes <- dados %>%
          group_by(descricao_produto) %>%
          summarise(
            anos = paste(sort(unique(ano)), collapse = ", "),
            max_ano = max(ano, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(label = paste0(descricao_produto, " (", anos, ")")) %>%
          arrange(desc(max_ano))
        
        descricoes %>% glimpse()
        
        selectizeInput(
          inputId = ns("descricao_filtro"),
          label = "Descrição específica:",
          choices = setNames(descricoes$descricao_produto, descricoes$label),
          selected = NULL,
          multiple = TRUE,
          options = list(
            maxOptions = 1000,
            placeholder = 'Selecione uma ou mais descrições...',
            dropdownParent = 'body',
            size = 5
          )
        )
      })
      
      # output$filtro_programa <- renderUI({
      #   programas <- df_consumo_ceos$lst_nome_programa
      #   
      #   selectInput(
      #     inputId = ns("programa_filtro"),
      #     label = "Programa:",
      #     choices = c("Selecione um programa" = "", programas),
      #     selected = NULL,
      #     multiple = FALSE
      #   )
      # })
      
      output$filtro_programa <- renderUI({
        programas <- df_consumo_ceos$lst_nome_programa
        
        tagList(
          # filtro múltiplo
          selectInput(
            inputId = ns("programa_filtro"),
            label = "Programa:",
            choices = c("Selecione um programa" = "", programas),
            selected = NULL,
            multiple = TRUE
          ),
          
          # linha de botões
          div(
            style = "display:flex; gap:8px; margin-top:4px;",
            actionButton(
              ns("btn_sel_hosp"), 
              "Selecionar 6 grandes",
              icon = icon("filter"),
              class = "btn-aplicar",
              style = "margin-right: 10px;"
              ),
            actionButton(
              ns("btn_limpar_prog"), 
              "Limpar Programa",
              icon = icon("eraser"),
              class = "btn-limpar",
              style = "margin-right: 10px;"
              )
          )
        )
      })
      
      observeEvent(input$btn_sel_hosp, {
        updateSelectInput(
          session,
          inputId = "programa_filtro",
          selected = c("HR", "HAM", "HOF", "HBL", "HGV", "HRA")
        )
      })
      
      observeEvent(input$btn_limpar_prog, {
        updateSelectInput(
          session,
          inputId = "programa_filtro",
          selected = character(0)  # limpa tudo
        )
      })
      
      output$ui_justificativas_complementares <- renderUI({
        req(input$filtro_justificativas)
        
        just <- input$filtro_justificativas
        campos <- list()
        
        if ("desabastecimento" %in% just) {
          campos[[length(campos) + 1]] <- dateRangeInput(ns("periodo_desabastecimento"), "Período de desabastecimento", language = "pt-BR", format = "dd/mm/yyyy")
        }
        
        if ("emprestimo" %in% just) {
          campos[[length(campos) + 1]] <- textAreaInput(ns("detalhe_emprestimo"), "Detalhe o(s) empréstimo(s)", width = "100%")
        }
        
        if ("nova_especialidade" %in% just) {
          campos[[length(campos) + 1]] <- textInput(ns("nova_especialidade_txt"), "Informe a(s) especialidade(s) habilitada(s)", width = "100%")
        }
        
        if ("aumento_leito" %in% just) {
          campos[[length(campos) + 1]] <- numericInput(ns("qtde_leitos"), "Indique a quantidade de leitos acrescidos", value = NA, min = 1)
        }
        
        if ("aumento_procedimento" %in% just) {
          campos[[length(campos) + 1]] <- textInput(ns("procedimentos"), "Indique o(s) procedimento(s)", width = "100%")
        }
        
        if ("fracasso_licitacao" %in% just) {
          campos[[length(campos) + 1]] <- textAreaInput(ns("itens_fracassados"), "Informe o(s) processo(s) fracassado(s)", width = "100%")
        }
        
        if ("sazonalidade" %in% just) {
          campos[[length(campos) + 1]] <- checkboxGroupInput(
            ns("tipo_sazonalidade"),
            "Indique a(s) sazonalidade(s)",
            choices = c("Gripe", "Catapora", "Dengue", "Zika", "Chikungunya", "Febre amarela"),
            inline = TRUE
          )
        }
        
        # Garantir que só tenta renderizar se houver algo
        if (length(campos) > 0) {
          layout_column_wrap(width = 1/2, gap = "16px", !!!campos)
        } else {
          NULL
        }
      })
      
      valores_justificativas <- reactiveValues()
      
      observe({
        valores_justificativas$detalhe_emprestimo <- input$detalhe_emprestimo
        valores_justificativas$nova_especialidade <- input$nova_especialidade_txt
        valores_justificativas$qtde_leitos <- input$qtde_leitos
        valores_justificativas$procedimentos <- input$procedimentos
        valores_justificativas$fracasso <- input$itens_fracassados
        valores_justificativas$sazonalidade <- input$tipo_sazonalidade
        valores_justificativas$periodo_desabastecimento <- input$periodo_desabastecimento
      })
      
      observe({
        req(input$filtro_justificativas)
        
        isolate({
          if ("emprestimo" %in% input$filtro_justificativas && !is.null(valores_justificativas$detalhe_emprestimo)) {
            updateTextAreaInput(session, "detalhe_emprestimo", value = valores_justificativas$detalhe_emprestimo)
          }
          if ("nova_especialidade" %in% input$filtro_justificativas && !is.null(valores_justificativas$nova_especialidade)) {
            updateTextInput(session, "nova_especialidade_txt", value = valores_justificativas$nova_especialidade)
          }
          if ("aumento_leito" %in% input$filtro_justificativas && !is.null(valores_justificativas$qtde_leitos)) {
            updateNumericInput(session, "qtde_leitos", value = valores_justificativas$qtde_leitos)
          }
          if ("aumento_procedimento" %in% input$filtro_justificativas && !is.null(valores_justificativas$procedimentos)) {
            updateTextInput(session, "procedimentos", value = valores_justificativas$procedimentos)
          }
          if ("fracasso_licitacao" %in% input$filtro_justificativas && !is.null(valores_justificativas$fracasso)) {
            updateTextAreaInput(session, "itens_fracassados", value = valores_justificativas$fracasso)
          }
          if ("sazonalidade" %in% input$filtro_justificativas && !is.null(valores_justificativas$sazonalidade)) {
            updateCheckboxGroupInput(session, "tipo_sazonalidade", selected = valores_justificativas$sazonalidade)
          }
          if ("desabastecimento" %in% input$filtro_justificativas && !is.null(valores_justificativas$periodo_desabastecimento)) {
            updateDateRangeInput(session, "periodo_desabastecimento", 
                                 start = valores_justificativas$periodo_desabastecimento[1],
                                 end   = valores_justificativas$periodo_desabastecimento[2])
          }
        })
      })
      
      # botões ----
      
      output$baixar_relatorio <- downloadHandler(
        filename = function() {
          glue::glue("relatorio_{input$programa_filtro}.docx")
        },
        content = function(file) {
          withProgress(message = "📊 Gerando o Relatório...", {
            
            incProgress(0.2)
            
            temp_dir <- tempdir()
            original_rmd <- "relatorio_word.Rmd"
            temp_rmd <- file.path(temp_dir, "relatorio_temp.Rmd")
            modelo_path <- file.path(temp_dir, "modelo_paisagem.docx")
            
            # Copiar modelo e Rmd para o diretório temporário
            file.copy("modelo_paisagem.docx", modelo_path, overwrite = TRUE)
            file.copy(original_rmd, temp_rmd, overwrite = TRUE)
            
            ## inserir cálculos aqui
            kpis <- df_kpis()
            
            # totais <- kpis$df %>%
            #   group_by(ano) %>%
            #   summarise(total = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
            # 
            # medias <- kpis$df %>%
            #   group_by(ano, mes) %>%
            #   summarise(total_mes = sum(quantidade_total, na.rm = TRUE), .groups = "drop") %>%
            #   group_by(ano) %>%
            #   summarise(
            #     media = mean(
            #       total_mes[if (input$remover_zeros) total_mes > 0 else TRUE],
            #       na.rm = TRUE
            #     ),
            #     .groups = "drop"
            #   )
            # 
            # totais_vetor <- setNames(totais$total, paste0("ano", totais$ano))
            # medias_vetor <- setNames(medias$media, paste0("ano", medias$ano))
            # 
            # df_plot_serie <- base_B() %>%
            #   filter(ano %in% c(as.numeric(input$ano_filtro) - 2, as.numeric(input$ano_filtro) - 1, as.numeric(input$ano_filtro))) %>%
            #   mutate(
            #     mes = factor(
            #       mes,
            #       levels = 1:12,
            #       labels = c("jan", "fev", "mar", "abr", "mai", "jun", 
            #                  "jul", "ago", "set", "out", "nov", "dez"),
            #       ordered = TRUE
            #     )
            #   ) %>%
            #   group_by(ano, mes) %>%
            #   summarise(total = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
            
            # código novo
            # Determinar anos a partir do df filtrado
            anos_disponiveis <- sort(unique(base_B()$ano))
            anos_selecionados <- tail(anos_disponiveis, 3)
            
            # Totais por ano
            totais <- base_B() %>%
              filter(ano %in% anos_selecionados) %>%
              group_by(ano) %>%
              summarise(total = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
            
            # Médias mensais por ano (respeitando a opção de remover zeros)
            medias <- base_B() %>%
              filter(ano %in% anos_selecionados) %>%
              group_by(ano, mes) %>%
              summarise(total_mes = sum(quantidade_total, na.rm = TRUE), .groups = "drop") %>%
              group_by(ano) %>%
              summarise(
                media = mean(
                  total_mes[if (input$remover_zeros) total_mes > 0 else TRUE],
                  na.rm = TRUE
                ),
                .groups = "drop"
              )
            
            # Prepara vetores nomeados para passar como parâmetro
            totais_vetor <- setNames(totais$total, paste0("ano", totais$ano))
            medias_vetor <- setNames(medias$media, paste0("ano", medias$ano))
            
            # Dados para gráfico de série histórica
            df_plot_serie <- base_B() %>%
              filter(ano %in% anos_selecionados) %>%
              mutate(
                mes = factor(
                  mes,
                  levels = 1:12,
                  labels = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez"),
                  ordered = TRUE
                )
              ) %>%
              group_by(ano, mes) %>%
              summarise(total = sum(quantidade_total, na.rm = TRUE), .groups = "drop")
            
            # Renderiza o relatório com parâmetros
            rmarkdown::render(
              input = temp_rmd,
              output_file = "relatorio_quantitativo.docx",
              output_dir = temp_dir,
              params = list(
                unidade_saude = input$programa_filtro,
                ano_filtro = input$ano_filtro,
                justificativas = input$filtro_justificativas,
                descricao_filtro = input$descricao_filtro,
                totais_vetor = totais_vetor,
                medias_vetor = medias_vetor,
                numero_processo = input$numero_processo,
                numero_ipr = input$numero_ipr,
                objeto_irp = input$objeto_irp,
                tipo_item = input$tipo_item,
                nome_solicitante = input$nome_solicitante,
                cargo_solicitante = input$cargo_solicitante,
                periodo_desabastecimento = input$periodo_desabastecimento,
                detalhe_emprestimo = input$detalhe_emprestimo,
                nova_especialidade = input$nova_especialidade_txt,
                qtde_leitos = input$qtde_leitos,
                procedimentos = input$procedimentos,
                fracasso_licitacao = input$itens_fracassados,
                tipo_sazonalidade = input$tipo_sazonalidade,
                df_plot_serie = df_plot_serie,
                tbl_mes = tbl_mes_a_mes()
              ),
              envir = new.env(parent = globalenv()),
              output_options = list(reference_docx = modelo_path)
            )
            
            # Copiar resultado final para o destino do download
            file.copy(file.path(temp_dir, "relatorio_quantitativo.docx"), file, overwrite = TRUE)
          })
        }
      )
      
    }
  )
} 
