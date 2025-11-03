cat("🖼️  Renderizando UI...\n")

ui <- page(

  ## Uso da função tag para utilizar a estrutura HTML
  title = "Argos | CEMEP",
  tags$script(
    src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
  ),
  # Inclusão de estilos CSS externos
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "login.css"),
  ),
  tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200")
  ),

  useShinyjs(),
  useWaiter(),

  includeScript("www/script.js"),

  theme = fn_custom_theme(),

  tagList(
    page_navbar(
      fillable = FALSE,
      lang = "pt",
      #navbar_options = navbar_options(collapsible = TRUE, underline = FALSE),
      window_title = "Monitoramento &mdash; Dashboard",
      title = fn_navbar(),
      footer = div(
        class = "mlk-footer-logos",
        tags$a(
          href = "https://pt.wikipedia.org/wiki/Café",
          target = "_blank",
          img(
            src = "images/coffee.svg",
            height = "50",
            alt = "Coffee")),
        tags$a(
          href = "https://pt.wikipedia.org/wiki/Ubuntu_(filosofia)",
          target = "_blank",
          img(
            src = "images/ubuntu_logo.svg",
            height = "50",
            alt = "I am what I am because of who we all are")),
        tags$a(
          href = "https://www.r-project.org/",
          target = "_blank",
          img(
            src = "images/Rlogo.svg",
            height = "50",
            alt = "R Project")),
        tags$a(
          href = "https://shiny.rstudio.com/",
          target = "_blank",
          img(
            src = "images/shiny_hex_logo.svg",
            height = "50",
            alt = "Shiny Package")),
        tags$a(
          href = "https://greenplum.org/",
          target = "_blank",
          img(
            src = "images/greenplum_logo.png",
            height = "50",
            alt = "Greenplum")),
        tags$a(
          href = "https://www.bigdata.pe.gov.br/",
          target = "_blank",
          img(
            src = "images/bigdata_logo.png",
            height = "50",
            style = "border-radius: 5px;",
            alt = "BigData Pernambuco")),
        tags$a(
          href = "https://www.pe.gov.br/",
          target = "_blank",
          img(
            src = "images/logo_pe.svg",
            height = "50",
            alt = "Governo de Pernambuco"))),

      id = NS("main","tabs"),
      nav_panel(
        title = "Sobre",
        class = "mlk-main",
        value = "sobre",
        spsGoTop(
          "default",
          right = "3%",
          bottom= "3%",
          icon = icon("arrow-up"),
          color = "#007bff"),
        ui_Sobre("sobre")
      ),
      nav_spacer(),
      nav_item(link_home),
      nav_item(link_git)
    )
  )
)
