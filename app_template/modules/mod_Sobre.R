ui_Sobre <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Sobre",
    #### Equipe ----
    card(
      style = "margin-bottom: 20px;", 
      full_screen = TRUE,
      card_header(
        style = "background-color: #f7f7f7;", 
        "Equipe", 
        class = "bg-light"
      ),
      card_body(
        class = "cards_sobre",
        style = "padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);", 
        layout_column_wrap(
          fill = TRUE,
          width = "400px", gap = "1rem",
          value_box(
            style = "padding: 15px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); text-align: center;",
            title = "Secretário de Planejamento (SEPLAG)",
            value = "Fabrício Marques",
            theme = value_box_theme(bg = "#F9E79F", fg = "black"),
            showcase = tags$img(class = "mlk-userbox", src = "images/fabricio_marques.png", style = "border-radius: 50%; width: 91px; height: 91px; object-fit: cover; margin-bottom: 10px;"),
            tags$p(
              style = "position: absolute; bottom: 10px; right: 10px; font-size: 0.9em; color: black;",
              tags$a(href = "mailto:fabricio.marques@seplag.pe.gov.br", bs_icon("envelope-at-fill"), " fabricio.marques@seplag.pe.gov.br", style = "color: black; text-decoration: none;")
            )
          ),
          value_box(
            style = "padding: 15px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); text-align: center;",
            title = "Secretário Executivo de Gestão para Resultados (SEPLAG)",
            value = "Ricardo Albuquerque",
            theme = value_box_theme(bg = "#A9DFBF", fg = "black"),
            showcase = tags$img(class = "mlk-userbox", src = "images/ricardo_albuquerque.png", style = "border-radius: 50%; width: 91px; height: 91px; object-fit: cover; margin-bottom: 10px;"),
            tags$p(
              style = "position: absolute; bottom: 10px; right: 10px; font-size: 0.9em; color: black;",
              tags$a(href = "mailto:ricardoluiz.moreira@seplag.pe.gov.br", bs_icon("envelope-at-fill"), " ricardoluiz.moreira@seplag.pe.gov.br", style = "color: black; text-decoration: none;")
            )
          ),
          value_box(
            style = "padding: 15px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); text-align: center;",
            title = "Gerente Geral de Dados para Resultados (SEPLAG)",
            value = "Hugo Medeiros",
            theme = value_box_theme(bg = "#A9DFBF", fg = "black"),
            showcase = tags$img(class = "mlk-userbox", src = "images/hugo.png", style = "border-radius: 50%; width: 91px; height: 91px; object-fit: cover; margin-bottom: 10px;"),
            tags$p(
              style = "position: absolute; bottom: 10px; right: 10px; font-size: 0.9em; color: black;",
              tags$a(href = "mailto:hugo.vasconcelos@seplag.pe.gov.br", bs_icon("envelope-at-fill"), " hugo.vasconcelos@seplag.pe.gov.br", style = "color: black; text-decoration: none;")
            )
          ),
          value_box(
            style = "padding: 15px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); text-align: center;",
            title = "Analista de Dados (SEPLAG)",
            value = "Júlia Barreto",
            theme = value_box_theme(bg = "#A9DFBF", fg = "black"),
            showcase = tags$img(class = "mlk-userbox", src = "images/julia_barreto.png", style = "border-radius: 50%; width: 91px; height: 91px; object-fit: cover; margin-bottom: 10px;"),
            tags$p(
              style = "position: absolute; bottom: 10px; right: 10px; font-size: 0.9em; color: black;",
              tags$a(href = "mailto:juliabarreto@seplag.pe.gov.br", bs_icon("envelope-at-fill"), " juliabarreto@seplag.pe.gov.br", style = "color: black; text-decoration: none;")
            )
          )
        )
      )
    )
    
  )
}


server_Sobre <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns

    }
  )
}
