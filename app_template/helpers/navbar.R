fn_navbar <- function(){
  a(href = "https://box.pe.gov.br",
    class = "govpe-logo",
    target="_blank",
    rel="noopener noreferrer",
    #<i class="fa-brands fa-waze"></i>
    tags$img(
      src = "images/GovPE100Preto 3.svg",
      style = "filter: invert(1); width: 100px; padding-left: 5px; padding-right: 5px;"),
    tags$img(
      src = "images/argos.svg",
      style = "filter: invert(1); width: 50px; height: auto; padding-left: 5px; padding-right: 5px;")
  )
}

## Links

# link_home <-  tags$a(HTML("Contato"), href = "https://www.seplag.pe.gov.br/38-secretaria/30-segpr", target="_blank", rel="noopener noreferrer")

link_home <- tags$a(
  HTML("Contato"),
  href = "mailto:segpr@seplag.pe.gov.br"
)

link_git <- tags$a(shiny::icon("github"), href = "https://github.com/segpr-ndgr", target = "_blank", rel="noopener noreferrer")

link_logout <- actionLink(inputId = "logout", icon = shiny::icon("arrow-right-from-bracket"), label = "") # , href = "https://monitoramento.sepe.pe.gov.br", target = "_self")

