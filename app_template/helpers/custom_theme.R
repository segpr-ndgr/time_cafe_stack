# Overarching bslib theme
fn_custom_theme <- function() {
  bslib::bs_theme(
    version = "5",
    # base_font = sass::font_google("Jost", ital = c(0, 1), 
    #                               wght = "200..900"),
    base_font = sass::font_link(
      "Jost",
      href="https://fonts.googleapis.com/css2?family=Jost:ital,wght@0,100..900;1,100..900&display=swap"
    ),
    preset = "flatly", #"zephyr",
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE,
    `enable-rounded` = TRUE,
    #bootswatch = "flatly", #"flatly",
    
    # fg = "rgb(16, 100, 176)",
    # bg = "#fff",
    # primary = "#142755", # "rgb(20, 39, 85)",
    # secondary = "#0708F5", # "rgb(7, 8, 245)",
    # success = "#76DF43", # "rgb(118, 223, 67)",
    info = "#400FFF", # "rgb(84, 178, 249)",
    warning = "#FFCE00", # "rgb(240, 177, 61)",
    danger = "#ED282C", #"rgb(216, 56, 49)"
    ) |>
    # bs_add_variables("border-bottom-width" = "6px",
    #                  "border-color" = "$primary",
    #                  .where = "declarations") |>
    bs_add_rules(
      sass::sass_file("www/styles.scss")
      )
}


br_react <- reactable::reactableLang(
  searchPlaceholder = "Pesquisar...",
  noData = "Nenhum item encontrado.",
  pageSizeOptions = "Mostrar {rows}",
  pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
  pagePrevious = "\u276e",
  pageNext = "\u276f",
  pagePreviousLabel = "Pág. anterior",
  pageNextLabel = "Pág. seguinte"
)
# # Thematic theme for ggplot2 outputs
# fn_thematic_theme <- function() {
#   thematic::thematic_theme(
#     bg = "#ffffff",
#     fg = "#1d2d42",
#     accent = "#f3d436",
#     font = font_spec(sass::font_google("Open Sans"), scale = 1.75)
#   )
# }