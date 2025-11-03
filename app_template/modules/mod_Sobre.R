# ui_Sobre
# Cria um painel UI com informações sobre a Secretaria Executiva de Monitoramento Estratégico.
# Este painel inclui detalhes como missão, valores, equipe e colaboradores, organizados em seções visualmente distintas.
#
# Args:
#   id: Identificador único para os elementos da UI, facilitando a manipulação no lado do servidor em aplicativos Shiny.
#
# Returns:
#   Um painel UI contendo várias seções com informações detalhadas sobre a organização, sua equipe e colaboradores.
#
# Components:
# - nav_panel: Utilizado para criar um painel de navegação.
# - card: Estrutura para conter as diferentes seções de informações.
# - card_header: Cabeçalho para cada card, geralmente contendo o título da seção.
# - card_body: Corpo de cada card, onde as informações são efetivamente apresentadas.
# - layout_column_wrap: Layout para organizar conteúdo em colunas.
# - value_box: Caixas de valor utilizadas para destacar membros da equipe e colaboradores.

ui_Sobre <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Sobre",
    card(
      class = "sobre-pikchr",
      full_screen = FALSE, # min_height = '200px',
      card_header("Sobre a Secretaria Executiva de Monitoramento Estratégico", class = "bg-secondary"),
      card_body(
        class = "cards-sobre-info",
        layout_column_wrap(
          width = NULL, fill = TRUE,
          style = css(text.align = "justify", grid_template_columns = "2fr 1fr", justify.content = "center"),
          markdown("
### Missão
Promover o desenvolvimento sustentável e a eficiência na gestão pública, fornecendo informações estratégicas e apoio decisivo às autoridades governamentais, por meio de monitoramento, análise e avaliação contínuos, para garantir a entrega eficaz de serviços públicos de qualidade à sociedade.
                "),
          div(style = "display: flex; justify-content: center; margin-top: 15px;", tags$img(src = "images/logo_pe.svg", height = 150, style = "max-width:80%"))
        ),
        markdown("
### Valores
1. **Transparência:** Comprometemo-nos a operar de forma aberta e transparente, garantindo o acesso à informação estratégica para o público em geral, promovendo a prestação de contas e a confiança na gestão governamental.

2. **Integridade:** Pautamos nossas ações pelos mais elevados padrões éticos, agindo com honestidade, responsabilidade e imparcialidade em todos os nossos processos de monitoramento e análise.

3. **Eficiência:** Buscamos a excelência na gestão dos recursos públicos, otimizando processos e garantindo o uso eficaz dos recursos disponíveis para atingir nossos objetivos estratégicos.

4. **Colaboração:** Valorizamos parcerias e cooperação interinstitucional, promovendo o trabalho em equipe e a troca de conhecimentos para alcançar resultados melhores e mais abrangentes.

5. **Inovação:** Estamos comprometidos com a busca contínua por soluções inovadoras para o aprimoramento da gestão pública, aplicando tecnologias e metodologias avançadas de monitoramento estratégico.

6. **Foco no Cidadão:** Colocamos o cidadão no centro de nossas ações, priorizando suas necessidades e expectativas na tomada de decisões e no monitoramento das políticas públicas.

7. **Responsabilidade Ambiental:** Consideramos os impactos ambientais em nossas ações e promovemos a adoção de práticas sustentáveis em todas as áreas de atuação.

8. **Aprendizado Contínuo:** Buscamos aprimorar constantemente nossas habilidades e conhecimentos para nos mantermos atualizados e eficazes na prestação de serviços de monitoramento estratégico.
                ")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Equipe", class = "bg-secondary"),
      card_body(
        class = "cards_sobre",
        layout_column_wrap(
          fill = FALSE,
          width = "475px", gap = "1rem",
          value_box(
            title = "Secretário-Chefe da SEMOBI",
            value = "Diogo Bezerra",
            theme = "primary",
            showcase = tags$img(class = "mlk-userbox", src = "images/diogo.png"),
            p(bs_icon("envelope-at-fill"), "diogo.bezerra@semobi.pe.gov.br")
          ),
          value_box(
            title = "Secretário Executivo de Monitoramento",
            value = "André Leite",
            theme = "secondary",
            showcase = tags$img(class = "mlk-userbox", src = "images/leite.png"),
            p(bs_icon("envelope-at-fill"), "andre.leite@sepe.pe.gov.br")
          ),
          value_box(
            title = "Diretora Jurídica da SEMOBI",
            value = "Hortênsia Oliveira",
            theme = "success",
            showcase = tags$img(class = "mlk-userbox", src = "images/hortensia.jpg"),
            p(bs_icon("envelope-at-fill"), "hortensia.nunes@semobi.pe.gov.br")
          ),
          value_box(
            title = "Gestor de Projetos Especiais (SEPLAG)",
            value = "Hugo Medeiros",
            theme = "text-dark",
            showcase = tags$img(class = "mlk-userbox", src = "images/hugo.png"),
            p(bs_icon("envelope-at-fill"), "hugo.medeiros@semobi.pe.gov.br")
          ),
          value_box(
            title = "Assessor Especial",
            value = "Rafael Zimmerle",
            theme = "danger",
            showcase = tags$img(class = "mlk-userbox", src = "images/rafael.png"),
            p(bs_icon("envelope-at-fill"), "rafael.zimmerle@sepe.pe.gov.br")
          ),
          value_box(
            title = "Assessor Especial",
            value = "Carlos Amorim",
            theme = "warning",
            showcase = tags$img(class = "mlk-userbox", src = "images/carlos.png"),
            p(bs_icon("envelope-at-fill"), "carlos.andrade@sepe.pe.gov.br")
          )
        )
      )
    )
  )
}

# server_Sobre
# Define o servidor de um módulo Shiny para gerenciar a lógica do lado do servidor associada ao módulo UI 'Sobre'.
# Esta função é parte do padrão de modularização em Shiny, permitindo separar a lógica de UI da lógica do servidor,
# o que facilita a manutenção e reutilização do código em aplicativos Shiny grandes e complexos.
#
# Args:
#   id: Identificador único para o módulo, utilizado para associar este servidor ao seu respectivo UI.
#
# Exemplo de uso:
#   Dentro do servidor principal de um aplicativo Shiny, chame esta função para ativar a lógica do servidor
#   específica para o módulo 'Sobre'. Isso requer que um módulo UI correspondente, tipicamente criado pela
#   função 'ui_Sobre(id)', já esteja definido e incluído no UI do aplicativo.
#
# Nota:
#   - Este módulo está atualmente vazio, indicando que nenhuma lógica específica do servidor foi implementada
#     para o módulo 'Sobre'. Isso pode incluir, por exemplo, reagir a entradas do usuário, atualizar elementos UI,
#     ou realizar cálculos baseados em dados do lado do servidor.
#   - Para adicionar funcionalidades a este módulo, implemente a lógica necessária dentro da função anônima fornecida
#     ao 'moduleServer'.
server_Sobre <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Implemente a lógica do servidor aqui. Por exemplo, você pode querer reagir a ações do usuário,
      # atualizar outputs baseados em entradas, ou realizar operações de dados.
      ns <- session$ns

    }
  )
}
