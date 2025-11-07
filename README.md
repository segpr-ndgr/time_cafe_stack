---
title: "🧠 Stack - Time Café GOVPE (R)"
author: "Time Café GOVPE"
output:
  html_document:
    toc: true
    toc_float: true
    number_sections: true
    df_print: paged
lang: "pt-BR"
---

# ☕ Visão geral

Este documento apresenta o **stack mínimo de desenvolvimento em R** adotado pelo **Time Café GOVPE** — um guia prático para configurar o ambiente, entender o estilo de código e se integrar rapidamente ao time.

---

# ⚙️ Pré-requisitos

## 💻 Desenvolvimento

| Ferramenta | Descrição / Link |
|-------------|------------------|
| **R >= 4.5** | [Download CRAN](https://cran.r-project.org/bin/windows/base/) |
| **RStudio** | [Download IDE](https://posit.co/download/rstudio-desktop/) |
| **R Tools** | [Download](https://cran.r-project.org/bin/windows/Rtools/) |

**Pacotes essenciais:**
```r
install.packages("tidyverse")
install.packages("~/projetos/segpr_ndgr/time_cafe_stack/pacotes/vialactea_0.0.199.tar.gz", repos = NULL, type = "source")
```

## 🏭 Produção

- Docker ([site oficial](https://www.docker.com/get-started/))
- VPN ATI
- Acesso aos servidores de **homologação**, **produção** e **banco**
- Acesso aos **GitHubs** do time

### 📦 Pacotes principais (stack mínimo)

| Categoria | Pacotes |
|------------|----------|
| **Framework** | `shiny` |
| **Banco de dados** | `DBI`, `dbplyr`, `RPostgres`, `pool` |
| **Ciência de dados** | `echarts4r`, `ggplot2`, `mapgl`, `plotly`, `sf` |
| **Segurança** | `shinymanager`, `safer`, `sodium` |
| **UI / UX** | `bslib`, `bsicons`, `htmltools`, `htmlwidgets`, `shinyWidgets` |

---

# 🖥️ Shiny no stack

## O que é o Shiny?

**Shiny** é um *framework* em R que permite criar aplicações web interativas — como dashboards e painéis — **usando apenas R**.  
Ele integra o código R diretamente com tecnologias web, como **HTML**, **CSS** e **JavaScript**, tornando possível construir interfaces modernas sem sair do ecossistema R.

## Relação com HTML, CSS e JavaScript

- **HTML:** O Shiny gera automaticamente o HTML da interface através de funções como `fluidPage()`, `sidebarLayout()`, `tags$div()`, etc.  
  Tudo que aparece na tela é, no fundo, HTML renderizado no navegador.  

- **CSS:** O estilo visual é baseado no **Bootstrap** (via `{bslib}`), e pode ser personalizado com arquivos `.css` no diretório `app/www`.  
  É possível alterar temas, fontes, espaçamentos e cores sem precisar modificar o código R.

- **JavaScript:** O comportamento dinâmico (animações, eventos, mensagens cliente-servidor) é controlado por JavaScript.  
  O Shiny faz essa ponte via `shinyjs`, `htmlwidgets` ou `session$sendCustomMessage()`.

> Em resumo: o Shiny é a “cola reativa” que conecta **R (lógica de dados)** com **HTML/CSS/JS (camada de apresentação)**.

## Estrutura típica do Shiny

```
app/
├─ www/          # arquivos estáticos (CSS, JS, imagens)
├─ modules/      # módulos reutilizáveis de interface e lógica
├─ helpers/      # funções utilitárias para o app
├─ global.R      # objetos e variáveis globais
├─ server.R      # lógica reativa (back-end em R)
└─ ui.R          # layout e componentes da interface
```

## Exemplo mínimo

```r
library(shiny)

ui <- fluidPage(
  titlePanel("Exemplo Shiny GOVPE"),
  sidebarLayout(
    sidebarPanel(textInput("nome", "Digite seu nome:"), actionButton("ok", "Enviar")),
    mainPanel(textOutput("saudacao"))
  )
)

server <- function(input, output, session) {
  output$saudacao <- renderText({
    req(input$ok)
    paste("Olá,", input$nome, "— bem-vindo(a) ao Time Café GOVPE!")
  })
}

shinyApp(ui, server)
```

## Exemplo do stack

Quer ver o stack em funcionamento?  
Na pasta `app_template/` você encontra um exemplo completo de app Shiny com estrutura e layout padrão do time.  

Abra o projeto no RStudio, execute:
```r
shiny::runApp("app_template")
```

## Boas práticas

- Modularize telas e fluxos (`mod_nome_ui`, `mod_nome_server`).  
- Use `{bslib}` para unificar temas e estilos.  
- Armazene CSS/JS personalizados em `app/www/`.  
- Sempre isole scripts de manipulação de dados fora do `server`.  
- Teste reatividade com `req()` e `observeEvent()` antes de expandir o app.


## 🧩 Convenções de código

- **Estilo:** encadeamento com `%>%`, seções com `# ----`.
- **Nomenclatura:** `snake_case` para objetos e arquivos.
- **Scripts reprodutíveis:** parametrizados, sem caminhos absolutos.
- **Aleatoriedade:** sempre defina `set.seed()` quando aplicável.


---

# 📁 Trabalhando com projetos no RStudio

## O que é um projeto no RStudio?

Um **projeto RStudio (`.Rproj`)** é uma forma de organizar o trabalho em R com isolamento, reprodutibilidade e caminhos consistentes.  
Quando você abre um projeto, o RStudio define automaticamente:

- o **diretório de trabalho** (`getwd()`) como a raiz do projeto;  
- o **ambiente de sessão** e histórico específicos daquele projeto;  
- e, se configurado, ativa dependências via **`renv`**, **`packrat`** ou **Makefile**.

Isso evita confusões com caminhos absolutos e mantém os scripts portáveis — algo essencial em ambientes colaborativos como o Time Café GOVPE.

## Por que usar projetos?

| Benefício | Descrição |
|------------|------------|
| **Reprodutibilidade** | Scripts rodam em qualquer máquina com o mesmo comportamento. |
| **Organização** | Cada projeto tem sua estrutura (`data/`, `etl/`, `app/`, etc.). |
| **Integração com Git** | O RStudio detecta e integra repositórios Git automaticamente. |
| **Ambientes isolados** | Dependências e histórico não se misturam entre projetos. |

## Como criar um projeto

1. No RStudio, vá em:  
   **File → New Project → Existing Directory** (ou *New Directory* para um novo).  
2. Escolha a pasta raiz do seu stack (ex: `time_cafe_stack/`).  
3. O RStudio criará um arquivo `nome_do_projeto.Rproj`.  
4. A partir daí, sempre **abra o projeto clicando nesse arquivo** — não pelo menu “Open File”.

💡 *Dica:* O `.Rproj` deve ficar **na raiz** do repositório e ser versionado junto com o restante dos arquivos.

## Boas práticas para o time

- **Nunca use caminhos absolutos** (`C:/Users/...`) — prefira caminhos relativos, ex.:  
  ```r
  read_csv("data/insumos.csv")
  ```  
  (ao abrir pelo `.Rproj`, o diretório raiz já é reconhecido automaticamente)
  
- **Separe scripts por tipo:** `etl/`, `app/`, `scripts/`, `pacotes/`.  
- **Mantenha o ambiente limpo:** evite `rm(list = ls())` e prefira reiniciar a sessão (Ctrl + Shift + F10).  
- **Ative o controle de versão** (Git) dentro do projeto — o RStudio já exibe aba “Git” quando detecta um repositório.  
- **Centralize dependências** em `renv::init()` ou `requirements.R` para facilitar reprodutibilidade.

## Exemplo de fluxo de trabalho

```bash
# 1. Abrir o projeto
time_cafe_stack/
├─ time_cafe_stack.Rproj

# 2. No RStudio:
#    - Ctrl + Shift + O abre o projeto
#    - Ctrl + Shift + F10 reinicia sessão

# 3. Rodar scripts do stack
source("etl/etl_safety_supply.R")
shiny::runApp("app_template/")
```

## Integração com o Stack GOVPE

O arquivo `.Rproj` do stack já traz **configurações básicas**:
- encoding UTF-8  
- uso de tabs com 2 espaços  
- opções de salvamento padrão  
- compatibilidade com o `Makefile` e o `renv` do time  

> 💬 *Sempre inicie seu trabalho a partir do `.Rproj` da raiz do projeto — é o ponto de partida oficial para manter consistência entre máquinas e desenvolvedores.*

## 🗂️ Estrutura de pastas sugerida

```bash
projeto/
├─ _legado/         # códigos antigos ou descontinuados
├─ app/             # aplicativo principal
│  ├─ data/         # dados do app
│  ├─ helpers/      # funções auxiliares
│  ├─ mapas/        # dados geográficos
│  ├─ modules/      # módulos do app
│  ├─ www/          # estáticos: imagens, fontes, estilo...
│  ├─ global.R
│  ├─ server.R
│  └─ ui.R
├─ data/            # dados brutos ou tratados
├─ docker/          # imagens e configurações Docker
├─ etl/             # scripts de ETL
├─ pacotes/         # pacotes fora do CRAN
├─ references/      # pdfs, links e docs de apoio
├─ scripts/         # scripts diversos
├─ .dockerignore
├─ .gitignore
├─ README.md
└─ projeto.Rproj
```

---

# 🏭 Deploy e Alternativas de Produção

O time de desenvolvimento utiliza a plataforma **ShinyProxy** como padrão para o ambiente de produção. Essa escolha é motivada pela necessidade de **isolamento de sessão** e **escalabilidade** inerentes à arquitetura de contêineres.

## Comparação: Shiny Server vs ShinyProxy

| Aspecto | Shiny Server (Alternativa) | ShinyProxy (Solução do Time) |
| :--- | :--- | :--- |
| **Lançamento do App** | Diretamente no sistema operacional. | Via Container Docker. |
| **Arquitetura** | Sessões compartilham recursos do SO. Baixo isolamento. | Um container por sessão. Alto isolamento. |
| **Isolamento** | Baixo. | Alto. |
| **Escalabilidade** | Limitada. | Excelente (Containers leves e sob demanda). |
| **Facilidade de Uso** | Muito fácil (Deploy *as is*). | Requer conhecimento de Docker e montagem da imagem. Curva de aprendizado maior. |

## Por que o Time Café GOVPE usa ShinyProxy?

O **ShinyProxy** utiliza a tecnologia Docker, onde cada sessão de usuário é executada em um contêiner separado. Isso é crucial para as aplicações do governo, pois:

* **Segurança e Isolamento:** Garante alto isolamento entre sessões.
* **Controle de Recursos:** Permite controle de recursos por usuário.
* **Ambiente Padronizado:** A aplicação é empacotada com todas as suas dependências em uma Imagem Docker, garantindo que o ambiente seja padronizado e o deploy facilitado.

# 🌐 Arquitetura de Infraestrutura

A infraestrutura do Time Café GOVPE é segmentada em três ambientes principais, otimizados para cada etapa do ciclo de vida de uma aplicação Shiny (Desenvolvimento, ETL e Produção).

## 💻 Desenvolvimento (Notebooks / Desktops dos Desenvolvedores)

* **Finalidade:** Codificação, testes locais e prototipagem dos aplicativos Shiny, scripts de ETL e pacotes R.
* **Características:** Ambiente local não conectado à produção. É aqui que o código é versionado (Git) antes de ir para os repositórios do time.
* **Dados:** Utiliza bases de dados de desenvolvimento ou amostras.

## ⚙️ Rotinas e ETL (Máquina 1 - Rotinas SEGPR)

* **Finalidade:** Execução das rotinas programadas de **ETL (Extração, Transformação e Carga)** do time.
* **Características:**
    * Roda scripts que consomem dados de diversas fontes (APIs, bancos) e os processa.
    * Possui um **serviço WebDAV** ativo, que serve como *stage* para hospedar os arquivos de dados (CSV, RDS, etc.) que serão consumidos pelo ambiente de produção.
* **Fluxo de Dados:** Geração de dados processados e disponibilização via WebDAV.

## 🚀 Produção (Máquina 2 - Plataforma Resultados)

* **Finalidade:** Hospedagem e disponibilização pública dos aplicativos Shiny do time.
* **Plataforma:** Roda o **ShinyProxy**, garantindo o alto isolamento e escalabilidade dos aplicativos.
* **Características de Segurança/Dados:**
    * **Volumes Read-Only (ro):** Os containers do ShinyProxy acessam os dados em *volumes de montagem* com permissão apenas de leitura. Isso impede que os aplicativos (e, consequentemente, os usuários) alterem qualquer arquivo de dado.
    * **Rotina de Sincronização:** Uma rotina de sistema é responsável por buscar os dados atualizados no WebDAV da **Máquina 1** e atualizar os volumes de leitura da **Máquina 2**.
* **Exceção (SI Tradicional):** Aplicativos como os de "Administração" e "Encaminhamentos" (que se assemelham a Sistemas de Informação tradicionais, embora feitos em R/Shiny) também são hospedados aqui. Eles se conectam diretamente ao **Banco (Máquina 3)** para operações de leitura/escrita transacionais.

## 🗄️ Banco de Dados (Máquina 3)

* **Finalidade:** Servir como o repositório central de dados transacionais e de administração.
* **Uso Primário:** Essencial para os aplicativos de **Administração** e **Encaminhamentos**, que exigem operações de escrita/atualização de dados (transacionais).
* **Uso Secundário (ETL):** Também é fonte de dados para as rotinas de ETL da **Máquina 1**.

---

# 🚀 Onboarding

## 📚 Como começar

1. Leia o artigo seminal do time:  
   📰 [*Government Data Science Teams – A Framework for Implementing Strategic Monitoring Solutions*](https://proceedings.open.tudelft.nl/DGO2025/article/view/925)  
   (Premiado como **Melhor Política Pública de Inovação Digital – 2025**)

2. Faça os cursos recomendados:  
   - [Curso R – Básico (livro.curso-r.com)](https://livro.curso-r.com/)  
   - [EVG – Análise de Dados em Linguagem R](https://www.escolavirtual.gov.br/curso/325)  
   - [YouTube – R para Políticas (Prof. Hugo Medeiros)](https://www.youtube.com/channel/UCtg6tgjgrFTWkWKCFN22HOg)

## 🌐 Plataformas de referência

- [Portal Resultados](https://resultados.seplag.pe.gov.br/)  
- [Portal Box](https://box.pe.gov.br/)

## 📦 Pacotes R do Time

| Pacote | Link |
|--------|------|
| **BigDataPE** | [monitoramento.sepe.pe.gov.br/bigdatape](https://monitoramento.sepe.pe.gov.br/bigdatape/) |
| **capesR** | [hugoavmedeiros.github.io/capesR](https://hugoavmedeiros.github.io/capesR/) |
| **diario** | [monitoramento.sepe.pe.gov.br/diario](https://monitoramento.sepe.pe.gov.br/diario/) |
| **pikchr** | [monitoramento.sepe.pe.gov.br/pikchr](https://monitoramento.sepe.pe.gov.br/pikchr/) |
| **plug** | [monitoramento.sepe.pe.gov.br/plug](https://monitoramento.sepe.pe.gov.br/plug/) |
| **RapidFuzz** | [monitoramento.sepe.pe.gov.br/rapidfuzz](https://monitoramento.sepe.pe.gov.br/rapidfuzz/) |
| **WebDAV** | [monitoramento.sepe.pe.gov.br/webdav](https://monitoramento.sepe.pe.gov.br/webdav/) |
| **whapi** | [monitoramento.sepe.pe.gov.br/whapi](https://monitoramento.sepe.pe.gov.br/whapi/) |

---

# 👥 Time Café GOVPE

| Nome | Função | LinkedIn |
|------|---------|-----------|
| **André Leite** | Cientista de Dados Chefe | [linkedin.com/in/milkway](https://www.linkedin.com/in/milkway/) |
| **Diogo Bezerra** | Pesquisador Chefe | [linkedin.com/in/dicbezerra](https://www.linkedin.com/in/dicbezerra/) |
| **Hugo Medeiros** | Cientista de Dados Sênior | [linkedin.com/in/hugoavmedeiros](https://www.linkedin.com/in/hugoavmedeiros/) |
| **Marcos Wasiliew** | Cientista de Dados Pleno | []() |
| **Júlia Barreto** | Cientista de Dados Júnior | [linkedin.com/in/j%C3%BAlia-barr%C3%AAto/](https://www.linkedin.com/in/j%C3%BAlia-barr%C3%AAto/) |
| **Miguel Santos** | Cientista de Dados Júnior | [linkedin.com/in/miguel-santos-6a66322b6/](https://www.linkedin.com/in/miguel-santos-6a66322b6/) |


---

📍 *Este documento faz parte do stack padrão do Time Café GOVPE e serve como referência para novos integrantes e projetos em R.*
