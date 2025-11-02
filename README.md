---
title: "Stack - Time Café GOVPE (R)"
author: "Time Café GOVPE"
output:
  html_document:
    toc: true
    toc_float: true
    number_sections: true
    df_print: paged
lang: "pt-BR"
---

# Visão geral

Este arquivo apresenta **stack mínimo de desenvolvimento em R de acordo com o modelo seguido pelo "Time Café do GOVPE"**.  

# Pré‑requisitos
## Desenvolvimento

- **R >= 4.5** (https://cran.r-project.org/bin/windows/base/)
- IDE **RStudio** (https://posit.co/download/rstudio-desktop/)  
- R Tools (https://cran.r-project.org/bin/windows/Rtools/)
- Pacote `tidyverse`
- Pacote vialactea com instalação offline: `install.packages("~/projetos/segpr_ndgr/time_cafe_stack/pacotes/vialactea_0.0.199.tar.gz", repos = NULL, type = "source")`

## Produção

- Docker (https://www.docker.com/get-started/)
- VPN ATI
- Acesso aos servers de homologação / teste, produção e banco
- Acesso ao(s) github(s) do time

**Pacotes principais deste stack (mínimo):**

- Banco de dados: `DBI`, `dbplyr`, `RPostgres`, `pool`
- Ciência de dados: `echarts4r`, `ggplot2`, `mapgl`, `plotly`, `sf`
- Segurança: `shinymanager`, `safer`, `sodium``
- UI/UX: `bslib`, `bsicons`, `htmltools`, `htmlwidgets`, `shinyWidgets`

# Estrutura sugerida de pastas

```
projeto
├─ _legado/         # códigos não utilizados mais
├─ app/             # aplicativo
   ├─ app/data      # dados do app
   ├─ app/helpers   # helpers do app
   ├─ app/mapas     # dados específicos de mapas
   ├─ app/modules   # os módulos do app
   ├─ app/www       # estáticos: imagens, fontes, estilo...
   global.R         # arquivo global do app
   server.R         # servidor principal (main) do app
   ui.R             # interface principal (main) do app
├─ data/            # dados
├─ docker/          # imagens
├─ etl/             # scripts de ETL
├─ pacotes/         # pacotes que não estão no CRAN
├─ references/      # links de interesse, pdfs, etc.
├─ scripts/         # scripts de interesse geral
.dockerignore       # importante sempre ignorar as pastas data
.gitignore          # importante sempre ignorar as pastas data
README.md           # readme do projeto
projeto.Rproj       # arquivo Rproj que organiza o projeto
```

# Convenções de código (mínimo)

- **Estilo:** código encadeado com pipe tidyverse (%>%), trechos organizados com "# ----", estilo padrão do R.
- **Nomes:** `snake_case` para objetos e arquivos.
- **Scripts reprodutíveis:** parametrizados quando possível, sem caminhos absolutos.
- **Seeds:** fixe `set.seed()` quando houver aleatoriedade.

# Onboarding
## Como estudar?
- artigo DGO: [Government Data Science Teams A Framework for Implementing Strategic Monitoring Solutions](https://proceedings.open.tudelft.nl/DGO2025/article/view/925). Artigo seminal do time, vencedor do prêmio internacional de melhor política pública de Transformação Digital e Inovação de 2025.
- curso R: [Básico de R](https://livro.curso-r.com/)
- curso EVG: [Análise de Dados em Linguagem R](https://www.escolavirtual.gov.br/curso/325)
- Vídeo aulas do prof. Hugo Medeiros: [R para Políticas](https://www.youtube.com/channel/UCtg6tgjgrFTWkWKCFN22HOg)

## O que conhecer
- [portal Resultados](https://resultados.seplag.pe.gov.br/)
- [portal Box](https://box.pe.gov.br/)

## Pacotes R produzidos pelo time
- [BigDataPE](https://monitoramento.sepe.pe.gov.br/bigdatape/)
- [capesR](https://hugoavmedeiros.github.io/capesR/)
- [diario](https://monitoramento.sepe.pe.gov.br/diario/)
- [pikchr](https://monitoramento.sepe.pe.gov.br/pikchr/)
- [plug](https://monitoramento.sepe.pe.gov.br/plug/)
- [RapidFuzz](https://monitoramento.sepe.pe.gov.br/rapidfuzz/)
- [WebDAV](https://monitoramento.sepe.pe.gov.br/webdav/)
- [whapi](https://monitoramento.sepe.pe.gov.br/whapi/)

# Quem é o time?
**André Leite** - *Cientista de Dados Chefe*
[linkedin](https://www.linkedin.com/in/milkway/)

**Diogo Bezerra** - *Pesquisador Chefe*
[linkedin](https://www.linkedin.com/in/dicbezerra/)

**Hugo Medeiros** - *Cientista de Dados Sênior*
[linkedin](https://www.linkedin.com/in/hugoavmedeiros/)

**Júlia Barreto** - *Cientista de Dados Júnior*
[linkedin](https://www.linkedin.com/in/j%C3%BAlia-barr%C3%AAto/)

**Miguel Santos** - *Cientista de Dados Júnior*
[linkedin](https://www.linkedin.com/in/miguel-santos-6a66322b6/)
