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
| **Banco de dados** | `DBI`, `dbplyr`, `RPostgres`, `pool` |
| **Ciência de dados** | `echarts4r`, `ggplot2`, `mapgl`, `plotly`, `sf` |
| **Segurança** | `shinymanager`, `safer`, `sodium` |
| **UI / UX** | `bslib`, `bsicons`, `htmltools`, `htmlwidgets`, `shinyWidgets` |

---

# 🗂️ Estrutura de pastas sugerida

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

# 🧩 Convenções de código

- **Estilo:** encadeamento com `%>%`, seções com `# ----`.
- **Nomenclatura:** `snake_case` para objetos e arquivos.
- **Scripts reprodutíveis:** parametrizados, sem caminhos absolutos.
- **Aleatoriedade:** sempre defina `set.seed()` quando aplicável.

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
