# 0 - Pacotes necessários ----

# install.packages("pacman")

pacman::p_load(tidyverse,
               rio,
               readxl)


# 1 - Lendo os dados ----

# Baixando os dados

## Diretamente no R: ----

### Função download.file, onde você especifica o link e informa uma pasta. 
download.file(
  url = "https://www.ssp.sp.gov.br/assets/estatistica/transparencia/spDados/SPDadosCriminais_2025.xlsx", 
  destfile = "dados/dados_ssp.xlsx", 
  mode = "wb")


ssp_2025 <- read_excel("dados/dados_ssp.xlsx", sheet = 2)

ssp_2025

saveRDS(ssp_2025, file = "dados/ssp_criminais_2025.rds")

## Diretamente pelo site da SSP: ----