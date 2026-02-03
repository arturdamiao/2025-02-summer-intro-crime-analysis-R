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

# A base completa  possui quase 600 mil linhas, e 29 colunas. É bastante
# pesado para baixar e ler no R. Portanto, vamos samplear 10 mil observações
# aleatoriamente

ssp_amostra <- ssp_2025 %>% 
  sample_n(10000)

# Salvando a amostra em xlsx
writexl::write_xlsx(ssp_amostra, path = "dados/amostra_ssp.xlsx")

# Salvando como csv
write.csv2(ssp_amostra, file = "dados/amostra_ssp.csv")

# Salvando como rds
saveRDS(ssp_2025, file = "dados/ssp_criminais_2025.rds")

## Diretamente pelo site da SSP: ----