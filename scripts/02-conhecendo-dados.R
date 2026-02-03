# 0 - Primeiro passo: importar bibliotecas ----
 
## Forma tradicional ----

install.packages("tidyverse", # Coleção de pacotes para análise de dados.
                 "readxl",    # Pacote que facilita a leitura de .xlsx
                 "writexl",   # Pacote que facilita salvar arquivos .xlsx
                 "pacman")    # Pacote que instala e carrega pacotes

library(tidyverse)
library(readxl)
library(writexl)
library(pacman)

## Forma mais otimizada {pacman}

pacman::p_load(tidyverse,
               readxl,
               writexl,
               janitor)

# 1 - Importando a base de dados ----

# Base de dados para fins didáticos

dados <- dplyr::starwars

# 2 - Manipulando dados ----

## 2.1 - Com o R Base ----

## Visualiza a base de dados
### Começo
head(dados)
head(dados, 3)

### Fim
tail(dados)

## Calculando a média
mean(dados$height)

## Removendo o NA
mean(dados$height, na.rm = TRUE)

## Calculando o Desvio Padrão 
sd(dados$height, na.rm = TRUE)

## 2.2 - Com o Tidyverse -----
dados |> 
  group_by(sex) |> 
  summarise(media_altura = mean(height, na.rm = TRUE))

## Exemplo sem o operador %>%  (CTRL + SHIFT+ M)

summarise(group_by(dados, sex), mean(height, na.rm = TRUE))

### Filtrando informações ----

dados |> 
  filter(gender == "masculine") |> 
  group_by(sex) |> 
  summarise(media_altura = mean(height, na.rm = TRUE))

### Fazendo uma tabela descritiva 

tabela_descritiva <- dados |> 
  group_by(sex) |> 
  summarise(media_altura = mean(height, na.rm = TRUE),
            variancia_altura = var(height, na.rm = TRUE),
            desvio_altura = sd(height, na.rm = TRUE),
            n_obs = n()) # número de observações

tabela_descritiva

tabela_descritiva2 <- tabela_descritiva |> 
  filter(n_obs > 1)

tabela_descritiva2

### Filtrando NAs ----
#### Visualizando quais são os NAs
dados |> 
  filter(is.na(sex))


### Removendo NAs

dados |> 
  filter(!is.na(sex)) |> 
  group_by(sex) |> 
  summarise(media_altura = mean(height, na.rm = TRUE),
            variancia_altura = var(height, na.rm = TRUE),
            desvio_altura = sd(height, na.rm = TRUE),
            n_obs = n()) |> 
  filter(n_obs > 1)

### Agrupando variáveis ----

dados |> 
  filter(!is.na(sex)) |> 
  group_by(sex, eye_color) |> 
  summarise(media_altura = mean(height, na.rm = TRUE),
            variancia_altura = var(height, na.rm = TRUE),
            desvio_altura = sd(height, na.rm = TRUE),
            n_obs = n()) |> 
  filter(n_obs > 1)


# 3 - Dados de Gasolina ----

## Instalando pacote
install.packages("plm")

## Carregando dados do pacote

data(Gasoline, package = "plm")

# Vendo a classe do conjunto de dados
class(Gasoline)

## Transformando para tibble 
gasoline <- as_tibble(Gasoline)

## Deixando em minúsculo

gasoline <- gasoline |> 
  mutate(country = tolower(country))

## Filtrando, sem o pipe
filter(gasoline, year == 1972)

## Com o pipe
gasoline |> 
  filter(year == 1972)

## Filtrando condições
gasoline %>%
  filter(year %in% seq(1969, 1973)) # sequência de anos entre 1969 a 1973

gasoline %>%
  filter(year %in% 1969:1973) # sem a função seq()

## Função auxiliadora

gasoline %>%
  filter(between(year, 1969, 1973))

## Anos não consecutivos

gasoline %>%
  filter(year %in% c(1969, 1973, 1977))

## Função select() ----
gasoline %>%
  select(country, year, lrpmg)