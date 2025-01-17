pacotes <- c("tidyr","tidyverse", "data.table", "readr", "readxl", "writexl", "purrr", "lubridate", "foreign")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

rm(list=ls()) # limpa  o ambiente
gc(T)
Sys.setlocale("LC_ALL","pt-BR.UTF-8")
setwd("C:/Users/Sanglard/Desktop/ESUS_SINAN")
getwd()


pop <- read.csv2("C:/Users/Sanglard/Desktop/ESUS_SINAN/pop.csv", fileEncoding = "latin1")
notific <- read_csv("RESULTADOS/dados_final_intoxAgro.csv")

casos_por_municipio_ano <- notific %>%
  group_by(ID_MUNICIP, NU_ANO) %>%
  summarise(total_casos = n())



# Transformar a tabela de casos para o formato wide (largo)
casos_wider <- casos_por_municipio_ano %>%
  pivot_wider(
    names_from = NU_ANO,         # Coloca o ano como nomes das novas colunas
    values_from = total_casos    # Usa os valores de total_casos como conteúdo das novas colunas
  )



# Converta o tipo de dados para character
casos_por_municipio_ano$ID_MUNICIP <- as.character(casos_por_municipio_ano$ID_MUNICIP)
pop$COD_6D <- as.character(pop$COD_6D)

# Converta o tipo de dados para numeric
casos_por_municipio_ano$ID_MUNICIP <- as.numeric(casos_por_municipio_ano$ID_MUNICIP)
pop$COD_6D <- as.numeric(pop$COD_6D)

# Unir as duas tabelas por município e ano
dados_unidos <- casos_por_municipio_ano %>%
  left_join(pop, by = c("ID_MUNICIP" = "COD_6D"))


# Converter total_casos para numérico
dados_unidos$total_casos <- as.numeric(dados_unidos$total_casos)

# Converter as colunas de população para numérico
dados_unidos <- dados_unidos %>%
  mutate(across(starts_with("X"), as.numeric))

# Calcular a taxa de incidência para cada ano e município
dados_unidos <- dados_unidos %>%
  rowwise() %>%
  mutate(taxa_incidencia_2007 = (total_casos / X2007) * 100000,
         taxa_incidencia_2008 = (total_casos / X2008) * 100000,
         taxa_incidencia_2009 = (total_casos / X2009) * 100000,
         taxa_incidencia_2010 = (total_casos / X2010) * 100000,
         taxa_incidencia_2011 = (total_casos / X2011) * 100000,
         taxa_incidencia_2012 = (total_casos / X2012) * 100000,
         taxa_incidencia_2013 = (total_casos / X2013) * 100000,
         taxa_incidencia_2014 = (total_casos / X2014) * 100000,
         taxa_incidencia_2015 = (total_casos / X2015) * 100000,
         taxa_incidencia_2016 = (total_casos / X2016) * 100000,
         taxa_incidencia_2017 = (total_casos / X2017) * 100000,
         taxa_incidencia_2018 = (total_casos / X2018) * 100000,
         taxa_incidencia_2019 = (total_casos / X2019) * 100000,
         taxa_incidencia_2020 = (total_casos / X2020) * 100000,
         taxa_incidencia_2021 = (total_casos / X2021) * 100000,
         taxa_incidencia_2022 = (total_casos / X2022) * 100000,
         taxa_incidencia_2023 = (total_casos / X2023) * 100000,
         taxa_incidencia_2024 = (total_casos / X2024) * 100000) %>%
  ungroup()

# Verifique os resultados
head(dados_unidos)

# Criar a coluna de REGIAO
dados_unidos <- dados_unidos |> 
  mutate(REGIAO = substr(ID_MUNICIP, 1, 1))

# Renomear as regiões usando case_when
dados_unidos <- dados_unidos |> 
  mutate(REGIAO = case_when(
    REGIAO == "1" ~ "NORTE",
    REGIAO == "2" ~ "NORDESTE",
    REGIAO == "3" ~ "SUDESTE",
    REGIAO == "4" ~ "SUL",
    REGIAO == "5" ~ "CENTRO OESTE"
  ))


# Agrupar por região e calcular a média da taxa de incidência para cada ano
dados_por_regiao <- dados_unidos %>%
  group_by(REGIAO) %>%
  summarise(across(starts_with("taxa_incidencia_"), mean, na.rm = TRUE))

# Transformar para o formato long
dados_por_regiao_long <- dados_por_regiao %>%
  pivot_longer(
    cols = starts_with("taxa_incidencia_"),
    names_to = "ANO",
    names_prefix = "taxa_incidencia_",
    values_to = "taxa_incidencia"
  )


library(ggplot2)

# Garantir que todos os anos apareçam no eixo x
todos_os_anos <- seq(min(as.numeric(dados_por_regiao_long$ANO)), max(as.numeric(dados_por_regiao_long$ANO)), by = 1)

# Criar o gráfico de linhas com pontos e rótulos
ggplot(dados_por_regiao_long, aes(x = as.numeric(ANO), y = taxa_incidencia, color = REGIAO, group = REGIAO)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Adiciona as bolinhas nos pontos
  geom_text(aes(label = round(taxa_incidencia, 1)), vjust = -0.5, size = 3) +  # Adiciona rótulos aos pontos
  scale_x_continuous(breaks = todos_os_anos) +  # Garante que todos os anos apareçam no eixo x
  labs(title = "Taxa de Incidência de Intoxicações por Região",
       x = "Ano",
       y = "Taxa de Incidência por 100 mil habitantes",
       color = "Região") +  # Renomeia a legenda de cor
  theme_classic() +
  theme(legend.title = element_blank())  # Remove qualquer título adicional da legenda

# Ajustar a legenda para garantir que apenas os valores desejados apareçam

# Redefinir a coluna REGIAO como fator com os níveis corretos
dados_por_regiao_long$REGIAO <- factor(dados_por_regiao_long$REGIAO,
                                       levels = c("NORTE", "NORDESTE", "SUDESTE", "SUL", "CENTRO OESTE"))

# Criar o gráfico de linhas com pontos e rótulos
ggplot(dados_por_regiao_long, aes(x = as.numeric(ANO), y = taxa_incidencia, color = REGIAO, group = REGIAO)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Adiciona as bolinhas nos pontos
  geom_text(aes(label = round(taxa_incidencia, 1)), vjust = -0.5, size = 3) +  # Adiciona rótulos aos pontos
  scale_x_continuous(breaks = todos_os_anos) +  # Garante que todos os anos apareçam no eixo x
  labs(title = "Taxa de Incidência de Intoxicações por Região",
       x = "Ano",
       y = "Taxa de Incidência por 100 mil habitantes",
       color = "Região") +  
  theme_minimal() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 10))  # Remove o título adicional da legenda e ajusta o texto



