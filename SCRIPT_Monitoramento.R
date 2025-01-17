pacotes <- c("tidyr", "gtsummary", "xml2", "knitr", "kableExtra", "DT", "tidyverse", 
             "data.table", "readr", "readxl", "writexl", "purrr", "lubridate", "foreign",
             "rlist", "stringr", "stringdist", "shiny", "rgdal", 
             "leaflet", "ggplot2", "gridExtra", "treemap", "RColorBrewer", "googleVis", 
             "ggpubr", "sf", "reshape")


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
data <- read_csv("RESULTADOS/dados_final_intoxAgro.csv")





load_dict_sinan <- function(){
  #Atribui o nome do diretório onde estão os arquivos .csv referentes ao dicionário de cada campo do SINAN
  dir_input <- paste(getwd(), "/dict/", sep="")
  
  #Cria uma lista com o nome dos campos para os quais os dicionários de dados são fornecidos
  list_files<-c("TP_NOT","SG_UF","UF_EMP","UF_HOSP", "SG_UF_NOT", "ID_MUNICIP", "CS_SEXO", "CS_GESTANT","CS_RACA", "CS_ESCOL_N", 
                "SG_UF","ID_MN_RESI","CS_ZONA", "ID_OCUPA_N", "SIT_TRAB", "LOC_EXPO", 
                "ZONA_EXP", "AGENTE_TOX", "UTILIZACAO", "ATIVIDA_1", "ATIVIDA_2", "ATIVIDA_3",
                "VIA_1", "VIA_2", "VIA_3", "CIRCUNSTAN", "DOENCA_TRA", "TPEXP", "TPATENDE", "HOSPITAL",
                "CLASSI_FIN", "CRITERIO", "EVOLUCAO", "CAT")
  
  #Inicializa a lista final de dicionários
  list_dict <- NULL
  
  #Lê os arquivos .csv referentes aos dicionários e consolida em uma lista só (list_dict)
  for (file in list_files){
    #Define o diretório de cada arquivo de dicionário .csv
    dir_file <- paste(paste(dir_input, file, sep = ""),".csv", sep="")
    
    #Realiza a leitura dos dicionários .csv. Para os campos especificados realiza uma leitura como character
    #considerando a presença de dois caracteres o que leva a ocorrência de 0 à esquerda (e.g. 01, 02, 03 etc)
    ifelse(file %in% c("CS_ESCOL_N","ID_OCUPA_N", "SIT_TRAB", "AGENTE_TOX", "ATIVIDA_1", "ATIVIDA_2", "ATIVIDA_3", "CIRCUNSTAN"),
           df_dict <- read.csv(dir_file, header =TRUE, sep=";", colClasses = c("key"="character"), fileEncoding = "latin1"),
           df_dict <- read.csv(dir_file, header =TRUE, sep=";",fileEncoding = "latin1")
    )
    
    #Consolida e atualiza list_dict
    list_dict <- append(list_dict, list(df_dict))
  }
  
  #Atribui ao nome dos items da lista os mesmos nomes que os campos do SINAN
  names(list_dict)<-list_files
  return(list_dict)
}


lista_dict <- load_dict_sinan()



reclass_sinan <- function(data, lista_dict, list_prop = NULL){
  # Utiliza a lista proposta de campos (list_prop) para processar os dados
  if (is.null(list_prop)){
    lista <- names(lista_dict)
  } else {
    lista <- list_prop  
  }
  
  # Para cada campo na lista, realiza a tradução dos dados
  for (col in lista) {
    # Escolhe o dicionário para cada campo
    dict_col <- lista_dict[[col]]
    
    # Mapeia os valores do dicionário com os valores dos dados
    # Corrigido para acessar as colunas corretamente
    suppressMessages(
      data[[col]] <- mapvalues(data[[col]], from = dict_col[,"key"], to = dict_col[,"value"])
    )
  }
  return(data)
}

# Aplicando a função corrigida
data1 <- reclass_sinan(data, lista_dict) |>  as.data.frame(data)






# Carregar os pacotes necessários



# Supondo que o dataframe `data` já está carregado e possui a coluna `NU_ANO`

# Histograma por Ano: Quantidade de notificações por ano com percentuais e números absolutos
hist_data <- data1 %>%
  group_by(NU_ANO) %>%
  dplyr::summarise(Count = n(), .groups = 'drop') %>%  # .groups = 'drop' para evitar agrupamento adicional
  mutate(Percentage = (Count / sum(Count)) * 100)

# Gráfico de histograma por ano com números absolutos dentro das colunas e percentuais acima
ggplot(hist_data, aes(x = NU_ANO, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Count), 
            vjust = 1.5, # Posicionar o número absoluto dentro da barra
            color = "white", # Cor branca para visibilidade
            position = position_stack(vjust = 0.5), 
            size = 4) +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            vjust = -0.5, # Posicionar o percentual acima da barra
            color = "black", # Cor preta para o percentual
            size = 4) +
  labs(title = "Número de Notificações por Ano", x = "Ano", y = "Número de Notificações") +
  theme_classic()




## Ajustar a NU_IDADE_N e criar IDADE_REAL
data1$NU_IDADE_N[data1$NU_IDADE_N < 3999] <- 0

# Diagnóstico: Verificar transformação de NU_IDADE_N
print(head(data1$NU_IDADE_N, 10))

data1 <- data1 %>%
  mutate(IDADE_REAL = as.numeric(substr(NU_IDADE_N, 3, 4))) %>%
  mutate(Faixa_Etaria = cut(IDADE_REAL, 
                            breaks = c(0, 1, 4, 9, 14, 19, 29, 39, 49, 59, Inf),
                            labels = c("0-1", "1-4", "5-9", "10-14", "15-19", 
                                       "20-29", "30-39", "40-49", "50-59", "60+"),
                            right = FALSE))

# Diagnóstico: Verificar resultado de IDADE_REAL e Faixa_Etaria
print(head(data1[, c("IDADE_REAL", "Faixa_Etaria")], 10))

# Ajustar a contagem por sexo e faixa etária
sexo_faixa_count <- data1 %>%
  filter(CS_SEXO %in% c("Masculino", "Feminino")) %>%  # Filtrar apenas M e F
  group_by(CS_SEXO, Faixa_Etaria) %>%
  dplyr::summarise(Count = n(), .groups = 'drop') %>%  # Inclui .groups = 'drop' para evitar agrupamento
  ungroup() %>%
  mutate(Total = sum(Count, na.rm = TRUE),  # Adiciona na.rm = TRUE para segurança
         Percentage = (Count / Total) * 100,
         CS_SEXO = recode(CS_SEXO, "M" = "Masculino", "F" = "Feminino"),
         Count = ifelse(CS_SEXO == "Masculino", -Count, Count))  # Inverter valores para o sexo masculino

# Verificar se `Count` tem valores válidos
print(sexo_faixa_count)

# Verifique se há problemas de NA
if (all(is.na(sexo_faixa_count$Count))) {
  stop("Erro: Todos os valores de Count são NA. Verifique os dados de entrada e a filtragem.")
}

# Definir o valor máximo para centralizar as barras
max_count <- max(abs(sexo_faixa_count$Count), na.rm = TRUE)

# Diagnóstico: Verificar valor máximo calculado
print(max_count)


ggplot(sexo_faixa_count, aes(x = Faixa_Etaria, y = Count, fill = CS_SEXO)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(abs(Count), " (", round(Percentage, 2), "%)")), 
            position = position_stack(vjust = 0.5), size = 3, fontface = "bold") +
  labs(title = "Pirâmide Etária por Sexo e Faixa Etária", 
       x = "Faixa Etária", y = "Número de Notificações") +
  scale_y_continuous(labels = abs, breaks = seq(-max_count, max_count, by = 5000), limits = c(-max_count, max_count)) +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue")) +
  coord_flip() +  # Inverter os eixos para fazer o gráfico horizontal
  theme_classic() +
  theme(legend.position = "top", 
        legend.title = element_blank(),  # Remover o título da legenda
        plot.title = element_text(face = "bold", hjust = 0.5))









# Criando as faixas etárias especificadas
grafico2 <- data1 %>%
  mutate(Faixa_Etaria1 = case_when(
    IDADE_REAL < 1 ~ "Menor de 1 ano",
    IDADE_REAL >= 1 & IDADE_REAL <= 5 ~ "1-5 anos",
    IDADE_REAL >= 6 & IDADE_REAL <= 12 ~ "6-12 anos",
    IDADE_REAL > 12 ~ "+12"
  )) %>%
  filter(!is.na(CS_SEXO) & CS_SEXO %in% c("M", "F"))  # Filtrar apenas M e F, removendo NA

# Contagem por sexo e faixa etária
sexo_faixa_count <- grafico2 %>%
  group_by(CS_SEXO, Faixa_Etaria1) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100)

# Gráfico de barras empilhadas para sexo e as novas faixas etárias
ggplot(sexo_faixa_count, aes(x = CS_SEXO, y = Count, fill = Faixa_Etaria1)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 2), "%)")), 
            position = position_stack(vjust = 0.5), size = 3, fontface = "bold") +  # Aplicar negrito aos números
  labs(title = "Distribuição por Sexo e Faixa Etária", x = "Sexo", y = "Número de Notificações") +
  scale_fill_brewer(palette = "Pastel1", na.translate = FALSE) +  # Remover NA da legenda
  guides(fill = guide_legend(title = NULL)) +  # Remover o título da legenda
  theme_classic()



# Criar tabela de contagem por Local de Exposição e Zona de Exposição
tabela_zona_exposicao <- data %>%
  group_by(LOC_EXPO, ZONA_EXP) %>%
  summarise(Count = n() ) %>%
  group_by(LOC_EXPO) %>%
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100)

# Reorganizar a tabela para ter ZONA_EXP como colunas
tabela_zona_exposicao_wide <- tabela_zona_exposicao %>%
  pivot_wider(names_from = ZONA_EXP, values_from = c(Count, Percentage), 
              values_fill = list(Count = 0, Percentage = 0))

# Capturando os nomes das colunas para ajustar dinamicamente
count_cols <- grep("Count_", names(tabela_zona_exposicao_wide), value = TRUE)
percent_cols <- grep("Percentage_", names(tabela_zona_exposicao_wide), value = TRUE)
col_names <- c('Local de Exposição', count_cols, percent_cols)

# Criar a tabela usando gtsummary
tabela_gtsummary <- tabela_zona_exposicao %>%
  select(LOC_EXPO, ZONA_EXP, Count, Percentage) %>%
  tbl_summary(
    by = ZONA_EXP,
    statistic = list(
      all_continuous() ~ "{sum} ({mean}%)"  # Exibe o total e percentual corretamente
    ),
    label = list(
      LOC_EXPO = "Local de Exposição",
      Count = "Número de Casos",
      Percentage = "Percentual de Casos (%)"
    ),
    digits = list(
      all_continuous() ~ 1  # Definir 1 casa decimal para percentuais
    )
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Local de Exposição**") %>%
  modify_caption("Tabela de Exposição por Local e Zona de Exposição")

# Exibir a tabela
tabela_gtsummary


# Criar tabela de contagem por Local da Exposição e Zona de Exposição
tabela_zona_exposicao <- data %>%
  group_by(LOC_EXPO, ZONA_EXP) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(LOC_EXPO) %>%
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100) %>%
  ungroup()

# Reorganizar a tabela para ter ZONA_EXP como colunas
tabela_zona_exposicao_wide <- tabela_zona_exposicao %>%
  pivot_wider(names_from = ZONA_EXP, values_from = c(Count, Percentage), 
              values_fill = list(Count = 0, Percentage = 0))

# Capturando os nomes das colunas para ajustar dinamicamente
count_cols <- grep("Count_", names(tabela_zona_exposicao_wide), value = TRUE)
percent_cols <- grep("Percentage_", names(tabela_zona_exposicao_wide), value = TRUE)
col_names <- c('Local de Exposição', count_cols, percent_cols)

# Criar a tabela usando kableExtra
tabela_zona_exposicao_wide %>%
  mutate(across(starts_with("Percentage"), ~cell_spec(sprintf("%.1f%%", .), bold = TRUE))) %>%  # Formatar percentuais com negrito
  kbl(caption = "Tabela de Exposição por Local e Zona de Exposição", 
      col.names = col_names,  # Usar nomes de colunas dinâmicos
      escape = FALSE) %>%  # escape = FALSE para renderizar HTML em cell_spec
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "center") %>%
  column_spec(2:ncol(tabela_zona_exposicao_wide), bold = TRUE) %>%  # Negritar todas as colunas de contagem e percentual
  add_header_above(c(" " = 1, "Exposição por Zona" = length(count_cols) + length(percent_cols)))





# Filtrar apenas as zonas relevantes: urbana, rural e periurbana

zonas_interesse <- data %>%
  mutate(ZONA_EXP = recode(ZONA_EXP, 
                           `1` = "Urbana", 
                           `2` = "Rural", 
                           `3` = "Periurbana",
                           `9` = NA_character_)) %>%  # Convertendo 9 para NA
  filter(ZONA_EXP %in% c("Urbana", "Rural", "Periurbana"))  # Filtrar zonas válidas

# Contar ocorrências por zona de exposição ao longo do tempo
contagem_historica_zonas <- zonas_interesse %>%
  group_by(NU_ANO, ZONA_EXP) %>%
  summarise(Count = n()) %>%
  ungroup()

# Garantir que NU_ANO está no formato numérico
contagem_historica_zonas$NU_ANO <- as.numeric(as.character(contagem_historica_zonas$NU_ANO))

# Gráfico de linhas para série histórica de intoxicações por zona de exposição com rótulos
ggplot(contagem_historica_zonas, aes(x = NU_ANO, y = Count, color = ZONA_EXP, group = ZONA_EXP)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(contagem_historica_zonas$NU_ANO), max(contagem_historica_zonas$NU_ANO), by = 1)) +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +  # Adicionar rótulos nos pontos
  labs(title = "Série Histórica de Intoxicações por Zona de Exposição",
       x = "Ano", y = "Número de Notificações",
       color = "Zona de Exposição") +
  theme_classic() +
  scale_color_brewer(palette = "Set1")





# Supondo que a coluna EXPO_TRAB tem valores 'Sim' para exposição relacionada ao trabalho e 'Não' para não relacionada
exposicao_trabalho <- data %>%
  filter(DOENCA_TRA %in% c("Sim", "Não"))  # Filtrar apenas 'Sim' e 'Não'

# Contar casos relacionados ao trabalho
exposicao_trabalho_sim <- exposicao_trabalho %>%
  filter(DOENCA_TRA == "Sim") %>%
  group_by(LOC_EXPO) %>%
  summarise(Count_Sim = n()) %>%
  ungroup()

# Contar casos não relacionados ao trabalho
exposicao_trabalho_nao <- exposicao_trabalho %>%
  filter(DOENCA_TRA == "Não") %>%
  group_by(LOC_EXPO) %>%
  summarise(Count_Nao = n()) %>%
  ungroup()

# Juntar as duas tabelas e calcular percentuais
tabela_exposicao_trabalho <- full_join(exposicao_trabalho_sim, exposicao_trabalho_nao, by = "LOC_EXPO") %>%
  mutate(Total_Sim = sum(Count_Sim, na.rm = TRUE),
         Total_Nao = sum(Count_Nao, na.rm = TRUE),
         Percentage_Sim = (Count_Sim / Total_Sim) * 100,
         Percentage_Nao = (Count_Nao / Total_Nao) * 100)

# Substituir NA por 0 para contagens e percentuais
tabela_exposicao_trabalho[is.na(tabela_exposicao_trabalho)] <- 0

# Capturar o número de colunas na tabela para aplicar formatação corretamente
n_cols <- ncol(tabela_exposicao_trabalho)

# Criar a tabela usando kableExtra
tabela_exposicao_trabalho %>%
  mutate(Percentage_Sim = cell_spec(sprintf("%.1f%%", Percentage_Sim), bold = TRUE),
         Percentage_Nao = cell_spec(sprintf("%.1f%%", Percentage_Nao), bold = TRUE)) %>%
  kbl(caption = "Tabela de Exposição por Local e Relação ao Trabalho", 
      col.names = c('Local de Exposição', 
                    'Casos Relacionados ao Trabalho (Sim)', 
                    'Casos Não Relacionados ao Trabalho (Não)', 
                    'Total Relacionados', 
                    'Total Não Relacionados',
                    'Percentual Relacionados (%)', 
                    'Percentual Não Relacionados (%)'),
      escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "center") %>%
  column_spec(2:n_cols, bold = TRUE)




children <- xml_children(x)
max_index <- length(children)

# Verifique o valor de 'search'
if (search > 0 && search <= max_index) {
  selected_child <- children[[search]]
} else {
  stop("O índice 'search' está fora dos limites disponíveis.")
}


children <- xml_children(x)
print(children)











# Carregar os pacotes necessários
library(ggplot2)
library(dplyr)

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Filtrar e contar ocorrências para exposição decorrente do trabalho
# Supondo que a coluna EXPO_TRAB tem valores 'Sim' para exposição relacionada ao trabalho e 'Não' para não relacionada
exposicao_trabalho <- data %>%
  filter(EXPO_TRAB %in% c("Sim", "Não")) %>%
  group_by(LOC_EXPO, EXPO_TRAB) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100)

# Gráfico 1: Histograma de casos relacionados ao trabalho
exposicao_trabalho_sim <- exposicao_trabalho %>%
  filter(EXPO_TRAB == "Sim")

ggplot(exposicao_trabalho_sim, aes(x = LOC_EXPO, y = Count, fill = LOC_EXPO)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 2), "%)")), vjust = -0.5) +
  labs(title = "Casos Relacionados ao Trabalho por Local de Exposição",
       x = "Local de Exposição", y = "Número de Casos") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

# Gráfico 2: Histograma de casos não relacionados ao trabalho
exposicao_trabalho_nao <- exposicao_trabalho %>%
  filter(EXPO_TRAB == "Não")

ggplot(exposicao_trabalho_nao, aes(x = LOC_EXPO, y = Count, fill = LOC_EXPO)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Count, " (", round(Percentage, 2), "%)")), vjust = -0.5) +
  labs(title = "Casos Não Relacionados ao Trabalho por Local de Exposição",
       x = "Local de Exposição", y = "Número de Casos") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")


# Carregar os pacotes necessários
library(ggplot2)
library(dplyr)

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Filtrar dados para atividades exercidas e vias de exposição
# Agrupar por atividade exercida na exposição e via de exposição
exposicao_atividade <- data %>%
  filter(!is.na(ATIVIDA_1) & !is.na(VIA_1)) %>%
  group_by(ATIVIDA_1, VIA_1) %>%
  summarise(Count = n()) %>%
  ungroup()

# Lista de vias de exposição para criar gráficos individuais
vias_exposicao <- unique(exposicao_atividade$VIA_1)

# Criar histogramas separados para cada via de exposição
for (via in vias_exposicao) {
  # Filtrar dados para a via atual
  data_via <- exposicao_atividade %>%
    filter(VIA_1 == via)
  
  # Criar histograma para a via atual
  p <- ggplot(data_via, aes(x = ATIVIDA_1, y = Count, fill = ATIVIDA_1)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = Count), vjust = -0.5) +
    labs(title = paste("Atividade Exercida na Exposição por", via),
         x = "Atividade Exercida", y = "Número de Casos") +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostrar o gráfico
  print(p)
}


# Carregar os pacotes necessários
library(ggplot2)
library(dplyr)

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Filtrar dados para atividades exercidas e vias de exposição
# Agrupar por atividade exercida na exposição e via de exposição
exposicao_atividade <- data %>%
  filter(!is.na(ATIVIDA_1) & !is.na(VIA_1)) %>%
  group_by(ATIVIDA_1, VIA_1) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(VIA_1) %>%
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100)

# Lista de vias de exposição para criar gráficos individuais
vias_exposicao <- unique(exposicao_atividade$VIA_1)

# Criar histogramas separados para cada via de exposição
for (via in vias_exposicao) {
  # Filtrar dados para a via atual
  data_via <- exposicao_atividade %>%
    filter(VIA_1 == via)
  
  # Criar histograma para a via atual
  p <- ggplot(data_via, aes(x = ATIVIDA_1, y = Count, fill = ATIVIDA_1)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = paste0(Count, " (", round(Percentage, 2), "%)")), vjust = -0.5) +
    labs(title = paste("Atividade Exercida na Exposição por", via),
         x = "Atividade Exercida", y = "Número de Casos") +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostrar o gráfico
  print(p)
}

# Carregar os pacotes necessários
library(ggplot2)
library(dplyr)
library(janitor)

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Garantir que a coluna de data esteja no formato Date
data$DT_NOTIFIC <- as.Date(data$DT_NOTIFIC)  # Substitua DT_NOTIFIC pelo nome correto da coluna de data

# Adicionar coluna de semana epidemiológica usando epiweek do pacote janitor
data <- data %>%
  mutate(Semana_Epidemiologica = epiweek(DT_NOTIFIC),
         Ano = format(DT_NOTIFIC, "%Y"))

# Agrupar por semana epidemiológica e ano, calcular o número absoluto e percentual
frequencia_semana <- data %>%
  group_by(Ano, Semana_Epidemiologica) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Ano) %>%
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100)

# Lista de anos para criar gráficos individuais
anos <- unique(frequencia_semana$Ano)

# Criar histogramas separados para cada ano
for (ano in anos) {
  # Filtrar dados para o ano atual
  data_ano <- frequencia_semana %>%
    filter(Ano == ano)
  
  # Criar histograma para o ano atual
  p <- ggplot(data_ano, aes(x = Semana_Epidemiologica, y = Count, fill = as.factor(Semana_Epidemiologica))) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = paste0(Count, " (", round(Percentage, 2), "%)")), vjust = -0.5, size = 3) +
    labs(title = paste("Frequência de Casos por Semana Epidemiológica em", ano),
         x = "Semana Epidemiológica", y = "Número de Casos") +
    theme_minimal() +
    scale_fill_brewer(palette = "Pastel1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Mostrar o gráfico
  print(p)
}

# Carregar os pacotes necessários
library(dplyr)
library(tidyr)
library(stringr)

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Combinar todos os campos de interesse em uma única coluna
ingredientes <- data %>%
  select(AGENTE_1, AGENTE_2, AGENTE_3, P_ATIVO_1, P_ATIVO_2, P_ATIVO_3, OUT_AGENTE) %>%
  pivot_longer(cols = everything(), names_to = "Campo", values_to = "Ingrediente") %>%
  filter(!is.na(Ingrediente))

# Separar ingredientes múltiplos em cada campo, se houver
ingredientes_separados <- ingredientes %>%
  separate_rows(Ingrediente, sep = ",|;|/|\\s+") %>% # Divide por vírgula, ponto e vírgula, barra ou espaço
  filter(Ingrediente != "") # Remove entradas vazias

# Contar a frequência de cada ingrediente
frequencia_ingredientes <- ingredientes_separados %>%
  group_by(Ingrediente) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Exibir os 5 principais ingredientes ativos
top_5_ingredientes <- frequencia_ingredientes %>%
  top_n(5, wt = Count)

print(top_5_ingredientes)


# Carregar os pacotes necessários
library(dplyr)

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Definindo as colunas de interesse para a análise de completitude
colunas_completitude <- c("CS_SEXO", "NU_IDADE_N", "CS_ESCOL_N", "CS_RACA", "ID_MN_RESI", 
                          "AGENTE_TOX", "LAVOURA", "CBO", "CNAE", "VIA_1", 
                          "ZONA_EXP", "LOC_EXPO", "CLASSI_FIN", "EVOLUCAO")

# Função para calcular o percentual de valores faltantes (brancos ou "9")
calcular_completitude <- function(data, coluna) {
  total <- nrow(data)
  faltantes <- data %>%
    filter(is.na(.data[[coluna]]) | .data[[coluna]] == "" | .data[[coluna]] == "9") %>%
    nrow()
  percentual_faltante <- (faltantes / total) * 100
  return(percentual_faltante)
}

# Aplicar a função a todas as colunas de interesse
completitude_resultados <- sapply(colunas_completitude, calcular_completitude, data = data)

# Exibir os resultados da completitude
completitude_resultados <- data.frame(Coluna = colunas_completitude, Percentual_Faltante = completitude_resultados)
print(completitude_resultados)

# Contar notificações que não declararam a lavoura
nao_declararam_lavoura <- data %>%
  filter(is.na(LAVOURA) | LAVOURA == "" | LAVOURA == "9") %>%
  nrow()

cat("Número de notificações que não declararam LAVOURA:", nao_declararam_lavoura, "\n")

# Carregar os pacotes necessários
library(dplyr)
library(ggplot2)

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Definindo as colunas de interesse para a análise de completitude
colunas_completitude <- c("CS_SEXO", "NU_IDADE_N", "CS_ESCOL_N", "CS_RACA", "ID_MN_RESI", 
                          "AGENTE_TOX", "LAVOURA", "CBO", "CNAE", "VIA_1", 
                          "ZONA_EXP", "LOC_EXPO", "CLASSI_FIN", "EVOLUCAO")

# Função para calcular o preenchimento correto, faltantes e ignorados
calcular_completitude <- function(data, coluna) {
  total <- nrow(data)
  preenchido <- data %>%
    filter(!is.na(.data[[coluna]]) & .data[[coluna]] != "" & .data[[coluna]] != "9") %>%
    nrow()
  faltantes <- data %>%
    filter(is.na(.data[[coluna]]) | .data[[coluna]] == "") %>%
    nrow()
  ignorados <- data %>%
    filter(.data[[coluna]] == "9") %>%
    nrow()
  
  resultado <- data.frame(
    Categoria = c("Preenchido Corretamente", "Em Branco", "Ignorado"),
    Count = c(preenchido, faltantes, ignorados),
    Percentual = c((preenchido / total) * 100, (faltantes / total) * 100, (ignorados / total) * 100)
  )
  
  return(resultado)
}

# Aplicar a função a todas as colunas de interesse e criar gráficos de pizza
for (coluna in colunas_completitude) {
  completitude_dados <- calcular_completitude(data, coluna)
  
  # Criar gráfico de pizza para cada coluna
  p <- ggplot(completitude_dados, aes(x = "", y = Count, fill = Categoria)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y") +
    geom_text(aes(label = paste0(Count, " (", round(Percentual, 2), "%)")), 
              position = position_stack(vjust = 0.5), size = 4) +
    labs(title = paste("Completitude da Coluna:", coluna),
         fill = "Categoria") +
    theme_void() +
    scale_fill_brewer(palette = "Pastel1")
  
  # Mostrar o gráfico
  print(p)
}

# Contar notificações que não declararam a lavoura
nao_declararam_lavoura <- data %>%
  filter(is.na(LAVOURA) | LAVOURA == "" | LAVOURA == "9") %>%
  nrow()

cat("Número de notificações que não declararam LAVOURA:", nao_declararam_lavoura, "\n")


# Carregar os pacotes necessários
library(dplyr)
library(ggplot2)
library(gridExtra)  # Para agrupar os gráficos
library(grid)       # Para funções adicionais de layout

# Carregar os dados
# Ajuste o caminho do arquivo conforme necessário
data <- read.csv("caminho/do/seu/arquivo.csv") # Altere para o caminho correto do seu arquivo

# Função para calcular a completitude e criar gráficos de pizza
criar_grafico_pizza <- function(data, coluna) {
  total <- nrow(data)
  preenchido <- data %>%
    filter(!is.na(.data[[coluna]]) & .data[[coluna]] != "" & .data[[coluna]] != "9") %>%
    nrow()
  faltantes <- data %>%
    filter(is.na(.data[[coluna]]) | .data[[coluna]] == "") %>%
    nrow()
  ignorados <- data %>%
    filter(.data[[coluna]] == "9") %>%
    nrow()
  
  resultado <- data.frame(
    Categoria = c("Preenchido Corretamente", "Em Branco", "Ignorado"),
    Count = c(preenchido, faltantes, ignorados),
    Percentual = c((preenchido / total) * 100, (faltantes / total) * 100, (ignorados / total) * 100)
  )
  
  # Criar gráfico de pizza
  p <- ggplot(resultado, aes(x = "", y = Count, fill = Categoria)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y") +
    geom_text(aes(label = paste0(Count, " (", round(Percentual, 2), "%)")), 
              position = position_stack(vjust = 0.5), size = 4) +
    labs(title = paste("Completitude da Coluna:", coluna),
         fill = "Categoria") +
    theme_void() +
    scale_fill_brewer(palette = "Pastel1")
  
  return(p)
}

# Primeiro conjunto de gráficos: Sexo, Raça, Escolaridade, Faixa de Idade, Município de Residência
graficos_identificacao <- list(
  criar_grafico_pizza(data, "CS_SEXO"),
  criar_grafico_pizza(data, "CS_RACA"),
  criar_grafico_pizza(data, "CS_ESCOL_N"),
  criar_grafico_pizza(data, "NU_IDADE_N"),
  criar_grafico_pizza(data, "ID_MN_RESI")
)

# Segundo conjunto de gráficos: Agente Tóxico, Via de Exposição, Lavouras, Zonas de Exposição, Local de Exposição
graficos_exposicao <- list(
  criar_grafico_pizza(data, "AGENTE_TOX"),
  criar_grafico_pizza(data, "VIA_1"),
  criar_grafico_pizza(data, "LAVOURA"),
  criar_grafico_pizza(data, "ZONA_EXP"),
  criar_grafico_pizza(data, "LOC_EXPO")
)

# Terceiro conjunto de gráficos: Desfecho, Classificação Final, Evolução
graficos_desfecho <- list(
  criar_grafico_pizza(data, "EVOLUCAO"),
  criar_grafico_pizza(data, "CLASSI_FIN"),
  criar_grafico_pizza(data, "DESFECHO")  # Ajuste com o nome correto se DESFECHO não for a coluna certa
)

# Agrupar gráficos com gridExtra
grid.arrange(grobs = graficos_identificacao, ncol = 2, top = "Identificação")
grid.arrange(grobs = graficos_exposicao, ncol = 2, top = "Exposição")
grid.arrange(grobs = graficos_desfecho, ncol = 2, top = "Desfecho e Classificação")
