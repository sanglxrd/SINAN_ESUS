pacotes <- c("tidyverse", "data.table", "readr", "readxl", "writexl", "purrr", "lubridate", "foreign")


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


att_db_Sinan <- function(dir_dbf) {
  # #Diretório de output com base no diretório definido em setwd
  dir_out <- paste(getwd(), "/banco_rds_att/", sep="")
  #
  # #Cria a variável lista que armazena o nome de todos os documentos .dbf da minha pasta do SINAN
  lista <- list.files(dir_dbf)
  #
  # #Lê cada um dos dbf e salva no diretório dir_out como arquivo .rds (R)
  for (file in lista){
    # #Cria a variável de diretório do arquivo referente a um ano
    dir_file <- paste(dir_dbf,file, sep="/")
    #
    # #Lê o arquivo dbf na forma de dataframe
    dado_ano <- read.dbf(dir_file)
    #
    # #Renomeia os nomes dos arquivos para a forma IEXOGN_AAAA (A= ano)
    nm1 <- str_replace(file,".dbf","")
    nm2 <- str_replace(nm1, "IEXOGN", "IEXOGN_20")
    #
    # #Cria o nome e diretório para o arquivo .rds
    nome_arq_rds <- paste(dir_out, nm2, ".rds", sep="")
    #
    # #Salva os de cada dbf como arquivo .rds
    saveRDS(dado_ano, nome_arq_rds)
    message(paste0(substr(file,1,8), ".rds salvo!"))
  }
}

#LINHA 70 INICIALIZA A ATUALIZAÇÃO DO BANCO!
att_db_Sinan(paste0(getwd(), "/dbf/"))

#4

load_rds_sinan <- function(){
  
  dir_input <- paste(getwd(), "/banco_rds_att/", sep="")
  lista <- list.files(dir_input)
  dados_final <- NULL
  
  
  for (file in lista){
    #Define o endere?o do arquivo .rds a ser lido
    dir <- paste(dir_input,file, sep="/")
    #L? o arquivo .rds
    dados_add<- readRDS(dir)
    #Junta os arquivos .rds em um mesmo dataframe
    dados_final <- rbind(dados_final, dados_add)
  }
  
  #Converte o data.frame em data.table
  dados_final <- data.table(dados_final)
  return(dados_final)
}
#Agora a tabela com todos os dados do banco de dados do SINAN vai ser gerado. Aqui o nome ? "dados_final"
dados_final <- load_rds_sinan()



#banco de dados ESUS ES
dados_esus <- readxl::read_xlsx('./eSUS/IEXO_AGOSTO_2024.xlsx')

# Ajustar as colunas de data do ESUS
cols_to_convert <- c("DT_NOTIFIC", "DT_SIN_PRI", "DT_NASC", "DT_INVEST", "DTINTERNA", "DT_OBITO", "DT_ENCERRA", 
                     "DT_DIGITA", "DT_TRANSDM", "DT_TRANSUS", "DT_TRANSSE", "DT_TRANSSM", "DT_TRANSRM", "DT_TRANSRS")

dados_esus <- dados_esus %>%
  mutate(across(all_of(cols_to_convert), ~ suppressWarnings(parse_date_time(., orders = c("ymd", "dmy")))))

# Converter tipos de dados para garantir compatibilidade
convert_to_compatible <- function(df) {
  df %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(across(where(is.double), as.character)) %>%
    mutate(across(where(is.integer), as.character))
}

dados_final <- convert_to_compatible(dados_final)
dados_esus <- convert_to_compatible(dados_esus)

# Unir os dados
dadosFINAL_union <- bind_rows(dados_final, dados_esus)


#FILTRAR AGENTE TÓXICO
dadosFINAL_union <- dadosFINAL_union |> 
  filter(AGENTE_TOX %in% c("2", "02", "3", "03", "4", "04", "5", "05", "6", "06"))


#CIDS de interesse ----- NÃO É NECESSÁRIO RODAR
#CIDS_INTOX <- c("X48", "X49", "X68","X69", "X87", "X89", "Y18", "Y19", "T60", "T61")

write.csv(dadosFINAL_union,"./Resultados/dados_final_intoxAgro.csv")




