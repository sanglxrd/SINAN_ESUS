#script juntar bancos de dados E-SUS e SINAN

setwd("F:/lucas.sanglard/OneDrive - Ministério da Saúde/Área de Trabalho/Unir_Bancos/")
Sys.setlocale("LC_ALL","Portuguese_Brazil")
library(foreign)
library(rlist)
library(plyr)
library(data.table)
library(dplyr)
library(stringr)
library(ggpubr)
library(sf)
library(reshape)
library(purrr)
library(openxlsx)
library(writexl)
library(dplyr)
library (magrittr)
library (abjutils)
library(data.table)
library(lubridate)

gc(T)
rm(list=ls())

#2

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

#write.csv(dados_final, "C:/Users/lucas.sanglard/Desktop/Monitoramento_sinan/dados_finalC.csv")


#5 - Dicionário

load_dict_sinan <- function(){
  
  dir_input <- paste(getwd(), "/dict/", sep="")
  list_files<-c("TP_NOT","SG_UF","UF_EMP","UF_HOSP", "SG_UF_NOT", "ID_MUNICIP", "CS_SEXO", "CS_GESTANT","CS_RACA", "CS_ESCOL_N",
                "SG_UF","ID_MN_RESI","CS_ZONA", "ID_OCUPA_N", "SIT_TRAB", "LOC_EXPO",
                "ZONA_EXP", "AGENTE_TOX", "UTILIZACAO", "ATIVIDA_1", "ATIVIDA_2", "ATIVIDA_3",
                "VIA_1", "VIA_2", "VIA_3", "CIRCUNSTAN", "DOENCA_TRA", "TPEXP", "TPATENDE", "HOSPITAL",
                "CLASSI_FIN", "CRITERIO", "EVOLUCAO", "CAT")
  list_dict <- NULL
  
  #Looping para ler cada um dos arquivos .csv referentes aos dicion?rios e consolida em uma lista (list_dict)
  for (file in list_files){
    
    dir_file <- paste(paste(dir_input, file, sep = ""),".csv", sep="")
    ifelse(file %in% c("CS_ESCOL_N","ID_OCUPA_N", "SIT_TRAB", "AGENTE_TOX", "ATIVIDA_1", "ATIVIDA_2", "ATIVIDA_3", "CIRCUNSTAN"),
           df_dict <- read.csv(dir_file, header =TRUE, sep=";", colClasses = c("key"="character")),
           df_dict <- read.csv(dir_file, header =TRUE, sep=";")
    )
    
    #Consolida e atualiza list_dict
    list_dict <- append(list_dict, list(df_dict))
  }
  
  #Atribui ao nome dos items da lista os mesmos nomes que os campos do SINAN
  names(list_dict)<-list_files
  return(list_dict)
}

#Depois de rodar a fun??o, agora ? o momento para aplic?-la
dict_Sinan <- load_dict_sinan()

#ler CSV do banco
#banco_esus <- read.xlsx('C:/Users/lucas.sanglard/Desktop/Unir_Bancos/eSUS/IEXO_FEVEREIRO2023.xlsx', sep = ";")
banco_esus_jan <- read.csv('C:/Users/lucas.sanglard/Desktop/Unir_Bancos/eSUS/IEXO_JUNHO2023.csv', sep = ";")
banco_esus_jan <- as.data.table(banco_esus_jan)


#traduz o nome das colunas
colnames(banco_esus_jan) <- c("NU_NOTIFIC", "TP_NOT", "ID_AGRAVO", "descricao", "ID_MUNICIP", "nome_unidade", "DT_NOTIFIC", "SEM_NOT",
                              "semana_data_diagnostico", "SG_UF_NOT", "codigo_ibge_unidade", "municipio_unidade", "DT_SIN_PRI",
                              "ID_CNS_SUS", "cpf",  "NM_PACIENT", "DT_NASC", "NU_IDADE_N", "CS_SEXO", "CS_GESTANT", "CS_RACA",
                              "etnia", "pcd", "morador_rua", "CS_ESCOL_N", "NM_MAE", "SG_UF", "ID_MN_RESI", "municipio_paciente",
                              "NM_BAIRRO", "logradouro", "endereco_outra_cidade", "cnes_unidade_referencia", "unidade_referencia",
                              "quadra", "lote_num","latitude_bairro", "longitude_bairro", "CS_ZONA", "ID_PAIS", "NU_TELEFON", "telefone_2",
                              "telefone_3", "DT_INVEST","ID_OCUPA_N", "SIT_TRAB", "TRAB_DESC", "LOC_EXPO","LOC_EXP_DE", "NOEMPRESA","CNAE",
                              "UF_EMP","MUN_EMP" ,"DIS_EMP", "NOBAIEMP","no_endereco_exposicao", "co_endereco_exposicao","COMP_EMP",
                              "REF_EMP", "CEP_EMP", "FONE_EMP", "ZONA_EXP","PAIS_EXP","AGENTE_TOX","OUT_AGENTE", "COAGTOXMA1", "AGENTE_1",
                              "P_ATIVO_1","COAGTOXMA2", "AGENTE_2","P_ATIVO_2","COAGTOXMA3","AGENTE_3","P_ATIVO_3","UTILIZACAO", "UTIL_DESC",
                              "ATIVIDA_1", "ATIVIDA_2","ATIVIDA_3","LAVOURA","VIA_1","VIA_2","VIA_3", "CIRCUNSTAN", "CIRCUN_DESC", "DOENCA_TRA",
                              "TPEXP", "NUTEMPO","TPTEMPO", "TPATENDE", "HOSPITAL",  "DTINTERNA","UF_HOSP","MUN_HOSP", "CNES_HOSP","CLASSI_FIN",
                              "co_cid_diagnostico","DIAG_CONF","CRITERIO","EVOLUCAO","DT_OBITO","CAT", "DT_ENCERRA")


#ARRUMA OS BANCOS
banco_esus_jan$DT_NOTIFIC <- as.character(banco_esus_jan$DT_NOTIFIC)
banco_esus_jan$DT_NASC <- as.character(banco_esus_jan$DT_NASC)
banco_esus_jan$DT_INVEST <- as.character(banco_esus_jan$DT_INVEST)
banco_esus_jan$DT_ENCERRA <- as.character(banco_esus_jan$DT_ENCERRA)
banco_esus_jan$DT_OBITO <- as.character(banco_esus_jan$DT_OBITO)
banco_esus_jan$DT_SIN_PRI <- as.character(banco_esus_jan$DT_SIN_PRI)
banco_esus_jan$DTINTERNA <- as.character(banco_esus_jan$DTINTERNA)


dados_final$DT_NOTIFIC <- as.character(dados_final$DT_NOTIFIC)
dados_final$DT_NASC <- as.character(dados_final$DT_NASC)
dados_final$DT_INVEST <- as.character(dados_final$DT_INVEST)
dados_final$DT_ENCERRA <- as.character(dados_final$DT_ENCERRA)
dados_final$DT_OBITO <- as.character(dados_final$DT_OBITO)
dados_final$DT_SIN_PRI <- as.character(dados_final$DT_SIN_PRI)
dados_final$DTINTERNA <- as.character(dados_final$DTINTERNA)




#seleciona colunas para o merge
data_selec_ES <- select(banco_esus_jan, "NU_NOTIFIC", "TP_NOT", "ID_AGRAVO", "ID_MUNICIP", "DT_NOTIFIC", "SEM_NOT", "SG_UF_NOT",
                               "DT_SIN_PRI", "ID_CNS_SUS", "NM_PACIENT", "DT_NASC", "NU_IDADE_N","CS_SEXO", "CS_GESTANT", "CS_RACA",
                               "CS_ESCOL_N","SG_UF", "ID_MN_RESI", "NM_BAIRRO",
                               "CS_ZONA", "ID_PAIS", "NU_TELEFON", "DT_INVEST", "ID_OCUPA_N", "SIT_TRAB",
                               "TRAB_DESC", "LOC_EXPO","LOC_EXP_DE","NOEMPRESA","CNAE","UF_EMP","MUN_EMP", "DIS_EMP", "NOBAIEMP",
                               "COMP_EMP", "REF_EMP", "CEP_EMP", "FONE_EMP", "ZONA_EXP", "PAIS_EXP", "AGENTE_TOX", "OUT_AGENTE","COAGTOXMA1","AGENTE_1","P_ATIVO_1",  "COAGTOXMA2","AGENTE_2",
                               "P_ATIVO_2","COAGTOXMA3", "AGENTE_3", "P_ATIVO_3","UTILIZACAO", "UTIL_DESC","ATIVIDA_1", "ATIVIDA_2","ATIVIDA_3","LAVOURA","VIA_1","VIA_2", "VIA_3",
                               "CIRCUNSTAN", "DOENCA_TRA","TPEXP",'NUTEMPO',"TPTEMPO","TPATENDE","HOSPITAL", "DTINTERNA", "UF_HOSP","MUN_HOSP","CNES_HOSP", "CLASSI_FIN",
                               "DIAG_CONF","CRITERIO","EVOLUCAO","DT_OBITO", "CAT", "DT_ENCERRA")

data_selec_SINAN <- select(dados_final, "NU_NOTIFIC", "TP_NOT", "ID_AGRAVO", "ID_MUNICIP", "DT_NOTIFIC", "SEM_NOT", "SG_UF_NOT",
                                  "DT_SIN_PRI", "ID_CNS_SUS", "NM_PACIENT", "DT_NASC", "NU_IDADE_N","CS_SEXO", "CS_GESTANT", "CS_RACA",
                                  "CS_ESCOL_N","SG_UF", "ID_MN_RESI", "NM_BAIRRO",
                                  "CS_ZONA", "ID_PAIS", "NU_TELEFON", "DT_INVEST", "ID_OCUPA_N", "SIT_TRAB",
                                  "TRAB_DESC", "LOC_EXPO","LOC_EXP_DE","NOEMPRESA","CNAE","UF_EMP","MUN_EMP", "DIS_EMP", "NOBAIEMP",
                                  "COMP_EMP", "REF_EMP", "CEP_EMP", "FONE_EMP", "ZONA_EXP", "PAIS_EXP", "AGENTE_TOX", "OUT_AGENTE","COAGTOXMA1","AGENTE_1","P_ATIVO_1",  "COAGTOXMA2","AGENTE_2",
                                  "P_ATIVO_2","COAGTOXMA3", "AGENTE_3", "P_ATIVO_3","UTILIZACAO", "UTIL_DESC","ATIVIDA_1", "ATIVIDA_2","ATIVIDA_3","LAVOURA","VIA_1","VIA_2", "VIA_3",
                                  "CIRCUNSTAN", "DOENCA_TRA","TPEXP",'NUTEMPO',"TPTEMPO","TPATENDE","HOSPITAL", "DTINTERNA", "UF_HOSP","MUN_HOSP","CNES_HOSP", "CLASSI_FIN",
                                  "DIAG_CONF","CRITERIO","EVOLUCAO","DT_OBITO", "CAT", "DT_ENCERRA")




#junta os bancos
bancos_unidos <- rbind(data_selec_ES, data_selec_SINAN)


#Filtragem por ano

dados_final_SINAN <- bancos_unidos |>
  mutate(ano=year(as.Date(DT_NOTIFIC)))

#Fitlros por ano
dados_SINAN <- subset(dados_final_SINAN, ano %in% 2013:2022)

# filtro por idade
dados_final <- dados_SINAN |>  
  mutate(IDADE_REAL = ((as_date(DT_NOTIFIC) - as_date(DT_NASC)) |> as.numeric() / 365) |> round())

dados_final_filt <- subset(dados_final, IDADE_REAL %in% 0:10)

write.csv(dados_final_filt,"C:/Users/lucas.sanglard/Desktop/Unir_Bancos/dados_final_filtrados.csv")


  

#dados_finish <- subset(dados_SINAN, AGENTE_TOX %in% c("02","2","03","3","04","4","05","5", "06", "6"))





write.csv(dados_finish,"C:/Users/lucas.sanglard/Desktop/Unir_Bancos/dados_SINAN.csv")
saveRDS(dados_finish, "C:/Users/lucas.sanglard/Desktop/Unir_Bancos/dados_SINAN.rds")











bancos_unidos$DT_NOTIFIC <- format(bancos_unidos$DT_NOTIFIC, "%d/%m/%Y")








########## LISTA DE CORRESPONDENCIA


list_corresp <- c('OMEGAFIX', 'ÔMEGA-FIX',"OMEGA", "METILCLOROISOTIAZOLINA", "MCI",
                  "METILSOTIAZOLINONA", "MI", "MICROFARMA", "CASSU", "BRAIDS", "caçulinha cabelas", "CABELO", "POMADA CAPILAR",
                  "POMADA MODELADORA", "TRANÇA", "CAPILAR")

#FILTER SUBs - sinan ############

filter_subs_sinan<- function(dados_final, list_corresp) {
  #Lista de campos nos quais o filtro é aplicado
  col_list <- c("AGENTE_1", "AGENTE_2", "AGENTE_3", "P_ATIVO_1", "P_ATIVO_2", "P_ATIVO_3","OUT_AGENTE","COAGTOXMA2","COAGTOXMA3")
  
  #Filtra dados_cru por diversos agentes tóxicos
  col_result = FALSE
  for (col in col_list) {
    col_bool <- grepl(paste(list_corresp, collapse="|"), dados_final[,get(col)])
    col_result <- !!rowSums(data.frame(col_result, col_bool))
  }
  
  dt_final <- dados_final[col_result,]
  
  #Limpa caracteres "\n" que causam "buracos" na tabela 
  dt_final <- data.frame(lapply(dados_final, function(x) {
    gsub("\n","", x)
  }))
  
  dt_final<- data.table(dados_final)
  return(dt_final) 
}


dados_filtrados <- filter_subs_sinan()

##### PRE PROCESS


reclass_sinan<- function(dados_final, dict_Sinan, list_prop = NULL){
  #Utiliza a lista proposta de campos (list_prop) para processar os dados_brutos
  if (is.null(list_prop)){
    lista<-list.names(dict_Sinan)
  } else {
    lista <- list_prop  
  }
  
  #Para cada campo de lista realizada a tradução dos dados_brutos
  for (col in lista) {
    #Escolhe o dicionário para cada campo
    dict_col <- dict_Sinan[[col]]
    
    #Mapeia os valores do dicionário com os valores de dados_brutos
    suppressMessages(dados_final[,col]<-mapvalues(dados_final[,get(col)], from = dict_col[,"key"],to = dict_col[,"value"]))
  }
  return(dados_final)
}

tabela_traduzida <- reclass_sinan(dados_final, dict_Sinan)

                                  