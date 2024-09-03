# Thais Takeuchi
# 15/7/24
# Project: Master thesis

#########################
# PREPARE THE ENVIRONMENT
#########################

### Cleaning the R environment
rm(list=ls())

### Load Packages (and install packages if needed)
load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

##################################
##################################
######## CENSO 2000 ##############
##################################
##################################

# ----
# Primeira vez usando o Google Cloud
# rm(list = ls())
# 
# library(DBI)
# library(bigrquery)
# library(ggplot2)
# 
# # PASSO 3: apontar a autenticação para o arquivo json
# bq_auth(path = "~/Desktop/teste/august-tower-429517-m7-8854ec7e5c27.json")
# 
# # PASSO 4: criar conexão com o BigQuery
# con <- dbConnect(
#   bigrquery::bigquery(),
#   billing = "august-tower-429517-m7",
#   project = "basedosdados"
# )
#----

# Definindo meu projeto no Google Cloud
set_billing_id("august-tower-429517-m7")

####################################
# CARREGANDO DADOS - BASE DOS DADOS
####################################
# https://basedosdados.org/dataset/b8e8bd62-4eb9-42f9-9ffa-b5cca093f58e?table=ab64f117-11c7-456f-9b3a-3fc1b7f6bd0e


######
#1970
######

# Censo 1970
# Domicílio
# query <- bdplyr("br_ibge_censo_demografico.microdados_domicilio_1970")
# censo_domicilio_1970 <- bd_collect(query)

# Pessoa
# query <- bdplyr("br_ibge_censo_demografico.microdados_pessoa_1970")
# censo_pessoa_1970 <- bd_collect(query)



######
#1980
######

# Censo 1980
# Domicílio
# query <- bdplyr("br_ibge_censo_demografico.microdados_domicilio_1980")
# censo_domicilio_1980 <- bd_collect(query)

# Pessoa
query <- bdplyr("br_ibge_censo_demografico.microdados_pessoa_1980")
censo_pessoa_1980 <- bd_collect(query)

######
#1991
######

# Censo 1991
# Domicílio
# query <- bdplyr("br_ibge_censo_demografico.microdados_domicilio_1991")
# censo_domicilio_1991 <- bd_collect(query)

# Pessoa
query <- bdplyr("br_ibge_censo_demografico.microdados_pessoa_1991")
censo_pessoa_1991 <- bd_collect(query)

######
#2000
######

# Censo 2000
# Domicílio
# query <- bdplyr("br_ibge_censo_demografico.microdados_domicilio_2000")
# censo_domicilio_2000 <- bd_collect(query)

# Pessoa
query <- bdplyr("br_ibge_censo_demografico.microdados_pessoa_2000")
censo_pessoa_2000 <- bd_collect(query)

######
#2010
######

# Censo 2010- não está dando certo (base de dados muito grande, peguei compatibilizada do datazoom - stata)
# Domicílio
# query <- bdplyr("br_ibge_censo_demografico.microdados_domicilio_2010")
# censo_domicilio_2010 <- bd_collect(query)

# Pessoa
query <- basedosdados::bdplyr("br_ibge_censo_demografico.microdados_pessoa_2010")
censo_pessoa_2010 <- bd_collect(query)

# Registro civil
query <- basedosdados::bdplyr("br_ibge_censo_demografico.setor_censitario_registro_civil_2010")
censo_registro_civil_2010 <- bd_collect(query)

# Cônjuges
query <- bdplyr("br_ibge_censo_demografico.setor_censitario_relacao_parentesco_conjuges_2010")
censo_conjuges_2010 <- bd_collect(query)

######################
# Save generated bases
######################

### 2000
# Path
# file_path <- "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/censo_pessoa_2000.csv"

# # Save in CSV format
# write.csv(censo_pessoa_2000, file = file_path, row.names = FALSE)

# Save in parquet format (more efficient)
install.packages(arrow)
library(arrow)

one_drive_path <- "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases"
file_path <- file.path(one_drive_path, "censo_pessoa_2000.parquet")
write_parquet(censo_pessoa_2000, file_path)

#########################
#########################
########  wvs  ##########
#########################
#########################

# % saying divorce never justifiable
wvs1 <- readRDS("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/WVS/WVS1/WV1_Data_R_v20200208.rds")
wvs2 <- readRDS("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/WVS/WVS2/F00008311-WV2_Data_R_v20180912.rds")
wvs3 <- readRDS("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/WVS/WVS3/F00008205-WV3_Data_R_v20180912.rds")
wvs4 <- load("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/WVS/WVS4/WV4_Data_R_v20201117.rdata")

wvs4 <- WV4_Data_R_v20201117
rm(WV4_Data_R_v20201117)

# Number of divorces per year






