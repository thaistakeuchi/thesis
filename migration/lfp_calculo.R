# Script calculo da lfp dos anos de 1990, 1980, 1970, 1960, 1950 para as mulheres
# 09/10/2024

#######################
# PREPARE ENVIRONMENT
#######################

rm(list = ls())

load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx", "fixest", "readxl")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

# ------------------------------------------------------------------------------
# 1990 PNAD
# ------------------------------------------------------------------------------

## PNAD
pnad_1990_pessoa <- read_parquet("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/PNAD/1990/pessoas_1990.parquet")

# transforma em dta
#pnad_1990_pessoa <- write_dta(pnad_1990_pessoa, "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/PNAD/1990/pessoas_1990.dta")

pnad_1990_dom <- read_dta("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/PNAD/1990/domic_1990.dta")

pnad_1990_pessoa <- pnad_1990_pessoa |>
  mutate(v0102 = as.numeric(v0102), v0103 = as.numeric(v0103))

# junta peso em pnad_pessoa
pnad_1990_dom_selecionado <- pnad_1990_dom |>
  dplyr::select(v0102, v0103, v1091)

pnad <- pnad_1990_pessoa |>
  left_join(pnad_1990_dom_selecionado, by = c("v0102", "v0103"))


pnad <- pnad |>
  mutate(regiao = case_when(
    uf %in% c(11, 12, 13, 14, 15, 16) ~ "Norte",
    uf %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Nordeste",
    uf %in% c(50, 51, 52, 53) ~ "Centro-Oeste",
    uf %in% c(31, 32, 33, 35) ~ "Sudeste",
    uf %in% c(41, 42, 43) ~ "Sul",
    TRUE ~ "Outros"  
  ))

# uf
econ_ativa_uf <- pnad |>
  filter(v0501 %in% c(1, 2, 3), v0805 >= 30 & v0805 <= 49, v0303 == 3) |>
  group_by(uf) |> 
  summarise(soma_eco_ativa = sum(v1091, na.rm = TRUE))

idade_ativa_uf <- pnad |> 
  filter(v0805 >= 30 & v0805 <= 49, v0303 == 3) |> 
  group_by(uf) |> 
  summarise(soma_idade_ativa = sum(v1091, na.rm = TRUE))

mulher_ativa_por_uf_1990 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf") |>
  mutate(lfp_uf_1990 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1990, starts_with("soma_")), na.rm = TRUE)

# https://seculoxx.ibge.gov.br/populacionais-sociais-politicas-e-culturais/busca-por-palavra-chave/trabalho/1030-populacao-economicamente-ativa

# Taxa de atividade das pessoas de 10 anos ou mais de idade, por sexo, segundo as Grandes Regiões, a situação do domicílio e os grupos de idade - 1981/1990

# https://biblioteca.ibge.gov.br/visualizacao/periodicos/59/pnad_1990_v14_n1_br.pdf
# 3.1

#https://seriesestatisticas.ibge.gov.br/series.aspx?no=7&op=0&vcodigo=PD372&t=grupos-idade-periodo-referencia-365-dias

### BATEU !

# gera base final
mulher_ativa_por_uf_1990 <- mulher_ativa_por_uf_1990 |>
  rename(uf_nascim = uf) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

# gera xlsx

write_parquet(pnad, "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/PNAD/1990/pnad_1990.parquet")

write_xlsx(mulher_ativa_por_uf_1990, "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/lfp/pnad_lfp_uf_1990.xlsx")

# ------------------------------------------------------------------------------
# 1991 CENSO
# ------------------------------------------------------------------------------
censo_1991 <- read_dta("D:/1_migration/1_datasets_migrants/censo/nova/censo/1991/censo_1991_temp_v2.dta")

# uf: 30-39
econ_ativa_uf <- censo_1991 |>
  filter((V0358 %in% c(10,1,2) ) & V3072 >= 30 & V3072 <= 39 & V0301 == 2) |>
  group_by(UF) |> 
  summarise(soma_eco_ativa = sum(V7301, na.rm = TRUE))

idade_ativa_uf <- censo_1991 |> #numero oficial: 4865939 (https://biblioteca.ibge.gov.br/visualizacao/periodicos/82/cd_1991_n1_caracteristicas_populacao_domicilios_br.pdf - p.126)
  filter(V3072 >= 30 & V3072 <= 39, V0301 == 2) |> 
  group_by(UF) |> 
  summarise(soma_idade_ativa = sum(V7301, na.rm = TRUE))

mulher_ativa_por_uf_1991 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "UF") |>
  mutate(lfp_uf_1991 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1991, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# bate com os dados ILO (15+) não é bom bater com essa fonte
#https://data.worldbank.org/indicator/SL.TLF.CACT.FE.NE.ZS?end=1971&locations=BR&start=1960

# e com o anuario estatistico do brasil 1994 (p.266) PNAD
# http://memoria.org.br/pub/meb000000350/90000/90000053.pdf

# gera base final
mulher_ativa_por_uf_1991 <- mulher_ativa_por_uf_1991 |>
  rename(uf_nascim = UF) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1991, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-39_1991.xlsx")

#-------------------------------------------------------------------------------
# uf: 40-49
econ_ativa_uf <- censo_1991 |>
  filter((V0358 %in% c(10,1,2) ) & V3072 >= 40 & V3072 <= 49 & V0301 == 2) |>
  group_by(UF) |> 
  summarise(soma_eco_ativa = sum(V7301, na.rm = TRUE))

idade_ativa_uf <- censo_1991 |>
  filter(V3072 >= 40 & V3072 <= 49, V0301 == 2) |> 
  group_by(UF) |> 
  summarise(soma_idade_ativa = sum(V7301, na.rm = TRUE))

mulher_ativa_por_uf_1991 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "UF") |>
  mutate(lfp_uf_1991 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1991, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# gera base final
mulher_ativa_por_uf_1991 <- mulher_ativa_por_uf_1991 |>
  rename(uf_nascim = UF) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1991, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_40-49_1991.xlsx")
#-------------------------------------------------------------------------------
# uf: 30-49
econ_ativa_uf <- censo_1991 |>
  filter((V0358 %in% c(10,1,2) ) & V3072 >= 30 & V3072 <= 49 & V0301 == 2) |>
  group_by(UF) |> 
  summarise(soma_eco_ativa = sum(V7301, na.rm = TRUE))

idade_ativa_uf <- censo_1991 |> #numero oficial: 4865939 (https://biblioteca.ibge.gov.br/visualizacao/periodicos/82/cd_1991_n1_caracteristicas_populacao_domicilios_br.pdf - p.126)
  filter(V3072 >= 30 & V3072 <= 49, V0301 == 2) |> 
  group_by(UF) |> 
  summarise(soma_idade_ativa = sum(V7301, na.rm = TRUE))

mulher_ativa_por_uf_1991 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "UF") |>
  mutate(lfp_uf_1991 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1991, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# gera base final
mulher_ativa_por_uf_1991 <- mulher_ativa_por_uf_1991 |>
  rename(uf_nascim = UF) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1991, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1991.xlsx")
#-------------------------------------------------------------------------------
# mais de 10 anos
# uf
econ_ativa_uf <- censo_1991 |>
  filter((V0358 %in% c(10,1,2) ) & V3072 >= 10 & V0301 == 2) |>
  group_by(UF) |> 
  summarise(soma_eco_ativa = sum(V7301, na.rm = TRUE))

idade_ativa_uf <- censo_1991 |> #numero oficial: 4865939 (https://biblioteca.ibge.gov.br/visualizacao/periodicos/82/cd_1991_n1_caracteristicas_populacao_domicilios_br.pdf - p.126)
  filter(V3072 >= 10, V0301 == 2) |> 
  group_by(UF) |> 
  summarise(soma_idade_ativa = sum(V7301, na.rm = TRUE))

mulher_ativa_por_uf_1991 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "UF") |>
  mutate(lfp_uf_1991 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1991, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# gera base final
mulher_ativa_por_uf_1991 <- mulher_ativa_por_uf_1991 |>
  rename(uf_nascim = UF) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1991, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_10-mais_1991.xlsx")


# ------------------------------------------------------------------------------
# 1981 PNAD
# ------------------------------------------------------------------------------
pnad_dom_1981 <- read_delim("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/PNAD/PNAD 1981/pnad.dom_1981.csv", 
  delim = "\t")

mapeamento_ufs <- c(
  "11" = "33", "12" = "33",  # Rio de Janeiro
  "21" = "35", "22" = "35", "23" = "35",  # São Paulo
  "31" = "41",  # Paraná
  "32" = "42",  # Santa Catarina
  "33" = "43", "34" = "43",  # Rio Grande do Sul
  "41" = "31", "42" = "31",  # Minas Gerais
  "43" = "32",  # Espírito Santo
  "51" = "21",  # Maranhão
  "52" = "22",  # Piauí
  "53" = "23",  # Ceará
  "54" = "24",  # Rio Grande do Norte
  "55" = "25",  # Paraíba
  "56" = "26",  # Pernambuco
  "57" = "27",  # Alagoas
  "58" = "28",  # Sergipe
  "59" = "29",  # Bahia
  "61" = "53",  # Distrito Federal
  "71" = "11",  # Rondônia
  "72" = "12",  # Acre
  "73" = "13",  # Amazonas
  "74" = "14",  # Roraima
  "75" = "15",  # Pará
  "76" = "16",  # Amapá
  "81" = "50",  # Mato Grosso do Sul
  "82" = "51",  # Mato Grosso
  "83" = "52"   # Goiás
)

pnad_pessoa_1981 <- read_delim(
  "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/PNAD/PNAD 1981/pnad.pes_1981.csv",
  delim = "\t",
  na = c("", " ")
) |>
  mutate(
    uf = recode(v0010, !!!mapeamento_ufs),
    uf = as.numeric(uf),
    v0501 = as.numeric(v0501)
  ) |> 
  mutate(regiao = case_when(
    uf %in% c(11, 12, 13, 14, 15, 16) ~ "Norte",
    uf %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Nordeste",
    uf %in% c(50, 51, 52, 53) ~ "Centro-Oeste",
    uf %in% c(31, 32, 33, 35) ~ "Sudeste",
    uf %in% c(41, 42, 43) ~ "Sul",
    TRUE ~ "Outros"  
  ))

# Calcula mulheres economicamente ativas entre 30-49 anos----

# uf
econ_ativa_uf <- pnad_pessoa_1981 |>
  filter(v0501 %in% c(1, 2, 3), v0805 >= 30 & v0805 <= 49, v0303 == 3) |>
  group_by(uf) |> 
  summarise(soma_eco_ativa = sum(v9991, na.rm = TRUE))

idade_ativa_uf <- pnad_pessoa_1981 |> 
  filter(v0805 >= 30 & v0805 <= 49, v0303 == 3) |> 
  group_by(uf) |> 
  summarise(soma_idade_ativa = sum(v9991, na.rm = TRUE))

mulher_ativa_por_uf_1981 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf") |>
  mutate(lfp_uf_1981 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1981, starts_with("soma_")), na.rm = TRUE)

# regiao
econ_ativa_regiao <- pnad_pessoa_1981 |>
  filter(v0501 %in% c(1, 2, 3), v0805 >= 30 & v0805 <= 49, v0303 == 3) |>
  group_by(regiao) |> 
  summarise(soma_eco_ativa = sum(v9991, na.rm = TRUE))

idade_ativa_regiao <- pnad_pessoa_1981 |> 
  filter(v0805 >= 30 & v0805 <= 49, v0303 == 3) |> 
  group_by(regiao) |> 
  summarise(soma_idade_ativa = sum(v9991, na.rm = TRUE))

mulher_ativa_por_regiao_1981 <- econ_ativa_regiao |>
  left_join(idade_ativa_regiao, by = "regiao") |>
  mutate(lfp_regiao_1981 = soma_eco_ativa / soma_idade_ativa)

somas_regiao <- colSums(dplyr::select(mulher_ativa_por_regiao_1981, starts_with("soma_")), na.rm = TRUE)

# bate com os dados oficiais EBA!
# População residente de 10 anos e mais de idade, por condição de atividade e sexo, segundo a situação do domicílio e grupos de idade - 1981
#https://seculoxx.ibge.gov.br/populacionais-sociais-politicas-e-culturais/busca-por-palavra-chave/trabalho/1030-populacao-economicamente-ativa

# gera base final
mulher_ativa_por_uf_1981 <- mulher_ativa_por_uf_1981 |>
  rename(uf_nascim = uf) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1981, 
  "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/lfp/pnad_lfp_uf_1981.xlsx")

mulher_ativa_por_regiao_1981 <- mulher_ativa_por_regiao_1981 |>
  rename(regiao_nascim = regiao) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_regiao_1981, 
  "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/lfp/pnad_lfp_regiao_1981.xlsx")

# ------------------------------------------------------------------------------
# 1980 CENSO
# ------------------------------------------------------------------------------
censo_1980 <- read_dta("D:/1_migration/1_datasets_migrants/censo/nova/censo/1980/censo_1980_temp.dta")

### 30-49 anos
# uf
econ_ativa_uf <- censo_1980 |>
  filter((V529 %in% c(0,1,2) ) & V606 >= 30 & V606 <= 49 & V501 == 3) |>
  group_by(V2) |> 
  summarise(soma_eco_ativa = sum(V604, na.rm = TRUE))

idade_ativa_uf <- censo_1980 |> #numero oficial: 2432965 (https://seculoxx.ibge.gov.br/images/seculoxx/arquivos_download/trabalho/1983/trabalho1983aeb_01_1.pdf)
  filter(V606 >= 30 & V606 <= 49, V501 == 3) |> 
  group_by(V2) |> 
  summarise(soma_idade_ativa = sum(V604, na.rm = TRUE))

mulher_ativa_por_uf_1980 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "V2") |>
  mutate(lfp_uf_1980 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1980, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result


#https://seculoxx.ibge.gov.br/images/seculoxx/arquivos_download/trabalho/1983/trabalho1983aeb_01_1.pdf

# gera base final
mulher_ativa_por_uf_1980 <- mulher_ativa_por_uf_1980 |>
  rename(uf_nascim = V2) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1980, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1980.xlsx")

#-------------------------------------------------------------------------------

### 40-49 anos
# uf
econ_ativa_uf <- censo_1980 |>
  filter((V529 %in% c(0,1,2) ) & V606 >= 40 & V606 <= 49 & V501 == 3) |>
  group_by(V2) |> 
  summarise(soma_eco_ativa = sum(V604, na.rm = TRUE))

idade_ativa_uf <- censo_1980 |>
  filter(V606 >= 40 & V606 <= 49, V501 == 3) |> 
  group_by(V2) |> 
  summarise(soma_idade_ativa = sum(V604, na.rm = TRUE))

mulher_ativa_por_uf_1980 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "V2") |>
  mutate(lfp_uf_1980 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1980, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# gera base final
mulher_ativa_por_uf_1980 <- mulher_ativa_por_uf_1980 |>
  rename(uf_nascim = V2) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1980, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_40-49_1980.xlsx")
#-------------------------------------------------------------------------------
### 30-39 anos
# uf
econ_ativa_uf <- censo_1980 |>
  filter((V529 %in% c(0,1,2) ) & V606 >= 30 & V606 <= 39 & V501 == 3) |>
  group_by(V2) |> 
  summarise(soma_eco_ativa = sum(V604, na.rm = TRUE))

idade_ativa_uf <- censo_1980 |> #numero oficial: 2432965 (https://seculoxx.ibge.gov.br/images/seculoxx/arquivos_download/trabalho/1983/trabalho1983aeb_01_1.pdf)
  filter(V606 >= 30 & V606 <= 39, V501 == 3) |> 
  group_by(V2) |> 
  summarise(soma_idade_ativa = sum(V604, na.rm = TRUE))

mulher_ativa_por_uf_1980 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "V2") |>
  mutate(lfp_uf_1980 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1980, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

mulher_ativa_por_uf_1980 <- mulher_ativa_por_uf_1980 |>
  rename(uf_nascim = V2) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1980, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-39_1980.xlsx")
#-------------------------------------------------------------------------------
### more than 10 years
# uf
econ_ativa_uf <- censo_1980 |>
  filter((V529 %in% c(0,1,2) ) & V606 >= 10 & V501 == 3) |>
  group_by(V2) |> 
  summarise(soma_eco_ativa = sum(V604, na.rm = TRUE))

idade_ativa_uf <- censo_1980 |> #numero oficial: 2432965 (https://seculoxx.ibge.gov.br/images/seculoxx/arquivos_download/trabalho/1983/trabalho1983aeb_01_1.pdf)
  filter(V606 >= 10, V501 == 3) |> 
  group_by(V2) |> 
  summarise(soma_idade_ativa = sum(V604, na.rm = TRUE))

mulher_ativa_por_uf_1980 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "V2") |>
  mutate(lfp_uf_1980 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1980, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

mulher_ativa_por_uf_1980 <- mulher_ativa_por_uf_1980 |>
  rename(uf_nascim = V2) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1980, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_10-mais_1980.xlsx")


# ------------------------------------------------------------------------------
# 1970 CENSO
# ------------------------------------------------------------------------------
censo_1970 <- read_dta("D:/1_migration/1_datasets_migrants/censo/nova/censo/1970/censo_1970_temp.dta")

# uf: 30-49
econ_ativa_uf <- censo_1970 |>
  filter(V043 == 7, V027 >= 30 & V027 <= 49, V023 == 1) |>
  group_by(uf) |> 
  summarise(soma_eco_ativa = sum(V054, na.rm = TRUE))

idade_ativa_uf <- censo_1970 |> 
  filter(V027 >= 30 & V027 <= 49, V023 == 1) |> 
  group_by(uf) |> 
  summarise(soma_idade_ativa = sum(V054, na.rm = TRUE))

mulher_ativa_por_uf_1970 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf") |>
  mutate(lfp_uf_1970 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1970, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# bate com os dados ILO (15+)
#https://data.worldbank.org/indicator/SL.TLF.CACT.FE.NE.ZS?end=1971&locations=BR&start=1960

# https://seculoxx.ibge.gov.br/images/seculoxx/arquivos_download/populacao/1972/populacao_m_1972aeb_016.pdf
# we have 98% of the sample for >= 10 years old (miscount 80924)

# barros jatobá and mendonça (1995) - PEA/PIA female - 18.1%
# https://web.bndes.gov.br/bib/jspui/bitstream/1408/13506/2/RB%2013%20Segrega%C3%A7%C3%A3o%20por%20G%C3%AAnero%20no%20Mercado%20Formal%20de%20Trabalho_P_BD.pdf

# gera base final
mulher_ativa_por_uf_1970 <- mulher_ativa_por_uf_1970 |>
  rename(uf_nascim = uf) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1970, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1970.xlsx")

# Fontes
# https://biblioteca.ibge.gov.br/visualizacao/periodicos/69/cd_1970_v1_br.pdf

#-------------------------------------------------------------------------------
### uf:40-49

# uf
econ_ativa_uf <- censo_1970 |>
  filter(V043 == 7, V027 >= 40 & V027 <= 49, V023 == 1) |>
  group_by(uf) |> 
  summarise(soma_eco_ativa = sum(V054, na.rm = TRUE))

idade_ativa_uf <- censo_1970 |> 
  filter(V027 >= 40 & V027 <= 49, V023 == 1) |> 
  group_by(uf) |> 
  summarise(soma_idade_ativa = sum(V054, na.rm = TRUE))

mulher_ativa_por_uf_1970 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf") |>
  mutate(lfp_uf_1970 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1970, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# gera base final
mulher_ativa_por_uf_1970 <- mulher_ativa_por_uf_1970 |>
  rename(uf_nascim = uf) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1970, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_40-49_1970.xlsx")

#-------------------------------------------------------------------------------
### uf:30-39

# uf
econ_ativa_uf <- censo_1970 |>
  filter(V043 == 7, V027 >= 30 & V027 <= 39, V023 == 1) |>
  group_by(uf) |> 
  summarise(soma_eco_ativa = sum(V054, na.rm = TRUE))

idade_ativa_uf <- censo_1970 |> 
  filter(V027 >= 30 & V027 <= 39, V023 == 1) |> 
  group_by(uf) |> 
  summarise(soma_idade_ativa = sum(V054, na.rm = TRUE))

mulher_ativa_por_uf_1970 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf") |>
  mutate(lfp_uf_1970 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1970, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# gera base final
mulher_ativa_por_uf_1970 <- mulher_ativa_por_uf_1970 |>
  rename(uf_nascim = uf) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1970, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-39_1970.xlsx")

#-------------------------------------------------------------------------------
### uf: 10 years or more
econ_ativa_uf <- censo_1970 |>
  filter(V043 == 7, V027 >= 10, V023 == 1) |>
  group_by(uf) |> 
  summarise(soma_eco_ativa = sum(V054, na.rm = TRUE))

idade_ativa_uf <- censo_1970 |> 
  filter(V027 >= 10, V023 == 1) |> 
  group_by(uf) |> 
  summarise(soma_idade_ativa = sum(V054, na.rm = TRUE))

mulher_ativa_por_uf_1970 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf") |>
  mutate(lfp_uf_1970 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1970, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# gera base final
mulher_ativa_por_uf_1970 <- mulher_ativa_por_uf_1970 |>
  rename(uf_nascim = uf) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1970, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_10-mais_1970.xlsx")


# ------------------------------------------------------------------------------
# 1960 CENSO
# ------------------------------------------------------------------------------

censo_1960_corrigido <- read_csv("D:/IBGE_1960/Censo 1960 1.27pct CEM/censo1960_corrigido/Censo.1960.brasil.pessoas.amostra.1.27porcento.csv")

censo_1960_pess_ipmus <- read_dta("D:/IBGE_1960/Censo 1960 5pct IPUMS/censo1960pes_ipums.dta")

censo_1960_dom_ipmus <- read_dta("D:/IBGE_1960/Censo 1960 5pct IPUMS/censo1960dom_ipums.dta")

# create censo_1960 with merged data from id and hh level
censo_1960_ipmus <- merge(censo_1960_pess_ipmus, censo_1960_dom_ipmus, by = c("pernum", "serial", "sample"))


# Verificar se as colunas com o mesmo nome, mas com .x e .y, são iguais
comparar_variaveis <- function(data, var_prefix) {
  col_x <- paste0(var_prefix, ".x")
  col_y <- paste0(var_prefix, ".y")
  
  if (col_x %in% colnames(data) && col_y %in% colnames(data)) {
    return(all(data[[col_x]] == data[[col_y]], na.rm = TRUE))
  } else {
    return(NA)  # Se uma das colunas não existir
  }
}

variaveis_para_comparar <- c("cntry", "year", "wtper", "resident")
resultado_comparacao <- sapply(variaveis_para_comparar, function(v) comparar_variaveis(censo_1960_ipmus, v))
resultado_comparacao

# uf: 30-49
econ_ativa_uf <- censo_1960_ipmus |>
  filter(empstat %in% c(1,2), age >= 30 & age <= 49, sex == 2) |>
  group_by(statebr) |> 
  summarise(soma_eco_ativa = sum(wtper.x, na.rm = TRUE))

idade_ativa_uf <- censo_1960_ipmus |> 
  filter(age >= 30 & age <= 49, sex == 2) |> 
  group_by(statebr) |> 
  summarise(soma_idade_ativa = sum(wtper.x, na.rm = TRUE))

mulher_ativa_por_uf_1960 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "statebr") |>
  mutate(lfp_uf_1960 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1960, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# final dt
mulher_ativa_por_uf_1960 <- mulher_ativa_por_uf_1960 |>
  rename(uf_nascim = statebr) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1960, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1960.xlsx")

# statebr: 30-39
econ_ativa_uf <- censo_1960_ipmus |>
  filter(empstat %in% c(1,2), age >= 30 & age <= 39, sex == 2) |>
  group_by(statebr) |> 
  summarise(soma_eco_ativa = sum(wtper.x, na.rm = TRUE))

idade_ativa_uf <- censo_1960_ipmus |> 
  filter(age >= 30 & age <= 39, sex == 2) |> 
  group_by(statebr) |> 
  summarise(soma_idade_ativa = sum(wtper.x, na.rm = TRUE))

mulher_ativa_por_uf_1960 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "statebr") |>
  mutate(lfp_uf_1960 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1960, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# final dt
mulher_ativa_por_uf_1960 <- mulher_ativa_por_uf_1960 |>
  rename(uf_nascim = statebr) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1960, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-39_1960.xlsx")


# statebr: more than 10 years
econ_ativa_uf <- censo_1960_ipmus |>
  filter(empstat %in% c(1,2), age >= 10, sex == 2) |>
  group_by(statebr) |> 
  summarise(soma_eco_ativa = sum(wtper.x, na.rm = TRUE))

idade_ativa_uf <- censo_1960_ipmus |> 
  filter(age >= 10, sex == 2) |> 
  group_by(statebr) |> 
  summarise(soma_idade_ativa = sum(wtper.x, na.rm = TRUE))

mulher_ativa_por_uf_1960 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "statebr") |>
  mutate(lfp_uf_1960 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1960, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# final dt
mulher_ativa_por_uf_1960 <- mulher_ativa_por_uf_1960 |>
  rename(uf_nascim = statebr) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1960, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_10-mais_1960.xlsx")


#-----------------------------------------------------------------------------
censo_pess_cem_1960 <- read_dta("D:/IBGE_1960/Censo 1960 1.27pct CEM/novo/censo1960pes.dta")
censo_dom_cem_1960 <- read_dta("D:/IBGE_1960/Censo 1960 1.27pct CEM/novo/censo1960dom.dta")

# Listar os nomes das colunas de cada dataframe
colunas_dom <- colnames(censo_dom_cem_1960)
colunas_pess <- colnames(censo_pess_cem_1960)

# Encontrar variáveis iguais nas duas bases
variaveis_comuns <- intersect(colunas_dom, colunas_pess)

# Ver o resultado
variaveis_comuns

variaveis_comuns <- c("cem_iddomicilio", "cem_wgt", "v101", "v102", "v103", "v104", "v105", "v106", "v107", "v108", "v109", "v110", "v111", "v112", "v113", 
  "cem_num_pessoas", "cem_num_moradores", "cem_num_familias", "cem_diagnosis_dom", "cem_problematic_vars_list_dom", "cem_dissonant_uf", 
  "cem_dissonant_v116", "cem_dissonant_v118")

comparacao <- sapply(variaveis_comuns, function(var) {
  all(censo_dom_cem_1960[[var]] == censo_pess_cem_1960[[var]], na.rm = TRUE)
})
comparacao

censo_pess_cem_1960 <- censo_pess_cem_1960 |>
  filter(v204 != 0)

# uf codes
# Create a named vector to map old codes to current UF codes
uf_mapping <- c(
  "0"  = "11",  # Rondônia
  "1"  = "12",  # Acre
  "2"  = "13",  # Amazonas
  "3"  = "14",  # Roraima
  "4"  = "15",  # Pará
  "6"  = "16",  # Amapá
  "10" = "21",  # Maranhão
  "12" = "22",  # Piauí
  "14" = "23",  # Ceará
  "17" = "24",  # Rio Grande do Norte
  "19" = "25",  # Paraíba
  "21" = "26",  # Pernambuco
  "24" = "26",  # Fernando de Noronha (not an official state, leave as is or assign a custom code)
  "25" = "27",  # Alagoas
  "30" = "28",  # Sergipe
  "31" = "29",  # Bahia
  "40" = "31",  # Minas Gerais
  "50" = "31",  # Serra dos Aimorés (does not exist today, leave as is or assign a custom code)
  "51" = "32",  # Espírito Santo
  "52" = "33",  # Rio de Janeiro
  "54" = "33",  # Guanabara (merged with Rio de Janeiro in 1975, can assign "33")
  "60" = "35",  # São Paulo
  "71" = "41",  # Paraná
  "74" = "42",  # Santa Catarina
  "81" = "43",  # Rio Grande do Sul
  "91" = "51",  # Mato Grosso
  "94" = "52",  # Goiás
  "97" = "53"   # Distrito Federal
)
censo_pess_cem_1960$uf_atual <- uf_mapping[as.character(censo_pess_cem_1960$uf_pess)]

# missing values in v220 (non-economic activity)
censo_pess_cem_1960 <- censo_pess_cem_1960 %>%
  mutate(v220 = ifelse(is.na(v220), 999, v220))









# uf: 30-49
econ_ativa_uf <- censo_pess_cem_1960 |>
  filter(v220 == 3, v204b >= 30 & v204b <= 49, v202 %in% c(2,4,6)) |>
  group_by(uf_atual) |> 
  summarise(soma_eco_ativa = sum(cem_wgt, na.rm = TRUE))

idade_ativa_uf <- censo_pess_cem_1960 |> 
  filter(v204b >= 30 & v204b <= 49, v202 %in% c(2,4,6)) |> 
  group_by(uf_atual) |> 
  summarise(soma_idade_ativa = sum(cem_wgt, na.rm = TRUE))

mulher_ativa_por_uf_1960 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf_atual") |>
  mutate(lfp_uf_1960 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1960, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# final dt
mulher_ativa_por_uf_1960 <- mulher_ativa_por_uf_1960 |>
  rename(uf_nascim = uf_atual) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1960, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1960_cem.xlsx")

# uf_atual: 30-39
econ_ativa_uf <- censo_pess_cem_1960 |>
  filter(v220 == 3, v204b >= 30 & v204b <= 39, v202 %in% c(2,4,6)) |>
  group_by(uf_atual) |> 
  summarise(soma_eco_ativa = sum(cem_wgt, na.rm = TRUE))

idade_ativa_uf <- censo_pess_cem_1960 |> 
  filter(v204b >= 30 & v204b <= 39, v202 %in% c(2,4,6)) |> 
  group_by(uf_atual) |> 
  summarise(soma_idade_ativa = sum(cem_wgt, na.rm = TRUE))

mulher_ativa_por_uf_1960 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf_atual") |>
  mutate(lfp_uf_1960 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1960, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# final dt
mulher_ativa_por_uf_1960 <- mulher_ativa_por_uf_1960 |>
  rename(uf_nascim = uf_atual) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1960, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-39_1960_cem.xlsx")


# uf_atual: more than 10 years
econ_ativa_uf <- censo_pess_cem_1960 |>
  filter((v223 %in% c(2,3,4,5)| v220 == 3| v220 == 2| v220 == 8), v204b >= 10, v202 %in% c(2,4)) |>
  group_by(uf_atual) |> 
  summarise(soma_eco_ativa = sum(cem_wgt, na.rm = TRUE))

idade_ativa_uf <- censo_pess_cem_1960 |> 
  filter(v204b >= 10, v202 %in% c(2,4)) |> 
  group_by(uf_atual) |> 
  summarise(soma_idade_ativa = sum(cem_wgt, na.rm = TRUE))

mulher_ativa_por_uf_1960 <- econ_ativa_uf |>
  left_join(idade_ativa_uf, by = "uf_atual") |>
  mutate(lfp_uf_1960 = soma_eco_ativa / soma_idade_ativa)

somas_uf <- colSums(dplyr::select(mulher_ativa_por_uf_1960, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_eco_ativa"] / somas_uf["soma_idade_ativa"]
result

# final dt
mulher_ativa_por_uf_1960 <- mulher_ativa_por_uf_1960 |>
  rename(uf_nascim = uf_atual) |> 
  dplyr::select(-soma_eco_ativa, -soma_idade_ativa) 

write_xlsx(mulher_ativa_por_uf_1960, 
  "D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_10-mais_1960_cem.xlsx")















