# Script calculo da lfp usando a PNAD
# 13/10/2024

#######################
# PREPARE ENVIRONMENT
#######################

rm(list = ls())

load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx", "fixest", "readxl")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


# 1992

pnad_1992_pessoa <- read_parquet("D:/pnad_antiga/dados/1992/pessoas_1992.parquet")

pnad <- pnad_1992_pessoa

rm(pnad_1992_pessoa)

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

