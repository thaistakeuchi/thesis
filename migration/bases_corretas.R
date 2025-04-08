# Thais Takeuchi
# Manipula censos: 1991, 2000 e 2010
# 25/02/2025

rm(list = ls())

load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx", "fixest", "readxl", "xtable", "kableExtra", "margins","marginaleffects", "Hmisc", "here")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

################################################################################
# 1991----
################################################################################

data_1991_raw <- read_parquet("D:/1_migration/1_datasets_migrants/bases/census_1991_30_49.parquet") # só tem migrantes
data_1991_raw <- data_1991_raw |> 
  mutate(uf_nascim = if_else(uf_nascim == 34, 35, uf_nascim))

#data_1991_raw <- data_1991_raw[data_1991_raw$income_sp_raw != 9999999, ]

# deflaciona para 2010
library(deflateBR)

# converter moedas
data_1991_raw$real_1994 <- data_1991_raw$income_sp_raw / 2750

# Deflacionar a renda de 1994 para valores reais de 2010 usando IPCA
data_1991_raw$def_1991_2010 <- deflate(
  nominal_values = data_1991_raw$real_1994,
  nominal_dates = as.Date("1994-07-01"),
  real_date = format("07/2010"),
  index = "ipca"
)

## LFP----
lfp_1970_30_49_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1970.xlsx")

# Criar as novas linhas para MS (50) e TO (17) com os valores de MT (51) e GO (52)
novos_estados <- lfp_1970_30_49_uf %>%
  filter(uf_nascim %in% c(51, 52)) %>% # Seleciona MT e GO
  mutate(uf_nascim = ifelse(uf_nascim == 51, 50, 17)) # Substitui 51 → 50 e 52 → 17

# Adicionar os novos estados ao dataframe original
lfp_1970_30_49_uf <- bind_rows(lfp_1970_30_49_uf, novos_estados)

lfp_1970_30_49_uf <- lfp_1970_30_49_uf |> 
  rename(lfp_uf_30_49_1970 = lfp_uf_1970)

## TFR----
tfr_1940_2010_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/fecundidade_brasil/tfr_1940_2010.xlsx")
tfr_1970_uf <- tfr_1940_2010_uf |>
  dplyr::select(uf_nascim, tfr_1970)

## Junta bases----
data_1991 <- merge(data_1991_raw, lfp_1970_30_49_uf, by = "uf_nascim")
data_1991 <- merge(data_1991, tfr_1970_uf, by = "uf_nascim")

# Salva base final
# write_parquet(data_1991, "D:/1_migration/1_datasets_migrants/bases/census_1991_proxies_1970_30_49_correta.parquet")
write_parquet(data_1991, "D:/1_migration/1_datasets_migrants/bases/census_1991_proxies_1970_30_49_20250308.parquet")


################################################################################
# 2000----
################################################################################
data_2000_1 <- read_dta("D:/1_migration/1_datasets_migrants/censo/2000/censo_2000_migracao_tratada_migrante_15_final.dta")

data_2000 <- data_2000_1 |> 
  filter(age_fem %in% c(30:49)) |> 
  mutate(uf_nascim = ifelse(uf_nascim == 34, 35, uf_nascim))

# Deflacionar a renda de 2000 para valores reais de 2010 usando IPCA
data_2000$def_2000_2010 <- deflate(
  nominal_values = data_2000$income_sp_raw,
  nominal_dates = as.Date("2000-07-01"),
  real_date = format("07/2010"),
  index = "ipca"
)

## LFP----
lfp_1980_30_49_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1980.xlsx")
names(lfp_1980_30_49_uf)[names(lfp_1980_30_49_uf) == "lfp_uf_1980"] <- "lfp_uf_30_49_1980"

# Criar as novas linhas para MS (50) e TO (17) com os valores de MT (51) e GO (52)
novos_estados <- lfp_1980_30_49_uf %>%
  filter(uf_nascim %in% c(51, 52)) %>% # Seleciona MT e GO
  mutate(uf_nascim = ifelse(uf_nascim == 51, 50, 17)) # Substitui 51 → 50 e 52 → 17

# Adicionar os novos estados ao dataframe original
lfp_1980_30_49_uf <- bind_rows(lfp_1980_30_49_uf, novos_estados)

## TFR----
tfr_1980_uf <- tfr_1940_2010_uf |>
  dplyr::select(uf_nascim, tfr_1980)

## Junta bases----
data_2000 <- merge(data_2000, lfp_1980_30_49_uf, by = "uf_nascim")
data_2000 <- merge(data_2000, tfr_1980_uf, by = "uf_nascim")

# Salva base final
# write_parquet(data_2000, "D:/1_migration/1_datasets_migrants/bases/census_2000_proxies_1980_30_49_correta.parquet")
write_parquet(data_2000, "D:/1_migration/1_datasets_migrants/bases/census_2000_proxies_1980_30_49_20250308.parquet")

################################################################################
# 2010----
################################################################################
data_2010<- read_parquet("D:/1_migration/1_datasets_migrants/bases/census_2010_proxies_1991_30_49_v3.parquet")
data_2010$log_income_sp_raw <- ifelse(data_2010$income_sp_raw > 0, log(data_2010$income_sp_raw), NA)

# write_parquet(data_2010, "D:/1_migration/1_datasets_migrants/bases/census_2010_proxies_1991_30_49_v2.parquet")

## LFP----
lfp_1991_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_1991.xlsx")
#lfp_1991_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1991_v2.xlsx")

lfp_1991_30_49_uf <- lfp_1991_uf |> 
  mutate(lfp_uf_30_49_1991 = lfp_uf_1991) |> 
  dplyr::select(-lfp_uf_1991)

## TFR----
tfr_1990_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/fecundidade_brasil/tfr_1991.xlsx")
tfr_1990_uf$tfr_1991 <- as.numeric(gsub(",", ".", tfr_1990_uf$tfr_1991))

## Junta bases----
data_2010 <- merge(data_2010, lfp_1991_30_49_uf, by = "uf_nascim")
data_2010 <- merge(data_2010, tfr_1990_uf, by = "uf_nascim")

# Salva base final
write_parquet(data_2010, "D:/1_migration/1_datasets_migrants/bases/census_2010_proxies_1991_30_49_20250303.parquet")

