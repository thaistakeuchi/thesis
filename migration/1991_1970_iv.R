# Tentativas de IV para o censo de 1991 com proxies de 1970
# Thais Takeuchi
# 23/10/2024

#######################
# PREPARE ENVIRONMENT
#######################

rm(list = ls())

load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx", "fixest", "readxl", "xtable")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

# Data
data_1991 <- read_parquet("D:/1_migration/1_datasets_migrants/censo/1991/censo_1991_migracao_tratada_migrante_15_final.parquet")

lfp_1970_30_49_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-49_1970.xlsx")
lfp_1970_30_49_uf <- lfp_1970_30_49_uf |> 
  rename(lfp_uf_30_49_1970 = lfp_uf_1970)

lfp_1970_30_39_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_30-39_1970.xlsx")
lfp_1970_30_39_uf <- lfp_1970_30_39_uf |> 
  rename(lfp_uf_30_39_1970 = lfp_uf_1970)

lfp_1970_mais_10_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/lfp/censo_lfp_uf_10-mais_1970.xlsx")
lfp_1970_mais_10_uf <- lfp_1970_mais_10_uf |> 
  rename(lfp_uf_mais_10_1970 = lfp_uf_1970)

tfr_1940_1980_uf <- read_xlsx("D:/1_migration/1_datasets_migrants/fecundidade_brasil/tfr_1940_1980.xlsx")
tfr_1970_uf <- tfr_1940_1980_uf |>
  dplyr::select(uf_nascim, tfr_1970)

urban_1970 <- read_xlsx("D:/1_migration/1_datasets_migrants/infra/urban_1970.xlsx")


data_1991 <- merge(data_1991, lfp_1970_30_49_uf, by = "uf_nascim")
data_1991 <- merge(data_1991, lfp_1970_30_39_uf, by = "uf_nascim")
data_1991 <- merge(data_1991, lfp_1970_mais_10_uf, by = "uf_nascim")

data_1991 <- merge(data_1991, tfr_1970_uf, by = "uf_nascim")

data_1991 <- merge(data_1991, urban_1970, by = "uf_nascim")


data_30_49_1991 <- data_1991 |> 
  filter(age_fem %in% c(30:49))

data_30_39_1991 <- data_1991 |> 
  filter(age_fem %in% c(30:39))

# 1 and second stage

first_stage <- feols(lfp_uf_30_49_1970 ~ tfr_1970 + tx_urban_1970, 
  data = data_30_49_1991, weights = ~V7301)
data_30_49_1991$lfp_pred <- fitted(first_stage)  # Valores previstos da primeira etapa

iv_probit_step2 <- feglm(ocupada_migrante_15 ~ lfp_pred +  fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + age_sp | V1102, 
  data = data_30_49_1991, 
  weights = ~V7301, 
  cluster = ~uf_nascim,
  family = binomial(link = "probit"))
summary(iv_probit_step2)

## robustez

# F
summary_first_stage <- summary(first_stage)

coef_instrument <- coef(summary_first_stage)["tfr_1970"]
se_instrument <- summary_first_stage$se["tfr_1970"]

# Visualizar o coeficiente e erro-padrão
coef_instrument
se_instrument

F_stat <- (coef_instrument / se_instrument)^2
F_stat

# Hansen
data_clean <- na.omit(data_30_49_1991[, c("ocupada_migrante_15", "lfp_uf_30_49_1970", 
  "tfr_1970", "tx_urban_1970", 
  "fund_homem", "em_homem", "superior_homem", 
  "income_sp_raw", "age_fem", "age_squared_fem", "age_sp")])
nrow(data_clean)
data_clean$ocupada_migrante_15 <- as.numeric(data_clean$ocupada_migrante_15)

# Verifique se as variáveis explicativas estão no formato correto
data_clean$fund_homem <- as.numeric(data_clean$fund_homem)
data_clean$em_homem <- as.numeric(data_clean$em_homem)
data_clean$superior_homem <- as.numeric(data_clean$superior_homem)
data_clean$income_sp_raw <- as.numeric(data_clean$income_sp_raw)
data_clean$age_fem <- as.numeric(data_clean$age_fem)
data_clean$age_squared_fem <- as.numeric(data_clean$age_squared_fem)
data_clean$age_sp <- as.numeric(data_clean$age_sp)
data_clean$tfr_1970 <- as.numeric(data_clean$tfr_1970)
data_clean$tx_urban_1970 <- as.numeric(data_clean$tx_urban_1970)


iv_model <- ivreg(ocupada_migrante_15 ~ fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + age_sp | lfp_uf_30_49_1970 ~ tfr_1970 + tx_urban_1970, 
  data = data_clean)

# Resumo do modelo com o teste de Hansen (J-test)
summary(iv_model, diagnostics = TRUE)

# 

# link: https://chatgpt.com/share/671925a8-e5f0-8009-8ca0-ec9f60f052d2


#########
# RANDOM
#########

first_stage <- feols(lfp_uf_30_49_1970 ~ tfr_1970 + fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + age_sp,
  data = data_30_49_1991)
summary_first_stage <- summary(first_stage)

coef_instrument <- coef(summary_first_stage)["tfr_1970"]
se_instrument <- summary_first_stage$se["tfr_1970"]

# Visualizar o coeficiente e erro-padrão
coef_instrument
se_instrument

F_stat <- (coef_instrument / se_instrument)^2
F_stat

data_30_49_1991_clean <- na.omit(data_30_49_1991)

work_iv <- feols(
  ocupada_migrante_15 ~ fund_homem + em_homem + superior_homem + age_fem + age_squared_fem | 
    V1102,
  data = data_30_49_1991,
  weights = ~V7301,
  cluster = ~uf_nascim,
  iv = ~ lfp_uf_30_49_1970 ~ tfr_1970
)
summary(work_iv)



work_iv <- feols(
  ocupada_migrante_15 ~ fund_homem + em_homem + superior_homem +
    income_sp_raw + age_fem + age_squared_fem + age_sp | V1102,
  data = data_30_49_1991,
  weights = ~V7301,
  cluster = ~uf_nascim,
  iv = ~ lfp_uf_30_49_1970 ~ tfr_1970+tx_urban_1970
)

summary(work_iv)
summary(work_iv, stage = 2)
summary(work_iv, stage = 1)
etable(work_iv, stage = 1:2, tex = TRUE)


work_iv_alt <- ivreg(
  ocupada_migrante_15 ~ fund_homem + em_homem + superior_homem +
    income_sp_raw + age_fem + age_sp + lfp_uf_30_49_1970 |
    fund_homem + em_homem + superior_homem + income_sp_raw + age_fem +
    age_squared_fem + age_sp + tfr_1970,
  data = data_30_49_1991,
  weights = V7301
)

summary(work_iv_alt, diagnostics = TRUE)