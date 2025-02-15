# Trata base 2010
# Thais Takeuchi
# 31/10/2024


#######################
# PREPARE ENVIRONMENT
#######################

rm(list = ls())

load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx", "fixest", "readxl", "xtable")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

#########
# CENSUS
#########
# 2010
censo_2010 <- arrow::read_parquet("D:/1_migration/1_datasets_migrants/censo/2010/censo_2010_migracao_tratada_2024_10_31_v1.parquet")
arrow::write_parquet(censo_2010, "D:/1_migration/1_datasets_migrants/censo/2010/censo_2010_migracao_tratada_2024_10_31_v1.parquet")

setDT(censo_2010)

# hh variables

df_2010 <- censo_2010 %>%
  group_by(id_dom) %>%
  mutate(count_responsavel = max(responsavel),
    count_conjuge = max(conjuge),
    main_sample = max(migrante_casada == 1)) %>%
  ungroup() %>%
  filter(count_responsavel == 1 & count_conjuge == 1, main_sample == 1)

df_2010_15 <- censo_2010 %>%
  group_by(id_dom) %>%
  mutate(count_responsavel = max(responsavel),
    count_conjuge = max(conjuge),
    main_sample = max(migrante_casada_15 == 1)) %>%
  ungroup() %>%
  filter(count_responsavel == 1 & count_conjuge == 1, main_sample == 1)

# filter rows where responsavel, conjuge or casado == 1
df_2010_15 <- df_2010_15 %>%
  group_by(id_dom) %>%
  filter(responsavel == 1 | conjuge == 1 | casado == 1) %>%
  ungroup()

# filter to select hh with migrant_15
max_with_na <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(max(x, na.rm = TRUE))
  }
}

# max values in each hh
df_2010_15$max_age_sp <- ave(df_2010_15$age_sp, df_2010_15$id_dom, FUN = max_with_na)
df_2010_15$max_menos_fund_homem <- ave(df_2010_15$menos_fund_homem, df_2010_15$id_dom, FUN = max_with_na)
df_2010_15$max_fund_homem <- ave(df_2010_15$fund_homem, df_2010_15$id_dom, FUN = max_with_na)
df_2010_15$max_em_homem <- ave(df_2010_15$em_homem, df_2010_15$id_dom, FUN = max_with_na)
df_2010_15$max_superior_homem <- ave(df_2010_15$superior_homem, df_2010_15$id_dom, FUN = max_with_na)
df_2010_15$max_educ_homem <- ave(df_2010_15$educ_homem, df_2010_15$id_dom, FUN = max_with_na)
df_2010_15$max_income_sp_raw <- ave(df_2010_15$income_sp_raw, df_2010_15$id_dom, FUN = max_with_na)

df_2010_15$age_sp[df_2010_15$migrante_casada_15 == 1] <- df_2010_15$max_age_sp[df_2010_15$migrante_casada_15 == 1]
df_2010_15$menos_fund_homem[df_2010_15$migrante_casada_15 == 1] <- df_2010_15$max_menos_fund_homem[df_2010_15$migrante_casada_15 == 1]
df_2010_15$fund_homem[df_2010_15$migrante_casada_15 == 1] <- df_2010_15$max_fund_homem[df_2010_15$migrante_casada_15 == 1]
df_2010_15$em_homem[df_2010_15$migrante_casada_15 == 1] <- df_2010_15$max_em_homem[df_2010_15$migrante_casada_15 == 1]
df_2010_15$superior_homem[df_2010_15$migrante_casada_15 == 1] <- df_2010_15$max_superior_homem[df_2010_15$migrante_casada_15 == 1]
df_2010_15$educ_homem[df_2010_15$migrante_casada_15 == 1] <- df_2010_15$max_educ_homem[df_2010_15$migrante_casada_15 == 1]
df_2010_15$income_sp_raw[df_2010_15$migrante_casada_15 == 1] <- df_2010_15$max_income_sp_raw[df_2010_15$migrante_casada_15 == 1]

df_2010_15 <- df_2010_15[, !names(df_2010_15) %in% c("max_age_sp","max_menos_fund_homem", "max_fund_homem", "max_em_homem", "max_superior_homem", "max_educ_homem", "max_income_sp_raw")]

# deixa somente linhas onde migrante_casada_15 == 1
data_2010 <- df_2010_15[df_2010_15$migrante_casada_15 == 1, ]
data_2010 <- data_2010[data_2010$age_fem %in% c(30:49), ]
data_2010 <- data_2010  |> 
  mutate(across(where(is.character), ~ iconv(., from = "latin1", to = "UTF-8")))

write.xlsx(data_2010, "D:/1_migration/1_datasets_migrants/bases/census_2010_proxies_1991_30_49_v2.xlsx")

setwd("D:/2_scripts_migration")
save.image("2010_base.RData")
