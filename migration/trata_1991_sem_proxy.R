# Trata base 2010
# Thais Takeuchi
# 31/10/2024


#######################
# PREPARE ENVIRONMENT
#######################

rm(list = ls())

required_packages <- c("data.table", "haven", "tidyverse", "dplyr", "arrow", "openxlsx")

install_packages <- required_packages[!required_packages %in% installed.packages()]
if (length(install_packages) > 0) install.packages(install_packages, dependencies = TRUE)

sapply(required_packages, require, character.only = TRUE)

#########
# CENSUS
#########
# 1991
censo_1991 <- read_dta("D:/1_migration/1_datasets_migrants/censo/1991/censo_1991_migrante_tratada_final_2025_03_05.dta")
setDT(censo_1991) # Converter para data.table

# # Identificar as colunas a partir de "uf_nascim"
# start_col <- which(names(censo_1991) == "uf_nascim")
# cols_to_keep <- names(censo_1991)[start_col:length(names(censo_1991))]
# cols_to_keep <- c(cols_to_keep, "V7301", "V1102", "V0310", "V0309", "V1061", "IDQues", "V0098", "V0303")
# cols_to_keep <- setdiff(cols_to_keep, c("has_conjuge", "has_responsavel"))
# censo <- censo_1991[, ..cols_to_keep]

# hh variables

# df_2010 <- censo_1991 %>%
#   group_by(id_dom) %>%
#   mutate(count_responsavel = max(responsavel),
#     count_conjuge = max(conjuge),
#     main_sample = max(migrante_casada == 1)) %>%
#   ungroup() %>%
#   filter(count_responsavel == 1 & count_conjuge == 1, main_sample == 1)


df <- censo_1991 %>%
  group_by(IDQues) %>%
  mutate(count_responsavel = max(responsavel),
    count_conjuge = max(conjuge),
    main_sample = max(migrante_casada_15 == 1)) %>%
  ungroup() %>%
  filter(count_responsavel == 1 & count_conjuge == 1, main_sample == 1)

# filter rows where responsavel, conjuge or casado == 1
df <- df %>%
  group_by(IDQues) %>%
  filter(responsavel == 1 | conjuge == 1 | casado == 1) %>%
  ungroup()

# Converter para data.table
setDT(df)

df[, rend_nom_total := fifelse(V3561 == 99999999, NA_real_, V3561)]
df[, income_sp_raw := fifelse(sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1), rend_nom_total, NA_real_)]

# Criar uma chave para cada domicílio
df[, key_id := .GRP, by = IDQues]

# Criar uma cópia para identificar casais dentro do domicílio
df_migrantes <- df[migrante_casada_15 == 1, .(IDQues, key_id, age_fem = idade)] # Seleciona migrantes casadas e sua idade

# Criar uma cópia dos potenciais maridos (homens responsáveis, cônjuges ou casados)
df_homens <- df[sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1), 
  .(IDQues, key_id, age_sp, menos_fund_homem, fund_homem, em_homem, superior_homem, educ_homem, income_sp_raw)]

# Juntar bases pelo IDQues para casar casais no mesmo domicílio
df_casais <- merge(df_migrantes, df_homens, by = "IDQues", allow.cartesian = TRUE)

# Criar variável com a diferença absoluta de idade entre a mulher e o potencial esposo
df_casais[, age_diff := abs(age_fem - age_sp)]

# Remover duplicação da chave key_id
df_casais <- df_casais[, .(IDQues, key_id = key_id.x, age_fem, age_sp, 
  menos_fund_homem, fund_homem, em_homem, 
  superior_homem, educ_homem, income_sp_raw, age_diff)]

# Selecionar o esposo com a menor diferença de idade para cada migrante
df_casais <- df_casais[order(IDQues, age_diff)] # Ordena por domicílio e menor diferença de idade
df_casais <- df_casais[, .SD[1], by = .(IDQues, key_id)] # Mantém apenas a melhor correspondência

# Atribuir os valores do esposo correto às migrantes casadas na base original
df <- merge(df, df_casais[, .(IDQues, key_id, age_sp, menos_fund_homem, fund_homem, 
  em_homem, superior_homem, educ_homem, income_sp_raw)], 
  by = c("IDQues", "key_id"), all.x = TRUE, suffixes = c("", "_esposo"))

# Substituir valores das migrantes casadas pelos do esposo correto
df[migrante_casada_15 == 1, `:=` (
  age_sp = age_sp_esposo,
  menos_fund_homem = menos_fund_homem_esposo,
  fund_homem = fund_homem_esposo,
  em_homem = em_homem_esposo,
  superior_homem = superior_homem_esposo,
  educ_homem = educ_homem_esposo,
  income_sp_raw = income_sp_raw_esposo
)]

# Remover colunas temporárias
df[, c("age_sp_esposo", "menos_fund_homem_esposo", "fund_homem_esposo", 
  "em_homem_esposo", "superior_homem_esposo", "educ_homem_esposo", 
  "income_sp_raw_esposo", "key_id") := NULL]

# deixa somente linhas onde migrante_casada_15 == 1
df_1991 <- df[df$migrante_casada_15 == 1 & !is.na(df$age_fem), ]
df_1991[, age_squared_sp := age_sp^2]
df_1991 <- df_1991[df_1991$age_sp >= 18, ]
df_1991 <- df_1991[df_1991$age_fem %in% c(30:49), ]
# df_1991[] <- lapply(df_1991, function(x) {
#   if (is.character(x)) iconv(x, from = "latin1", to = "UTF-8") else x
# })
df_1991$ocupada_migrante_15 <- df_1991$ocupado
df_1991 <- df_1991[, !names(df_1991) %in% c("ocupado"), with = FALSE]
df_1991[children == 99, children := NA]

rm(censo, censo_1991, df, df_casais, df_homens, df_migrantes)
gc()

# write.xlsx(data_2010, "D:/1_migration/1_datasets_migrants/bases/census_2010_proxies_1991_30_49_v3.xlsx")

arrow::write_parquet(df_1991, "D:/1_migration/1_datasets_migrants/bases/census_1991_30_49.parquet")

