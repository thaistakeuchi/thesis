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
# 2010
# censo_2010 <- read_dta("D:/1_migration/1_datasets_migrants/censo/2010/censo_2010_tratada_2025_03_02.dta")
censo_2010 <- read_dta("D:/1_migration/1_datasets_migrants/censo/2010/censo_2010_migrante_tratada_2025_03_02.dta")

setDT(censo_2010)

# hh variables

# df_2010 <- censo_2010 %>%
#   group_by(id_dom) %>%
#   mutate(count_responsavel = max(responsavel),
#     count_conjuge = max(conjuge),
#     main_sample = max(migrante_casada == 1)) %>%
#   ungroup() %>%
#   filter(count_responsavel == 1 & count_conjuge == 1, main_sample == 1)


df <- censo_2010 %>%
  group_by(id_dom) %>%
  mutate(count_responsavel = max(responsavel),
    count_conjuge = max(conjuge),
    main_sample = max(migrante_casada_15 == 1)) %>%
  ungroup() %>%
  filter(count_responsavel == 1 & count_conjuge == 1, main_sample == 1)

# filter rows where responsavel, conjuge or casado == 1
df <- df %>%
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
df$max_age_sp <- ave(df$age_sp, df$id_dom, FUN = max_with_na)
df$max_age_squared_sp <- ave(df$age_squared_sp, df$id_dom, FUN = max_with_na)
df$max_menos_fund_homem <- ave(df$menos_fund_homem, df$id_dom, FUN = max_with_na)
df$max_fund_homem <- ave(df$fund_homem, df$id_dom, FUN = max_with_na)
df$max_em_homem <- ave(df$em_homem, df$id_dom, FUN = max_with_na)
df$max_superior_homem <- ave(df$superior_homem, df$id_dom, FUN = max_with_na)
df$max_educ_homem <- ave(df$educ_homem, df$id_dom, FUN = max_with_na)
df$max_income_sp_raw <- ave(df$income_sp_raw, df$id_dom, FUN = max_with_na)

df$age_sp <- df$max_age_sp
df$age_squared_sp <- df$max_age_squared_sp
df$menos_fund_homem <- df$max_menos_fund_homem
df$fund_homem <- df$max_fund_homem
df$em_homem <- df$max_em_homem
df$superior_homem <- df$max_superior_homem
df$educ_homem <- df$max_educ_homem
df$income_sp_raw <- df$max_income_sp_raw

# Lista de colunas a serem movidas
cols_to_move <- c("codmun", "nome", "merge", "uf_nascim", "migrante", "n_domicilio", 
  "responsavel", "conjuge", "has_conjuge", "has_responsavel", "casal", 
  "casado", "migrante_casada", "anos_mor_uf", "migrante_casada_15", 
  "migrante_casada_10", "migrante_casada_5", "menos_fund_mulher", 
  "menos_fund_homem", "fund_mulher", "fund_homem", "em_mulher", 
  "em_homem", "superior_mulher", "superior_homem", "children", 
  "children_under_5", "ocupado", "ocup_migrante", "horas_migrante", 
  "main_sample", "age_fem", "age_sp", "age_squared_fem", "age_squared_sp", 
  "income_sp_raw", "educ_mulher", "educ_homem")

all_cols <- colnames(df)
pos_raca <- which(all_cols == "raca")
new_order <- c(
  all_cols[1:pos_raca],  # Mantém tudo até "raca"
  cols_to_move,  # Insere as colunas desejadas logo após "raca"
  setdiff(all_cols, c(all_cols[1:pos_raca], cols_to_move)) # Adiciona o restante sem duplicar
)
setcolorder(df, new_order)

df <- df[, !grepl("^max", names(df)) & !names(df) %in% c("has_conjuge", "has_responsavel", "merge"), with = FALSE]

# deixa somente linhas onde migrante_casada_15 == 1
data_2010 <- df[df$migrante_casada_15 == 1 & !is.na(df$age_fem), ]
setDT(data_2010)
data_2010 <- data_2010[data_2010$age_sp >= 18, ]
data_2010 <- data_2010[data_2010$age_fem %in% c(30:49), ]
data_2010[] <- lapply(data_2010, function(x) {
  if (is.character(x)) iconv(x, from = "latin1", to = "UTF-8") else x
})
data_2010$ocupada_migrante_15 <- data_2010$ocupado
data_2010 <- data_2010[, !names(data_2010) %in% c("ocupado"), with = FALSE]


# write.xlsx(data_2010, "D:/1_migration/1_datasets_migrants/bases/census_2010_proxies_1991_30_49_v3.xlsx")

arrow::write_parquet(data_2010, "D:/1_migration/1_datasets_migrants/bases/census_2010_proxies_1991_30_49_v3.parquet")

rm(df, censo_2010)

setwd("D:/2_scripts_migration")
save.image("2010_base.RData")
