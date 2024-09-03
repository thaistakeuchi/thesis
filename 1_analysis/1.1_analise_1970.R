# Thais Takeuchi
# 17/8/24
# Project: Análise 1970

#############################
# CENSO NAO COMPATIBILIZADO #
#############################

rm(list = setdiff(ls(), "cp2000"))


### Load Packages (and install packages if needed)
load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx", "fixest","car")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

#############################
# CENSO NAO COMPATIBILIZADO #
#############################

##############
##   1970   ##
##############
# cp1970_nc <- read_dta("E:/Thais/Tese_Mestrado/censo_1970/censo_pessoas_1970_filtrado_nao_comp.dta")
# 
# write_parquet(cp1970_nc, "E:/Thais/Tese_Mestrado/censo_1970/censo_pessoas_1970_filtrado_nao_comp.parquet")

cp1970_nc <- read_parquet("E:/Thais/Tese_Mestrado/censo_1970/censo_pessoas_1970_filtrado_nao_comp.parquet")

#######################
# ABRE BASES AUXILIARES
#######################
### Cultural proxies----

## Home country divorce rate
home_country_div_rate_1970 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate_1970.xlsx")

#-------------------------------------------------------------------------------
### Country of Origin Variables, measured in the year 1970 except where otherwise noted (paper)----

# ## GDP per capita
# gdp_pc <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/gdp_per_capita/gdp_pc.xlsx")
# 
# ## Average Female Age at First Marriage
# # age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/female_age_first_marriage.xlsx")
# age_ffm <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/wb_mean_age_first_marriage.xlsx")
# 
# ## Total fertility rate
# fertility_rate <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/fertility_rate/fertility_rate.xlsx")
# 
# ## Remarriage rate
# 
# # ## Proportion: weekly  church attendance and catholics
# # church_catholic_1995_1998 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/church/church_catholic_1995_1998.xlsx")
# # church_catholic_1999_2004 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/church/church_catholic_1999_2004.xlsx")
# # church_catholic_2005_2009 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/church/church_catholic_2005_2009.xlsx")
# 
# ##### ff2009 (idea)
# ## LFP 2000
# lfp <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/lfp/lfp_final.xlsx")

##############
# MANIPULAÇÕES
##############

# Mantendo cópia das bases
cp1970_nc_copy <- cp1970_nc

# Renomeando países de origem
cp1970_nc <- cp1970_nc %>% rename(pais_nascim = V512)
cp1970_nc_copy <- cp1970_nc_copy %>% rename(pais_nascim = V512)

setDT(cp1970_nc, cp1970_nc_copy)


cp1970_nc <- cp1970_nc_copy %>%
  mutate(pais_nascim = case_when(
    pais_nascim == 30 ~ "ARG",
    pais_nascim == 31 ~ "BOL",
    pais_nascim == 32 ~ "CAN",
    pais_nascim == 33 ~ "CHL",
    pais_nascim == 34 ~ "COL",
    pais_nascim == 35 ~ "CRI",
    pais_nascim == 36 ~ "CUB",
    pais_nascim == 37 ~ "ECU",
    pais_nascim == 38 ~ "USA",
    pais_nascim == 39 ~ "GTM",
    pais_nascim == 40 ~ "GUY",
    pais_nascim == 41 ~ "GUF",
    pais_nascim == 42 ~ "HTI",
    pais_nascim == 43 ~ "HND",
    pais_nascim == 44 ~ "BLZ",
    pais_nascim == 45 ~ "JAM",
    pais_nascim == 46 ~ "MEX",
    pais_nascim == 47 ~ "NIC",
    pais_nascim == 48 ~ "PAN",
    pais_nascim == 49 ~ "PRY",
    pais_nascim == 50 ~ "PER",
    pais_nascim == 51 ~ "DOM",
    pais_nascim == 52 ~ "SLV",
    pais_nascim == 53 ~ "SUR",
    pais_nascim == 54 ~ "URY",
    pais_nascim == 55 ~ "VEN",
    pais_nascim == 56 ~ "OTH_AM",
    pais_nascim == 58 | pais_nascim == 57 ~ "DEU",
    pais_nascim == 59 ~ "AUT",
    pais_nascim == 60 ~ "BEL",
    pais_nascim == 61 ~ "BGR",
    pais_nascim == 62 ~ "DNK",
    pais_nascim == 63 ~ "ESP",
    pais_nascim == 64 ~ "FIN",
    pais_nascim == 65 ~ "FRA",
    pais_nascim == 66 ~ "GBR",
    pais_nascim == 67 ~ "GRC",
    pais_nascim == 68 ~ "NLD",
    pais_nascim == 69 ~ "HUN",
    pais_nascim == 70 ~ "IRL",
    pais_nascim == 71 ~ "ITA",
    pais_nascim == 72 ~ "YUG",
    pais_nascim == 73 ~ "NOR",
    pais_nascim == 74 ~ "POL",
    pais_nascim == 75 ~ "PRT",
    pais_nascim == 76 ~ "ROU",
    pais_nascim == 77 ~ "SWE",
    pais_nascim == 78 ~ "CHE",
    pais_nascim == 79 ~ "CZE",
    pais_nascim == 80 ~ "RUS",
    pais_nascim == 81 ~ "OTH_EUR",
    pais_nascim == 82 ~ "AGO",
    pais_nascim == 83 ~ "EGY",
    pais_nascim == 84 ~ "MOZ",
    pais_nascim == 85 ~ "OTH_AFR",
    pais_nascim == 86 ~ "CHN",
    pais_nascim == 87 ~ "TWN",
    pais_nascim == 88 ~ "KOR",
    pais_nascim == 89 ~ "IND",
    pais_nascim == 90 ~ "ISR",
    pais_nascim == 91 ~ "JPN",
    pais_nascim == 92 ~ "LBN",
    pais_nascim == 93 ~ "PAK",
    pais_nascim == 94 ~ "SYR",
    pais_nascim == 95 ~ "TUR",
    pais_nascim == 96 ~ "OTH_ASIA",
    pais_nascim == 97 ~ "AUS",
    pais_nascim == 98 ~ "OTH_OCE",
    pais_nascim == 99 ~ "NO_SPEC",
    TRUE ~ as.character(pais_nascim)
  ))


cp1970_nc$ano <- 1970

# separados, divorciados e desquitados
cp1970_nc <- cp1970_nc %>%
  mutate(
    ano_nasc = ano - V606,
    imm = ifelse(V516 == 7, 1, 0),
    imm_div = ifelse(imm == 1
                     & V526 %in% c(6,7,8), 1, 0),
  )

# so divorciados
# cp1970_nc <- cp1970_nc %>%
#   mutate(
#     ano_nasc = ano - V606,
#     imm = ifelse(V516 == 7, 1, 0),
#     imm_div = ifelse(imm == 1
#                      & V526 %in% c(8), 1, 0),
#   )

# se imm_div e NA entao coloca zero
cp1970_nc$imm_div[is.na(cp1970_nc$imm_div)] <- 0


cp1970_nc <- cp1970_nc %>%
  mutate(
    imm_homem = ifelse(V501 == 1
                       & imm == 1, 1, 0),
    imm_alf = ifelse(imm == 1 & (V519 == 2), 1, 0), 
    imm_fund_completo = ifelse(imm == 1
                               & (V524 == 1 | V524 == 6), 1, 0), 
    imm_em_completo = ifelse(imm == 1
                             & (V524 == 4 | V524 == 7 | V524 == 5), 1, 0), 
    imm_superior_completo = ifelse(imm == 1
                                   & (V524 == 8), 1, 0), 
    imm_mulher1filho = ifelse(imm == 1
                              & V501 == 3
                              & (V554 >= 1), 1, 0)
  )

# Juntando as bases

# cp1970_nc com home country divorce rate
cp1970_nc_1 <- merge(cp1970_nc, home_country_div_rate_1970, by = "pais_nascim", all.x = TRUE)
#----
# # com gdp_pc: considerar ano 2000
# # gdp_pc_2000 <- gdp_pc %>% filter(ano_gdp_pc == 2000)
# 
# cp1970_nc_2 <- merge(cp1970_nc_1, gdp_pc_2000, by = "pais_nascim", all.x = TRUE)
# 
# # com age_f_first_marriage
# cp1970_nc_3 <- merge(cp1970_nc_2, age_ffm, by = "pais_nascim", all.x = TRUE)
# 
# # com fertility_rate
# cp1970_nc_4 <- merge(cp1970_nc_3, fertility_rate, by = "pais_nascim", all.x = TRUE)
# 
# # com fem lfp
# cp1970_nc_4 <- merge(cp1970_nc_3, lfp, by = "pais_nascim", all.x = TRUE)

# # com church variables
# ## 1995_1998
# cp1970_nc_5 <- merge(cp1970_nc_4, church_catholic_1995_1998, by = "pais_nascim", all.x = TRUE)
# ## 1999_2004
# cp1970_nc_6 <- merge(cp1970_nc_5, church_catholic_1999_2004, by = "pais_nascim", all.x = TRUE)
# ## 2005_2009
# cp1970_nc_7 <- merge(cp1970_nc_6, church_catholic_2005_2009, by = "pais_nascim", all.x = TRUE)
# 
# # com divorce never justifiable
# ## 1995_1998
# cp1970_nc_5 <- merge(cp1970_nc_4, church_catholic_1995_1998, by = "pais_nascim", all.x = TRUE)
# ## 1999_2004
# cp1970_nc_6 <- merge(cp1970_nc_5, church_catholic_1999_2004, by = "pais_nascim", all.x = TRUE)
# ## 2005_2009
# cp1970_nc_7 <- merge(cp1970_nc_6, church_catholic_2005_2009, by = "pais_nascim", all.x = TRUE)

# Tirando outros paises (...-others e paises com NA)----
cp1970_nc_1 <- cp1970_nc_1 %>% 
  filter(!`pais_nascim` %in% c("OTH_AFR", "OTH_AM", "OTH_ASIA","OTH_EUR", "OTH_OCE"))

cp1970_nc_1 <- cp1970_nc_1 %>% rename(escolaridade = V524)
cp1970_nc_1$fund_completo <- ifelse(cp1970_nc_1$escolaridade == 1 | cp1970_nc_1$escolaridade == 6, 1, 0)
cp1970_nc_1$med_completo <- ifelse(cp1970_nc_1$escolaridade == 4 | cp1970_nc_1$escolaridade == 7 | cp1970_nc_1$escolaridade == 5, 1, 0)
cp1970_nc_1$sup_completo <- ifelse(cp1970_nc_1$escolaridade == 8, 1, 0)

# transforma escolaridade em continua
cp1970_nc_1$escolaridade_cont <- as.numeric(cp1970_nc_1$escolaridade)

# idade ao quadrado
cp1970_nc_1 <- cp1970_nc_1 %>%
  mutate(
    age_fem = if_else(V501 == 3, V606, NA_real_),
  ) %>%
  mutate(
    age_squared = V606^2
  )

# Remove valores NA na coluna de pais_nascim e hcdr
cp1970_nc_1 <- cp1970_nc_1[!is.na(cp1970_nc_1$pais_nascim), ]
cp1970_nc_1 <- cp1970_nc_1[!is.na(cp1970_nc_1$hcdr), ]

base_partial_nc_1970 <- as.data.frame(cp1970_nc_1)
write_xlsx(base_partial_nc_1970, "E:/Thais/Tese_Mestrado/tese_bases/base_gerada/base_nc_1970.xlsx")

setDT(base_partial_nc_1970)

## Rodando a regressao (1) - table 2
# FE: idade
model1_nc <- feols(imm_div ~ hcdr + imm_homem + fund_completo + med_completo + sup_completo | V606 + age_squared, data = base_partial_nc_1970, weights = ~V604)
summary(model1_nc)

# FE: idade com escolaridade continua
model1_1_nc <- feols(imm_div ~ hcdr + imm_homem + escolaridade_cont | V606 + age_squared, data = base_partial_nc_1970, weights = ~V604)
summary(model1_1_nc)

# FE: idade e RM (sem rm - gerar)
model2_nc <- feols(imm_div ~ hcdr + imm_homem + escolaridade_cont | V606+age_squared+V7003, data = base_partial_nc_1970, weights = ~V604)
summary(model2_nc)

# Drop Portugal (3)
base_partial_nc_sem_portugal <- base_partial_nc_1970 %>%
  filter(pais_nascim != "PRT")

model2_nc_sem_portugal <- feols(imm_div ~ hcdr + imm_homem + escolaridade_cont | V606+age_squared, data = base_partial_nc_sem_portugal, weights = ~V604)
summary(model2_nc_sem_portugal)

# Drop Russia and Italy (4)
# Ensure dplyr functions are used explicitly
max_hcdr_info <- base_partial_nc_1970 %>%
  dplyr::filter(hcdr == max(hcdr, na.rm = TRUE)) %>%
  dplyr::select(pais_nascim, hcdr)

min_hcdr_info <- base_partial_nc_1970 %>%
  dplyr::filter(hcdr == min(hcdr, na.rm = TRUE)) %>%
  dplyr::select(pais_nascim, hcdr)

base_partial_nc_sem_russia_italy <- base_partial_nc_1970 %>%
  filter(pais_nascim != "ITA")

model2_nc_sem_russia_italy <- feols(imm_div ~ hcdr + imm_homem + escolaridade_cont | V606+age_squared, data = base_partial_nc_sem_russia_italy, weights = ~V604)
summary(model2_nc_sem_russia_italy)
