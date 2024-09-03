### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


#############
# Open bases
#############

#######
# CENSO
#######

### 2000

# # Censo 2000 (extraído da basedosdados)
# one_drive_path <- "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases"
# file_path <- file.path(one_drive_path, "censo_pessoa_2000.parquet")
# censo_pessoa_2000 <- read_parquet(file_path)
# gc()
# 
# # Censo 2000 (extraído do DataZoom, com variáveis escolhidas)
# censo_pessoa_2000 <- read_dta("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/censo/microdados_2000/2000_censo_pessoas/censo_pessoas_2000.dta")
# gc()
# 
# # Escrevendo em parquet
# write_parquet(censo_pessoa_2000, "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/censo/microdados_2000/2000_censo_pessoas/censo_pessoas_2000.parquet")
# 
# # Puxando em parquet
# censo_pessoa_2000 <- read_parquet("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/censo/microdados_2000/2000_censo_pessoas/censo_pessoas_2000.parquet")
# 
# cp2000 <- censo_pessoa_2000
# 
# # Como a base é muito grande vou puxar apenas para aqueles que têm entre 25 e 64 anos
# censo_p_2000 <- cp2000 %>%
#   filter(idade >= 25 & idade <= 64) %>%
#   collect()
# 
# # Escrevendo em parquet novamente
# write_parquet(censo_p_2000, "E:/Thais/Tese_Mestrado/censo_2000/censo_pessoas_2000_25_64.parquet")

# Puxando em parquet
censo_p_2000_25_64 <- read_parquet("E:/Thais/Tese_Mestrado/censo_2000/censo_pessoas_2010_25_64.parquet")

cp2000 <- censo_p_2000_25_64



### 2010

# # Censo 2010 (extraído do DataZoom, com variáveis escolhidas)
# censo_pessoa_2010 <- read_dta("C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/censo/microdados_2010/censo_pessoas_2010.dta")
# 
# # Escrevendo em parquet
# write_parquet(censo_pessoa_2010, "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/censo/microdados_2010/censo_pessoas_2010.parquet")
# 
# # Como a base é muito grande vou puxar apenas para aqueles que têm entre 25 e 64 anos
# censo_p_2010 <- censo_pessoa_2010 %>%
#   filter(idade >= 25 & idade <= 64) %>%
#   collect()
# 
# # Escrevendo em parquet novamente
# write_parquet(censo_p_2010, "E:/Thais/Tese_Mestrado/censo_2010/censo_pessoas_2010_25_64.parquet")

# Puxando em parquet
censo_p_2010_25_64 <- read_parquet("E:/Thais/Tese_Mestrado/censo_2010/censo_pessoas_2010_25_64.parquet")

cp2010 <- censo_p_2010_25_64

# Transformando em data.table
as.data.table(cp2000)
as.data.table(cp2010)
gc()

############################
### UN DEMOGRAPHIC YEARBOOK
############################

# Autoras pegam para o ano de 2000. Pegarei para o ano de 2000 e 2010.

# Cálculo do Home Country Divorce Rate

# Number of divorces
div_1997_2001 <- read_xlsx("E:/Thais/Tese_Mestrado/apoio/n_divorce_un_dem_ybook/1997_2001.xlsx", col_names = FALSE)
colnames(div_1997_2001) <- div_1997_2001[9, ]
div_1997_2001 <- div_1997_2001[-(1:12), ] # Remove the first 12 rows
div_1997_2001 <- div_1997_2001[rowSums(is.na(div_1997_2001)) != ncol(div_1997_2001), ] #remove NA rows
div_1997_2001 <- div_1997_2001[, -2] # Remove the second column
div_1997_2001 <- div_1997_2001[, 1:6] # Remove columns from the eighth onwards
div_1997_2001 <- div_1997_2001[1:122, ]
div_1997_2001 <- div_1997_2001[!rowSums(is.na(div_1997_2001[, 2:6])) == 5, ] # Remove rows where all columns from 1997 to 2001 are NA
colnames(div_1997_2001)[1] <- "Country"

country_name_mapping <- c(
  "Côte d'Ivoire................................................................." = "Côte d'Ivoire",
  "Djibouti......................................................................" = "Djibouti",
  "Egypt - Égypte(2)............................................................." = "Egypt",
  "Ethiopia - Éthiopie..........................................................." = "Ethiopia",
  "Libyan Arab Jamahiriya - Jamahiriya arabe libyenne(3)........................." = "Libya",
  "Mauritius - Maurice..........................................................." = "Mauritius",
  "Réunion......................................................................." = "Réunion",
  "Saint Helena ex. dep. - Sainte-Hélène sans dép................................" = "Saint Helena",
  "Seychelles...................................................................." = "Seychelles",
  "South Africa - Afrique du Sud................................................." = "South Africa",
  "Tunisia - Tunisie............................................................." = "Tunisia",
  "Anguilla......................................................................" = "Anguilla",
  "Aruba........................................................................." = "Aruba",
  "Belize........................................................................" = "Belize",
  "Bermuda - Bermudes............................................................" = "Bermuda",
  "Canada........................................................................" = "Canada",
  "Cayman Islands - Îles Caïmanes................................................" = "Cayman Islands",
  "Costa Rica...................................................................." = "Costa Rica",
  "Cuba.........................................................................." = "Cuba",
  "Dominica - Dominique.........................................................." = "Dominica",
  "Dominican Republic - République dominicaine..................................." = "Dominican Republic",
  "El Salvador..................................................................." = "El Salvador",
  "Grenada - Grenade............................................................." = "Grenada",
  "Guatemala....................................................................." = "Guatemala",
  "Jamaica - Jamaïque............................................................" = "Jamaica",
  "Mexico - Mexique.............................................................." = "Mexico",
  "Netherlands Antilles - Antilles néerlandaises................................." = "Netherlands Antilles",
  "Nicaragua(4).................................................................." = "Nicaragua",
  "Panama(4)....................................................................." = "Panama",
  "Puerto Rico - Porto Rico......................................................" = "Puerto Rico",
  "Saint Lucia - Sainte-Lucie...................................................." = "Saint Lucia",
  "Saint Vincent and the Grenadines - Saint Vincent-et-les Grenadines............" = "Saint Vincent and the Grenadines",
  "Trinidad and Tobago - Trinité-et-Tobago......................................." = "Trinidad and Tobago",
  "United States - États-Unis(5)................................................." = "United States",
  "Brazil - Brésil(6)............................................................" = "Brazil",
  "Chile - Chili................................................................." = "Chile",
  "Ecuador - Équateur(7)........................................................." = "Ecuador",
  "French Guiana - Guyane française.............................................." = "French Guiana",
  "Peru - Pérou.................................................................." = "Peru",
  "Suriname......................................................................" = "Suriname",
  "Uruguay......................................................................." = "Uruguay",
  "Venezuela(6).................................................................." = "Venezuela",
  "Armenia - Arménie............................................................." = "Armenia",
  "Azerbaijan - Azerbaïdjan......................................................" = "Azerbaijan",
  "Bahrain - Bahreïn............................................................." = "Bahrain",
  "Brunei Darussalam - Brunéi Darussalam........................................." = "Brunei Darussalam",
  "China - Chine................................................................." = "China",
  "China: Hong Kong SAR - Chine: Hong Kong RAS..................................." = "Hong Kong SAR",
  "China: Macao SAR - Chine: Macao RAS..........................................." = "Macao SAR",
  "Cyprus - Chypre(8)............................................................" = "Cyprus",
  "Georgia - Géorgie............................................................." = "Georgia",
  "Iran (Islamic Republic of) - Iran (République islamique d')..................." = "Iran",
  "Iraq(9)......................................................................." = "Iraq",
  "Israel - Israël(10)..........................................................." = "Israel",
  "Japan - Japon(11)............................................................." = "Japan",
  "Jordan - Jordanie(12)........................................................." = "Jordan",
  "Kazakhstan...................................................................." = "Kazakhstan",
  "Korea (Republic of) - Corée (République de)(13)..............................." = "Korea (Republic of)",
  "Kuwait - Koweït..............................................................." = "Kuwait",
  "Kyrgyzstan - Kirghizistan....................................................." = "Kyrgyzstan",
  "Lebanon - Liban(9)............................................................" = "Lebanon",
  "Mongolia - Mongolie..........................................................." = "Mongolia",
  "Occupied Palestinian Territory - Territoire palestinien occupé................" = "Occupied Palestinian Territory",
  "Qatar........................................................................." = "Qatar",
  "Saudi Arabia - Arabie saoudite................................................" = "Saudi Arabia",
  "Singapore - Singapour........................................................." = "Singapore",
  "Syrian Arab Republic - République arabe syrienne(14).........................." = "Syrian Arab Republic",
  "Turkey - Turquie.............................................................." = "Turkey",
  "Turkmenistan - Turkménistan..................................................." = "Turkmenistan",
  "Uzbekistan - Ouzbékistan......................................................" = "Uzbekistan",
  "Austria - Autriche............................................................" = "Austria",
  "Belarus - Bélarus............................................................." = "Belarus",
  "Belgium - Belgique(15)........................................................" = "Belgium",
  "Bosnia and Herzegovina - Bosnie-Herzégovine..................................." = "Bosnia and Herzegovina",
  "Bulgaria - Bulgarie(16)......................................................." = "Bulgaria",
  "Channel Islands: Guernsey - Îles Anglo-Normandes: Guernesey..................." = "Channel Islands: Guernsey",
  "Croatia - Croatie............................................................." = "Croatia",
  "Czech Republic - République tchèque..........................................." = "Czech Republic",
  "Denmark - Danemark(17)........................................................" = "Denmark",
  "Estonia - Estonie............................................................." = "Estonia",
  "Finland - Finlande(18)........................................................" = "Finland",
  "France(19)...................................................................." = "France",
  "Germany - Allemagne..........................................................." = "Germany",
  "Greece - Grèce................................................................" = "Greece",
  "Hungary - Hongrie............................................................." = "Hungary",
  "Iceland - Islande(20)........................................................." = "Iceland",
  "Ireland - Irlande............................................................." = "Ireland",
  "Isle of Man - Îles de Man....................................................." = "Isle of Man",
  "Italy - Italie................................................................" = "Italy",
  "Latvia - Lettonie............................................................." = "Latvia",
  "Liechtenstein................................................................." = "Liechtenstein",
  "Lithuania - Lituanie.........................................................." = "Lithuania",
  "Luxembourg...................................................................." = "Luxembourg",
  "Monaco........................................................................" = "Monaco",
  "Netherlands - Pays-Bas........................................................" = "Netherlands",
  "Norway - Norvège.............................................................." = "Norway",
  "Poland - Pologne.............................................................." = "Poland",
  "Portugal......................................................................" = "Portugal",
  "Republic of Moldova - République de Moldova..................................." = "Republic of Moldova",
  "Romania - Roumanie............................................................" = "Romania",
  "Russian Federation - Fédération de Russie....................................." = "Russian Federation",
  "San Marino - Saint-Marin......................................................" = "San Marino",
  "Serbia and Montenegro - Serbie-et-Montenegro.................................." = "Serbia and Montenegro",
  "Slovakia - Slovaquie.........................................................." = "Slovakia",
  "Slovenia - Slovénie..........................................................." = "Slovenia",
  "Spain - Espagne..............................................................." = "Spain",
  "Sweden - Suède................................................................" = "Sweden",
  "Switzerland - Suisse.........................................................." = "Switzerland",
  "Macédoine.................................................................." = "Macedonia",
  "Ukraine......................................................................." = "Ukraine",
  "United Kingdom - Royaume-Uni.................................................." = "United Kingdom",
  "Australia - Australie(21)....................................................." = "Australia",
  "New Caledonia - Nouvelle-Calédonie............................................" = "New Caledonia",
  "New Zealand - Nouvelle-Zélande................................................" = "New Zealand",
  "Tonga(22)....................................................................." = "Tonga"
)

div_1997_2001$Country <- country_name_mapping[div_1997_2001$Country]

write_xlsx(div_1997_2001, "E:/Thais/Tese_Mestrado/apoio/n_divorce_un_dem_ybook/div_1997_2001_tratada.xlsx")

div_1997_2001 <- read_xlsx("E:/Thais/Tese_Mestrado/apoio/n_divorce_un_dem_ybook/div_1997_2001_tratada_nova.xlsx")


# The crude divorce-rate for a particular year is calculated by dividing the number
# of divorces occurring within a population over the year, by the average or mid-year
# population for that year, expressed times 1000.

# Number of married people
married_europe_1980_2023 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/married/c0000435_20240723-141532.xlsx", skip = 2)
married_europe_1980_2023 <- married_europe_1980_2023[, -(1:7)]
married_europe_1980_2023 <- married_europe_1980_2023[1:56, ]
colnames(married_europe_1980_2023)[1] <- "Country"

# Manipulate divorce and married df
# Ensure country names match exactly in both datasets
div_1997_2001$Country <- trimws(div_1997_2001$Country)
married_europe_1980_2023$Country <- trimws(married_europe_1980_2023$Country)

write_xlsx(married_europe_1980_2023, "E:/Thais/Tese_Mestrado/apoio/n_married_un_dem_ybook/married_europe_1980_2023_tratada.xlsx")

married_europe_1980_2023 <- read_xlsx("E:/Thais/Tese_Mestrado/apoio/n_married_un_dem_ybook/married_europe_1980_2023_tratada.xlsx")

# Married worldwide
married_2000 <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/married/UNdata_Export_20240806_153522515.csv")
married_1998_2002 <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/married/UNdata_Export_20240806_153309814.csv")
married_1990_2015 <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/married/UNdata_Export_20240806_153826383.csv")
married_all <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/married/UNdata_Export_20240806_153948438.csv")


##############################
#### HOME COUNTRY DIVORCE RATE
##############################

# Reshape the data to long format
married_long <- married_europe_1980_2023 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Married")

divorces_long <- div_1997_2001 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Divorces")

# Convert Year to numeric
married_long$Year <- as.numeric(substr(married_long$Year, 1, 4))
divorces_long$Year <- as.numeric(substr(divorces_long$Year, 1, 4))

# Convert Married and Divorces to numeric
married_long$Married <- as.numeric(married_long$Married)
divorces_long$Divorces <- as.numeric(divorces_long$Divorces)

# Merge the datasets
home_country_div_rate <- merge(married_long, divorces_long, by = c("Country", "Year"))

# Calculate divorces per 100 married inhabitants
home_country_div_rate <- home_country_div_rate %>%
  mutate(Divorces_per_100 = (Divorces / Married) * 100) %>%
  filter(!is.na(Divorces_per_100))

# Renomeando colunas
names(home_country_div_rate)[names(home_country_div_rate) == "Country"] <- "pais_nascim"
names(home_country_div_rate)[names(home_country_div_rate) == "Year"] <- "ano_hcdr"
names(home_country_div_rate)[names(home_country_div_rate) == "Married"] <- "n_married_people"
names(home_country_div_rate)[names(home_country_div_rate) == "Divorces"] <- "n_divorces"
names(home_country_div_rate)[names(home_country_div_rate) == "Divorces_per_100"] <- "hcdr"

# Filtrando os dados para manter apenas o ano 2000 se ambos 2000 e 2001 existirem para um país
home_country_div_rate <- home_country_div_rate %>%
  group_by(pais_nascim) %>%
  filter(!(n() > 1 & ano_hcdr == 2001)) %>%
  ungroup()

# Save the result to an Excel file
write.xlsx(home_country_div_rate, "E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate.xlsx")

# % saying divorce never justifiable esta abaixo nos dados de wvs para tx de catolicos e tx_freq_igreja


##############################################################################################
##############################################################################################
### Country of Origin Variables, measured in the year 2000 except where otherwise noted (paper)
##############################################################################################
##############################################################################################


#################
## GDP PER CAPITA: UN DATA
#################
# Per Capita GDP at current prices in US Dollars (all countries)
gdp_pc <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/gdp_per_capita/UNdata_Export_20240722_202249727.csv")

gdp_pc <- gdp_pc %>%
  filter(Year %in% c(2000, 2010))

gdp_pc <- gdp_pc %>%
  mutate(Value_per_100000 = Value / 100000) #números batem com APPENDIX B

# Remover coluna 'Item'
gdp_pc <- gdp_pc[ , !(names(gdp_pc) %in% c("Item"))]

# Renomear nome coluna
gdp_pc <- gdp_pc %>%
  rename(
    pais_nascim = "Country or Area",
    ano_gdp_pc = "Year",
    gdp_pc = "Value",
    gdp_pc_in_100000 = "Value_per_100000")

# Renomeando nome dos países
gdp_pc <- gdp_pc %>%
  mutate(pais_nascim = recode(pais_nascim,
                              "Bolivia (Plurinational State of)" = "Bolivia",
                              "China (mainland)" = "China",
                              "Republic of Korea" = "Korea",
                              "Iran, Islamic Republic of" = "Iran",
                              "State of Palestine" = "Palestine",
                              "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                              "United States" = "United States",
                              "Syrian Arab Republic" = "Syria",
                              "Türkiye" = "Turkey",
                              "Venezuela (Bolivarian Republic of)" = "Venezuela"))

# Save the result to an Excel file
write.xlsx(gdp_pc, "E:/Thais/Tese_Mestrado/tese_bases/gdp_per_capita/gdp_pc.xlsx")


########################################
## AVERAGE FEMALE AGE AT FIRST MARRIAGE
########################################
# age_f_first_marriage <- readr::read_csv("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/mean_age_first_marriage.csv")
# age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/P_Data_Extract_From_Gender_Statistics.xlsx")

europe_age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/UNECE/europeans_mean_age_female_first_marriage.xlsx", skip = 2) #Paper

age_f_first_marriage <- read_delim("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/singulate-mean-age-marriage-women.csv", delim = ",", col_names = TRUE)

# Renomear colunas
age_f_first_marriage <- age_f_first_marriage %>%
  rename(
    pais_nascim = "Entity",
    ano_age_first_marriage = "Year",
    age_first_marriage = "Estimated average age at marriage for women (UN)")

# Renomear valores 
age_f_first_marriage <- age_f_first_marriage %>%
  mutate(pais_nascim = recode(pais_nascim,
                              "United States" = "United States"))

# Remover coluna 'Code'
age_f_first_marriage <- age_f_first_marriage[ , !(names(age_f_first_marriage) %in% c("Code"))]

# Selecionar o ano mais próximo de 2000 para cada país
age_f_first_marriage <- age_f_first_marriage %>%
  group_by(pais_nascim) %>%
  filter(abs(ano_age_first_marriage - 2000) == min(abs(ano_age_first_marriage - 2000))) %>%
  ungroup()

# Save the result to an Excel file
write.xlsx(age_f_first_marriage, "E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/female_age_first_marriage.xlsx")

#######################
## TOTAL FERTILITY RATE
#######################
fertility_rate <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/fertility_rate/undesa_pd_2019_world_fertility_dataset.xlsx", sheet = "FERTILITY INDICATORS", skip = 6)

# Renomear colunas
fertility_rate <- fertility_rate %>%
  rename(
    pais_nascim = "Country or Area",
    age_group_fertility = "Age Group",
    indic_fertility = "Indicator",
    ano_fertility = "Date",
    fertility_rate = "Value")

# Escolher variaveis
fertility_rate <- fertility_rate[, c("pais_nascim", "age_group_fertility", "indic_fertility", "ano_fertility", "fertility_rate")]

# Filtrar para manter apenas as linhas onde indic_fertility é "TFR"
fertility_rate <- fertility_rate %>%
  filter(indic_fertility == "TFR")

# Renomear nome de paises
fertility_rate <- fertility_rate %>%
  mutate(pais_nascim = recode(pais_nascim,
                              "United States of America" = "United States",
                              "Bolivia (Plurinational State of)" = "Bolivia",
                              "Venezuela (Bolivarian Republic of)" = "Venezuela",
                              "Syrian Arab Republic" = "Syria",
                              "Republic of Korea" = "Korea",
  ))

# Selecionar o ano mais próximo de 2000 para cada país
fertility_rate <- fertility_rate %>%
  group_by(pais_nascim) %>%
  filter(abs(ano_fertility - 2000) == min(abs(ano_fertility - 2000))) %>%
  ungroup()

# Save the result to an Excel file
write.xlsx(fertility_rate, "E:/Thais/Tese_Mestrado/tese_bases/fertility_rate/fertility_rate.xlsx")

# Saber se países do censo estao na base fertility_rate
# Criar um vetor com os nomes dos países da lista
paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                  "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                  "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                  "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

# Verificar se os países da lista estão presentes no dataframe fertility_rate
paises_presentes <- paises_lista %in% fertility_rate$pais_nascim

# Criar um dataframe para mostrar os resultados
resultado_fertilty <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_fertilty

################################################################
## PROPORTION: WEEKLY CHURCH ATTENDANCE AND PROPORTION: CATHOLIC
################################################################

# Proportion Catholic in the home country
# Although religiosity, as measured by weekly church attendance,has a negative and statistically significant effect on the probability of being currently divorced,
# the home-country divorce coefficient decreases only slightly and remains positive
# and statistically significant after controlling for religiosity


# wvs1: V179=1 (Religious denomination),
# V197N=2070 (Religious denomination Normalized),
# V181=2 (How often do you attend religious services)

# 1981-1984
wvs1 <- readRDS("E:/Thais/Tese_Mestrado/tese_bases/WVS/WVS1/WV1_Data_R_v20200208.rds")
setDT(wvs1)

# 1989-1993
wvs2 <- readRDS("E:/Thais/Tese_Mestrado/tese_bases/WVS/WVS2/WVS2/F00008311-WV2_Data_R_v20180912.rds")
setDT(wvs2)

# 1994-1998---------------------------------------------------------------------
# Carregar os dados e transformar em data.table
wvs3 <- readRDS("E:/Thais/Tese_Mestrado/tese_bases/WVS/WVS3/F00008205-WV3_Data_R_v20180912.rds")
setDT(wvs3)

# Criar colunas para os cálculos
wvs3[, catholic_1995_1998 := ifelse(V179 == 1, 1, 0)]
wvs3[is.na(catholic_1995_1998), catholic_1995_1998 := 0]
wvs3[, once_week_church_1995_1998 := ifelse(V181 == 2, 1, 0)]
wvs3[is.na(once_week_church_1995_1998), once_week_church_1995_1998 := 0]

# Mapeamento de códigos para nomes dos países
country_mapping <- data.frame(
  Code = c(-4, 8, 31, 32, 36, 50, 51, 76, 100, 101, 112, 152, 156, 158, 170, 191, 203, 214, 222, 233, 246, 268, 276, 348, 356, 392, 410, 428, 440, 484, 498, 554, 566, 578, 586, 604, 608, 616, 630, 642, 643, 703, 705, 710, 724, 752, 756, 792, 804, 807, 826, 840, 858, 862, 911, 912, 914),
  Label = c("Not asked in survey", "Albania", "Azerbaijan", "Argentina", "Australia", "Bangladesh", "Armenia", "Brazil", "Bulgaria", "Srpska Republic", "Belarus", "Chile", "China", "Taiwan", "Colombia", "Croatia", "Czech Rep.", "Dominican Rep.", "El Salvador", "Estonia", "Finland", "Georgia", "Germany", "Hungary", "India", "Japan", "Korea", "Latvia", "Lithuania", "Mexico", "Moldova", "New Zealand", "Nigeria", "Norway", "Pakistan", "Peru", "Philippines", "Poland", "Puerto Rico", "Romania", "Russian Federation", "Slovakia", "Slovenia", "South Africa", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "United States", "Uruguay", "Venezuela", "Serbia", "Montenegro", "Bosnia")
)

# Converter a coluna V2 para character
wvs3 <- wvs3 %>%
  mutate(V2 = as.character(V2))

# Mudar os códigos para os nomes dos países
wvs3 <- wvs3 %>%
  mutate(V2 = recode(V2, !!!setNames(as.character(country_mapping$Label), country_mapping$Code)))

# Renomear a coluna de paises
setnames(wvs3, "V2", "pais_nascim")

# wvs3: 1995-1998
lista_wvs3 <- c("pais_nascim")
Data_list_wvs3 <- list()

# Calcular médias ponderadas
for (i in lista_wvs3) {
  Data_list_wvs3[[i]] <- wvs3[, .(
    tx_catholic_1995_1998 = weighted.mean(catholic_1995_1998, w = V236) * 100,
    tx_once_week_church_1995_1998 = weighted.mean(once_week_church_1995_1998, w = V236) * 100
  ), by = i]
  gc()
}

church_catholic_1995_1998 <- rbindlist(Data_list_wvs3, fill=TRUE)

# Save the result to an Excel file
write.xlsx(church_catholic_1995_1998, "E:/Thais/Tese_Mestrado/tese_bases/church/church_catholic_1995_1998.xlsx")

# Saber se países do censo estao na base church_catholic_1995_1998
paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                  "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                  "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                  "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

paises_presentes <- paises_lista %in% church_catholic_1995_1998$pais_nascim

resultado_church_1995_1998 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_church_1995_1998

# 1999-2004---------------------------------------------------------------------
# Carregar os dados e transformar em data.table
load("E:/Thais/Tese_Mestrado/tese_bases/WVS/WVS4/WV4_Data_R_v20201117.rdata")
wvs4 <- WV4_Data_R_v20201117
rm(WV4_Data_R_v20201117)
setDT(wvs4)

# Named vector for country code to country name mapping
country_names <- c(
  "ALB" = "Albania", "DZA" = "Algeria", "ARG" = "Argentina", "BGD" = "Bangladesh",
  "BIH" = "Bosnia and Herzegovina", "CAN" = "Canada", "CHL" = "Chile", "CHN" = "China",
  "VNM" = "Vietnam", "EGY" = "Egypt", "IND" = "India", "IDN" = "Indonesia",
  "IRN" = "Iran", "IRQ" = "Iraq", "ISR" = "Israel", "JOR" = "Jordan",
  "JPN" = "Japan", "KGZ" = "Kyrgyzstan", "MKD" = "North Macedonia", "MEX" = "Mexico",
  "MDA" = "Moldova", "MNE" = "Montenegro", "MAR" = "Morocco", "NGA" = "Nigeria",
  "PAK" = "Pakistan", "PER" = "Peru", "PHL" = "Philippines", "PRI" = "Puerto Rico",
  "KOR" = "South Korea", "ZAF" = "South Africa", "SAU" = "Saudi Arabia", "SGP" = "Singapore",
  "ESP" = "Spain", "SRB" = "Serbia", "SWE" = "Sweden", "TZA" = "Tanzania",
  "TUR" = "Turkey", "UGA" = "Uganda", "United States" = "United States", "VEN" = "Venezuela",
  "ZWE" = "Zimbabwe"
)

# Replace country codes with country names in your dataset
wvs4[, B_COUNTRY_ALPHA := country_names[B_COUNTRY_ALPHA]]

# Criar colunas para os cálculos
wvs4[, catholic_1999_2004 := ifelse(V184G == 1, 1, 0)]
wvs4[is.na(catholic_1999_2004), catholic_1999_2004 := 0]
wvs4[, once_week_church_1999_2004 := ifelse(V185 == 2, 1, 0)]
wvs4[is.na(once_week_church_1999_2004), once_week_church_1999_2004 := 0]

# Renomear coluna de paises
setnames(wvs4, "B_COUNTRY_ALPHA", "pais_nascim")

# wvs4: 1999-2004
lista_wvs4 <- c("pais_nascim")
Data_list_wvs4 <- list()

# Calcular médias ponderadas
for (i in lista) {
  Data_list_wvs4[[i]] <- wvs4[, .(
    tx_catholic_1999_2004 = weighted.mean(catholic_1999_2004, w = V245) * 100,
    tx_once_week_church_1999_2004 = weighted.mean(once_week_church_1999_2004, w = V245) * 100
  ), by = i]
  gc()
}

church_catholic_1999_2004 <- rbindlist(Data_list_wvs4, fill=TRUE)

# Save the result to an Excel file
write.xlsx(church_catholic_1999_2004, "E:/Thais/Tese_Mestrado/tese_bases/church/church_catholic_1999_2004.xlsx")

# Saber se países do censo estao na base church_catholic_1999_2004
paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                  "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                  "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                  "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

paises_presentes <- paises_lista %in% church_catholic_1999_2004$pais_nascim

resultado_church_1999_2004 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_church_1999_2004

# 2005-2009---------------------------------------------------------------------
# Carregar os dados e transformar em data.table
wvs5 <- readRDS("E:/Thais/Tese_Mestrado/tese_bases/WVS/WVS5/F00007944-WV5_Data_R_v20180912.rds")
setDT(wvs5)

# Criar colunas para os cálculos
wvs5[, catholic_2005_2009 := ifelse(V185 == 64, 1, 0)]
wvs5[is.na(catholic_2005_2009), catholic_2005_2009 := 0]
wvs5[, once_week_church_2005_2009 := ifelse(V186 == 2, 1, 0)]
wvs5[is.na(once_week_church_2005_2009), once_week_church_2005_2009 := 0]

# Mapeamento de códigos para nomes dos países
country_mapping <- data.frame(
  Code = c(2, 20, 52, 70, 90, 100, 135, 140, 155, 160, 165, 200, 210, 220, 225, 230, 232, 255, 290, 310, 325, 345, 349, 352, 355, 359, 360, 365, 369, 372, 375, 380, 385, 432, 439, 452, 517, 530, 551, 560, 600, 630, 640, 645, 651, 663, 710, 713, 714, 732, 740, 750, 800, 816, 820, 850, 910, 920),
  Label = c("United States", "Canada", "Trinidad and Tobago", "Mexico", "Guatemala", "Colombia", "Peru", "Brazil", "Chile", "Argentina", "Uruguay", "United Kingdom", "Netherlands", "France", "Switzerland", "Spain", "Andorra", "Germany", "Poland", "Hungary", "Italy", "Yugoslavia", "Slovenia", "Cyprus", "Bulgaria", "Moldova", "Romania", "Russian Federation", "Ukraine", "Georgia", "Finland", "Sweden", "Norway", "Mali", "Burkina Faso", "Ghana", "Rwanda", "Ethiopia", "Zambia", "South Africa", "Morocco", "Iran", "Turkey", "Iraq", "Egypt", "Jordan", "China", "Taiwan", "Hong Kong","Korea", "Japan", "India", "Thailand", "Vietnam", "Malaysia", "Indonesia", "Australia", "New Zealand")
)

# Converter a coluna COW para character e renomear códigos para nomes de países
wvs5 <- wvs5 %>%
  mutate(COW = as.character(COW)) %>%
  mutate(COW = recode(COW, !!!setNames(as.character(country_mapping$Label), country_mapping$Code))) %>%
  rename(pais_nascim = "COW")

# Transformar de volta para data.table
setDT(wvs5)

# wvs5: 2005-2009
lista_wvs5 <- c("pais_nascim")
Data_list_wvs5 <- list()

# Calcular médias ponderadas
for (i in lista_wvs5) {
  Data_list_wvs5[[i]] <- wvs5[, .(
    tx_catholic_2005_2009 = weighted.mean(catholic_2005_2009, w = V259) * 100,
    tx_once_week_church_2005_2009 = weighted.mean(once_week_church_2005_2009, w = V259) * 100
  ), by = i]
  gc()
}

# print(Data_list_wvs5)
church_catholic_2005_2009 <- rbindlist(Data_list_wvs5, fill=TRUE)

# Save the result to an Excel file
write.xlsx(church_catholic_2005_2009, "E:/Thais/Tese_Mestrado/tese_bases/church/church_catholic_2005_2009.xlsx")

# Saber se países do censo estao na base church_catholic_2005_2009
paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                  "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                  "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                  "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

paises_presentes <- paises_lista %in% church_catholic_2005_2009$pais_nascim

resultado_church_2005_2009 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_church_2005_2009

######################################
#### % saying divorce never justifiable
######################################
## wvs3:

# Criar variavel de never_just_div
# wvs3[, never_just_div_1995_1998 := ifelse(V200 %in% c(1:4), 1, 0)]
# wvs3[is.na(never_just_div_1995_1998), never_just_div_1995_1998 := 0]

wvs3[, never_just_div_1995_1998 := ifelse(V200 == 1, 1, 0)]
wvs3[is.na(never_just_div_1995_1998), never_just_div_1995_1998 := 0]

# Calcular médias ponderadas
lista_wvs3 <- c("pais_nascim")
Data_list_wvs3 <- list()
for (i in lista_wvs3) {
  Data_list_wvs3[[i]] <- wvs3[, .(
    tx_never_just_div_1995_1998 = weighted.mean(never_just_div_1995_1998, w = V236) * 100), by = i]
  gc()
}

never_just_div_1995_1998 <- rbindlist(Data_list_wvs3, fill=TRUE)

# Save the result to an Excel file
write.xlsx(never_just_div_1995_1998, "E:/Thais/Tese_Mestrado/tese_bases/div_never_just/never_just_div_1995_1998.xlsx")

# Saber se países do censo estao na base never_just_div_1995_1998
paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                  "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                  "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                  "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

paises_presentes <- paises_lista %in% never_just_div_1995_1998$pais_nascim

resultado_never_just_div_1995_1998 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_never_just_div_1995_1998

# wvs4:

# Criar variavel de never_just_div
wvs4[, never_just_div_1999_2004 := ifelse(V211 == 1, 1, 0)]
wvs4[is.na(never_just_div_1999_2004), never_just_div_1999_2004 := 0]

# wvs4[, never_just_div_1999_2004 := ifelse(V211 %in% c(1:4), 1, 0)]
# wvs4[is.na(never_just_div_1999_2004), never_just_div_1999_2004 := 0]

# Calcular médias ponderadas
lista_wvs4 <- c("pais_nascim")
Data_list_wvs4 <- list()
for (i in lista_wvs4) {
  Data_list_wvs4[[i]] <- wvs4[, .(
    tx_never_just_div_1999_2004 = weighted.mean(never_just_div_1999_2004, w = V236) * 100), by = i]
  gc()
}

never_just_div_1999_2004 <- rbindlist(Data_list_wvs4, fill=TRUE)

# Save the result to an Excel file
write.xlsx(never_just_div_1999_2004, "E:/Thais/Tese_Mestrado/tese_bases/div_never_just/never_just_div_1999_2004.xlsx")

# Saber se países do censo estao na base never_just_div_1999_2004
paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                  "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                  "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                  "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

paises_presentes <- paises_lista %in% never_just_div_1999_2004$pais_nascim

resultado_never_just_div_1999_2004 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_never_just_div_1999_2004

# wvs5:

# Criar variavel de never_just_div
# wvs5[, never_just_div_2005_2009 := ifelse(V205 %in% c(1:4), 1, 0)]
# wvs5[is.na(never_just_div_2005_2009), never_just_div_2005_2009 := 0]

wvs5[, never_just_div_2005_2009 := ifelse(V205 == 1, 1, 0)]
wvs5[is.na(never_just_div_2005_2009), never_just_div_2005_2009 := 0]

# Calcular médias ponderadas
lista_wvs5 <- c("pais_nascim")
Data_list_wvs5 <- list()
for (i in lista_wvs5) {
  Data_list_wvs5[[i]] <- wvs5[, .(
    tx_never_just_div_2005_2009 = weighted.mean(never_just_div_2005_2009, w = V236) * 100), by = i]
  gc()
}

never_just_div_2005_2009 <- rbindlist(Data_list_wvs5, fill=TRUE)

# Save the result to an Excel file
write.xlsx(never_just_div_2005_2009, "E:/Thais/Tese_Mestrado/tese_bases/div_never_just/never_just_div_2005_2009.xlsx")

# Saber se países do censo estao na base never_just_div_2005_2009
paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                  "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                  "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                  "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

paises_presentes <- paises_lista %in% never_just_div_2005_2009$pais_nascim

resultado_never_just_div_2005_2009 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_never_just_div_2005_2009

##################
## REMARRIAGE RATE
##################

# Remarriage rates, the number of married people (conditional on having ever divorced
# divided by the number of people who have ever divorced, were calculated by the
# authors using data from the 1999–2004 World Values Survey (WSV) (WSVAssociation 2000) on a sample
# of individuals aged 25–64. 



###################
###################
# CHOOSE VARIABLES
###################
###################

######################################################################################
# Variável dependente: imigrante divorciado que veio no máximo com 5 anos para o Brasil
######################################################################################

# Imigrante que veio com no máximo 5 anos para o Brasil e é divorciado

cp2010 <- cp2010 %>%
  mutate(
    ano_nasc = ano - idade,
    imm5anos = ifelse((ano_nasc - ano_fix_res <= 5)
                      & (idade >= 25 & idade <= 64), 1, 0),
    imm = ifelse((ano_nasc - ano_fix_res <= 5)
                 & (idade >= 25 & idade <= 64)
                 & (nacionalidade == 1 | nacionalidade == 2)
                 & (estado_conj %in% c(1:3) | estado_conj == 8), 1, 0),
    imm_div = ifelse(imm5anos == 1
                     & (nacionalidade == 1 | nacionalidade == 2)
                     & estado_conj == 8, 1, 0)
  )

#######################
# Variáveis de controle
#######################

## Imigrante homem

# Paper: The sample consists of immigrants aged 25–64 who arrived in the United States at age 5 or younger, reside in an identifiable metropolitan area, and are either married or divorced.

cp2010 <- cp2010 %>%
  mutate(
    imm_homem = ifelse(sexo == 1
                       & imm == 1
                       & (estado_conj %in% c(1:3) | estado_conj == 8), 1, 0)
  )


## Imm que completou o ensino médio

# IBGE cálculo anos de estudo: https://download.inep.gov.br/publicacoes/institucionais/estatisticas_e_indicadores/dicionario_de_indicadores_educacionais_formulas_de_calculo.pdf

cp2010 <- cp2010 %>%
  mutate(
    imm_em = ifelse(imm == 1
                    & (anos_estudoC == 3 | anos_estudo %in% c(11:14)), 1, 0)
  )

## Imm com escolaridade maior que o EM

cp2010 <- cp2010 %>%
  mutate(
    imm_em = ifelse(imm == 1
                    & (anos_estudoC == 4 | anos_estudo >= 16), 1, 0)
  )


## Imm: Proportion of MSA with same origin HELP

## Imm mulher: com pelo menos um filho

cp2010 <- cp2010 %>%
  mutate(
    imm_em = ifelse(imm == 1
                    & sexo == 0
                    & (filhos_nasc_vivos >= 1 | filhos_vivos >= 1), 1, 0)
  )


## Log wage
# Paper: They run a placebo regression with log wage—a measure of unobserved 
# human capital—as the dependent variable for the whole sample as well as for
# men and women separately. Home-country divorce rates have no statistically
# significant impact on wages, suggesting that unobserved human capital is not
# likely to bias our baseline estimates. p.1028

# Elas não controlam por log wage no baseline, deixar para depois

###################
# CULTURAL PROXIES
###################

# HOME COUNTRY DIVORCE RATE

# % saying divorce never justifiable

