# Thais Takeuchi
# 19/7/24
# Project: Master Thesis


#######################
# PREPARE ENVIRONMENT
#######################

#### Remove some objects
rm(list=setdiff(ls(), c("cp2000", "cp2000_copy")))

### Load Packages (and install packages if needed)
load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

### Country codes ISO-3166
codes_raw <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/country_codes/codes_raw.csv")

#############
# Open bases
#############

########
# PAPER
########
paper_b <- read_excel("E:/Thais/Tese_Mestrado/tese_bases/paper_comparison/apdx_b_div_imm.xlsx")

# Rename Russia to Russian Federation
paper_b <- paper_b %>%
  mutate(Country = ifelse(Country == "Russia", "Russian Federation", Country))

#######
# CENSO
#######

# Brasileiro nato definicao: https://www.jusbrasil.com.br/artigos/e-possivel-ser-considerado-brasileiro-nato-nascido-no-estrangeiro-filho-de-pai-naturalizado-e-mae-estrangeira/687322432

### 1970
censo_1970_raw <- read_sav("E:/Thais/Tese_Mestrado/IBGE_1970/Censo 1970/censo_1970_25_percent.sav")
censo_1970_raw <- write_dta(censo_1970_raw, "censo_1970_raw.dta")

### 1980
censo_1980_raw <- read_sav("E:/Thais/Tese_Mestrado/IBGE_1980/censo_1970_25_percent.sav")






### 1991

censo_1991 <- read_sav("E:/Thais/Tese_Mestrado/Censo Demográfico de 1991/Censo 1991/Censo.1991.brasil.amostra.25porcento.sav")

## Selecionando variáveis
c1991 <- censo_1991[, c("TipoReg", "V7301", "V3151", "V3152", "V0316","V3072", "V0333")]

## Arruma variaveis
c1991 <- subset(c1991, TipoReg == 2)
c1991$V3152 <- paste0("19", sprintf("%02d", c1991$V3152))
c1991$V3152 <- ifelse(c1991$V3152 == "19NA", NA, c1991$V3152)
c1991$V3152 <- as.numeric(c1991$V3152)
c1991$ano <- 1991
c1991 <- subset(c1991, V3072 >= 25 & V3072 <= 64)

c1991 <- c1991 %>%
  mutate(
    imm_div = ifelse(V3152 - (ano-V3072) <= 5 &
                       (V3151 == 2 | V3151 == 3)
                     & (V0333 %in% c(8)), 1, 0)
  )

teste <- c1991[c1991$imm_div == 1, ]
sum(teste$V7301, na.rm = TRUE)

teste <- censo_1991[censo_1991$V0301 == 1, ]
sum(teste$V7301, na.rm = TRUE)







### 2000------------------------------------------------------------------------
## compatibilizada
# Censo 2000 (extraído da basedosdados)
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
# 
# # Puxando em parquet
# censo_p_2000_25_64 <- arrow::read_parquet("E:/Thais/Tese_Mestrado/censo_2000/censo_pessoas_2000_25_64.parquet")
# 
# cp2000 <- censo_p_2000_25_64
# 
# rm(censo_p_2000_25_64)
# 
# ## Comparando com a sidra
# teste <- cp2000[cp2000$sexo %in% c(0,1), ]
# sum(teste$peso_pess)

## nao compatibilizada

# Censo 2000 (extraído do DataZoom, com variáveis escolhidas)
censo_pessoa_2000_ncomp <- read_dta("E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_pessoas_2000_filtrado_nao_comp.dta")
gc()

# Escrevendo em parquet
write_parquet(censo_pessoa_2000_ncomp, "E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_pessoas_2000_filtrado_nao_comp.parquet")

# Puxando em parquet
cp2000_nc <- read_parquet("E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_pessoas_2000_filtrado_nao_comp.parquet")









### 2010------------------------------------------------------------------------

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

# # Puxando em parquet
# censo_p_2010_25_64 <- read_parquet("E:/Thais/Tese_Mestrado/censo_2010/censo_pessoas_2010_25_64.parquet")
# 
# cp2010 <- censo_p_2010_25_64

# Transformando em data.table
as.data.table(cp2000)
# as.data.table(cp2010)
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

# Função para substituir NA no ano 2000 pelo valor mais próximo
replace_na_with_closest <- function(row) {
  if (is.na(row["2000"])) {
    # Obter o valor mais próximo não nulo
    closest_year <- names(row[!is.na(row)])[which.min(abs(as.numeric(names(row[!is.na(row)])) - 2000))]
    row["2000"] <- row[closest_year]
    row["Closest_Year"] <- closest_year
  } else {
    row["Closest_Year"] <- "2000"
  }
  return(row)
}

# Aplicar a função ao DataFrame
div_1997_2001 <- as.data.frame(t(apply(div_1997_2001, 1, replace_na_with_closest)))

# Saber se países do censo estao na base div_1997_2001
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

paises_presentes <- paises_lista %in% div_1997_2001$Country

resultado_div_1997_2001 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_div_1997_2001

# Tira coluna "Closest Year"
# div_1997_2001 <- div_1997_2001[, !names(div_1997_2001) %in% "Closest_Year"]


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

# Converter as colunas de anos para numérico
married_europe_1980_2023[, -1] <- lapply(married_europe_1980_2023[, -1], as.numeric)

# Função para substituir NA no ano 2000 pelo valor mais próximo
replace_na_with_closest <- function(row) {
  if (is.na(row["2000"])) {
    # Obter os nomes dos anos que não são NA
    non_na_years <- names(row)[!is.na(row)]
    
    # Se não houver anos não-NA, retorne a linha como está
    if (length(non_na_years) == 0) {
      row["Closest_Year"] <- NA
      return(row)
    }
    
    # Converter os nomes dos anos para numérico
    numeric_years <- as.numeric(non_na_years)
    
    # Encontrar o ano mais próximo de 2000
    closest_year <- numeric_years[which.min(abs(numeric_years - 2000))]
    
    # Substituir o valor NA no ano 2000 pelo valor do ano mais próximo
    row["2000"] <- row[as.character(closest_year)]
    
    # Adicionar a coluna de ano mais próximo
    row["Closest_Year"] <- as.character(closest_year)
  } else {
    # Se o valor no ano 2000 não é NA, definir "Closest_Year" como "2000"
    row["Closest_Year"] <- "2000"
  }
  return(row)
}

# Aplicar a função linha por linha
married_europe_1980_2023 <- as.data.frame(married_europe_1980_2023)  # Converter para data.frame se necessário

# Adicionar a coluna "Closest_Year" inicialmente
married_europe_1980_2023$Closest_Year <- NA

# Iterar sobre cada linha e aplicar a função
for (i in 1:nrow(married_europe_1980_2023)) {
  married_europe_1980_2023[i, ] <- replace_na_with_closest(married_europe_1980_2023[i, ])
}

# Verificar o DataFrame transformado
head(married_europe_1980_2023)

# Organiza colunas 
married_europe_1980_2023 <- married_europe_1980_2023[, c("Country", "2000", "Closest_Year")]

# Saber se países do censo estao na base married_europe_1980_2023
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

paises_presentes <- paises_lista %in% married_europe_1980_2023$Country

resultado_married_europe_1980_2023 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
resultado_married_europe_1980_2023

# Tira coluna "Closest Year"
married_europe_1980_2023 <- married_europe_1980_2023[, !names(married_europe_1980_2023) %in% "Closest_Year"]

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

home_country_div_rate_1 <- merge(married_europe_1980_2023, div_1997_2001, by = c("Country"))

# Rename columns
home_country_div_rate_1 <- home_country_div_rate_1 %>%
  rename(
    n_married = "2000.x",
    n_divorce = "2000.y",
    year_n_married = "Closest_Year.x",
    year_n_divorce = "Closest_Year.y"
  )

# Organiza colunas 
home_country_div_rate_1 <- home_country_div_rate_1[, c("Country", "n_married", "n_divorce", "year_n_married", "year_n_divorce")]

# Atualiza valor na mao
## United States
home_country_div_rate_1 <- home_country_div_rate_1 %>%
  mutate(n_divorce = ifelse(Country == "United States", 944000, n_divorce))

home_country_div_rate_1 <- home_country_div_rate_1 %>%
  mutate(year_n_divorce = ifelse(Country == "United States", 2000, year_n_divorce))

## UK
home_country_div_rate_1 <- home_country_div_rate_1 %>%
  mutate(n_married = ifelse(Country == "United Kingdom", 23088100, n_married))

home_country_div_rate_1 <- home_country_div_rate_1 %>%
  mutate(year_n_married = ifelse(Country == "United Kingdom", 2002, year_n_married))

# https://www.statista.com/statistics/621325/marital-status-in-england-and-wales/


# Calculate divorces per 100 married inhabitants
home_country_div_rate <- home_country_div_rate %>%
  mutate(Divorces_per_100 = (Divorces / Married) * 100) %>%
  filter(!is.na(Divorces_per_100))

# Transformar a coluna n_divorce em numeric dentro do mutate
home_country_div_rate_1 <- home_country_div_rate_1 %>%
  mutate(
    n_divorce = as.numeric(n_divorce),
    Divorces_per_100 = (n_divorce / n_married) * 100
  ) %>%
  filter(!is.na(Divorces_per_100))

# Renomeando colunas
names(home_country_div_rate)[names(home_country_div_rate) == "Country"] <- "pais_nascim"
names(home_country_div_rate)[names(home_country_div_rate) == "Year"] <- "ano_hcdr"
names(home_country_div_rate)[names(home_country_div_rate) == "Married"] <- "n_married_people"
names(home_country_div_rate)[names(home_country_div_rate) == "Divorces"] <- "n_divorces"
names(home_country_div_rate)[names(home_country_div_rate) == "Divorces_per_100"] <- "hcdr"

names(home_country_div_rate_1)[names(home_country_div_rate_1) == "Country"] <- "pais_nascim"
names(home_country_div_rate_1)[names(home_country_div_rate_1) == "Divorces_per_100"] <- "hcdr"

# # Filtrando os dados para manter apenas o ano 2000 se ambos 2000 e 2001 existirem para um país
# home_country_div_rate <- home_country_div_rate %>%
#   group_by(pais_nascim) %>%
#   filter(!(n() > 1 & ano_hcdr == 2001)) %>%
#   ungroup()

# Save the result to an Excel file
write.xlsx(home_country_div_rate, "E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate.xlsx")
write.xlsx(home_country_div_rate_1, "E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate_1.xlsx")

# % saying divorce never justifiable esta abaixo nos dados de wvs para tx de catolicos e tx_freq_igreja

## Incluir mais paises em hcdr
paises_nao_presentes <- setdiff(paises_lista, unique(home_country_div_rate_1$pais_nascim))
paises_nao_presentes

## Home country divorce rate
# home_country_div_rate <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate.xlsx")
home_country_div_rate_1 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate_1.xlsx")

# Bolivia
## Casados em 2001
#https://bolivia.unfpa.org/sites/default/files/pub-pdf/Estadisticas_de_genero_final_INE.pdf
# p.38

## Divorcios 2000
#https://www.facebook.com/photo.php?fbid=3039888536284960&id=1544538869153275&set=a.1545323212408174&locale=bs_BA

bolivia <- data.frame(pais_nascim = "Bolivia",
                      n_married = 4600524,
                      n_divorce = 7940,
                      year_n_married = 2001,
                      year_n_divorce = 2000,
                      hcdr = 0.172589)

# Adiciona a nova linha ao data frame existente
home_country_div_rate_1 <- rbind(home_country_div_rate_1, bolivia)

# Peru
## Casada 1993
# https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1742/Libro.pdf
## Divorcios 2000
# https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1045/cap04.pdf
peru <- data.frame(pais_nascim = "Peru",
                      n_married = 5384534,
                      n_divorce = 2055,
                      year_n_married = 1993,
                      year_n_divorce = 2000,
                      hcdr = 0.172589)  

# Adiciona a nova linha ao data frame existente
home_country_div_rate_1 <- rbind(home_country_div_rate_1, peru)

# Japan
## Casados 2000
#https://www.e-stat.go.jp/en/stat-search/files?page=1&query=marital%20status&layout=dataset&toukei=00200521&tstat=000000030001&tclass1=000000030001&tclass2=000000030002&tclass3=000000030003&stat_infid=00000003000A&metadata=1&data=1
## Divorcios 2000
#https://www.mhlw.go.jp/english/database/db-hw/populate/dl/E01.pdf

japan <- data.frame(pais_nascim = "Japan",
                   n_married = 64883870,
                   n_divorce = 264246,
                   year_n_married = 2000,
                   year_n_divorce = 2000,
                   hcdr = 0.4072599)  

# Adiciona a nova linha ao data frame existente
home_country_div_rate_1 <- rbind(home_country_div_rate_1, japan)

# Paraguay
## Casados 2000
#https://www.ine.gov.py/publication-single.php?codec=74

## Divorcios 2000
#
# paraguay <- data.frame(pais_nascim = "Paraguay",
#                     n_married = 35622,
#                     n_divorce = ,
#                     year_n_married = 2000,
#                     year_n_divorce = 2000,
#                     hcdr = 0.4072599)  
# 
# # Adiciona a nova linha ao data frame existente
# home_country_div_rate_1 <- rbind(home_country_div_rate_1, paraguay)


# China



write.xlsx(home_country_div_rate_1, "E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate_1.xlsx")




paises_nao_presentes <- setdiff(paises_lista, unique(home_country_div_rate_1$pais_nascim))
paises_nao_presentes




##############################################################################################
##############################################################################################
### Country of Origin Variables, measured in the year 2000 except where otherwise noted (paper)
##############################################################################################
##############################################################################################


#################
## GDP PER CAPITA: UN DATA
#################
# Per Capita GDP at current prices in US Dollars (all countries)

countries_to_keep <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
  "Spain", "India", "United States", "Colombia", "Uruguay", 
  "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
  "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
  "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
  "Panama", "Belgium", "Egypt", "United Kingdom", "Greece", 
  "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
  "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
  "French Guiana", "Suriname", "Sweden", "Honduras", 
  "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

country_code_mapping <- c(
  "Bolivia" = "BOL", "Peru" = "PER", "Germany" = "DEU", "Japan" = "JPN", 
  "Paraguay" = "PRY", "Portugal" = "PRT", "Italy" = "ITA", 
  "Spain" = "ESP", "India" = "IND", "United States" = "USA", "Colombia" = "COL", 
  "Uruguay" = "URY", "Venezuela" = "VEN", "China" = "CHN", "Guyana" = "GUY", 
  "France" = "FRA", "Argentina" = "ARG", "Chile" = "CHL", "Hungary" = "HUN", 
  "Lebanon" = "LBN", "Austria" = "AUT", "Costa Rica" = "CRI", 
  "Russian Federation" = "RUS", "Romania" = "ROU", "Turkey" = "TUR", 
  "Czechoslovakia" = "CZE", "Poland" = "POL", "Cuba" = "CUB", "Switzerland" = "CHE", 
  "Nicaragua" = "NIC", "Panama" = "PAN", "Belgium" = "BEL", "Egypt" = "EGY", 
  "United Kingdom" = "GBR", "Greece" = "GRC", "Guatemala" = "GTM", 
  "Syria" = "SYR", "Korea" = "KOR", "Netherlands" = "NLD", 
  "Yugoslavia" = "YUG", "Pakistan" = "PAK", "Ireland" = "IRL", 
  "Ecuador" = "ECU", "Israel" = "ISR", "Denmark" = "DNK", 
  "Australia" = "AUS", "Mexico" = "MEX", "French Guiana" = "GUF", 
  "Suriname" = "SUR", "Sweden" = "SWE", "Honduras" = "HND", 
  "Canada" = "CAN", "El Salvador" = "SLV", "Finland" = "FIN", 
  "Dominican Republic" = "DOM", "Bulgaria" = "BGR", "Norway" = "NOR"
)

gdp_pc <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/gdp_per_capita/UNdata_Export_20240722_202249727.csv") |>
  filter(Year %in% c(1970, 1980, 1991, 2000, 2010)) |>
  mutate(Value_per_100000 = Value / 100000) |>
  dplyr::select(-3) |>
  rename(
    pais_nascim = "Country or Area",
    ano_gdp_pc = "Year",
    gdp_pc = "Value",
    gdp_pc_in_100000 = "Value_per_100000"
  ) |>
  filter(pais_nascim %in% countries_to_keep) |>
  dplyr::mutate(pais_nascim = dplyr::recode(pais_nascim, !!!country_code_mapping)) |>
  dplyr::filter(pais_nascim %in% country_code_mapping) |>
  dplyr::mutate(gdp_pc_in_100000 = round(gdp_pc_in_100000, 3))

years <- c(2010, 2000, 1991, 1980, 1970)

for (year in years) {
  gdp_pc_year <- gdp_pc |>
    dplyr::filter(ano_gdp_pc == year)
  
  write.xlsx(gdp_pc_year, paste0("E:/Thais/Tese_Mestrado/tese_bases/gdp_per_capita/gdp_pc_", year, ".xlsx"))
}

########################################
## AVERAGE FEMALE AGE AT FIRST MARRIAGE
########################################
# age_f_first_marriage <- readr::read_csv("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/mean_age_first_marriage.csv")
# age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/P_Data_Extract_From_Gender_Statistics.xlsx")

# 
europe_age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/UNECE/europeans_mean_age_female_first_marriage.xlsx", skip = 2) #Paper
#https://w3.unece.org/PXWeb2015/pxweb/en/STAT/STAT__30-GE__02-Families_households/052_en_GEFHAge1stMarige_r.px/table/tableViewLayout1/

age_f_first_marriage <- read_delim("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/singulate-mean-age-marriage-women.csv", delim = ",", col_names = TRUE) |>
  dplyr::select(-1) |>
  rename(
    pais_nascim = "Code",
    ano_ffm = "Year",
    ffm = "Estimated average age at marriage for women (UN)"
  ) |>
  #(https://w3.unece.org/PXWeb2015/pxweb/en/STAT/STAT__30-GE__02-Families_households/052_en_GEFHAge1stMarige_r.px/table/tableViewLayout1/)
  bind_rows(data.frame(
    pais_nascim = "RUS",
    ano_ffm = 1995,
    ffm = 22.6
  ))

# Save the result to an Excel file
write.xlsx(age_f_first_marriage, "E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/female_age_first_marriage.xlsx")

## world bank data
# https://databank.worldbank.org/reports.aspx?source=gender-statistics#
wb_age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/P_Data_Extract_From_Gender_Statistics_2024_08_08.xlsx") |>
  dplyr::select(-c(2:3)) |>
  rename(
    pais_nascim = "Country Code",
    ano_ffm = "Time",
    ffm = "Age at first marriage, female [SP.DYN.SMAM.FE]"
  ) |>
  dplyr::mutate(ffm = dplyr::na_if(ffm, "..")) |>
  dplyr::filter(!is.na(ffm)) |>
  dplyr::filter(pais_nascim %in% c(
    "ARG", "BOL", "CAN", "CHL", "COL", "CRI", "CUB", "ECU", "USA", 
    "GTM", "GUY", "GUF", "HTI", "HND", "BLZ", "JAM", "MEX", "NIC", 
    "PAN", "PRY", "PER", "DOM", "SLV", "SUR", "URY", "VEN", 
    "DEU", "AUT", "BEL", "BGR", "DNK", "ESP", "FIN", "FRA", "GBR", 
    "GRC", "NLD", "HUN", "IRL", "ITA", "YUG", "NOR", "POL", "PRT", 
    "ROU", "SWE", "CHE", "CZE", "RUS", "AGO", "EGY", 
    "MOZ", "CHN", "TWN", "KOR", "IND", "ISR", "JPN", 
    "LBN", "PAK", "SYR", "TUR", "AUS"
  )) |>
  dplyr::mutate(ffm = as.numeric(ffm)) |>
  bind_rows(data.frame(
    pais_nascim = "RUS",
    ano_ffm = 1995,
    ffm = 22.6
  )) |>
  bind_rows(data.frame(
    pais_nascim = "URY",
    ano_ffm = 1996,
    ffm = 23.3
  )) |>
  dplyr::mutate(ffm = round(ffm, 2))

# Save the result to an Excel file
write.xlsx(wb_age_f_first_marriage, "E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/wb_mean_age_first_marriage.xlsx")


## other dt
### https://genderdata.worldbank.org/en/indicator/sp-dyn-smam#data-table-section
mean_age_fmarr <- read.csv("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/mean_age_first_marriage.csv", header = FALSE, stringsAsFactors = FALSE) |>
  (\(mafm) strsplit(mafm$V1, split = ","))() |> 
  (\(lst) {
    max_len <- max(sapply(lst, length))
    lst <- lapply(lst, function(x) c(x, rep(NA, max_len - length(x))))
    do.call(rbind, lst)
  })() |> 
  as.data.frame(stringsAsFactors = FALSE) |> 
  (\(mafm) mafm[, -c(1, 3:4)])() |>
  within({
    V5 <- ifelse(!is.na(V9), V6, V5)
    V6 <- ifelse(!is.na(V9), V7, V6)
    V7 <- ifelse(!is.na(V9), V8, V7)
    V8 <- ifelse(!is.na(V9), V9, V8)
    V9 <- NULL  
  }) |>
  (\(mafm) mafm[, -c(1)])() |>
  (\(mafm) {
    mafm <- mafm[mafm$V8 == "\"female\"", ]  
    mafm <- mafm[-1, ]  
    mafm[] <- lapply(mafm, function(x) gsub('"', '', x)) 
    colnames(mafm) <- c("pais_nascim", "ano_ffm", "ffm", "gender") 
    mafm
  })() |> 
  dplyr::filter(pais_nascim %in% c(
    "ARG", "BOL", "CAN", "CHL", "COL", "CRI", "CUB", "ECU", "USA", 
    "GTM", "GUY", "GUF", "HTI", "HND", "BLZ", "JAM", "MEX", "NIC", 
    "PAN", "PRY", "PER", "DOM", "SLV", "SUR", "URY", "VEN", 
    "DEU", "AUT", "BEL", "BGR", "DNK", "ESP", "FIN", "FRA", "GBR", 
    "GRC", "NLD", "HUN", "IRL", "ITA", "YUG", "NOR", "POL", "PRT", 
    "ROU", "SWE", "CHE", "CZE", "RUS", "AGO", "EGY", 
    "MOZ", "CHN", "TWN", "KOR", "IND", "ISR", "JPN", 
    "LBN", "PAK", "SYR", "TUR", "AUS"
  )) |>
  (\(mafm) mafm[, -c(4)])()
  

write.xlsx(mean_age_fmarr, "E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/mean_age_fmarr.xlsx")


# Merge all dt
age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/female_age_first_marriage.xlsx")

wb_age_f_first_marriage <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/wb_mean_age_first_marriage.xlsx")

mean_age_fmarr <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/mean_age_fmarr.xlsx")

mean_age_fmarr <- mean_age_fmarr %>%
  mutate(ano_ffm = as.numeric(ano_ffm),
    ffm = as.numeric(ffm))

merged_ffm <- age_f_first_marriage %>%
  full_join(wb_age_f_first_marriage, by = c("pais_nascim", "ano_ffm", "ffm")) %>%
  full_join(mean_age_fmarr, by = c("pais_nascim", "ano_ffm", "ffm")) |> 
  dplyr::filter(pais_nascim %in% c(
    "ARG", "BOL", "CAN", "CHL", "COL", "CRI", "CUB", "ECU", "USA", 
    "GTM", "GUY", "GUF", "HTI", "HND", "BLZ", "JAM", "MEX", "NIC", 
    "PAN", "PRY", "PER", "DOM", "SLV", "SUR", "URY", "VEN", 
    "DEU", "AUT", "BEL", "BGR", "DNK", "ESP", "FIN", "FRA", "GBR", 
    "GRC", "NLD", "HUN", "IRL", "ITA", "YUG", "NOR", "POL", "PRT", 
    "ROU", "SWE", "CHE", "CZE", "RUS", "AGO", "EGY", 
    "MOZ", "CHN", "TWN", "KOR", "IND", "ISR", "JPN", 
    "LBN", "PAK", "SYR", "TUR", "AUS"
  ))

years <- c(2010, 2000, 1991, 1980, 1970)

for (year in years) {
  mean_age_fmarr_year <- merged_ffm |>
    dplyr::mutate(ano_ffm = as.numeric(ano_ffm)) |>
    dplyr::group_by(pais_nascim) |>
    dplyr::filter(abs(ano_ffm - year) == min(abs(ano_ffm - year))) |>
    dplyr::ungroup() |>
    dplyr::distinct(pais_nascim, .keep_all = TRUE)
  
  write.xlsx(mean_age_fmarr_year, paste0("E:/Thais/Tese_Mestrado/tese_bases/age_first_marriage/mean_age_fmarr_", year, ".xlsx"))
}

#######################
## TOTAL FERTILITY RATE
#######################

fertility_rate <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/fertility_rate/undesa_pd_2019_world_fertility_dataset.xlsx", sheet = "FERTILITY INDICATORS", skip = 6) |> 
  rename(
    pais_nascim = "Country or Area",
    age_group_fertility = "Age Group",
    indic_fertility = "Indicator",
    ano_fertility = "Date",
    fertility_rate = "Value"
  ) |>
  dplyr::select(pais_nascim, age_group_fertility, indic_fertility, ano_fertility, fertility_rate) |> 
  filter(indic_fertility == "TFR") |> 
  dplyr::select(-c(2:3)) |>
  mutate(ano_fertility = floor(ano_fertility)) |> 
  filter(pais_nascim %in% countries_to_keep) |> 
  dplyr::mutate(pais_nascim = dplyr::recode(pais_nascim, !!!country_code_mapping))

years <- c(2010, 2000, 1991, 1980, 1970)

for (year in years) {
  fertility_rate_year <- fertility_rate |>
    dplyr::mutate(ano_fertility = as.numeric(ano_fertility)) |>
    dplyr::group_by(pais_nascim) |>
    dplyr::filter(abs(ano_fertility - year) == min(abs(ano_fertility - year))) |>
    dplyr::ungroup() |>
    dplyr::distinct(pais_nascim, .keep_all = TRUE)
  
  write.xlsx(fertility_rate_year, paste0("E:/Thais/Tese_Mestrado/tese_bases/fertility_rate/fertility_rate_", year, ".xlsx"))
}

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
wvs2 <- readRDS("E:/Thais/Tese_Mestrado/tese_bases/WVS/WVS2/F00008311-WV2_Data_R_v20180912.rds")
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
for (i in lista_wvs4) {
  Data_list_wvs4[[i]] <- wvs4[, .(
    tx_catholic_1999_2004 = weighted.mean(catholic_1999_2004, w = V245) * 100,
    tx_once_week_church_1999_2004 = weighted.mean(once_week_church_1999_2004, w = V245) * 100
  ), by = i]
  gc()
}

church_catholic_1999_2004 <- rbindlist(Data_list_wvs4, fill=TRUE)

# Save the result to an Excel file
write.xlsx(church_catholic_1999_2004, "E:/Thais/Tese_Mestrado/tese_bases/church/church_catholic_1999_2004.xlsx")

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

# Adiciona dados
#https://www.statista.com/statistics/1268817/share-catholics-latam-by-country/


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






################################################################################
######
## LFP
######

# 2000

lfp <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/lfp/EAP_2WAP_SEX_AGE_RT_A-filtered-2024-08-11.xlsx")

lfp <- lfp %>%
  dplyr::select(-source.label, -indicator.label, -sex.label, -obs_status.label) %>%
  dplyr::rename(
    pais_nascim = ref_area.label,
    ano_lfp = time,
    lfp = obs_value,
    age_lfp = classif1.label
  ) %>%
  dplyr::mutate(
    pais_nascim = dplyr::case_when(
      pais_nascim == "Republic of Korea" ~ "Korea",
      pais_nascim == "United States of America" ~ "United States",
      pais_nascim == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      pais_nascim == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      pais_nascim == "Bolivia (Plurinational State of)" ~ "Bolivia",
      pais_nascim == "Syrian Arab Republic" ~ "Syria",
      TRUE ~ pais_nascim
    ),
    age_lfp = dplyr::case_when(
      age_lfp == "Age (Aggregate bands): Total" ~ "total",
      TRUE ~ age_lfp
    )
  )

# List of countries to keep
countries_to_keep <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
                       "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
                       "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
                       "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
                       "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
                       "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
                       "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
                       "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
                       "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
                       "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")

# Filter the dataframe
lfp <- lfp %>% filter(pais_nascim %in% countries_to_keep)

write.xlsx(lfp, "E:/Thais/Tese_Mestrado/tese_bases/lfp/lfp_final.xlsx")

# LFP 1991-2022
## https://rshiny.ilo.org/dataexplorer43/?lang=en&segment=indicator&id=EAP_2WAP_SEX_AGE_RT_A
lfp_1 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/lfp/P_Data_Extract_From_World_Development_Indicators.xlsx", sheet = 3)
lfp_1[,-(1:2)] <- lapply(lfp_1[,-(1:2)], function(x) round(as.numeric(x), 2))
# List of countries to keep
Code <- c("BOL", "PER", "DEU", "JPN", "PRY", "PRT", "ITA", 
          "ESP", "AFR", "IND", "USA", "COL", "URY", 
          "VEN", "CHN", "GUY", "FRA", "ARG", "CHL", 
          "HUN", "LBN", "AUT", "CRI", "RUS", "ROU", 
          "TUR", "CSK", "POL", "CUB", "CHE", "NIC", 
          "PAN", "BEL", "ASI", "EGY", "GBR", "GRC", 
          "GTM", "SYR", "KOR", "NLD", "YUG", "PAK", 
          "IRL", "ECU", "ISR", "DNK", "AUS", "MEX", 
          "GUF", "SUR", "SWE", "AMR", "HND", 
          "CAN", "SLV", "FIN", "DOM", "BGR", "NOR")
lfp_1 <- lfp_1[lfp_1$`Country Code` %in% Code, ]
colnames(lfp_1) <- gsub("\\[.*\\]", "", colnames(lfp_1))

write.xlsx(lfp_1, "E:/Thais/Tese_Mestrado/tese_bases/lfp/lfp_1.xlsx")

lfp_2 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/lfp/countries_list_with_codes.xlsx")
lfp_2[,-(1:2)] <- lapply(lfp_2[,-(1:2)], function(x) round(as.numeric(x), 2))

# merge data
lfp_merged <- lfp_2 %>%
  full_join(lfp_1, by = "Country Code", suffix = c(".lfp2", ".lfp1"))

# Replace missing values in lfp_2 with values from lfp_1
for (year in c("1991", "1995", "2000", "2005", "2010", "2020", "2022")) {
  lfp_merged[[year]] <- ifelse(is.na(lfp_merged[[paste0(year, ".lfp2")]]),
                               lfp_merged[[paste0(year, ".lfp1")]],
                               lfp_merged[[paste0(year, ".lfp2")]])
}

# Select only the relevant columns
lfp_final <- lfp_merged %>%
  select(`Country Name`, `Country Code`, `1950`, `1960`, `1970`, `1980`, `1991`, `1995`, `2000`, `2005`, `2010`, `2020`, `2022`)




