
rm(list = setdiff(ls()))


### Load Packages (and install packages if needed)
load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr", "haven", "openxlsx", "fixest","car", "pdftools")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


############################
### UN DEMOGRAPHIC YEARBOOK
############################
# Number of divorces
## https://unstats.un.org/unsd/demographic-social/products/dyb/dybsets/1990%20DYB.pdf

n_div_un_dem_ybook_1990 <- pdf_text("E:/Thais/Tese_Mestrado/tese_bases/divorce/n_div_un_dem_ybook_1990.pdf")

## https://ourworldindata.org/grapher/divorces-per-1000-people?time=1960..2000&country=GBR~USA~SGP~NOR~PER~CHL~MEX~TUR~KOR~IRL~BRA~ALB~DZA~ASM~AGO~AIA~ATG~ARM~YEM~ESH~VNM~VEN~UZB~URY~VIR~ARE~UKR~TCA~TKM~TUN~TTO~TON~THA~TJK~SYR~CHE~SWE~SUR~LKA~ESP~ZAF~SVN~SVK~SYC~SRB~SAU~SMR~WSM~VCT~Saint+Pierre+et+Miquelon~LCA~KNA~RUS~ROU~REU~QAT~PRI~PRT~POL~PAN~PSE~MNP~MKD~PRK~NFK~NIU~NIC~NZL~NCL~ANT~NLD~NRU~MOZ~MSR~MNE~MNG~MCO~MDA~MUS~MTQ~MHL~MLT~MDV~MAC~LUX~LTU~LIE~LBY~LBN~LVA~KGZ~KWT~OWID_KOS~KAZ~JOR~JPN~JAM~ITA~ISR~IMN~IRQ~IRN~IDN~ISL~HUN~HKG~HND~GUY~GNB~GTM~GUM~GLP~GRD~GRL~GRC~GIB~DEU~GEO~PYF~GUF~France+%28metropolitan%29~FRA~FIN~FJI~FRO~FLK~European+Union+%2828+countries%29~ETH~SWZ~EST~SLV~EGY~ECU~DOM~DMA~DJI~DNK~CZE~CYP~CUB~HRV~CIV~CRI~COK~COM~COL~CHN~OWID_CIS~CYM~CPV~CAN~BGR~BRN~VGB~BWA~BIH~BMU~BLZ~BEL~BLR~BRB~BHR~BHS~AZE~AUT~AUS~ABW

## Ler o arquivo CSV
n_div <- read.csv("E:/Thais/Tese_Mestrado/tese_bases/divorce/divorces-per-1000-people.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(n_div) <- c("Entity", "Code", "Year", "Crude_divorce_rate_per_1000")
n_div <- n_div[-1, ]

# Filtrar os países de interesse
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
n_div <- n_div[n_div$Code %in% Code, ]

# Garantir que a coluna Year seja numérica
n_div$Year <- as.numeric(n_div$Year)

# Anos de interesse
years_of_interest <- c(1970, 1980, 1990, 1991)

# Função para encontrar o ano mais próximo
find_closest_year <- function(year, years) {
  return(years[which.min(abs(years - year))])
}

# Adicionar coluna com o ano mais próximo
n_div$closest_year <- sapply(n_div$Year, function(y) {
  find_closest_year(y, years_of_interest)
})

# Filtrar para manter os anos mais próximos dos anos de interesse
n_div <- n_div[n_div$closest_year %in% years_of_interest, ]

# Para cada país, manter apenas o ano mais próximo de cada ano de interesse
n_div <- do.call(rbind, lapply(unique(n_div$Code), function(code) {
  df_country <- n_div[n_div$Code == code, ]
  # Manter o ano mais próximo de cada ano de interesse
  final_df <- do.call(rbind, lapply(years_of_interest, function(year) {
    closest_year <- find_closest_year(year, df_country$Year)
    df_country[df_country$Year == closest_year, ][1, ]
  }))
  return(final_df)
}))

# Remover a coluna temporária 'closest_year'
# n_div$closest_year <- NULL
n_div <- unique(n_div)

# renomeia colunas
n_div <- n_div %>%
  rename(
    real_year = Year,
    Year = closest_year
  )

n_div$Crude_divorce_rate <- n_div$Crude_divorce_rate_per_1000 / 1000


# outra forma divorcio----------
n_div_manual <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/divorce/div_manual.xlsx")



# Total population per country
#https://data.worldbank.org/indicator/SP.POP.TOTL?end=2000&start=1960

pop <- read_xls("E:/Thais/Tese_Mestrado/tese_bases/divorce/API_SP.POP.TOTL_DS2_en_excel_v2_3401611.xls")
colnames(pop) <- pop[3, ]
pop <- pop[, -c(3, 4)]
pop <- pop[-c(1:3), ]
names(pop)[names(pop) == "Country Code"] <- "Code"
# keep countries
Code = c("BOL", "PER", "DEU", "JPN", "PRY", "PRT", "ITA", 
         "ESP", "AFR", "IND", "USA", "COL", "URY", 
         "VEN", "CHN", "GUY", "FRA", "ARG", "CHL", 
         "HUN", "LBN", "AUT", "CRI", "RUS", "ROU", 
         "TUR", "CSK", "POL", "CUB", "CHE", "NIC", 
         "PAN", "BEL", "ASI", "EGY", "GBR", "GRC", 
         "GTM", "SYR", "KOR", "NLD", "YUG", "PAK", 
         "IRL", "ECU", "ISR", "DNK", "AUS", "MEX", 
         "GUF", "SUR", "SWE", "AMR", "HND", 
         "CAN", "SLV", "FIN", "DOM", "BGR", "NOR")
pop <- pop[pop$Code %in% Code, ]

# keep years 1960, 1970, 1980, 1991
cols <- c("Country Name", "Code", "1960", "1970", "1980", "1990", "1991")

# Filtrar as colunas do dataframe 'pop'
pop <- pop[, cols]

# Transformar os dados para o formato longo
pop_long <- melt(pop, id.vars = "Code", variable.name = "Year", value.name = "Population")
pop_long$Year <- as.numeric(gsub("X", "", pop_long$Year))  # Limpar o prefixo "X" se houver
pop_long <- na.omit(pop_long)
pop_long$Population <- as.numeric(pop_long$Population)

# Mesclar dados de divórcio com população
n_div_pop <- merge(n_div, pop_long, by = c("Code", "Year"))

# Calcular o número estimado de divórcios
n_div_pop$Estimated_divorces <- (n_div_pop$Crude_divorce_rate_per_1000 / 1000) * n_div_pop$Population

n_div_pop$`Entity`[n_div_pop$`Entity` == "South Korea"] <- "Korea"

n_div_pop <- n_div_pop[, c("Year", "Entity", "Estimated_divorces")]

names(n_div_pop)[names(n_div_pop) == "Entity"] <- "Country"


# Number of married people
## https://unstats.un.org/unsd/demographic/products/dyb/dybhist.htm

# married_1948_1997 <- read_csv("E:/Thais/Tese_Mestrado/tese_bases/married/HistTab12.csv", locale = locale(encoding = "ISO-8859-1"))
# married_1948_1997 <- write_xlsx(married_1948_1997, "E:/Thais/Tese_Mestrado/tese_bases/married/HistTab12.xlsx")
# married_1948_1997 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/married/HistTab12.xlsx")
married_1948_1997 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/married/hist_tratada.xlsx")
married_1948_1997 <- married_1948_1997 %>%
  mutate(Country = case_when(
    Country %in% c(
      "United Kingdom - Royaume-Uni",
      "United Kingdom: England and Wales - Royaume-Uni: Angleterre et Galle",
      "United Kingdom: Northern Ireland - Royaume-Uni: Irlande du Nord",
      "United Kingdom: Scotland - Royaume-Uni: Ecosse"
    ) ~ Country,
    TRUE ~ sub(" -.*|:.*| \\[.*", "", Country)
  ))

married_1948_1997 <- married_1948_1997 %>%
  mutate(Country = case_when(
    Country == "Korea (Republic of)" ~ "Korea",
    Country == "Syrian Arab Republic" ~ "Syria",
    TRUE ~ Country
  ))
married_1948_1997 <- married_1948_1997 %>%
  mutate(Year = str_extract(Year, "\\d{4}"))
married_1948_1997 <- married_1948_1997 %>%
  mutate(
    Type...4 = sub(" -.*", "", Type...4),  
    Type...5 = sub(" -.*", "", Type...5)   
  )
married_1948_1997 <- married_1948_1997 %>%
  mutate(Year = as.numeric(Year))

married_1948_1997 <- married_1948_1997 %>%
  filter(Year > 1960)

# Convert the 15_plus column to numeric if it isn't already
married_1948_1997$`15_plus` <- as.numeric(married_1948_1997$`15_plus`)
married_1948_1997$`All ages` <- as.numeric(married_1948_1997$`All ages`)


# Sum 15_plus by Type...4, aggregating across Sex (Male and Female)
married_1948_1997 <- married_1948_1997 %>%
  group_by(Country, Year,Type...4) %>%
  summarise(Total_15_plus = sum(`15_plus`, na.rm = TRUE),
            Total_All_ages = sum(`All ages`, na.rm = TRUE))

married_1948_1997 <- married_1948_1997 %>%
  filter(grepl("married", Type...4, ignore.case = TRUE))

married_1948_1997 <- married_1948_1997 %>%
  group_by(Country, Year) %>%
  slice_max(Total_15_plus, with_ties = FALSE) %>%
  slice_max(Total_All_ages, with_ties = FALSE) %>%
  ungroup()

married_1948_1997 <- married_1948_1997 %>%
  mutate(
    Married = ifelse(Total_15_plus == 0, Total_All_ages, Total_15_plus),
    Used_All_ages = ifelse(Total_15_plus == 0, "Yes", "No")
  )

# UK
# Filter for the specific UK regions
uk_regions_data <- married_1948_1997 %>%
  filter(Country %in% c(
    "United Kingdom - Royaume-Uni",
    "United Kingdom: England and Wales - Royaume-Uni: Angleterre et Galle",
    "United Kingdom: Northern Ireland - Royaume-Uni: Irlande du Nord",
    "United Kingdom: Scotland - Royaume-Uni: Ecosse"
  ))

# Summarize the data by Year, summing the Married column for the specified regions
summarized_uk_regions <- uk_regions_data %>%
  group_by(Year) %>%
  summarise(
    Country = "United Kingdom",
    Type...4 = "Married",
    Total_15_plus = sum(Total_15_plus, na.rm = TRUE),
    Total_All_ages = sum(Total_All_ages, na.rm = TRUE),
    Married = sum(Married, na.rm = TRUE),
    Used_All_ages = ifelse(any(Used_All_ages == "Yes"), "Yes", "No"),
    .groups = 'drop'
  )

# Combine the summarized data with the original dataset
married_1948_1997 <- bind_rows(married_1948_1997, summarized_uk_regions)

married_1948_1997 <- married_1948_1997 %>%
  filter(!Country %in% c(
    "United Kingdom - Royaume-Uni",
    "United Kingdom: England and Wales - Royaume-Uni: Angleterre et Galle",
    "United Kingdom: Northern Ireland - Royaume-Uni: Irlande du Nord",
    "United Kingdom: Scotland - Royaume-Uni: Ecosse"
  ))


married_1948_1997 <- married_1948_1997 %>%
  group_by(Country) %>%
  mutate(
    diff_to_1970 = abs(Year - 1970),
    ano_proximo_1970 = ifelse(diff_to_1970 == min(diff_to_1970), Year, NA),
    diff_to_1980 = abs(Year - 1980),
    ano_proximo_1980 = ifelse(diff_to_1980 == min(diff_to_1980), Year, NA),
    diff_to_1990 = abs(Year - 1990),
    ano_proximo_1990 = ifelse(diff_to_1990 == min(diff_to_1990), Year, NA)
  ) %>%
  ungroup() %>%
  fill(ano_proximo_1970, .direction = "downup") %>%
  fill(ano_proximo_1980, .direction = "downup") %>%
  fill(ano_proximo_1990, .direction = "downup") %>%
  dplyr::select(-diff_to_1970, -diff_to_1980, -diff_to_1990)  # Use dplyr::select to avoid conflicts

married_1948_1997 <- married_1948_1997 %>%
  mutate(closest_decade = case_when(
    abs(Year - 1960) <= abs(Year - 1970) & abs(Year - 1960) <= abs(Year - 1980) & abs(Year - 1960) <= abs(Year - 1990) ~ 1960,
    abs(Year - 1970) <= abs(Year - 1960) & abs(Year - 1970) <= abs(Year - 1980) & abs(Year - 1970) <= abs(Year - 1990) ~ 1970,
    abs(Year - 1980) <= abs(Year - 1960) & abs(Year - 1980) <= abs(Year - 1970) & abs(Year - 1980) <= abs(Year - 1990) ~ 1980,
    abs(Year - 1990) <= abs(Year - 1960) & abs(Year - 1990) <= abs(Year - 1970) & abs(Year - 1990) <= abs(Year - 1980) ~ 1990
  ))

# Rearrange columns to place closest_decade next to Year
married_1948_1997 <- married_1948_1997 %>%
  dplyr::select(Country, Year, closest_decade, everything())

married_1948_1997 <- married_1948_1997 %>%
  filter(closest_decade != 1960)

missing_decades <- married_1948_1997 %>%
  mutate(Decade = case_when(
    closest_decade >= 1970 & closest_decade < 1980 ~ "1970s",
    closest_decade >= 1980 & closest_decade < 1990 ~ "1980s",
    closest_decade >= 1990 & closest_decade < 2000 ~ "1990s"
  )) %>%
  group_by(Country) %>%
  summarise(
    Missing_Decades = paste(setdiff(c("1970s", "1980s", "1990s"), unique(Decade)), collapse = ", ")
  ) %>%
  mutate(Missing_Decades = ifelse(Missing_Decades == "", "None", Missing_Decades))

married_1948_1997 <- married_1948_1997[c("Country", "Year", "closest_decade", 
                               "Married")]
married_1948_1997$Year <- as.numeric(married_1948_1997$Year)
married_1948_1997$closest_decade <- as.numeric(married_1948_1997$closest_decade)

# Manually update the values for row 14 and 15
married_1948_1997[14, "closest_decade"] <- 1970  # Change Year for row 14

married_1948_1997[15, "closest_decade"] <- 1980  # Change Year for row 15

married_1948_1997 <- married_1948_1997 %>%
  group_by(Country, closest_decade) %>%
  filter(abs(Year - closest_decade) == min(abs(Year - closest_decade))) %>%
  ungroup()

married_1948_1997$Year <- NULL

# Rename closest_decade to Year
names(married_1948_1997)[names(married_1948_1997) == "closest_decade"] <- "Year"

# ## nova base----
# #https://w3.unece.org/PXWeb2015/pxweb/en/STAT/STAT__30-GE__01-Pop/005_en_GEPOPop5YearMaSta_r.px/
# married_europe_1980_2023_all <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/married/c0000435_20240723-141532.xlsx", skip = 2)
# married_europe_1980_2023_all <- married_europe_1980_2023_all[, -(1:3)]
# married_europe_1980_2023_all <- married_europe_1980_2023_all[1:56, ]
# colnames(married_europe_1980_2023_all)[1] <- "Country"
# 
# # Manipulate divorce and married df
# # Ensure country names match exactly in both datasets
# # div_1997_2001$Country <- trimws(div_1997_2001$Country)
# # married_europe_1980_2023_all$Country <- trimws(married_europe_1980_2023_all$Country)
# 
# write_xlsx(married_europe_1980_2023_all, "E:/Thais/Tese_Mestrado/tese_bases/married/married_europe_1980_2023_all.xlsx")
# 
# 
# 
# married_europe_1980_2023_all <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/married/married_europe_1980_2023_all.xlsx")
# 
# # Converter as colunas de anos para numérico
# married_europe_1980_2023_all[, -1] <- lapply(married_europe_1980_2023_all[, -1], as.numeric)
# 
# # General function to replace NA with closest year value for a specific year
# # Função para substituir NA no ano 2000 pelo valor mais próximo
# replace_na_with_closest <- function(row) {
#   if (is.na(row["2000"])) {
#     # Obter os nomes dos anos que não são NA
#     non_na_years <- names(row)[!is.na(row)]
#     
#     # Se não houver anos não-NA, retorne a linha como está
#     if (length(non_na_years) == 0) {
#       row["Closest_Year"] <- NA
#       return(row)
#     }
#     
#     # Converter os nomes dos anos para numérico
#     numeric_years <- as.numeric(non_na_years)
#     
#     # Encontrar o ano mais próximo de 2000
#     closest_year <- numeric_years[which.min(abs(numeric_years - 2000))]
#     
#     # Substituir o valor NA no ano 2000 pelo valor do ano mais próximo
#     row["2000"] <- row[as.character(closest_year)]
#     
#     # Adicionar a coluna de ano mais próximo
#     row["Closest_Year"] <- as.character(closest_year)
#   } else {
#     # Se o valor no ano 2000 não é NA, definir "Closest_Year" como "2000"
#     row["Closest_Year"] <- "2000"
#   }
#   return(row)
# }
# 
# # Aplicar a função linha por linha
# married_europe_1980_2023_all <- as.data.frame(married_europe_1980_2023_all)  # Converter para data.frame se necessário
# # Adicionar a coluna "Closest_Year" inicialmente
# married_europe_1980_2023_all$Closest_Year <- NA
# 
# # Iterar sobre cada linha e aplicar a função
# for (i in 1:nrow(married_europe_1980_2023_all)) {
#   married_europe_1980_2023_all[i, ] <- replace_na_with_closest(married_europe_1980_2023_all[i, ])
# }
# 
# # Adicionar a coluna "Closest_Year" inicialmente
# married_europe_1980_2023_all$Closest_Year <- NA
# 
# # Iterar sobre cada linha e aplicar a função
# for (i in 1:nrow(married_europe_1980_2023_all)) {
#   married_europe_1980_2023_all[i, ] <- replace_na_with_closest(married_europe_1980_2023_all[i, ])
# }
# 
# # Verificar o DataFrame transformado
# head(married_europe_1980_2023_all)
# 
# # Organiza colunas 
# married_europe_1980_2023_all <- married_europe_1980_2023_all[, c("Country", "2000", "Closest_Year")]
# 
# # Saber se países do censo estao na base married_europe_1980_2023_all
# paises_lista <- c("Bolivia", "Peru", "Germany", "Japan", "Paraguay", "Portugal", "Italy", 
#                   "Spain", "Africa - others", "India", "United States", "Colombia", "Uruguay", 
#                   "Venezuela", "China", "Guyana", "France", "Argentina", "Chile", 
#                   "Hungary", "Lebanon", "Austria", "Costa Rica", "Russian Federation", "Romania", 
#                   "Turkey", "Czechoslovakia", "Poland", "Cuba", "Switzerland", "Nicaragua", 
#                   "Panama", "Belgium", "Asia - others", "Egypt", "United Kingdom", "Greece", 
#                   "Guatemala", "Syria", "Korea", "Netherlands", "Yugoslavia", "Pakistan", 
#                   "Ireland", "Ecuador", "Israel", "Denmark", "Australia", "Mexico", 
#                   "French Guiana", "Suriname", "Sweden", "America - others", "Honduras", 
#                   "Canada", "El Salvador", "Finland", "Dominican Republic", "Bulgaria", "Norway")
# 
# paises_presentes <- paises_lista %in% married_europe_1980_2023_all$Country
# 
# resultado_married_europe_1980_2023 <- data.frame(Pais = paises_lista, Presente = paises_presentes)
# resultado_married_europe_1980_2023
# 
# # Tira coluna "Closest Year"
# married_europe_1980_2023_all <- married_europe_1980_2023_all[, !names(married_europe_1980_2023_all) %in% "Closest_Year"]
# 
# country_codes <- c(
#   "ARG", "AUS", "AUT", "BEL", "BOL", "BGR", "CAN", "CHL", "CHN",
#   "COL", "CRI", "CUB", "DNK", "DOM", "ECU", "EGY", "SLV", "FIN",
#   "CSK", "YUG", "FRA", "GUF", "DEU", "GRC", "GTM", "GUY", "HND",
#   "HUN", "IND", "IRL", "ISR", "ITA", "JPN", "KOR", "MEX", "ANT",
#   "NIC", "NOR", "PAK", "PAN", "PRY", "PER", "POL", "PRT", "ROU",
#   "RUS", "ESP", "SWE", "CHE", "SYR", "TUR", "USA", "VIR", "URY",
#   "VEN", "YUG", "GBR"
# )
# 
# # Ensure the country names in the data match the country names in the code list
# stopifnot(all(unique(married_1948_1997$Country) == names(country_codes)))
# 
# # Add the country codes to the data frame
# married_1948_1997$Country_Code <- country_codes[match(married_1948_1997$Country, unique(married_1948_1997$Country))]
# 
# # Reorder columns to place Country_Code next to Country
# married_1948_1997 <- married_1948_1997[, c("Country", "Country_Code", setdiff(names(married_1948_1997), c("Country", "Country_Code")))]
#-----
married_1948_1997 <- married_1948_1997[, !names(married_1948_1997) %in% "Type...4"]

# Criando um vetor com os códigos dos países
country_codes <- c("ARG", "AUS", "AUT", "BEL", "BOL", "BGR", "CAN", "CHL", "CHN", 
                   "COL", "CRI", "CUB", "DNK", "DOM", "ECU", "EGY", "SLV", "FIN", 
                   "CZE", "YUG", "FRA", "GUF", "DEU", "GRC", "GTM", "GUY", "HND", 
                   "HUN", "IND", "IRL", "ISR", "ITA", "JPN", "KOR", "MEX", "ANT", 
                   "NIC", "NOR", "PAK", "PAN", "PRY", "PER", "POL", "PRT", "ROU", 
                   "RUS", "ESP", "SWE", "CHE", "SYR", "TUR", "USA", "VIR", "URY", 
                   "VEN", "YUG", "GBR")

# Adicionando a coluna Country_Code ao dataframe
married_1948_1997$Country_Code <- country_codes[match(married_1948_1997$Country, unique(married_1948_1997$Country))]

writexl::write_xlsx(married_1948_1997, path="E:/Thais/Tese_Mestrado/tese_bases/married/n_married_1970_1990.xlsx")

married_1948_1997 <- read_xlsx("E:/Thais/Tese_Mestrado/tese_bases/married/n_married_1970_1990.xlsx")


# hcdr

divorces_long_all <- n_div_manual %>%
  pivot_longer(
    cols = -c(Country, Country_Code), 
    names_to = "Year",
    values_to = "Divorces"
  )
# married_1948_1997$Country <- NULL
# divorces_long_all$Country <- NULL


home_country_div_rate_2 <- merge(married_1948_1997, divorces_long_all, by = c("Country_Code", "Year"))

home_country_div_rate_2 <- home_country_div_rate_2[, !names(home_country_div_rate_2) %in% c("Country.x", "Country.y", "Country_igual")]


# Calculate divorces per 100 married inhabitants
home_country_div_rate_2 <- home_country_div_rate_2 %>%
  mutate(Divorces_per_100 = (Divorces / Married) * 100) %>%
  filter(!is.na(Divorces_per_100))

# Transformar a coluna n_divorce em numeric dentro do mutate
home_country_div_rate_2 <- home_country_div_rate_2 %>%
  mutate(
    Divorces_per_100 = (Divorces / Married) * 100
  ) %>%
  filter(!is.na(Divorces_per_100))

names(home_country_div_rate_2)[names(home_country_div_rate_2) == "Country_Code"] <- "pais_nascim"

names(home_country_div_rate_2) <- tolower(names(home_country_div_rate_2))

names(home_country_div_rate_1990) <- c("pais_nascim", "year_hcdr", "married", "divorces", "hcdr")

home_country_div_rate_1990 <- home_country_div_rate_2 %>% filter(year_hcdr == 1990)
home_country_div_rate_1980 <- home_country_div_rate_2 %>% filter(year_hcdr == 1980)
home_country_div_rate_1970 <- home_country_div_rate_2 %>% filter(year_hcdr == 1970)

writexl::write_xlsx(home_country_div_rate_1990, path="E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate_1990.xlsx")
writexl::write_xlsx(home_country_div_rate_1980, path="E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate_1980.xlsx")
writexl::write_xlsx(home_country_div_rate_1970, path="E:/Thais/Tese_Mestrado/tese_bases/home_country_div_rate/home_country_div_rate_1970.xlsx")
