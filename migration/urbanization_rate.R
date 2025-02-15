# 1970 urbanization rate - Master Thesis

# Thais Takeuchi
# 23/10/2024

data_1970 <- read_dta("D:/1_migration/1_datasets_migrants/censo/nova/censo/1970/censo_1970_temp.dta")

# create urban variable
data_1970$urbano <- NA
data_1970$urbano[data_1970$V004 %in% c(0, 1)] <- 1
data_1970$urbano[data_1970$V004 == 2] <- 0

# uf: https://biblioteca.ibge.gov.br/visualizacao/periodicos/20/aeb_1981.pdf (region)
urbano <- data_1970 |>
  filter(urbano == 1) |>
  group_by(uf) |> 
  summarise(soma_urbano = sum(V054, na.rm = TRUE))

total <- data_1970 |>
  filter(urbano == 1 | urbano == 0) |> 
  group_by(uf) |> 
  summarise(soma_total = sum(V054, na.rm = TRUE))

tx_urban_1970 <- total |>
  left_join(urbano, by = "uf") |>
  mutate(tx_urban_1970 = soma_urbano / soma_total)

somas_uf <- colSums(dplyr::select(tx_urban_1970, starts_with("soma_")), na.rm = TRUE)
somas_uf

result <- somas_uf["soma_urbano"] / somas_uf["soma_total"]
result
