### 1991
# Lista de estados correspondentes aos códigos UFs
uf_names <- c(
  "Mato Grosso", "Acre", "São Paulo", "Mato Grosso do Sul", "Santa Catarina", "Pará",
  "Amazonas", "Paraná", "Sergipe", "Rio de Janeiro", "Rio Grande do Sul", "Amapá",
  "Tocantins", "Distrito Federal", "Goiás", "Piauí", "Minas Gerais", "Pernambuco",
  "Rio Grande do Norte", "Espírito Santo", "Maranhão", "Rondônia", "Bahia",
  "Ceará", "Paraíba", "Alagoas", "Roraima"
)

# Loop para rodar as regressões e imprimir resultados
for (i in seq_along(ufs)) {
  uf <- ufs[i]
  uf_name <- uf_names[i]
  
  # Nome da base de dados dinamicamente
  data_name <- paste0("data_1991_", uf)
  
  # Rodar a regressão
  model_name <- paste0("work_1991_", uf, "_state")
  assign(
    model_name,
    feols(
      V0354 ~ lfp_uf_30_49_1970 + fund_mulher + em_mulher + superior_mulher + 
        fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + 
        age_sp + age_squared_sp + children_under_5 | V1102,
      data = get(data_name),
      weights = ~V7301,
      cluster = ~uf_nascim
    )
  )
  
  # Imprimir resultados com o nome do estado
  cat("\n\n==========================\n")
  cat("Results for State: ", uf_name, "\n")
  cat("==========================\n")
  print(summary(get(model_name)))
  
  # Gerar tabela em LaTeX com nome do estado
  etable(
    get(model_name), 
    tex = TRUE, 
    dict = var_labels, 
    digits = "r4",
    title = paste("Regression Results for", uf_name)
  )
}


### 2000
uf_names <- c(
  "Mato Grosso", "Acre", "São Paulo", "Mato Grosso do Sul", "Santa Catarina", "Pará",
  "Amazonas", "Paraná", "Sergipe", "Rio de Janeiro", "Rio Grande do Sul", "Amapá",
  "Tocantins", "Distrito Federal", "Goiás", "Piauí", "Minas Gerais", "Pernambuco",
  "Rio Grande do Norte", "Espírito Santo", "Maranhão", "Rondônia", "Bahia",
  "Ceará", "Paraíba", "Alagoas", "Roraima"
)

# Loop para rodar as regressões e imprimir resultados
for (i in seq_along(ufs)) {
  uf <- ufs[i]
  uf_name <- uf_names[i]
  
  # Nome da base de dados dinamicamente
  data_name <- paste0("data_2000_", uf)
  
  # Rodar a regressão
  model_name <- paste0("work_2000_", uf, "_state")
  assign(
    model_name,
    feols(
      v4534 ~ lfp_uf_30_49_1980 + fund_mulher + em_mulher + superior_mulher + 
        fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + 
        age_sp + age_squared_sp + children_under_5 | v0103,
      data = get(data_name),
      weights = ~P001,
      cluster = ~uf_nascim
    )
  )
  
  # Imprimir resultados com o nome do estado
  cat("\n\n==========================\n")
  cat("Results for State: ", uf_name, "\n")
  cat("==========================\n")
  print(summary(get(model_name)))
  
  # Gerar tabela em LaTeX com nome do estado
  etable(
    get(model_name), 
    tex = TRUE, 
    dict = var_labels, 
    digits = "r4",
    title = paste("Regression Results for", uf_name)
  )
}

### 2010
# Lista de estados correspondentes aos códigos UFs
uf_names <- c(
  "Mato Grosso", "Acre", "São Paulo", "Mato Grosso do Sul", "Santa Catarina", "Pará",
  "Amazonas", "Paraná", "Sergipe", "Rio de Janeiro", "Rio Grande do Sul", "Amapá",
  "Tocantins", "Distrito Federal", "Goiás", "Piauí", "Minas Gerais", "Pernambuco",
  "Rio Grande do Norte", "Espírito Santo", "Maranhão", "Rondônia", "Bahia",
  "Ceará", "Paraíba", "Alagoas", "Roraima"
)

# Loop para rodar as regressões e imprimir resultados
for (i in seq_along(ufs)) {
  uf <- ufs[i]
  uf_name <- uf_names[i]
  
  # Nome da base de dados dinamicamente
  data_name <- paste0("data_2010_", uf)
  
  # Rodar a regressão
  model_name <- paste0("work_2010_", uf, "_state")
  assign(
    model_name,
    feols(
      ocup_migrante ~ lfp_uf_30_49_1991 + fund_mulher + em_mulher + superior_mulher + 
        fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + 
        age_sp + age_squared_sp + children_under_5 | munic,
      data = get(data_name),
      weights = ~peso_pess,
      cluster = ~uf_nascim
    )
  )
  
  # Imprimir resultados com o nome do estado
  cat("\n\n==========================\n")
  cat("Results for State: ", uf_name, "\n")
  cat("==========================\n")
  print(summary(get(model_name)))
  
  # Gerar tabela em LaTeX com nome do estado
  etable(
    get(model_name), 
    tex = TRUE, 
    dict = var_labels, 
    digits = "r4",
    title = paste("Regression Results for", uf_name)
  )
}

# children
# 1991
# Loop para rodar as regressões e imprimir resultados
for (i in seq_along(ufs)) {
  uf <- ufs[i]
  uf_name <- uf_names[i]
  
  # Nome da base de dados dinamicamente
  data_name <- paste0("data_1991_", uf)
  
  # Rodar a regressão
  model_name <- paste0("children_1991_", uf, "_state")
  assign(
    model_name,
    feols(
      children~ tfr_1970 + fund_mulher + em_mulher + superior_mulher + 
        fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + 
        age_sp + age_squared_sp + children_under_5 | V1102,
      data = get(data_name),
      weights = ~V7301,
      cluster = ~uf_nascim
      
    )
  )
  
  # Imprimir resultados com o nome do estado
  cat("\n\n==========================\n")
  cat("Results for State: ", uf_name, "\n")
  cat("==========================\n")
  print(summary(get(model_name)))
  
  # Gerar tabela em LaTeX com nome do estado
  etable(
    get(model_name), 
    tex = TRUE, 
    dict = var_labels, 
    digits = "r4",
    title = paste("Regression Results for", uf_name)
  )
}

# 2000
# Loop para rodar as regressões e imprimir resultados
for (i in seq_along(ufs)) {
  uf <- ufs[i]
  uf_name <- uf_names[i]
  
  # Nome da base de dados dinamicamente
  data_name <- paste0("data_2000_", uf)
  
  # Rodar a regressão
  model_name <- paste0("work_2000_", uf, "_state")
  assign(
    model_name,
    feols(
      children ~ tfr_1980 + fund_mulher + em_mulher + superior_mulher + 
        fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + 
        age_sp + age_squared_sp + children_under_5 | v0103,
      data = get(data_name),
      weights = ~P001,
      cluster = ~uf_nascim
    )
  )
  
  # Imprimir resultados com o nome do estado
  cat("\n\n==========================\n")
  cat("Results for State: ", uf_name, "\n")
  cat("==========================\n")
  print(summary(get(model_name)))
  
  # Gerar tabela em LaTeX com nome do estado
  etable(
    get(model_name), 
    tex = TRUE, 
    dict = var_labels, 
    digits = "r4",
    title = paste("Regression Results for", uf_name)
  )
}

# 2010
# Loop para rodar as regressões e imprimir resultados
for (i in seq_along(ufs)) {
  uf <- ufs[i]
  uf_name <- uf_names[i]
  
  # Nome da base de dados dinamicamente
  data_name <- paste0("data_2010_", uf)
  
  # Rodar a regressão
  model_name <- paste0("work_2010_", uf, "_state")
  assign(
    model_name,
    feols(
      children ~ tfr_1991 + fund_mulher + em_mulher + superior_mulher + 
        fund_homem + em_homem + superior_homem + income_sp_raw + age_fem + age_squared_fem + 
        age_sp + age_squared_sp + children_under_5 | munic,
      data = get(data_name),
      weights = ~peso_pess,
      cluster = ~uf_nascim
    )
  )
  
  # Imprimir resultados com o nome do estado
  cat("\n\n==========================\n")
  cat("Results for State: ", uf_name, "\n")
  cat("==========================\n")
  print(summary(get(model_name)))
  
  # Gerar tabela em LaTeX com nome do estado
  etable(
    get(model_name), 
    tex = TRUE, 
    dict = var_labels, 
    digits = "r4",
    title = paste("Regression Results for", uf_name)
  )
}
