## PREPARA BASE 1980
# Thais Takeuchi 30/8/24

### Load Packages (and install packages if needed)
load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

set_billing_id("august-tower-429517-m7")

# Para carregar o dado direto no R
query <- "
WITH 
dicionario_v513 AS (
    SELECT
        chave AS chave_v513,
        valor AS descricao_v513
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v513'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v514 AS (
    SELECT
        chave AS chave_v514,
        valor AS descricao_v514
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v514'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v515 AS (
    SELECT
        chave AS chave_v515,
        valor AS descricao_v515
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v515'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v516 AS (
    SELECT
        chave AS chave_v516,
        valor AS descricao_v516
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v516'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v517 AS (
    SELECT
        chave AS chave_v517,
        valor AS descricao_v517
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v517'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v519 AS (
    SELECT
        chave AS chave_v519,
        valor AS descricao_v519
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v519'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v523 AS (
    SELECT
        chave AS chave_v523,
        valor AS descricao_v523
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v523'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v524 AS (
    SELECT
        chave AS chave_v524,
        valor AS descricao_v524
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v524'
        AND id_tabela = 'microdados_pessoa_1980'
),
dicionario_v526 AS (
    SELECT
        chave AS chave_v526,
        valor AS descricao_v526
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v526'
        AND id_tabela = 'microdados_pessoa_1980'
)
SELECT
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    numero_ordem,
    v211,
    v604,
    v598,
    v501,
    v503,
    v504,
    v505,
    v606,
    v508,
    v509,
    v511,
    v512,
    descricao_v513 AS v513,
    descricao_v514 AS v514,
    descricao_v515 AS v515,
    descricao_v516 AS v516,
    descricao_v517 AS v517,
    v518,
    descricao_v519 AS v519,
    descricao_v523 AS v523,
    descricao_v524 AS v524,
    descricao_v526 AS v526,
    v527,
    v528,
    v681,
    v535,
    v680,
    v607,
    v608,
    v541,
    v536,
    v542,
    v554,
    v555,
    v557,
    v570
FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1980` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN `dicionario_v513`
    ON dados.v513 = chave_v513
LEFT JOIN `dicionario_v514`
    ON dados.v514 = chave_v514
LEFT JOIN `dicionario_v515`
    ON dados.v515 = chave_v515
LEFT JOIN `dicionario_v516`
    ON dados.v516 = chave_v516
LEFT JOIN `dicionario_v517`
    ON dados.v517 = chave_v517
LEFT JOIN `dicionario_v519`
    ON dados.v519 = chave_v519
LEFT JOIN `dicionario_v523`
    ON dados.v523 = chave_v523
LEFT JOIN `dicionario_v524`
    ON dados.v524 = chave_v524
LEFT JOIN `dicionario_v526`
    ON dados.v526 = chave_v526
"

cp1980_bd <- read_sql(query, billing_project_id = get_billing_id())
write_parquet(cp1980_bd, "E:/Thais/Tese_Mestrado/censo_1980/censo_pessoas_1980_bd.parquet")

# filtra estrangeiros e brasileiros naturalizados
cp1980_bd_f <- cp1980_bd %>% 
  filter(v511 == 4 | v511 == 6)
# filtra idade
cp1980_bd_f <- cp1980_bd_f %>% 
  filter(v606 >= 25 & v606 <= 64)

write_parquet(cp1980_bd_f, "E:/Thais/Tese_Mestrado/censo_1980/censo_pessoas_1980_filtrado_bd.parquet")
haven::write_dta(cp1980_bd_f, "E:/Thais/Tese_Mestrado/censo_1980/censo_pessoas_1980_filtrado_bd.dta")

setwd("E:/Thais/Tese_Mestrado/censo_1980")
save.image("censo_1980.RData")

# compara com a sidra (tese_mestrado.xlsx)

# numero de mulheres (0,999670763)
mulher <- cp1980_bd |> filter(v501 == "3")
sum(mulher$v604)

# numero de homens (0,999183473)
homem <- cp1980_bd |> filter(v501 == "1")
sum(homem$v604)

