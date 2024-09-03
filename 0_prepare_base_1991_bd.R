## PREPARA BASE 1991
# Thais Takeuchi 31/8/24

rm(list = (ls()))

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
dicionario_v0302 AS (
    SELECT
        chave AS chave_v0302,
        valor AS descricao_v0302
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0302'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0303 AS (
    SELECT
        chave AS chave_v0303,
        valor AS descricao_v0303
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0303'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3046 AS (
    SELECT
        chave AS chave_v3046,
        valor AS descricao_v3046
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3046'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3047 AS (
    SELECT
        chave AS chave_v3047,
        valor AS descricao_v3047
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3047'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3049 AS (
    SELECT
        chave AS chave_v3049,
        valor AS descricao_v3049
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3049'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3151 AS (
    SELECT
        chave AS chave_v3151,
        valor AS descricao_v3151
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3151'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0316 AS (
    SELECT
        chave AS chave_v0316,
        valor AS descricao_v0316
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0316'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0319 AS (
    SELECT
        chave AS chave_v0319,
        valor AS descricao_v0319
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0319'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3191 AS (
    SELECT
        chave AS chave_v3191,
        valor AS descricao_v3191
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3191'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0320 AS (
    SELECT
        chave AS chave_v0320,
        valor AS descricao_v0320
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0320'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0321 AS (
    SELECT
        chave AS chave_v0321,
        valor AS descricao_v0321
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0321'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3211 AS (
    SELECT
        chave AS chave_v3211,
        valor AS descricao_v3211
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3211'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0322 AS (
    SELECT
        chave AS chave_v0322,
        valor AS descricao_v0322
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0322'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0323 AS (
    SELECT
        chave AS chave_v0323,
        valor AS descricao_v0323
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0323'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0328 AS (
    SELECT
        chave AS chave_v0328,
        valor AS descricao_v0328
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0328'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0330 AS (
    SELECT
        chave AS chave_v0330,
        valor AS descricao_v0330
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0330'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0333 AS (
    SELECT
        chave AS chave_v0333,
        valor AS descricao_v0333
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0333'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3342 AS (
    SELECT
        chave AS chave_v3342,
        valor AS descricao_v3342
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3342'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0345 AS (
    SELECT
        chave AS chave_v0345,
        valor AS descricao_v0345
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0345'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0350 AS (
    SELECT
        chave AS chave_v0350,
        valor AS descricao_v0350
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0350'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3562 AS (
    SELECT
        chave AS chave_v3562,
        valor AS descricao_v3562
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3562'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3563 AS (
    SELECT
        chave AS chave_v3563,
        valor AS descricao_v3563
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3563'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3564 AS (
    SELECT
        chave AS chave_v3564,
        valor AS descricao_v3564
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3564'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v3574 AS (
    SELECT
        chave AS chave_v3574,
        valor AS descricao_v3574
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v3574'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0358 AS (
    SELECT
        chave AS chave_v0358,
        valor AS descricao_v0358
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0358'
        AND id_tabela = 'microdados_pessoa_1991'
),
dicionario_v0310 AS (
    SELECT
        chave AS chave_v0310,
        valor AS descricao_v0310
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v0310'
        AND id_tabela = 'microdados_pessoa_1991'
)
SELECT
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    id_questionario,
    numero_ordem,
    v0301,
    descricao_v0302 AS v0302,
    descricao_v0303 AS v0303,
    v3041,
    v3042,
    v3043,
    v3045,
    descricao_v3046 AS v3046,
    descricao_v3047 AS v3047,
    descricao_v3049 AS v3049,
    v3005,
    v3072,
    v0309,
    v0312,
    v0313,
    v0314,
    descricao_v3151 AS v3151,
    v3152,
    descricao_v0316 AS v0316,
    v0317,
    v0318,
    descricao_v0319 AS v0319,
    descricao_v3191 AS v3191,
    descricao_v0320 AS v0320,
    descricao_v0321 AS v0321,
    descricao_v3211 AS v3211,
    descricao_v0322 AS v0322,
    descricao_v0323 AS v0323,
    v0327,
    descricao_v0328 AS v0328,
    v3241,
    descricao_v0330 AS v0330,
    v3311,
    v3312,
    v0332,
    descricao_v0333 AS v0333,
    v3341,
    descricao_v3342 AS v3342,
    descricao_v0345 AS v0345,
    descricao_v0350 AS v0350,
    v0354,
    v0355,
    v0356,
    v3561,
    descricao_v3562 AS v3562,
    descricao_v3563 AS v3563,
    descricao_v3564 AS v3564,
    v0357,
    descricao_v3574 AS v3574,
    descricao_v0358 AS v0358,
    v3351,
    v3352,
    v3353,
    v3354,
    v3355,
    v3356,
    v3360,
    v3361,
    v3362,
    v0335,
    v0336,
    v0337,
    v0338,
    v0343,
    v3443,
    descricao_v0310 AS v0310,
    v7301
FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1991` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN `dicionario_v0302`
    ON dados.v0302 = chave_v0302
LEFT JOIN `dicionario_v0303`
    ON dados.v0303 = chave_v0303
LEFT JOIN `dicionario_v3046`
    ON dados.v3046 = chave_v3046
LEFT JOIN `dicionario_v3047`
    ON dados.v3047 = chave_v3047
LEFT JOIN `dicionario_v3049`
    ON dados.v3049 = chave_v3049
LEFT JOIN `dicionario_v3151`
    ON dados.v3151 = chave_v3151
LEFT JOIN `dicionario_v0316`
    ON dados.v0316 = chave_v0316
LEFT JOIN `dicionario_v0319`
    ON dados.v0319 = chave_v0319
LEFT JOIN `dicionario_v3191`
    ON dados.v3191 = chave_v3191
LEFT JOIN `dicionario_v0320`
    ON dados.v0320 = chave_v0320
LEFT JOIN `dicionario_v0321`
    ON dados.v0321 = chave_v0321
LEFT JOIN `dicionario_v3211`
    ON dados.v3211 = chave_v3211
LEFT JOIN `dicionario_v0322`
    ON dados.v0322 = chave_v0322
LEFT JOIN `dicionario_v0323`
    ON dados.v0323 = chave_v0323
LEFT JOIN `dicionario_v0328`
    ON dados.v0328 = chave_v0328
LEFT JOIN `dicionario_v0330`
    ON dados.v0330 = chave_v0330
LEFT JOIN `dicionario_v0333`
    ON dados.v0333 = chave_v0333
LEFT JOIN `dicionario_v3342`
    ON dados.v3342 = chave_v3342
LEFT JOIN `dicionario_v0345`
    ON dados.v0345 = chave_v0345
LEFT JOIN `dicionario_v0350`
    ON dados.v0350 = chave_v0350
LEFT JOIN `dicionario_v3562`
    ON dados.v3562 = chave_v3562
LEFT JOIN `dicionario_v3563`
    ON dados.v3563 = chave_v3563
LEFT JOIN `dicionario_v3564`
    ON dados.v3564 = chave_v3564
LEFT JOIN `dicionario_v3574`
    ON dados.v3574 = chave_v3574
LEFT JOIN `dicionario_v0358`
    ON dados.v0358 = chave_v0358
LEFT JOIN `dicionario_v0310`
    ON dados.v0310 = chave_v0310
"

cp1991_bd <- read_sql(query, billing_project_id = get_billing_id())

write_parquet(cp1991_bd, "E:/Thais/Tese_Mestrado/censo_1991/censo_pessoas_1991_bd.parquet")

# filtra estrangeiros e brasileiros naturalizados
cp1991_bd_f <- cp1991_bd %>% 
  filter(v511 == 4 | v511 == 6)
# filtra idade
cp1991_bd_f <- cp1991_bd_f %>% 
  filter(v606 >= 25 & v606 <= 64)

write_parquet(cp1991_bd_f, "E:/Thais/Tese_Mestrado/censo_1991/censo_pessoas_1991_filtrado_bd.parquet")
haven::write_dta(cp1991_bd_f, "E:/Thais/Tese_Mestrado/censo_1991/censo_pessoas_1991_filtrado_bd.dta")

setwd("E:/Thais/Tese_Mestrado/censo_1991")
save.image("censo_1991.RData")



WHERE coluna_inteira = CAST('string' AS INT64)









