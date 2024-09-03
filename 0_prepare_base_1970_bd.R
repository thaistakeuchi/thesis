## PREPARA BASE 1970
# Thais Takeuchi 30/8/24

### Load Packages (and install packages if needed)
load.lib <- c("data.table","foreign","stargazer","devtools","stringi", "srvyr", "survey","tidyverse","gtools", "remote","installr","microdadosBrasil","ggplot2","viridis","hrbrthemes","WDI","dplyr", "arrow", "readxl","rio","writexl","dineq", "basedosdados", "httr")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

set_billing_id("august-tower-429517-m7")

# Para carregar o dado direto no R

# Para carregar o dado direto no R
query <- "
WITH 
dicionario_v038 AS (
    SELECT
        chave AS chave_v038,
        valor AS descricao_v038
    FROM `basedosdados.br_ibge_censo_demografico.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'v038'
        AND id_tabela = 'microdados_pessoa_1970'
)
SELECT
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    id_domicilio,
    numero_familia,
    ordem,
    v001,
    v002,
    v003,
    v022,
    v023,
    v024,
    v025,
    v026,
    v027,
    v028,
    v029,
    v030,
    v031,
    v032,
    v033,
    v034,
    v035,
    v036,
    v037,
    descricao_v038 AS v038,
    v039,
    v040,
    v041,
    v042,
    v043,
    v044,
    v045,
    v046,
    v047,
    v048,
    v049,
    v050,
    v051,
    v052,
    v053,
    v054
FROM `basedosdados.br_ibge_censo_demografico.microdados_pessoa_1970` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN `dicionario_v038`
    ON dados.v038 = chave_v038
"

cp_1970_bd <- read_sql(query, billing_project_id = get_billing_id())

setwd("E:/Thais/Tese_Mestrado/censo_1970")
save.image("censo_1970_bd.RData")

# filtra estrangeiros e brasileiros naturalizados
cp1970_bd_f <- cp_1970_bd %>% 
    filter(v029 == 1| v029 == 2)
# filtra idade
cp1970_bd_f <- cp1970_bd_f %>% 
    filter(v027 >= 25 & v027 <= 64)

write_parquet(cp1970_bd_f, "E:/Thais/Tese_Mestrado/censo_1970/censo_pessoas_1970_filtrado_bd.parquet")
haven::write_dta(cp1970_bd_f, "E:/Thais/Tese_Mestrado/censo_1970/censo_pessoas_1970_filtrado_bd.dta")

setwd("E:/Thais/Tese_Mestrado/censo_1970")
load("censo_1970_bd.RData")

# compara com a sidra (tese_mestrado.xlsx)

# numero de mulheres 
mulher <- cp_1970_bd |> filter(v023 == 1)
sum(mulher$v054)

# numero de homens 
homem <- cp_1970_bd |> filter(v023 == 0)
sum(homem$v054)



