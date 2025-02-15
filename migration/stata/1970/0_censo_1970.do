********************************************************************************
* Censo 1970
********* NAO COMPATIBILIZADA *********
* Set working directory
cd "E:/Thais/Tese_Mestrado/censo_1970"

* Open dataset
use "E:/Thais/Tese_Mestrado/censo_1970/censo_1970_raw.dta", clear

* Filtra observacoes onde idade esta entre 25 e 64
keep if V027 >= 25 & V027 <= 64

* Filtra os estrangeiros e brasileiros naturalizados
keep if V029 == 1 | V029 == 2

* use ANO UF V001 V002 V003 V004 V006 V022 V023 V024 V025 V027 V028 V029 V030 V035 V038 V040 V041 V043 V048 V050 V054 using "E:/Thais/Tese_Mestrado/censo_1970/censo_1970_raw.dta", clear

save "censo_pessoas_1970_filtrado_nao_comp.dta", replace
use "E:/Thais/Tese_Mestrado/censo_1970/censo_pessoas_1970_filtrado_nao_comp.dta", clear


***************
* TX ATIVIDADE
***************

cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1970"

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1970/censo_1970_raw.dta", clear

keep UF V054 V027 ANO V001 V002 V003 V004 V011 V022 V023 V024 V025 V027 V029 V030 V031 V032 V033 V034 V035 V038 V039 V040 V041 V042 V043 V044 V045 V046 V047 V048 V049 V050 V054

* renomeia codigo uf
gen uf_temp = UF

replace uf_temp = 11 if UF == 1    // Rondônia
replace uf_temp = 12 if UF == 2    // Acre
replace uf_temp = 13 if UF == 3    // Amazonas
replace uf_temp = 14 if UF == 4    // Roraima
replace uf_temp = 15 if UF == 5    // Pará
replace uf_temp = 16 if UF == 6    // Amapá
replace uf_temp = 21 if UF == 7    // Maranhão
replace uf_temp = 22 if UF == 8    // Piauí
replace uf_temp = 23 if UF == 9    // Ceará
replace uf_temp = 24 if UF == 10   // Rio Grande do Norte
replace uf_temp = 25 if UF == 11   // Paraíba
replace uf_temp = 26 if UF == 12 | UF == 14   // Pernambuco
replace uf_temp = 27 if UF == 13   // Alagoas
replace uf_temp = 28 if UF == 15   // Sergipe
replace uf_temp = 29 if UF == 16   // Bahia
replace uf_temp = 31 if UF == 17   // Minas Gerais
replace uf_temp = 32 if UF == 18   // Espírito Santo
replace uf_temp = 33 if UF == 19 | UF == 20  // Rio de Janeiro
replace uf_temp = 35 if UF == 21   // São Paulo
replace uf_temp = 41 if UF == 22   // Paraná
replace uf_temp = 42 if UF == 23   // Santa Catarina
replace uf_temp = 43 if UF == 24   // Rio Grande do Sul
replace uf_temp = 51 if UF == 25   // Mato Grosso
replace uf_temp = 52 if UF == 26   // Goiás (TO incluido)
replace uf_temp = 53 if UF == 27   // Distrito Federal

* MS foi criado em 1977
* nao temos TO

rename uf_temp uf

* cria regiao

gen regiao = ""
replace regiao = "Norte" if inlist(uf, 11, 12, 13, 14, 15, 16)
replace regiao = "Nordeste" if inlist(uf, 21, 22, 23, 24, 25, 26, 27, 28, 29)
replace regiao = "Centro-Oeste" if inlist(uf, 51, 52, 53)
replace regiao = "Sudeste" if inlist(uf, 31, 32, 33, 35)
replace regiao = "Sul" if inlist(uf, 41, 42, 43)
replace regiao = "Outros" if regiao == ""

gen cod_regiao = .
replace cod_regiao = 1 if regiao == "Norte"
replace cod_regiao = 2 if regiao == "Nordeste"
replace cod_regiao = 5 if regiao == "Centro-Oeste"
replace cod_regiao = 3 if regiao == "Sudeste"
replace cod_regiao = 4 if regiao == "Sul"
replace cod_regiao = 6 if regiao == "Outros"

save "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1970/censo_1970_temp.dta", replace




















