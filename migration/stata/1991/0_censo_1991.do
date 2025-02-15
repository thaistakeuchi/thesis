********************************************************************************
* Censo 1991
********* NAO COMPATIBILIZADA *********
* Set working directory
cd "E:/Thais/Tese_Mestrado/censo_1991"

* Open dataset
use "E:/Thais/Tese_Mestrado/censo_1991/censo_1991_raw.dta", clear

* Filtra os estrangeiros e brasileiros naturalizados
keep if V3151 == 2 | V3151 == 3

* Filtra observacoes onde idade esta entre 25 e 64
keep if V3072 >= 25 & V3072 <= 64


* use IDQues TipoReg UF V7004 V0098 V0301 V0302 V0303 V3043 V3044 V3045 V3046 V3047 ///
V3049 V3005 V3072 V0309 V3151 V3152 V0316 V0317 V0319 V0323 V0327 V0328 V3241 V0330 ///
V3311 V3312 V0332 V0333 V3341 V3342 V0345 V0350 V0354 V3561 V0358 V3351 V3352 V3353 ///
V3360 V3361 V3362 V3443 V0310 V7301 V7001 V7002 V1102 V0109 V1061 V7003 V7300 ///
using "E:/Thais/Tese_Mestrado/censo_1991/censo_1991_raw.dta", clear

save "censo_pessoas_1991_filtrado_nao_comp.dta", replace

***********
* MIGRACAO
***********
* Set working directory
cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991"

* Open dataset
use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991/censo_1991_raw.dta", clear

* Filtrar por V3151 == 1
keep if V3151 == 1

* Manter apenas os valores de UF diferentes de V0316
keep if UF != V0316

save "censo_pessoas_1991_migracao.dta", replace


*****************
* TX DE ATIVIDADE
*****************
cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991"

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991/censo_1991_raw.dta", clear

keep IDQues TipoReg UF V7004 V0098 V0301 V0302 V3072 V0309 V0316 V0332 V0333 V3342 V0345 V0354 V0355 V0358 V3351 V3354 V3360 V3443 V7301 V7001 V7002 V1102 V0109 V1061 V7003 V7300

save "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1991/censo_1991_temp.dta", replace

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1991/censo_1991_temp.dta", clear

keep if V3072 >= 10

replace V0358 = 10 if missing(V0358) //trabalha com mais de 10 anos

save "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1991/censo_1991_temp_v2.dta", replace


******************
* MANIPULA CENSO
******************
cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1991"

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991/censo_1991_raw.dta", clear

* só deixa V0316 com estados brasileiros
keep if inrange(V0316, 1, 27) | missing(V0316)

* gera uf_nascim
gen uf_nascim = .  
replace uf_nascim = V0316 if inrange(V0316, 1, 27)

* uf com estados do brasil
gen uf_nascim_new = uf_nascim

* renomeia nome uf
replace uf_nascim_new = 11 if uf_nascim == 1
replace uf_nascim_new = 12 if uf_nascim == 2
replace uf_nascim_new = 13 if uf_nascim == 3
replace uf_nascim_new = 14 if uf_nascim == 4
replace uf_nascim_new = 15 if uf_nascim == 5
replace uf_nascim_new = 16 if uf_nascim == 6
replace uf_nascim_new = 17 if uf_nascim == 7

replace uf_nascim_new = 21 if uf_nascim == 8
replace uf_nascim_new = 22 if uf_nascim == 9
replace uf_nascim_new = 23 if uf_nascim == 10
replace uf_nascim_new = 24 if uf_nascim == 11
replace uf_nascim_new = 25 if uf_nascim == 12
replace uf_nascim_new = 26 if uf_nascim == 13
replace uf_nascim_new = 27 if uf_nascim == 14
replace uf_nascim_new = 28 if uf_nascim == 15
replace uf_nascim_new = 29 if uf_nascim == 16

replace uf_nascim_new = 31 if uf_nascim == 17
replace uf_nascim_new = 32 if uf_nascim == 18
replace uf_nascim_new = 33 if uf_nascim == 19
replace uf_nascim_new = 34 if uf_nascim == 20

replace uf_nascim_new = 41 if uf_nascim == 21
replace uf_nascim_new = 42 if uf_nascim == 22
replace uf_nascim_new = 43 if uf_nascim == 23

replace uf_nascim_new = 50 if uf_nascim == 24
replace uf_nascim_new = 51 if uf_nascim == 25
replace uf_nascim_new = 52 if uf_nascim == 26
replace uf_nascim_new = 53 if uf_nascim == 27

use "C:\Users\thtak\OneDrive - Fundacao Getulio Vargas - FGV\Tese_Mestrado\1_migration\1_datasets_migrants\censo\1991\censo_1991_temp.dta", clear


drop uf_nascim
rename uf_nascim_new uf_nascim

* no questionario quando a pessoa nasceu na mesma uf que esta sendo entrevistada, entao v0102 e missing
gen uf = UF
replace uf_nascim = uf if missing(uf_nascim)

* migrante quando uf_nascimento diferente de uf
gen migrante = (uf != uf_nascim)

* sexo
gen sexo = V0301

* numero domicilio
gen n_domicilio = IDQues

cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991"

save "censo_1991_temp.dta", replace

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991/censo_1991_temp.dta", clear 

* emigracao e imigracao 
*collapse (sum) emigrantes = P001, by(uf_nascim)

* Ordene de forma decrescente para obter os estados com mais emigrantes
*sort emigrantes
*gsort -emigrantes

* Exiba os top 5 estados com maior número de emigrantes
*list uf_nascim emigrantes in 1/5


*use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/2000/censo_2000_temp.dta", clear 

*************
************
*** DUMMY ***
*************
*************

********
* CASAL
********
* responsavel e conjuge
gen responsavel = (V0302 == 1)
gen conjuge = (V0302 == 2)

* gera casal
bysort n_domicilio: gen casal = (sum(conjuge == 1) > 0 & sum(responsavel == 1) > 0)

* casados
gen casado = 0
replace casado = 1 if inlist(V3342, 1, 2, 3) | V0330 == 1

* migrante_casada
gen migrante_casada = 0
replace migrante_casada = 1 if (casado == 1 & sexo == 2 & migrante == 1) | (conjuge == 1 & sexo == 2 & migrante == 1) | (responsavel == 1 & sexo == 2 & migrante == 1)
*bysort n_domicilio: replace migrante_casada = 1 if responsavel == 1 & sexo == 2 & migrante == 1 & sum(conjuge == 1) > 0
*replace migrante_casada = 1 if conjuge == 1 & sexo == 2 & migrante == 1

* tempo de moradia uf
gen moradia_uf = V0317
replace moradia_uf = 0 if missing(V0317)

* Criar a variável migrante_casada_15
gen migrante_casada_15 = 0
replace migrante_casada_15 = 1 if migrante_casada == 1 & (V3072 - moradia_uf <= 15)

// Manter apenas os domicílios onde existe pelo menos um responsável e um cônjuge
bysort n_domicilio: egen count_responsavel = max(responsavel)
bysort n_domicilio: egen count_conjuge = max(conjuge)

keep if count_responsavel == 1 & count_conjuge == 1

********
* IDADE
********
* idade mulher
gen age_fem = . 
replace age_fem = V3072 if migrante_casada == 1

* idade mulher quadrado
gen age_squared_fem = . 
replace age_squared_fem = V3072^2 if migrante_casada == 1

* idade esposo
gen age_sp = . 
replace age_sp = V3072 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)

******************
* AGE RANGE SPOUSE
******************
summarize age_sp
local min_age = r(min)
local max_age = r(max)

gen age_sp_range = .

forval i = 1/10 {
    local lower_bound = (`i' - 1) * ((`max_age' - `min_age') / 10) + `min_age'
    local upper_bound = `i' * ((`max_age' - `min_age') / 10) + `min_age'

    replace age_sp_range = `i' if age_sp >= `lower_bound' & age_sp < `upper_bound'
}


* mantenho obs em que temos pelo menos 1 mulher migrante na casa
bysort n_domicilio: egen main_sample = max(migrante_casada == 1)

* gera base com somente migrantes casadas e suas/seus cônjuges
keep if main_sample == 1

**************
* ESCOLARIDADE
**************
gen fund_mulher = 0
replace fund_mulher = 1 if migrante_casada == 1 & (V0328 == 3 | V0328 == 4)

gen fund_homem = 0
replace fund_homem = 1 if sexo == 1 & (V0328 == 3 | V0328 == 4)

gen em_mulher = 0
replace em_mulher = 1 if migrante_casada == 1 & (V0328 == 5 | V0328 == 6)

gen em_homem = 0
replace em_homem = 1 if sexo == 1 & (V0328 == 5 | V0328 == 6)

gen superior_mulher = 0
replace superior_mulher = 1 if migrante_casada == 1 & (V0328 == 7 | V0328 == 8)

gen superior_homem = 0
replace superior_homem = 1 if sexo == 1 & (V0328 == 7 | V0328 == 8)

* Criar a variável educ_mulher (apenas para migrante_casada_15 == 1)
gen educ_mulher = .
replace educ_mulher = V0328 if migrante_casada_15 == 1

* Criar a variável educ_homem (apenas para homens responsáveis, cônjuges ou casados)
gen educ_homem = .
replace educ_homem = V0328 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)


********
* FILHO
********
gen children = V3354

gen children_under_5 = 0
replace children_under_5 = 1 if v4654 <= 5

*******************
* HORAS TRABALHADAS
*******************
* horas trabalhadas ocupacao principal
gen horas_trabalho_principal = V0354

* horas trabalhadas outras ocupacoes
gen horas_trabalho_outros = V0355

gen hworked_fem = .  
replace hworked_fem = max(v4534, v0453) if migrante_casada == 1



* Criar a variável horas_migrante
gen horas_migrante_15 = .
replace horas_migrante_15 = max(v4534, v0453) if migrante_casada_15 == 1

********
* INCOME
********
gen income_sp = .  
replace income_sp = (12 * v4525) / 1000 if sexo == 1

// olhar para essa divisao por 10000, se faz sentido

* Criar a variável income_sp_raw (apenas para homens)
gen income_sp_raw = .
replace income_sp_raw = v4525 if sexo == 1

* salva base
save "censo_2000_migracao_tratada.dta", replace

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/2000/censo_2000_migracao_tratada.dta", clear

* gera uma linha para cada domicilio
* Calcular o valor máximo de age_sp e age_sp_range dentro de cada domicílio
by n_domicilio: egen max_age_sp = max(age_sp)
by n_domicilio: egen max_age_sp_range = max(age_sp_range)
by n_domicilio: egen max_fund_homem = max(fund_homem)
by n_domicilio: egen max_em_homem = max(em_homem)
by n_domicilio: egen max_superior_homem = max(superior_homem)
by n_domicilio: egen max_educ_homem = max(educ_homem)
by n_domicilio: egen max_income_sp = max(income_sp)
by n_domicilio: egen max_income_sp_raw = max(income_sp_raw)

* Substituir os valores nas linhas onde migrante_casada_15 == 1
replace age_sp = max_age_sp if migrante_casada_15 == 1
replace age_sp_range = max_age_sp_range if migrante_casada_15 == 1
replace fund_homem = max_fund_homem if migrante_casada_15 == 1
replace em_homem = max_em_homem if migrante_casada_15 == 1
replace superior_homem = max_superior_homem if migrante_casada_15 == 1
replace educ_homem = max_educ_homem if migrante_casada_15 == 1
replace income_sp = max_income_sp if migrante_casada_15 == 1
replace income_sp_raw = max_income_sp_raw if migrante_casada_15 == 1

* Remover as variáveis temporárias
drop max_age_sp max_age_sp_range max_fund_homem max_em_homem max_superior_homem max_educ_homem max_income_sp max_income_sp_raw

* salva base
save "censo_2000_migracao_tratada_migrante_15.dta", replace

* deixa somente as linhas onde migrante_casada_15 == 1
keep if migrante_casada_15 == 1

* salva base
save "censo_2000_migracao_tratada_migrante_15_final.dta", replace

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/2000/censo_2000_migracao_tratada_migrante_15_final.dta", clear

* gen variable children
gen children = v4620

save "censo_2000_migracao_tratada_migrante_15_final.dta", replace

* conta missing values
foreach var of varlist migrante_casada_15 age_fem age_squared_fem age_sp fund_mulher fund_homem em_mulher em_homem superior_mulher superior_homem children_under_5 horas_migrante_15 ocup_migrante income_sp_raw {
    gen `var'_missing = missing(`var')
    quietly count if missing(`var')
    di "`var' has " r(N) " missing values"
}
* verification NA values
misstable summarize horas_migrante_15 income_sp_raw



