********************************************************************************
* Censo 1980
********* NAO COMPATIBILIZADA *********
* Set working directory
cd "E:/Thais/Tese_Mestrado/censo_1980"

* Open dataset
use "E:/Thais/Tese_Mestrado/censo_1980/censo_1980_raw.dta", clear

* Escolhe variaveis
* use V2 V5 V6 V501 V503 V504 V505 V508 V509 V511 V512 V519 V524 V526 V528 V529 V535 V536 V607 V609 V554 V555 V606 V570 V604 D_R using "E:/Thais/Tese_Mestrado/censo_1980/censo_1980_raw.dta", clear

* Filtra observacoes onde idade esta entre 25 e 64
keep if V606 >= 25 & V606 <= 64

* Filtra os estrangeiros e brasileiros naturalizados
keep if V511 == 4 | V511 == 6

save "censo_pessoas_1980_filtrado_nao_comp.dta", replace
use "E:/Thais/Tese_Mestrado/censo_1980/censo_pessoas_1980_filtrado_nao_comp.dta", clear

******************
* MIGRACAO INTERNA
******************
* cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1980"

* Open dataset
use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1980/censo_1980_raw.dta", clear

* Keep age>10
keep if V606 >= 10

* Keep brazilians only 
keep if v0418 == 2

save "censo_pessoas_2000_migracao.dta", replace

*****************
* TX DE ATIVIDADE
*****************
use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1980/censo_1980_raw.dta", clear

keep D_R V2 V5 V6 V501 V509 V528 V529 V535 V536 V606 V604

* regiao
gen regiao = .
replace regiao = 1 if inlist(V2, 11, 12, 13, 14, 15, 16)
replace regiao = 2 if inlist(V2, 21, 22, 23, 24, 25, 26, 27, 28, 29)
replace regiao = 5 if inlist(V2, 50, 51, 52, 53)
replace regiao = 3 if inlist(V2, 31, 32, 33, 35)
replace regiao = 4 if inlist(V2, 41, 42, 43)
replace regiao = 6 if missing(regiao) 

save "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1980/censo_1980_temp.dta", replace

******************
* MANIPULA CENSO
******************
cd "D:/1_migration/1_datasets_migrants/censo/1980"

use "D:/antigo/Tese_Mestrado/censo_1980/censo_1980_raw.dta", clear

* só deixa V512 com estados brasileiros
keep if inrange(V512, 1, 27) | missing(V512)

* gera uf_nascim
gen uf_nascim = .  
replace uf_nascim = V512 if inrange(V512, 1, 27)

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

drop uf_nascim
rename uf_nascim_new uf_nascim

gen uf = V2 //uf

* migrante quando uf_nascimento diferente de uf
gen migrante = (uf != uf_nascim)

* sexo
gen sexo = V501

* numero domicilio
gen n_domicilio = V6

cd "D:/censo_1980"

save "censo_1980_temp.dta", replace

use "D:/antigo/Tese_Mestrado/censo_1980/censo_1980_temp.dta", clear

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
gen menos_fund_mulher = 0
replace menos_fund_mulher = 1 if migrante_casada == 1 & V524 == 2

gen menos_fund_homem = 0
replace menos_fund_homem = 1 if sexo == 1 & V524 == 2

gen fund_mulher = 0
replace fund_mulher = 1 if migrante_casada == 1 & (V524 == 3 | V524 == 4)

gen fund_homem = 0
replace fund_homem = 1 if sexo == 1 & (V524 == 3 | V524 == 4)

gen em_mulher = 0
replace em_mulher = 1 if migrante_casada == 1 & (V524 == 5 | V524 == 6)

gen em_homem = 0
replace em_homem = 1 if sexo == 1 & (V524 == 5 | V524 == 6)

gen superior_mulher = 0
replace superior_mulher = 1 if migrante_casada == 1 & (V524 == 7 | V524 == 8)

gen superior_homem = 0
replace superior_homem = 1 if sexo == 1 & (V524 == 7 | V524 == 8)

* Criar a variável educ_mulher (apenas para migrante_casada_15 == 1)
gen educ_mulher = .
replace educ_mulher = V524 if migrante_casada_15 == 1

* Criar a variável educ_homem (apenas para homens responsáveis, cônjuges ou casados)
gen educ_homem = .
replace educ_homem = V524 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)


********
* FILHO
********
gen children = sum(V554, V555)

gen children_under_5 = 0
replace children_under_5 = 1 if V570 <= 5

*******************
* OCUPACAO
*******************
* Criar a variável ocup_migrante
gen ocup_migrante = .
replace ocup_migrante = v0439 if migrante_casada_15 == 1

gen ocup_migrante = 0
replace ocup_migrante = 1 if V528 == 1



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



