******************
* MIGRACAO INTERNA - COMBINED DATA
******************
cd "D:/1_migration/1_datasets_migrants/censo/1991"

set more off

* Open dataset
use "D:/1_migration/1_datasets_migrants/censo/1991/censo_1991_migrante_tratada_2025_03_05.dta", clear

******************
* MANIPULA CENSO
******************

* remove strange values
drop if uf < 11
drop if uf_nascim < 11

* migrante quando uf_nascimento diferente de uf
gen migrante = (uf != uf_nascim)

* numero domicilio
gen n_domicilio = IDQues
drop if n_domicilio == 0 | n_domicilio == 65000

*************
************
*** DUMMY ***
*************
*************

********
* CASAL
********
* responsavel e conjuge
gen responsavel = (V0303 == 1)
gen conjuge = (V0303 == 2)

* gera casal
bysort n_domicilio: gen has_conjuge = (sum(conjuge) > 0)
bysort n_domicilio: gen has_responsavel = (sum(responsavel) > 0)
gen casal = (has_conjuge > 0 & has_responsavel > 0)

* casados
gen casado = 0
replace casado = 1 if inlist(V3342, 1, 2, 3) | inlist(V0332, 1, 2, 3, 4) | V0333 == 8 | V0330 == 1 

* migrante_casada
gen migrante_casada = 0
replace migrante_casada = 1 if (casado == 1 & sexo == 2 & migrante == 1) | (conjuge == 1 & sexo == 2 & migrante == 1) | (responsavel == 1 & casado == 1 & sexo == 2 & migrante == 1)
*bysort n_domicilio: replace migrante_casada = 1 if responsavel == 1 & sexo == 2 & migrante == 1 & sum(conjuge == 1) > 0
*replace migrante_casada = 1 if conjuge == 1 & sexo == 2 & migrante == 1

********
* IDADE
********
gen idade = V3072

* idade mulher
gen age_fem = .
replace age_fem = idade if sexo == 2 & inlist(V0303, 1, 2)

* idade esposo
gen age_sp = .
replace age_sp = idade if sexo == 1 & inlist(V0303, 1, 2)

* idades ao quadrado
gen age_squared_fem = age_fem^2
gen age_squared_sp = age_sp^2


* tempo de moradia na uf
gen anos_mor_uf = V0317
replace anos_mor_uf = idade if missing(V0317)

* Criar a variável migrante_casada
gen migrante_casada_15 = 0
replace migrante_casada_15 = 1 if migrante_casada == 1 & (idade - anos_mor_uf <= 15)
gen migrante_casada_10 = 0
replace migrante_casada_10 = 1 if migrante_casada == 1 & (idade - anos_mor_uf <= 10)
gen migrante_casada_5 = 0
replace migrante_casada_5 = 1 if migrante_casada == 1 & (idade - anos_mor_uf <= 5)


**************
* ESCOLARIDADE
**************
gen menos_fund_mulher = 0
replace menos_fund_mulher = 1 if sexo == 2 & (V0328 == 1 | V0328 == 0 | V0328 == 2)

gen menos_fund_homem = 0
replace menos_fund_homem = 1 if sexo == 1 & (V0328 == 1 | V0328 == 0 | V0328 == 2)

gen fund_mulher = 0
replace fund_mulher = 1 if sexo == 2 & (V0328 == 3 | V0328 == 4)

gen fund_homem = 0
replace fund_homem = 1 if sexo == 1 & (V0328 == 3 | V0328 == 4)

gen em_mulher = 0
replace em_mulher = 1 if sexo == 2 & (V0328 == 5 | V0328 == 6)

gen em_homem = 0
replace em_homem = 1 if sexo == 1 & (V0328 == 5 | V0328 == 6)

gen superior_mulher = 0
replace superior_mulher = 1 if sexo == 2 & (V0328 == 7 | V0328 == 8)

gen superior_homem = 0
replace superior_homem = 1 if sexo == 1 & (V0328 == 7 | V0328 == 8)

* Criar a variável educ_mulher
gen educ_mulher = .
replace educ_mulher = V0328 if sexo == 2

* Criar a variável educ_homem
gen educ_homem = .
replace educ_homem = V0328 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)

********
* FILHO
********
gen children = V3354

gen children_under_5 = 0
replace children_under_5 = 1 if V3443 <= 5

*******************
* OCUPACAO
*******************
generate ocupado = 0
replace ocupado = 1 if inlist(V0345, 1, 2)
*replace ocupado = 1 if afast_trab_sem == 1
*replace ocupado = 1 if nao_remun == 1

* Criar a variável ocup_migrante (trabalho remunerado semana de referencia)
gen ocup_migrante = 0
replace ocup_migrante = 1 if ocupado == 1 & sexo == 2 & migrante == 1

* Criar a variável horas_migrante (principal)
gen horas_migrante = .
replace horas_migrante = V0354 if sexo == 2

********
* INCOME
********
* Criar a variável income_sp_raw (apenas para homens)
gen income_man = .
replace income_man = V3561 if sexo == 1

gen income_sp_raw = .
replace income_sp_raw = V3561 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)

* salva base
save "censo_1991_tratada_todos_2025_03_05.dta", replace //ULTIMA BASE USADA

* keep only hh with at least one migrant women
bysort IDQues: egen main_sample = max(migrante_casada_15 == 1)

* gera base com somente migrantes casadas e suas/seus cônjuges
keep if main_sample == 1

save "censo_1991_migrante_tratada_final_2025_03_05.dta", replace //ULTIMA BASE USADA



