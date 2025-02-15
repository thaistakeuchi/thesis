* 21/12/2024
* Thais Takeuchi
* Pseudo painel in 1991 Census

cd "D:/censo_1991/pseudo_painel"

use "D:/censo_1991/base geral/censo_1991_temp.dta", clear

* drop housing variables
drop V0213 V0214 V0216 V0217 V0218 V0219 V0220 V0221 V0222 V0223 V0224 V0225 V0226 V0227 V0210 V0211 V0203 V0204

* keep sample aged 20-50
keep if V3072 > 19
keep if V3072 < 51

* drop if number of children is 99
drop if V3351 == 99

* rename n_children
rename V3351 n_children

* variable have_children (separate sample: those with children and childless ones)
gen have_children = .
replace have_children = 0 if n_children == 0
replace have_children = 1 if n_children != 0

* create variables

**************
* SCHOOLING
**************
gen less_fund = 0
replace less_fund = 1 if (V0328 == 1 | V0328 == 2)

gen fund = 0
replace fund = 1 if (V0328 == 3 | V0328 == 4)

gen em = 0
replace em = 1 if (V0328 == 5 | V0328 == 6)

gen superior = 0
replace superior = 1 if (V0328 == 7 | V0328 == 8)

* by sex
gen less_fund_mulher = 0
replace less_fund_mulher = 1 if sexo == 2 & (V0328 == 1 | V0328 == 2)

gen less_fund_homem = 0
replace less_fund_homem = 1 if sexo == 1 & (V0328 == 1 | V0328 == 2)

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
replace educ_homem = V0328 if sexo == 1

**************
* EMPLOYMENT
**************
drop if V0345 == 0

gen ocup = 0
replace ocup = 1 if (V0345 == 1 | V0345 == 2)

*****
* AGE
*****
* idade mulher
gen age_women = . 
replace age_women = V3072 if sexo == 2

* idade mulher quadrado
gen age_squared_women = . 
replace age_squared_women = V3072^2 if sexo == 2

* idade homem
gen age_men= . 
replace age_men = V3072 if sexo == 1

* idade homem quadrado
gen age_squared_men = . 
replace age_squared_men = V3072^2 if sexo == 1

*********
* INCOME - I'll decide it later
*********

******
* RACE
******
rename V0309 race
drop if race == 9

*****************
* MARITAL STATUS
*****************
gen married = 0
replace married = 1 if (V3342 == 1 | V3342 == 2 | V3342 == 3)


* save dataset
save "censo_pessoas_1991_pseudo_painel.dta", replace


use "D:/censo_1991/pseudo_painel/censo_pessoas_1991_pseudo_painel.dta", clear



