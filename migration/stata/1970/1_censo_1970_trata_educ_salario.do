
*alternativa

use "D:\antigo\Tese_Mestrado\censo_1970\censo_1970_raw.dta", clear

keep if V027 > 29
keep if V027 < 61

* educacao *
* frequenta a escola? deixei só os que não frequentam
drop if V036 == 3 | V036 == 4 | V036 == 5 | V036 == 0

* último grau concluído com aprovação
drop if V038 == 6 | V038 == 7 | V038 == 8 | V038 == 0

* ultima serie concluida com aprovacao
drop if V037 == 9 | V037 == 7 | V037 == 8

* anos de estudo
gen anos_de_estudo = .

* No schooling or undeclared
replace anos_de_estudo = 0 if V038 == 5

* Primary/Elementary school
replace anos_de_estudo = 1 if V037 == 1 & V038 == 1
replace anos_de_estudo = 2 if V037 == 2 & V038 == 1
replace anos_de_estudo = 3 if V037 == 3 & V038 == 1
replace anos_de_estudo = 4 if V037 == 4 & V038 == 1
replace anos_de_estudo = 5 if V037 == 5 & V038 == 1
replace anos_de_estudo = 6.5 if (V037 == 6 & V038 == 1) | ((V037 == 2 | V037 == 3) & V038 == 2)

* Lower Secondary (Ginasial, 1st Cycle)
replace anos_de_estudo = 8 if V037 == 4 & V038 == 2
replace anos_de_estudo = 9 if V037 == 5 & V038 == 2

* Upper Secondary (2nd Cycle)
replace anos_de_estudo = 10 if V037 == 2 & V038 == 3
replace anos_de_estudo = 11 if V037 == 3 & V038 == 3
replace anos_de_estudo = 12 if V037 == 4 & V038 == 3

* Higher Education
replace anos_de_estudo = 13 if V037 == 2 & V038 == 4
replace anos_de_estudo = 14 if V037 == 3 & V038 == 4
replace anos_de_estudo = 15 if V037 == 4 & V038 == 4
replace anos_de_estudo = 16 if V037 == 5 & V038 == 4
replace anos_de_estudo = 17 if V037 == 6 & V038 == 4

* cria uf

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

* media por uf
svyset [pweight=V054]
svy: mean anos_de_estudo, over(uf)

*******************************************************************************

* rendimento medio mensal *
drop if V041 == 9999
svyset [pweight=V054]
svy: mean V041, over(uf)

