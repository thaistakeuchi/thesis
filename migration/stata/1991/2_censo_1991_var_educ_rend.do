* 14/11/2024
* Thais Takeuchi
* Income and education in 1991 Census

use "D:\censo_1991\tratada\censo_1991_migracao_tratada.dta", clear
keep if V3072 > 29
keep if V3072 < 61

drop V0213 V0214 V0216 V0217 V0218 V0219 V0220 V0221 V0222 V0223 V0224 V0225 V0226 V0227 V0210 V0211 V0203 V0204

** rendimento **
* fx rendimento nominal medio mensal familiar per capita
drop if V3049 == 0 | V3049 == 5.3e-315 | V3049 == 1.1e-314 | V3049 == 0.0078125 | V3049 == 13
sort uf_nascim

* 30-60 anos (como se fosse dos pais)
by uf_nascim: summarize V3049 if V3072 >= 30 & V3072 <= 60

* fx rendimento nominal medio mensal familiar per capita
gen rendimento_sm_30_60 = .
replace rendimento_sm_30_60 = 0.0625 if V3049 == 1
replace rendimento_sm_30_60 = 0.1875 if V3049 == 2
replace rendimento_sm_30_60 = 0.375 if V3049 == 3
replace rendimento_sm_30_60 = 0.625 if V3049 == 4
replace rendimento_sm_30_60 = 0.875 if V3049 == 5
replace rendimento_sm_30_60 = 1.125 if V3049 == 6
replace rendimento_sm_30_60 = 1.375 if V3049 == 7
replace rendimento_sm_30_60 = 1.75 if V3049 == 8
replace rendimento_sm_30_60 = 2.5 if V3049 == 9
replace rendimento_sm_30_60 = 4 if V3049 == 10
replace rendimento_sm_30_60 = 7.5 if V3049 == 11
replace rendimento_sm_30_60 = 10 if V3049 == 12
replace rendimento_sm_30_60 = 0 if V3049 == 14

*egen salario_medio_30_60 = mean(rendimento_sm_30_60), by(uf_nascim)

*table uf_nascim, c(mean salario_medio_30_60)

preserve
collapse (mean) rendimento_sm_30_60 [pweight=V7301], by(uf_nascim)
rename rendimento_sm_30_60 salario_medio_30_60
save temp_salario_medio, replace
restore

merge m:1 uf_nascim using temp_salario_medio, keepusing(salario_medio_30_60)

** educacao **
*** 30-60
drop if V0328 == 0
*tabulate uf_nascim V0328, row // sem peso
proportion V0328, over(uf_nascim) pweight(V7301)

* anos de estudo
preserve
collapse (mean) media_anos_estudo = V3241 [pw=V7301], by(uf_nascim)
list uf_nascim media_anos_estudo
restore






