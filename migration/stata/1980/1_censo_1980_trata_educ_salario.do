

keep if V606 > 29
keep if V606 < 61

* Inicialização
gen anos_de_estudo = .

* Sem escolaridade ou não declarado
replace anos_de_estudo = 0 if (V524 == 0)

* Escola Primária/Elementar e Ginasial
replace anos_de_estudo = 1 if ((V523 == 0 & V524 == 1) | (V523 == 1 & inlist(V524, 2,3,4)) | V522 == 2)
replace anos_de_estudo = 2 if (V523 == 2 & inlist(V524, 2,3,4))
replace anos_de_estudo = 3 if (V523 == 3 & inlist(V524, 2,3,4))
replace anos_de_estudo = 4 if (V523 == 4 & inlist(V524, 2,3,4))
replace anos_de_estudo = 5 if (V523 == 5 & inlist(V524, 2,3,4))
replace anos_de_estudo = 6 if (V523 == 6 & V524 == 4)
replace anos_de_estudo = 7 if (V523 == 7 & V524 == 4)
replace anos_de_estudo = 8 if ((V523 == 8 & V524 == 4) | inlist(V522, 3,5))

* Ensino Médio (2º Ciclo)
replace anos_de_estudo = 9 if V523 == 1 & inlist(V524, 5,6)
replace anos_de_estudo = 10 if V523 == 2 & inlist(V524, 5,6)
replace anos_de_estudo = 11 if (inlist(V523, 3,4) & inlist(V524, 5,6)) | inlist(V522, 4,6,7)

* Ensino Superior
replace anos_de_estudo = 12 if V523 == 1 & V524 == 7
replace anos_de_estudo = 13 if V523 == 2 & V524 == 7
replace anos_de_estudo = 14 if V523 == 3 & V524 == 7
replace anos_de_estudo = 15 if V523 == 4 & V524 == 7
replace anos_de_estudo = 16 if V523 == 5 & V524 == 7
replace anos_de_estudo = 17 if V523 == 6 & V524 == 7
replace anos_de_estudo = 19 if (V523 == 0 & V524 == 8) | V522 == 8

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
replace uf_nascim_new = 35 if uf_nascim == 20

replace uf_nascim_new = 41 if uf_nascim == 21
replace uf_nascim_new = 42 if uf_nascim == 22
replace uf_nascim_new = 43 if uf_nascim == 23

replace uf_nascim_new = 50 if uf_nascim == 24
replace uf_nascim_new = 51 if uf_nascim == 25
replace uf_nascim_new = 52 if uf_nascim == 26
replace uf_nascim_new = 53 if uf_nascim == 27

drop uf_nascim
rename uf_nascim_new uf_nascim


* media por uf
svyset [pweight=V604]
svy: mean anos_de_estudo, over(uf_nascim)

*******************************************************************************

* rendimento medio mensal *

drop if V607 == 9999999
drop if V608 == 9999999
drop if V609 == 9999999

gen renda = 0
replace renda = renda + V607 if !missing(V607)
replace renda = renda + V608 if !missing(V608)
replace renda = renda + V609 if !missing(V609)


svyset [pweight=V604]
svy: mean renda, over(uf_nascim)



