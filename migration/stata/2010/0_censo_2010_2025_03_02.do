******************
* MIGRACAO INTERNA - COMBINED DATA
******************
cd "D:/1_migration/1_datasets_migrants/censo/2010"

set more off

* Open dataset
use "D:/antigo/Tese_Mestrado/censo_2010/combined_data.dta", clear

******************
* MANIPULA CENSO
******************
drop dif_enxergar dif_ouvir dif_caminhar def_mental nacionalidade ano_fix_res curso_concl mun_escola qtos_empregados previd_B f_nasc_v_hom f_nasc_v_mul f_vivos_hom f_vivos_mul f_nasc_m_hom f_nasc_m_mul filhos_nasc_mortos m0502 m0601 m6033 m0606 m0613 m0614 m0615 m0616 m0617 m0618 m0619  m0620  m0621  m0622  m6222  m6224  m0623  m0624  m0625  m6252  m6254  m6256  m0626  m6262  m6264  m6266  m0627  m0628  m0629  m0630  m0631  m0632  m0633  m0634  m0635  m6352  m6354  m6356  m0636  m6362  m6364  m6366  m0637  m0638  m0639  m0640  m0641  m0642  m0643  m0644  m0645  m6461  m6471  m0648  m0649  m0650  m0651  m6511  m0652  m6521  m0653  m0654  m0655  m0656  m0657  m0658  m0659  m6591  m0660  m6602  m6604  m6606  m0661  m0662  m0663  m6631  m6632  m6633  m0664  m6641  m6642  m6643  m0665  m6660  m0667  m0668  m6681  m6682  m0669  m6691  m6692  m6693  m0670  m0671  m6800  m6121  m0604  m0605  m6462  m6472 pais_nascim pais_mor_ant pais_mor5anos amc0010 newamc0010 amc9100 newamc9100 amc7000 newamc7000 
drop anos_mor_mun mun_mor_ant mun_mor5anos rede_freq mun_trab cursos_c1 cursos_c2
drop idade_meses idade_presumida t_mor_UF_70 t_mor_mun_70 t_mor_UF_80 t_mor_mun_80

* remove strange values
drop if UF == 0 | UF == 1
drop if UF_nascim == 16908802 | UF_nascim == 33686018

* no questionario quando a pessoa nasceu na mesma uf que esta sendo entrevistada, entao v6222 e missing
gen uf_nascim = UF_nascim
replace uf_nascim = UF if missing(UF_nascim)

* migrante quando uf_nascimento diferente de uf
gen migrante = (UF != uf_nascim)

* numero domicilio
gen n_domicilio = id_dom

*************
************
*** DUMMY ***
*************
*************

********
* CASAL
********
* responsavel e conjuge
gen responsavel = (cond_dom_B == 1)
gen conjuge = (cond_dom_B == 2)

* gera casal
bysort n_domicilio: egen has_conjuge = total(conjuge == 1)
bysort n_domicilio: egen has_responsavel = total(responsavel == 1)
gen casal = (has_conjuge > 0 & has_responsavel > 0)

* casados
gen casado = 0
replace casado = 1 if inlist(estado_conj, 1, 2, 3, 4, 9) | vive_conjuge == 1 | teve_conjuge == 1

* migrante_casada
gen migrante_casada = 0
replace migrante_casada = 1 if (casado == 1 & sexo == 0 & migrante == 1) | (conjuge == 1 & sexo == 0 & migrante == 1) | (responsavel == 1 & casado == 1 & sexo == 0 & migrante == 1)
*bysort n_domicilio: replace migrante_casada = 1 if responsavel == 1 & sexo == 2 & migrante == 1 & sum(conjuge == 1) > 0
*replace migrante_casada = 1 if conjuge == 1 & sexo == 2 & migrante == 1

* tempo de moradia na uf
gen anos_mor_uf = anos_mor_UF
replace anos_mor_uf = idade if missing(anos_mor_UF)

* Criar a variável migrante_casada
gen migrante_casada_15 = 0
replace migrante_casada_15 = 1 if migrante_casada == 1 & (idade - anos_mor_uf <= 15)
gen migrante_casada_10 = 0
replace migrante_casada_10 = 1 if migrante_casada == 1 & (idade - anos_mor_uf <= 10)
gen migrante_casada_5 = 0
replace migrante_casada_5 = 1 if migrante_casada == 1 & (idade - anos_mor_uf <= 5)

********
* IDADE
********
* idade mulher
gen age_fem = .
replace age_fem = idade if sexo == 0 & inlist(cond_dom_B, 1, 2)

* idade esposo
gen age_sp = .
replace age_sp = idade if sexo == 1 & inlist(cond_dom_B, 1, 2)

* idades ao quadrado
gen age_squared_fem = age_fem^2
gen age_squared_sp = age_sp^2

**************
* ESCOLARIDADE
**************
gen menos_fund_mulher = 0
replace menos_fund_mulher = 1 if sexo == 0 & (anos_estudoC == 1 | anos_estudoC == 0)

gen menos_fund_homem = 0
replace menos_fund_homem = 1 if sexo == 1 & (anos_estudoC == 1 | anos_estudoC == 0)

gen fund_mulher = 0
replace fund_mulher = 1 if sexo == 0 & (anos_estudoC == 2)

gen fund_homem = 0
replace fund_homem = 1 if sexo == 1 & (anos_estudoC == 2)

gen em_mulher = 0
replace em_mulher = 1 if sexo == 0 & (anos_estudoC == 3)

gen em_homem = 0
replace em_homem = 1 if sexo == 1 & (anos_estudoC == 3)

gen superior_mulher = 0
replace superior_mulher = 1 if sexo == 0 & (anos_estudoC == 4)

gen superior_homem = 0
replace superior_homem = 1 if sexo == 1 & (anos_estudoC == 4)

* Criar a variável educ_mulher
gen educ_mulher = .
replace educ_mulher = anos_estudoC if sexo == 0

* Criar a variável educ_homem
gen educ_homem = .
replace educ_homem = anos_estudoC if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)

********
* FILHO
********
gen children = filhos_nasc_vivos

drop if idade_ult_nasc_v == 16256 | idade_ult_nasc_v == 16384

gen children_under_5 = 0
replace children_under_5 = 1 if idade_ult_nasc_v <= 5

*******************
* OCUPACAO
*******************
generate ocupado = 0
replace ocupado = 1 if trab_rem_sem == 1
replace ocupado = 1 if afast_trab_sem == 1
replace ocupado = 1 if nao_remun == 1

* Criar a variável ocup_migrante (trabalho remunerado semana de referencia)
gen ocup_migrante = 0
replace ocup_migrante = 1 if ocupado == 1 & sexo == 0 & migrante == 1

* Criar a variável horas_migrante
gen horas_migrante = .
replace horas_migrante = horas_trabprin if sexo == 0

********
* INCOME
********
* Criar a variável income_sp_raw (apenas para homens)
gen income_sp_raw = .
replace income_sp_raw = rend_total if sexo == 1

* salva base
save "censo_2010_tratada_2025_03_02.dta", replace //ULTIMA BASE USADA

* keep only hh with at least one migrant women
bysort n_domicilio: egen main_sample = max(migrante_casada_15 == 1)

* gera base com somente migrantes casadas e suas/seus cônjuges
keep if main_sample == 1

gen cond_dom_B_corrigido = cond_dom_B

bysort id_dom (sexo cond_dom_B): replace cond_dom_B_corrigido = 2 if sexo == 0 & cond_dom_B == 1 & cond_dom_B_corrigido[_n-1] == 1
bysort id_dom (sexo cond_dom_B): replace cond_dom_B_corrigido = 1 if sexo == 0 & cond_dom_B == 2 & cond_dom_B_corrigido[_n-1] == 2


save "censo_2010_migrante_tratada_2025_03_02.dta", replace //ULTIMA BASE USADA

use "D:/1_migration/1_datasets_migrants/censo/2010/censo_2010_migrante_tratada_2025_03_02.dta", clear


