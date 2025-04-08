db datazoom_censo

datazoom_censo, years( 2010 ) ufs( RO AC AM RR PA AP TO MA PI CE RN PB PE AL SE BA MG ES RJ SP PR SC RS MS MT G
O DF ) original(C:\Users\thtak\OneDrive - Fundacao Getulio Vargas - FGV\Tese_Mestrado\tese_bases\censo\microdad
os_2010\2010_censo_pessoas) saving(C:\Users\thtak\OneDrive - Fundacao Getulio Vargas - FGV\Tese_Mestrado\censo_
2010\raw) pes



*************
* Read files
*************

* Censo 2010
* Set the directory where your files are located
cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_2010/raw"

* Create a local macro containing the names of all your files
local files CENSO10_AC_pes.dta CENSO10_AL_pes.dta CENSO10_AM_pes.dta /// 
            CENSO10_AP_pes.dta CENSO10_BA_pes.dta CENSO10_CE_pes.dta /// 
            CENSO10_DF_pes.dta CENSO10_ES_pes.dta CENSO10_GO_pes.dta /// 
            CENSO10_MA_pes.dta CENSO10_MG_pes.dta CENSO10_MS_pes.dta /// 
            CENSO10_MT_pes.dta CENSO10_PA_pes.dta CENSO10_PB_pes.dta /// 
            CENSO10_PE_pes.dta CENSO10_PI_pes.dta CENSO10_PR_pes.dta /// 
            CENSO10_RJ_pes.dta CENSO10_RN_pes.dta CENSO10_RO_pes.dta /// 
            CENSO10_RR_pes.dta CENSO10_RS_pes.dta CENSO10_SC_pes.dta /// 
            CENSO10_SE_pes.dta CENSO10_SP_pes.dta CENSO10_TO_pes.dta

* Load the first file
use "CENSO10_AC_pes.dta", clear

* Loop through the remaining files and append them
foreach file of local files {
    if "`file'" != "CENSO10_AC_pes.dta" {
        append using `file'
    }
}

* Save the combined dataset
save "pessoas_2000.dta", replace

* Open dataset
use "C:/Users/thtak/Documents/FGV/2024/tese/dados_div_migr/censo/microdados_2010/combined_data.dta", clear

******************
* MIGRACAO INTERNA
******************
cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/2010"

* Open dataset
use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_2010/raw/pessoas_2010.dta", clear

keep if v6036 >= 18 & v6036 <= 65

keep if v0418 == 2

save "censo_pessoas_2000_migracao.dta", replace

* sum (sidra)
summarize peso_pess [w=peso_pess] if idade >= 25 & idade <= 64

******************
* MANIPULA CENSO
******************
cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/2010"

* abre base pessoas 2010 completa
use "C:\Users\thtak\OneDrive - Fundacao Getulio Vargas - FGV\Tese_Mestrado\censo_2010\raw\pessoas_2010.dta", clear

* renomeia variaveis de uf de nascimento
replace v6222 = 11 if v6222 == 1100000
replace v6222 = 12 if v6222 == 1200000
replace v6222 = 13 if v6222 == 1300000
replace v6222 = 14 if v6222 == 1400000
replace v6222 = 15 if v6222 == 1500000
replace v6222 = 16 if v6222 == 1600000
replace v6222 = 17 if v6222 == 1700000
replace v6222 = 21 if v6222 == 2100000
replace v6222 = 22 if v6222 == 2200000
replace v6222 = 23 if v6222 == 2300000
replace v6222 = 24 if v6222 == 2400000
replace v6222 = 25 if v6222 == 2500000
replace v6222 = 26 if v6222 == 2600000
replace v6222 = 27 if v6222 == 2700000
replace v6222 = 28 if v6222 == 2800000
replace v6222 = 29 if v6222 == 2900000
replace v6222 = 31 if v6222 == 3100000
replace v6222 = 32 if v6222 == 3200000
replace v6222 = 33 if v6222 == 3300000
replace v6222 = 35 if v6222 == 3500000
replace v6222 = 41 if v6222 == 4100000
replace v6222 = 42 if v6222 == 4200000
replace v6222 = 43 if v6222 == 4300000
replace v6222 = 50 if v6222 == 5000000
replace v6222 = 51 if v6222 == 5100000
replace v6222 = 52 if v6222 == 5200000
replace v6222 = 53 if v6222 == 5300000
replace v6222 = 88 if v6222 == 8888888
replace v6222 = 99 if v6222 == 9900000

* transforma a variavel de uf em long para deixar v6222 e uf com o mesmo tipo de variavel
gen long uf = v0001

* no questionario quando a pessoa nasceu na mesma uf que esta sendo entrevistada, entao v6222 e missing
gen uf_nascim = v6222
replace uf_nascim = uf if missing(v6222)

* migrante quando uf_nascimento diferente de uf
gen migrante = (uf != uf_nascim)

* sexo
gen sexo = v0601

* numero domicilio
gen n_domicilio = v0300

*************
************
*** DUMMY ***
*************
*************

********
* CASAL
********
* responsavel e conjuge
gen responsavel = (v0502 == 1)
gen conjuge = (v0502 == 2 | v0502 == 3)

* gera casal
bysort n_domicilio: gen casal = (sum(conjuge == 1) > 0 & sum(responsavel == 1) > 0)

* casados
gen casado = 0
replace casado = 1 if v0640 == 1

* migrante_casada
gen migrante_casada = 0
replace migrante_casada = 1 if casado == 1 & sexo == 2 & migrante == 1
bysort n_domicilio: replace migrante_casada = 1 if responsavel == 1 & sexo == 2 & migrante == 1 & sum(conjuge == 1) > 0
replace migrante_casada = 1 if conjuge == 1 & sexo == 2 & migrante == 1

* Criar a variável migrante_casada_15
gen migrante_casada_15 = 0
replace migrante_casada_15 = 1 if migrante_casada == 1 & (v6036 - v0623 <= 15)


* tempo de moradia uf
gen moradia_uf = v0623
replace moradia_uf = 0 if missing(v0623)

********
* IDADE
********
* idade mulher
gen age_fem = . 
replace age_fem = v6036 if migrante_casada == 1

* idade mulher quadrado
gen age_squared_fem = . 
replace age_squared_fem = v6036^2 if migrante_casada == 1

* idade esposo
gen age_sp = . 
replace age_sp = v6036 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)

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
replace fund_mulher = 1 if migrante_casada == 1 & (v0633 == 5 | v0633 == 6 | v0633 == 7 | v0633 == 8)

gen fund_homem = 0
replace fund_homem = 1 if sexo == 1 & (v0633 == 5 | v0633 == 6 | v0633 == 7 | v0633 == 8)

gen em_mulher = 0
replace em_mulher = 1 if migrante_casada == 1 & (v0633 == 9 | v0633 == 10)

gen em_homem = 0
replace em_homem = 1 if sexo == 1 & (v0633 == 9 | v0633 == 10)

gen superior_mulher = 0
replace superior_mulher = 1 if migrante_casada == 1 & (v0633 == 11 | v0633 == 12 | v0633 == 13 | v0633 == 14)

gen superior_homem = 0
replace superior_homem = 1 if sexo == 1 & (v0633 == 11 | v0633 == 12 | v0633 == 13 | v0633 == 14)

* Criar a variável educ_mulher (apenas para migrante_casada_15 == 1)
gen educ_mulher = .
replace educ_mulher = v0633 if migrante_casada_15 == 1

* Criar a variável educ_homem (apenas para homens responsáveis, cônjuges ou casados)
gen educ_homem = .
replace educ_homem = v0633 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)


********
* FILHO
********
gen children_under_5 = 0
replace children_under_5 = 1 if v6660 <= 5

*******************
* HORAS TRABALHADAS
*******************
gen hworked_fem = .  
replace hworked_fem = v0653 if migrante_casada == 1

* Criar a variável ocup_migrante
gen ocup_migrante = .
replace ocup_migrante = v6910 if migrante_casada_15 == 1

* Criar a variável horas_migrante
gen horas_migrante = .
replace horas_migrante = v0653 if migrante_casada_15 == 1

********
* INCOME
********
gen income_sp = .  
replace income_sp = (12 * v6525) / 1000 if sexo == 1

// olhar para essa divisao por 10000, se faz sentido

* Criar a variável income_sp_raw (apenas para homens) - rend em todos os trabalhos
gen income_sp_raw = .
replace income_sp_raw = v6525 if sexo == 1

* salva base
save "censo_2010_migracao_tratada.dta", replace

* gera uma linha para cada domicilio
* Definir as variáveis que precisam ser preenchidas
local vars age_sp age_sp_range fund_mulher fund_homem em_mulher em_homem superior_mulher superior_homem educ_mulher educ_homem children_under_5 horas_migrante income_sp income_sp_raw

* Para cada variável, calcular o valor máximo por n_domicilio e substituir os valores faltantes (apenas onde migrante_casada_15 == 1)
foreach var of local vars {
    * Criar uma variável temporária com o valor máximo da variável por n_domicilio
    bysort n_domicilio: egen max_`var' = max(`var')

    * Preencher a variável original com o valor máximo, apenas se migrante_casada_15 == 1 e a variável estiver missing
    replace `var' = max_`var' if migrante_casada_15 == 1 & missing(`var')

    * Limpar a variável temporária
    drop max_`var'
}
* fazer para variaveis _homem
* Definir as variáveis dummies que precisam ser preenchidas
local dummy_vars em_homem fund_homem superior_homem

* Para cada variável dummy, copiar o valor 1 se houver dentro do mesmo domicílio
foreach var of local dummy_vars {
    * Criar uma variável temporária que verifica se há pelo menos um valor 1 por n_domicilio
    bysort n_domicilio: egen max_`var' = max(`var')

    * Preencher a variável original com o valor 1 se migrante_casada_15 == 1
    replace `var' = 1 if migrante_casada_15 == 1 & max_`var' == 1

    * Limpar a variável temporária
    drop max_`var'
}
* salva base
save "censo_2010_migracao_tratada_migrante_15.dta", replace

* deixa somente as linhas onde migrante_casada_15 == 1
keep if migrante_casada_15 == 1

* salva base
save "censo_2010_migracao_tratada_migrante_15_final.dta", replace


********************************************************************************


******************
* MIGRACAO INTERNA - COMBINED DATA
******************
cd "D:/1_migration/1_datasets_migrants/censo/2010"

* Open dataset
use "D:/antigo/Tese_Mestrado/censo_2010/combined_data.dta", clear

******************
* MANIPULA CENSO
******************

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
gen responsavel = (cond_dom == 1)
gen conjuge = (cond_dom == 2)

* gera casal
bysort n_domicilio: egen has_conjuge = total(conjuge == 1)
bysort n_domicilio: egen has_responsavel = total(responsavel == 1)
gen casal = (has_conjuge > 0 & has_responsavel > 0)

* casados
gen casado = 0
replace casado = 1 if inlist(estado_conj, 1, 2, 3, 4) | vive_conjuge == 1 | teve_conjuge == 1

* migrante_casada
gen migrante_casada = 0
replace migrante_casada = 1 if (casado == 1 & sexo == 0 & migrante == 1) | (conjuge == 1 & sexo == 0 & migrante == 1) | (responsavel == 1 & sexo == 0 & migrante == 1)
*bysort n_domicilio: replace migrante_casada = 1 if responsavel == 1 & sexo == 2 & migrante == 1 & sum(conjuge == 1) > 0
*replace migrante_casada = 1 if conjuge == 1 & sexo == 2 & migrante == 1

* Criar a variável migrante_casada_15
gen migrante_casada_15 = 0
replace migrante_casada_15 = 1 if migrante_casada == 1 & (idade - anos_mor_UF <= 15)

* tempo de moradia uf
gen moradia_uf = anos_mor_UF
replace moradia_uf = 999 if missing(anos_mor_UF)

********
* IDADE
********
* idade mulher
gen age_fem = . 
replace age_fem = idade if migrante_casada == 1

* idade mulher quadrado
gen age_squared_fem = . 
replace age_squared_fem = idade^2 if migrante_casada == 1

* idade esposo
gen age_sp = . 
replace age_sp = idade if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)

* mantenho obs em que temos pelo menos 1 mulher migrante na casa
bysort n_domicilio: egen main_sample = max(migrante_casada == 1)

* gera base com somente migrantes casadas e suas/seus cônjuges
keep if main_sample == 1

**************
* ESCOLARIDADE
**************
gen menos_fund_mulher = 0
replace menos_fund_mulher = 1 if migrante_casada == 1 & (anos_estudoC == 1)

gen menos_fund_homem = 0
replace menos_fund_homem = 1 if sexo == 1 & (anos_estudoC == 1)

gen fund_mulher = 0
replace fund_mulher = 1 if migrante_casada == 1 & (anos_estudoC == 2)

gen fund_homem = 0
replace fund_homem = 1 if sexo == 1 & (anos_estudoC == 2)

gen em_mulher = 0
replace em_mulher = 1 if migrante_casada == 1 & (anos_estudoC == 3)

gen em_homem = 0
replace em_homem = 1 if sexo == 1 & (anos_estudoC == 3)

gen superior_mulher = 0
replace superior_mulher = 1 if migrante_casada == 1 & (anos_estudoC == 4)

gen superior_homem = 0
replace superior_homem = 1 if sexo == 1 & (anos_estudoC == 4)

* Criar a variável educ_mulher (apenas para migrante_casada_15 == 1)
gen educ_mulher = .
replace educ_mulher = anos_estudoC if migrante_casada_15 == 1

* Criar a variável educ_homem (apenas para homens responsáveis, cônjuges ou casados)
gen educ_homem = .
replace educ_homem = anos_estudoC if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)


********
* FILHO
********
gen children = filhos_vivos

gen children_under_5 = 0
replace children_under_5 = 1 if idade_ult_nasc_v <= 5

*******************
* OCUPACAO
*******************
* Criar a variável ocup_migrante (trabalho remunerado semana de referencia)
gen ocup_migrante = 0
replace ocup_migrante = 1 if trab_rem_sem == 1 & migrante_casada == 1

* Criar a variável horas_migrante
gen horas_migrante = .
replace horas_migrante = horas_trabprin if migrante_casada == 1

********
* INCOME
********
* Criar a variável income_sp_raw (apenas para homens)
gen income_sp_raw = .
replace income_sp_raw = rend_total if sexo == 1

* salva base
save "censo_2010_migracao_tratada_2024_10_31.dta", replace //ULTIMA BASE USADA

* gera uma linha para cada domicilio
* Definir as variáveis que precisam ser preenchidas
local vars age_sp age_sp_range fund_mulher fund_homem em_mulher em_homem superior_mulher superior_homem educ_mulher educ_homem children_under_5 horas_migrante income_sp income_sp_raw

* Para cada variável, calcular o valor máximo por n_domicilio e substituir os valores faltantes (apenas onde migrante_casada_15 == 1)
foreach var of local vars {
    * Criar uma variável temporária com o valor máximo da variável por n_domicilio
    bysort n_domicilio: egen max_`var' = max(`var')

    * Preencher a variável original com o valor máximo, apenas se migrante_casada_15 == 1 e a variável estiver missing
    replace `var' = max_`var' if migrante_casada_15 == 1 & missing(`var')

    * Limpar a variável temporária
    drop max_`var'
}
* fazer para variaveis _homem
* Definir as variáveis dummies que precisam ser preenchidas
local dummy_vars em_homem fund_homem superior_homem

* Para cada variável dummy, copiar o valor 1 se houver dentro do mesmo domicílio
foreach var of local dummy_vars {
    * Criar uma variável temporária que verifica se há pelo menos um valor 1 por n_domicilio
    bysort n_domicilio: egen max_`var' = max(`var')

    * Preencher a variável original com o valor 1 se migrante_casada_15 == 1
    replace `var' = 1 if migrante_casada_15 == 1 & max_`var' == 1

    * Limpar a variável temporária
    drop max_`var'
}
* salva base
save "censo_2010_migracao_tratada_migrante_15.dta", replace

* deixa somente as linhas onde migrante_casada_15 == 1
keep if migrante_casada_15 == 1

* salva base
save "censo_2010_migracao_tratada_migrante_15_final.dta", replace


* MIGRACAO INTERNA TOTAL - COM VARIAVEIS NECESSARIAS
cd "D:/1_migration/1_datasets_migrants/censo/2010"
use "D:/1_migration/1_datasets_migrants/censo/2010/censo_2010_migracao_tratada_2024_10_31.dta", clear

drop dif_enxergar dif_ouvir dif_caminhar def_mental nacionalidade ano_fix_res curso_concl mun_escola qtos_empregados previd_B f_nasc_v_hom f_nasc_v_mul f_vivos_hom f_vivos_mul f_nasc_m_hom f_nasc_m_mul filhos_nasc_mortos m0502 m0601 m6033 m0606 m0613 m0614 m0615 m0616 m0617 m0618 m0619  m0620  m0621  m0622  m6222  m6224  m0623  m0624  m0625  m6252  m6254  m6256  m0626  m6262  m6264  m6266  m0627  m0628  m0629  m0630  m0631  m0632  m0633  m0634  m0635  m6352  m6354  m6356  m0636  m6362  m6364  m6366  m0637  m0638  m0639  m0640  m0641  m0642  m0643  m0644  m0645  m6461  m6471  m0648  m0649  m0650  m0651  m6511  m0652  m6521  m0653  m0654  m0655  m0656  m0657  m0658  m0659  m6591  m0660  m6602  m6604  m6606  m0661  m0662  m0663  m6631  m6632  m6633  m0664  m6641  m6642  m6643  m0665  m6660  m0667  m0668  m6681  m6682  m0669  m6691  m6692  m6693  m0670  m0671  m6800  m6121  m0604  m0605  m6462  m6472 pais_nascim pais_mor_ant pais_mor5anos amc0010 newamc0010 amc9100 newamc9100 amc7000 newamc7000 

drop anos_mor_mun mun_mor_ant mun_mor5anos alfabetizado rede_freq mais_de_um_trab ocup2010 ativ2010 horas_trabprin mun_trab sexo_ult_nasc_v ocup2000 ativ2000 n_homem_dom n_mulher_dom n_homem_fam n_mulher_fam sempre_morou t_mor_UF_70 t_mor_mun_70 t_mor_UF_80 t_mor_mun_80 freq_escola freq_escolaB cursos_c1 cursos_c2

save "censo_2010_migracao_tratada_2025_03_01.dta", replace





