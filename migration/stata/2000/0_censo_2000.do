* para o ano de 2000
db datazoom_censo 

*************
* Read files
*************

* Censo 2000
******* COMPATIBILIZADA *******
* Set the directory where your files are located
cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/tese_bases/censo/microdados_2000/2000_censo_pessoas"

* Create a local macro containing the names of all your files
local files CENSO00_AC_pes_comp.dta CENSO00_AL_pes_comp.dta CENSO00_AM_pes_comp.dta /// 
            CENSO00_AP_pes_comp.dta CENSO00_BA_pes_comp.dta CENSO00_CE_pes_comp.dta /// 
            CENSO00_DF_pes_comp.dta CENSO00_ES_pes_comp.dta CENSO00_GO_pes_comp.dta /// 
            CENSO00_MA_pes_comp.dta CENSO00_MG_pes_comp.dta CENSO00_MS_pes_comp.dta /// 
            CENSO00_MT_pes_comp.dta CENSO00_PA_pes_comp.dta CENSO00_PB_pes_comp.dta /// 
            CENSO00_PE_pes_comp.dta CENSO00_PI_pes_comp.dta CENSO00_PR_pes_comp.dta /// 
            CENSO00_RJ_pes_comp.dta CENSO00_RN_pes_comp.dta CENSO00_RO_pes_comp.dta /// 
            CENSO00_RR_pes_comp.dta CENSO00_RS_pes_comp.dta CENSO00_SC_pes_comp.dta /// 
            CENSO00_SE_pes_comp.dta CENSO00_SP_pes_comp.dta CENSO00_TO_pes_comp.dta

* Load the first file
use "CENSO00_AC_pes_comp.dta", clear

* Loop through the remaining files and append them
foreach file of local files {
    if "`file'" != "CENSO00_AC_pes_comp.dta" {
        append using `file'
    }
}

* Save the combined dataset
save "censo_2000_raw.dta", replace

* Open dataset
use "E:/Thais/Tese_Mestrado/censo_2000/censo_2000_raw.dta", clear

 use ano UF regiao munic id_dom ordem peso_pess sit_setor_C cond_dom sexo idade ///
idade_meses idade raca nasceu_mun nasceu_UF nacionalidade ano_fix_res ///
UF_nascim anos_mor_UF anos_mor_mun UF_mor_ant UF_mor5anos ///
mun_mor5anos alfabetizado rede_freq curso_concl mun_escola vive_conjuge ///
rend_ocup_prin filhos_nasc_vivos filhos_vivos religiao num_fam n_pes_fam ///
rend_fam racaB religiao_B sempre_morou pais_nascim pais_mor_ant t_mor_UF_70 t_mor_mun_70 ///
t_mor_UF_80 t_mor_mun_80 pais_mor5anos freq_escola freq_escolaB anos_estudoC ///
cursos_c1 cursos_c2 teve_conjuge estado_conj deflator conversor ///
rend_ocup_prin_def rend_total_def rend_fam_def codmun using censo_2000_raw.dta, clear

save censo_pessoas_2000.dta

use "E:\Thais\Tese_Mestrado\censo_2000\censo_pessoas_2000.dta", clear
keep if idade >= 25 & idade <= 64
save "censo_pessoas_2000_25_64.dta", replace
********************************************************************************
* Censo 2000
********* NAO COMPATIBILIZADA *********
* Set the directory where your files are located
cd "E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp"

* Create a local macro containing the names of all your files
local files CENSO00_AC_pes.dta CENSO00_AL_pes.dta CENSO00_AM_pes.dta /// 
            CENSO00_AP_pes.dta CENSO00_BA_pes.dta CENSO00_CE_pes.dta /// 
            CENSO00_DF_pes.dta CENSO00_ES_pes.dta CENSO00_GO_pes.dta /// 
            CENSO00_MA_pes.dta CENSO00_MG_pes.dta CENSO00_MS_pes.dta /// 
            CENSO00_MT_pes.dta CENSO00_PA_pes.dta CENSO00_PB_pes.dta /// 
            CENSO00_PE_pes.dta CENSO00_PI_pes.dta CENSO00_PR_pes.dta /// 
            CENSO00_RJ_pes.dta CENSO00_RN_pes.dta CENSO00_RO_pes.dta /// 
            CENSO00_RR_pes.dta CENSO00_RS_pes.dta CENSO00_SC_pes.dta /// 
            CENSO00_SE_pes.dta CENSO00_SP_pes.dta CENSO00_TO_pes.dta

* Load the first file
use "CENSO00_AC_pes.dta", clear

* Loop through the remaining files and append them
foreach file of local files {
    if "`file'" != "CENSO00_AC_pes.dta" {
        append using `file'
    }
}

* Save the combined dataset
save "censo_2000_sem_comp.dta", replace

* Open dataset
use "E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_2000_sem_comp.dta", clear

* Filtra os estrangeiros e brasileiros naturalizados
keep if v0419 == 2 | v0419 == 3

* Essa base gerada sera fonte da replicacao do artigo da aer fernandez and fogli (2009)
save "cp2000_nc_ff.dta", replace

* Open dataset
use "E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_2000_sem_comp.dta", clear

* Filtra os estrangeiros e brasileiros naturalizados
keep if v0419 == 2 | v0419 == 3

* Filtra estrangeiros entre 25 e 64 anos
keep if v4752 >= 25 & v4752 <= 64

* Escolhe variaveis
*use v0102 v1002 v1003 v0103 v0104 v0105 v0300 v0400 v1004 AREAP v1001 v1005 v1006 ///
v0401 v0402 v0402 v0404 v4752 v0408 v4090 v0419 v0420 v4210 v0428 v0432 v0434 v4300 ///
v0436 v0437 v0438 v4512 v4513 v0453 v0453 v0463 v4654 P001 ESTR ESTRP v4631 v4632 ///
v0464 v4219 ano munic v4260 using censo_2000_sem_comp.dta, clear

* Esta base gera a replicacao: cultura afeta o divorcio
save "censo_pessoas_2000_filtrado_nao_comp.dta", replace

******************
* MIGRACAO INTERNA
******************
cd "E:\Thais\Tese_Mestrado\1_migration\1_datasets_migrants\censo\2000"

* Open dataset
use "E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_2000_sem_comp.dta", clear

keep if v4752 >= 18 & v4752 <= 65

keep if v0418 == 2

save "censo_pessoas_2000_migracao.dta", replace

******************
* MANIPULA CENSO
******************
cd "D:/1_migration/1_datasets_migrants/censo/2000"

* abre base pessoas 2000 completa
*use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_2000/censo_2000_raw", clear // compatibilizada

use "D:/antigo/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_2000_sem_comp.dta", clear // nao compatibilizada

* só deixa v4219 com estados brasileiros
keep if inlist(v4219, 901, 902, 903, 904, 905, 906, 907, 908, 909, 910, 911, 912, 913, 914, 915, 916, 917, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927) | missing(v4219)

* gera uf_nascim
gen uf_nascim = .  
replace uf_nascim = v4219 if inlist(v4219, 901, 902, 903, 904, 905, 906, 907, 908, 909, 910, 911, 912, 913, 914, 915, 916, 917, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927)

* uf com estados do brasil
gen uf_nascim_new = uf_nascim

* renomeia nome uf
replace uf_nascim_new = 11 if uf_nascim == 901
replace uf_nascim_new = 12 if uf_nascim == 902
replace uf_nascim_new = 13 if uf_nascim == 903
replace uf_nascim_new = 14 if uf_nascim == 904
replace uf_nascim_new = 15 if uf_nascim == 905
replace uf_nascim_new = 16 if uf_nascim == 906
replace uf_nascim_new = 17 if uf_nascim == 907

replace uf_nascim_new = 21 if uf_nascim == 908
replace uf_nascim_new = 22 if uf_nascim == 909
replace uf_nascim_new = 23 if uf_nascim == 910
replace uf_nascim_new = 24 if uf_nascim == 911
replace uf_nascim_new = 25 if uf_nascim == 912
replace uf_nascim_new = 26 if uf_nascim == 913
replace uf_nascim_new = 27 if uf_nascim == 914
replace uf_nascim_new = 28 if uf_nascim == 915
replace uf_nascim_new = 29 if uf_nascim == 916

replace uf_nascim_new = 31 if uf_nascim == 917
replace uf_nascim_new = 32 if uf_nascim == 918
replace uf_nascim_new = 33 if uf_nascim == 919
replace uf_nascim_new = 35 if uf_nascim == 920

replace uf_nascim_new = 41 if uf_nascim == 921
replace uf_nascim_new = 42 if uf_nascim == 922
replace uf_nascim_new = 43 if uf_nascim == 923

replace uf_nascim_new = 50 if uf_nascim == 924
replace uf_nascim_new = 51 if uf_nascim == 925
replace uf_nascim_new = 52 if uf_nascim == 926
replace uf_nascim_new = 53 if uf_nascim == 927

drop uf_nascim
rename uf_nascim_new uf_nascim

* no questionario quando a pessoa nasceu na mesma uf que esta sendo entrevistada, entao v0102 e missing
gen uf = v0102
replace uf_nascim = uf if missing(uf_nascim)

* migrante quando uf_nascimento diferente de uf
gen migrante = (uf != uf_nascim)

* sexo
gen sexo = v0401

* numero domicilio
gen n_domicilio = v0300

save "censo_2000_temp.dta", replace

use "D:/1_migration/1_datasets_migrants/censo/2000/censo_2000_temp.dta", clear 

* emigracao e imigracao 
collapse (sum) emigrantes = P001, by(uf_nascim)

* Ordene de forma decrescente para obter os estados com mais emigrantes
sort emigrantes
gsort -emigrantes

* Exiba os top 5 estados com maior número de emigrantes
list uf_nascim emigrantes in 1/5


use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/2000/censo_2000_temp.dta", clear 

*************
************
*** DUMMY ***
*************
*************

********
* CASAL
********
* responsavel e conjuge
gen responsavel = (v0402 == 1)
gen conjuge = (v0402 == 2)

* gera casal
bysort n_domicilio: gen casal = (sum(conjuge == 1) > 0 & sum(responsavel == 1) > 0)

* casados
gen casado = 0
replace casado = 1 if v0438 == 1

* migrante_casada
gen migrante_casada = 0
replace migrante_casada = 1 if (casado == 1 & sexo == 2 & migrante == 1) | (conjuge == 1 & sexo == 2 & migrante == 1) | (responsavel == 1 & sexo == 2 & migrante == 1)
*bysort n_domicilio: replace migrante_casada = 1 if responsavel == 1 & sexo == 2 & migrante == 1 & sum(conjuge == 1) > 0
*replace migrante_casada = 1 if conjuge == 1 & sexo == 2 & migrante == 1

* tempo de moradia uf
gen moradia_uf = v0422
replace moradia_uf = 999 if missing(v0422)

* Criar a variável migrante_casada_15
gen migrante_casada_15 = 0
replace migrante_casada_15 = 1 if migrante_casada == 1 & (v4752 - moradia_uf <= 15 & v4752 - moradia_uf >= 0)

// Manter apenas os domicílios onde existe pelo menos um responsável e um cônjuge
bysort n_domicilio: egen count_responsavel = max(responsavel)
bysort n_domicilio: egen count_conjuge = max(conjuge)

keep if count_responsavel == 1 & count_conjuge == 1

********
* IDADE
********
* idade mulher
gen age_fem = . 
replace age_fem = v4752 if migrante_casada_15 == 1

* idade mulher quadrado
gen age_squared_fem = . 
replace age_squared_fem = v4752^2 if migrante_casada_15 == 1

* idade esposo
gen age_sp = . 
replace age_sp = v4752 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)

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
bysort n_domicilio: egen main_sample = max(migrante_casada_15 == 1)

* gera base com somente migrantes casadas e suas/seus cônjuges
keep if main_sample == 1

**************
* ESCOLARIDADE
**************
gen fund_mulher = 0
replace fund_mulher = 1 if migrante_casada_15 == 1 & (v0432 == 3 | v0432 == 5)

gen fund_homem = 0
replace fund_homem = 1 if sexo == 1 & (v0432 == 3 | v0432 == 5)

gen em_mulher = 0
replace em_mulher = 1 if migrante_casada_15 == 1 & (v0432 == 4 | v0432 == 6)

gen em_homem = 0
replace em_homem = 1 if sexo == 1 & (v0432 == 4 | v0432 == 6)

gen superior_mulher = 0
replace superior_mulher = 1 if migrante_casada_15 == 1 & (v0432 == 7 | v0432 == 8)

gen superior_homem = 0
replace superior_homem = 1 if sexo == 1 & (v0432 == 7 | v0432 == 8)

* Criar a variável educ_mulher (apenas para migrante_casada_15_15 == 1)
gen educ_mulher = .
replace educ_mulher = v0432 if migrante_casada_15 == 1

* Criar a variável educ_homem (apenas para homens responsáveis, cônjuges ou casados)
gen educ_homem = .
replace educ_homem = v0432 if sexo == 1 & (responsavel == 1 | conjuge == 1 | casado == 1)


********
* FILHO
********
* gen variable children
gen children = v4620

gen children_under_5 = 0
replace children_under_5 = 1 if v4654 <= 5

*******************
* HORAS TRABALHADAS
*******************
* Criar a variável horas_migrante
gen horas_migrante_15 = .  
replace horas_migrante_15 = v4534 if migrante_casada_15 == 1

* Criar a variável ocup_migrante
gen ocupada_migrante_15 = .
replace ocupada_migrante_15 = 1 if v0439 == 1 & migrante_casada_15 == 1
replace ocupada_migrante_15 = 0 if v0439 == 2 & migrante_casada_15 == 1

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
save "censo_2000_migracao_tratada_v2.dta", replace

use "D:/1_migration/1_datasets_migrants/censo/2000/censo_2000_migracao_tratada_v2.dta", clear 

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

* conta missing values
foreach var of varlist migrante_casada_15 age_fem age_squared_fem age_sp fund_mulher fund_homem em_mulher em_homem superior_mulher superior_homem children_under_5 horas_migrante_15 ocup_migrante income_sp_raw {
    gen `var'_missing = missing(`var')
    quietly count if missing(`var')
    di "`var' has " r(N) " missing values"
}
* verification NA values
misstable summarize horas_migrante_15 income_sp_raw




