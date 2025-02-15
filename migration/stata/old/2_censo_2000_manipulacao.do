* Salva nova base advinda da base censo_2000.dta com pessoas entre 25 e 64 anos
** compatibilizada
use "E:\Thais\Tese_Mestrado\censo_2000\censo_pessoas_2000.dta", clear
keep if idade >= 25 & idade <= 64
save "censo_pessoas_2000_25_64.dta", replace

** nao compatibilizada
use "E:\Thais\Tese_Mestrado\tese_bases\censo\base_stata_2000_sem_comp\censo_pessoas_2000_sem_comp.dta", clear
keep if v4752 >= 25 & v4752 <= 64

* Filtra as observações onde v0438 é igual a 1, 2 ou 3
keep if v0438 == 1 | v0438 == 2 | v0438 == 3

* Filtra os estrangeiros e brasileiros naturalizados
keep if v0419 == 2 | v0419 == 3

save "censo_pessoas_2000_25_64_nao_comp.dta", replace

