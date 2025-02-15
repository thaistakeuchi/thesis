* Thais Takeuchi
* Master thesis
* 2024
* Script: manipulate variables and combine data censo 2000

* Choosing variables
** compatibilizada
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

** nao compatibilizada
use "E:/Thais/Tese_Mestrado/tese_bases/censo/base_stata_2000_sem_comp/censo_2000_sem_comp.dta", clear

use v0102 v1002 v1003 v0103 v0104 v0105 v0300 v0400 v1004 AREAP v1001 v1005 v1006 ///
v0401 v0402 v0403 v0404 v4752 v0408 v4090 v0419 v0420 v4210 v0428 v0432 v0434 v4300 ///
v0436 v0437 v0438 v4512 v4513 v0453 v4534 v0463 v4654 P001 ESTR ESTRP v4631 v4632 ///
v0464 v4219 ano munic v4260 using censo_2000_sem_comp.dta, clear

save censo_pessoas_2000_sem_comp.dta
