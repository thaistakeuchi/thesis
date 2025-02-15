* Thais Takeuchi
* Master thesis
* 2024
* Script: manipulate variables and combine data censo 2010

* Choosing variables

 use ano UF regiao munic id_dom ordem peso_pess sit_setor_C cond_dom sexo idade ///
idade_meses idade raca nasceu_mun nasceu_UF nacionalidade ano_fix_res ///
UF_nascim anos_mor_UF anos_mor_mun UF_mor_ant mun_mor_ant UF_mor5anos ///
mun_mor5anos alfabetizado rede_freq curso_concl mun_escola vive_conjuge ///
rend_ocup_prin filhos_nasc_vivos filhos_vivos religiao num_fam n_pes_fam ///
rend_fam m0601 m6033 m0606 m0618 m0619 m0620 m0621 m0622 m6222 m6224 m0623 ///
m0624 m0625 m6252 m6254 m6256 m0626 m6262 m6264 m6266 m0627 m0628 m0629 m0630 ///
m0631 m0632 m0633 m0634 m0635 m6352 m6354 m6356 m0636 m6362 m6364 m6366 m0637 ///
m0638 m0640 m0641 m0651 m6511 m0663 m0671 m6462 m6472 v1005 racaB religiao_B ///
sempre_morou pais_nascim pais_mor_ant t_mor_UF_70 t_mor_mun_70 ///
t_mor_UF_80 t_mor_mun_80 pais_mor5anos freq_escola freq_escolaB anos_estudoC ///
cursos_c1 cursos_c2 teve_conjuge estado_conj deflator conversor ///
rend_ocup_prin_def rend_total_def rend_fam_def codmun using combined_data.dta, clear

save censo_pessoas_2010.dta, replace
 
