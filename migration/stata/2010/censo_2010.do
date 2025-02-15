db datazoom_censo

datazoom_censo, years( 2010 ) ufs( RO AC AM RR PA AP TO MA PI CE RN PB PE AL SE ///
BA MG ES RJ SP PR SC RS MS MT GO DF ) original(C:\Users\thtak\Documents\FGV\2024\///
tese\dados_div_migr\censo\microdados_2010) saving(C:\Users\thtak\Documents\FGV\2024\///
tese\dados_div_migr\censo\dados_gerados) comp pes

*************
* Read files
*************

* Censo 2010
* Set the directory where your files are located
cd "C:/Users/thtak/Documents/FGV/2024/tese/dados_div_migr/censo/microdados_2010"

* Create a local macro containing the names of all your files
local files CENSO10_AC_pes_comp.dta CENSO10_AL_pes_comp.dta CENSO10_AM_pes_comp.dta /// 
            CENSO10_AP_pes_comp.dta CENSO10_BA_pes_comp.dta CENSO10_CE_pes_comp.dta /// 
            CENSO10_DF_pes_comp.dta CENSO10_ES_pes_comp.dta CENSO10_GO_pes_comp.dta /// 
            CENSO10_MA_pes_comp.dta CENSO10_MG_pes_comp.dta CENSO10_MS_pes_comp.dta /// 
            CENSO10_MT_pes_comp.dta CENSO10_PA_pes_comp.dta CENSO10_PB_pes_comp.dta /// 
            CENSO10_PE_pes_comp.dta CENSO10_PI_pes_comp.dta CENSO10_PR_pes_comp.dta /// 
            CENSO10_RJ_pes_comp.dta CENSO10_RN_pes_comp.dta CENSO10_RO_pes_comp.dta /// 
            CENSO10_RR_pes_comp.dta CENSO10_RS_pes_comp.dta CENSO10_SC_pes_comp.dta /// 
            CENSO10_SE_pes_comp.dta CENSO10_SP_pes_comp.dta CENSO10_TO_pes_comp.dta

* Load the first file
use "CENSO10_AC_pes_comp.dta", clear

* Loop through the remaining files and append them
foreach file of local files {
    if "`file'" != "CENSO10_AC_pes_comp.dta" {
        append using `file'
    }
}

* Save the combined dataset
save "combined_data.dta", replace

* Open dataset
use "C:/Users/thtak/Documents/FGV/2024/tese/dados_div_migr/censo/microdados_2010/combined_data.dta", clear


