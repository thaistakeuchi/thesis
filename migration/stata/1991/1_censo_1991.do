***************************************
* Censo 1970
********* NAO COMPATIBILIZADA *********

cd "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/1_migration/1_datasets_migrants/censo/1991"

use "C:/Users/thtak/OneDrive - Fundacao Getulio Vargas - FGV/Tese_Mestrado/censo_1991/censo_1991_raw.dta", clear

keep if inrange(V030, 28, 99)
