local root "C:\Users\a.santos\OneDrive - UAM\Escritorio\UAM_Sarcopenia\Functional-Decline-ENRICA-BKMR" //Introduce here your path to the repository
cd "`root'\tables\pips"


local model = "model1" //State here for which model you want to generate your PIPs table

local var_labels Grip_strength ///
				 Chair_stand ///
				 Calf_circumference ///
				 SPPB ///
				 Gait_speed ///
				 Sarcopenia

local i = 0

tempname pips

postfile `pips' str30 Variables ///
					str5 Al ///
                    str5 Co ///
                    str5 Cr ///
					str5 Cu ///
					str5 Fe ///
					str5 Mg ///
					str5 Mn ///
					str5 Mo ///
					str5 Ni ///
					str5 Pb ///
					str5 Se ///
					str5 V ///
					str5 Zn ///
                    using pips_temp.dta, replace
					
foreach var in grip chair calf sppb vel sarcopenia{
	local i = `i'+1
	local label : word `i' of `var_labels'
	
	post `pips' ("`label'") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
	
	import delimited "serum_ALL_`var'_`model'.csv", delimiter(comma) clear
	
	foreach metal in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		quietly sum pip if metal== "`metal'"
		local `metal'_pip : di %4.2f r(mean)
		dis "``metal'_pip'"
	}
	post `pips' ("   All") ("`Al_pip'") ("`Co_pip'") ("`Cr_pip'") ("`Cu_pip'") ("`Fe_pip'") ("`Mg_pip'") ("`Mn_pip'") ("`Mo_pip'") ("`Ni_pip'") ("`Pb_pip'") ("`Se_pip'") ("`V_pip'") ("`Zn_pip'")
	
	import delimited "serum_ESSENTIAL_`var'_`model'.csv", delimiter(comma) clear
	
	foreach metal in Co Fe Mg Mn Mo Ni Se V Zn{
		quietly sum pip if metal== "`metal'"
		local `metal'_pip : di %4.2f r(mean)
		dis "``metal'_pip'"
	}
	post `pips' ("   Essential") ("") ("`Co_pip'") ("") ("") ("`Fe_pip'") ("`Mg_pip'") ("`Mn_pip'") ("`Mo_pip'") ("`Ni_pip'") ("") ("`Se_pip'") ("`V_pip'") ("`Zn_pip'")
	
	import delimited "serum_TOXIC_`var'_`model'.csv", delimiter(comma) clear
	
	foreach metal in Al Cr Cu Pb{
		quietly sum pip if metal== "`metal'"
		local `metal'_pip : di %4.2f r(mean)
		dis "``metal'_pip'"
	}
	post `pips' ("   Toxic") ("`Al_pip'") ("") ("`Cr_pip'") ("`Cu_pip'") ("") ("") ("") ("") ("") ("`Pb_pip'") ("") ("") ("")

}


postclose `pips'

preserve

use pips_temp.dta, clear

asdoc list, title(Supplementary Table S3: Posterior Inclusion Probabilities of serum metals for each outcome across all, essential and toxic mixtures in model 1) save(../pips_`model'_table.doc) replace

restore
shell del "pips_temp.dta"