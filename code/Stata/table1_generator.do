local root "C:\Users\a.santos\OneDrive - UAM\Escritorio\UAM_Sarcopenia\Functional-Decline-ENRICA-BKMR" //Introduce here your path to the repository
cd "`root'\data"

use "bd_metales_deteriorommii.dta", replace

//Prepare the correct variables
capture gen gedad = edadw0
quietly sum gedad
local min = r(min)
local max = r(max)
recode gedad `min'/74=0 75/`max'=1

capture gen fuma2 = w17fuma
replace fuma2 = 1 if fuma2==2

capture gen educa2 = w17educa_3cat
replace educa2 = 1 if educa2==2

capture gen medas2 = w17xmedas
quietly sum medas2
local min = r(min)
local max = r(max)
recode medas2 `min'/7=0 8/`max'=1

quietly sum w17modvigpa_mets, d
local p50 = r(p50)
capture gen pa2 = w17modvigpa_mets
replace pa2 = 0 if pa2 < `p50'
replace pa2 = 1 if pa2 >= `p50' & pa2!=.

drop if missing(w17vis_enf) //No blood sample was extracted
drop if missing(Al, Co, Cr, Cu, Fe, Mg, Mn, Mo, Ni, Pb, Se, V, Zn)
drop if missing(w17sexo, edadw0, w17fuma, w17educa_3cat, w17imc3, alcohol4, w17xmedas, w17modvigpa_mets)
drop if w17IR_DEGREES==. | w17IR_DEGREES>=5 //We drop patients with severe kidney chronic disease or missing as we cannot trust those measurements


tempname tabla1

postfile `tabla1' str30 Variables ///
					str12 N ///
					str12 Al ///
                    str10 Co ///
                    str10 Cr ///
					str12 Cu ///
					str12 Fe ///
					str13 Mg ///
					str10 Mn ///
					str10 Mo ///
					str10 Ni ///
					str10 Pb ///
					str12 Se ///
					str10 V ///
					str12 Zn ///
                    using tabla1_temp.dta, replace
					

//OVERALL
count
local N_total = r(N)
foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
	capture gen ln`var'=ln(`var')
	quietly sum ln`var'
	local gmean : di %2.1f exp(r(mean))
	local gsd : di %2.1f exp(r(sd))
	local `var'_stat = "`gmean' (`gsd')"
}

post `tabla1' ("Overall") ("`N_total'") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")


//AGE
post `tabla1' ("Age (years)") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags <75 >=75

forvalues k = 0/1{
	local i = `k'+1
	local tag : word `i' of `tags'
	preserve
	drop if gedad!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}

//SEX
post `tabla1' ("Sex") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags Male Female

forvalues k = 0/1{
	local i = `k'+1
	local tag : word `i' of `tags'
	preserve
	drop if w17sexo!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}

//SMOKING
post `tabla1' ("Smoking") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags Never Ex-smoker Current

forvalues k = 0/2{
	local i = `k'+1
	local tag : word `i' of `tags'
	preserve
	drop if w17fuma!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}

//EDUCATION LEVEL
post `tabla1' ("Education level") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags <Secondary Secondary >Secondary

forvalues k = 0/2{
	local i = `k'+1
	local tag : word `i' of `tags'
	preserve
	drop if w17educa_3cat!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}

//BMI
post `tabla1' ("BMI (Kg/m2)") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags <25 25-30 >=30

forvalues k = 0/2{
	local i = `k'+1
	local tag : word `i' of `tags'
	preserve
	drop if w17imc3!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}

//ALCOHOL
post `tabla1' ("Drinking status") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags Never_drinker Moderate_drinker Heavy_drinker Ex_drinker

forvalues k = 0/3{
	local i = `k'+1
	local tag : word `i' of `tags'
	preserve
	drop if alcohol4!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}

//DIET
post `tabla1' ("MEDAS score (quartiles)") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags Q1 Q2 Q3 Q4	

forvalues k = 1/4{
	local tag : word `k' of `tags'
	preserve
	drop if medas4!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}

//PHYSICAL ACTIVITY
post `tabla1' ("Physical activity (METs-h/wk quartiles") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("") ("")
local tags Q1 Q2 Q3 Q4	

forvalues k = 1/4{
	local tag : word `k' of `tags'
	preserve
	drop if pa4!=`k'
	
	count
	local N = r(N)
	local porc : di %2.1f `N'/`N_total'*100
	
	foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn{
		capture gen ln`var'=ln(`var')
		quietly sum ln`var'
		local gmean : di %2.1f exp(r(mean))
		local gsd : di %2.1f exp(r(sd))
		local `var'_stat = "`gmean' (`gsd')"
	}

	post `tabla1' ("`tag'") ("`N' (`porc'%)") ("`Al_stat'") ("`Co_stat'") ("`Cr_stat'") ("`Cu_stat'") ("`Fe_stat'") ("`Mg_stat'") ("`Mn_stat'") ("`Mo_stat'") ("`Ni_stat'") ("`Pb_stat'") ("`Se_stat'") ("`V_stat'") ("`Zn_stat'")

	restore
}


postclose `tabla1'

preserve

use tabla1_temp.dta, clear

asdoc list, title(Table 1) save(../tables/table1.doc) replace

restore
shell del "tabla1_temp.dta"