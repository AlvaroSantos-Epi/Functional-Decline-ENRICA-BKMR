local root "" //Introduce here your path to the repository
cd "`root'\Functional-Decline-ENRICA-BKMR"

use "data/bd_metales_deteriorommii.dta", replace

drop if w17IR_DEGREES==. | w17IR_DEGREES>=5 //We firstly drop patients with severe kidney chronic disease or missing as we cannot trust those measurements
drop if missing(w17sexo, edadw0, w17fuma, w17educa_3cat, w17imc3)


	
tempname tabla2

postfile `tabla2' str28 a ///
					str20 SPPB ///
					str8 b ///
                    str20 Gait_Seed ///
					str8 c ///
                    str20 Grip_Strength ///
					str8 d ///
                    using tabla2_temp.dta, replace



post `tabla2' ("") ("OR (95% CI)") ("P-value")  ("OR (95% CI)") ("P-value")  ("OR (95% CI)") ("P-value")

foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn {
	capture drop log`var' log`var'4
	gen log`var'=log(`var')
	xtile log`var'4 = log`var', nq(4)	
	
	*Here we spcifiy the desired outcomes to be evaluated. In case of needing to check any other outcome, add the it to the for loop on line 38 and a custom name to outcome_names in the same position
	local outcomes sppb ///
                     vel ///
                     grip
					 
	//Log continuo
	local i = 1
	
	foreach outcome_var in ewgsop2_perform_sppbw0 ewgsop2_perform_velw0 ewgsop2_strength_gripw0{ 
		
		local outcome : word `i' of `outcomes'
		logistic `outcome_var' log`var' w17sexo edadw0 w17fuma w17educa_3cat w17imc3
		matrix b = r(table)
		
		local or : di %4.2f b[1,1]
		local ll : di %4.2f b[5,1]
		local ul : di %4.2f b[6,1]
		local or_`outcome' = "`or' (`ll'-`ul')"
		
		local p_`outcome' : di %4.3f b[4,1]
		if `p_`outcome'' == 0.000 {
			local p_`outcome' "<0.001"
		}
		
		local i = `i'+1
	}
	
	post `tabla2' ("Log `var'") ("`or_sppb'") ("`p_sppb'")  ("`or_vel'") ("`p_vel'") ("`or_grip'") ("`p_grip'")
	
	
	//Cuartiles
	local q1 = "1 (Ref)"
	local i = 1
	
	foreach outcome_var in ewgsop2_perform_sppbw0 ewgsop2_perform_velw0 ewgsop2_strength_gripw0{
		
		local outcome : word `i' of `outcomes'
		logistic `outcome_var' i.log`var'4 w17sexo edadw0 w17fuma w17educa_3cat w17imc3
		matrix b = r(table)
		
		local q2_or : di %4.2f b[1,2]
		local q2_ll : di %4.2f b[5,2]
		local q2_ul : di %4.2f b[6,2]
		local q2_`outcome' = "`q2_or' (`q2_ll'-`q2_ul')"
		
		local p2_`outcome' : di %4.3f b[4,2]
		if `p2_`outcome'' == 0.000 {
			local p2_`outcome' "<0.001"
		}
		
		local q3_or : di %4.2f b[1,3]
		local q3_ll : di %4.2f b[5,3]
		local q3_ul : di %4.2f b[6,3]
		local q3_`outcome' = "`q3_or' (`q3_ll'-`q3_ul')"
		
		local p3_`outcome' : di %4.3f b[4,3]
		if `p3_`outcome'' == 0.000 {
			local p3_`outcome' "<0.001"
		}
		
		local q4_or : di %4.2f b[1,4]
		local q4_ll : di %4.2f b[5,4]
		local q4_ul : di %4.2f b[6,4]
		local q4_`outcome' = "`q4_or' (`q4_ll'-`q4_ul')"
		
		local p4_`outcome' : di %4.3f b[4,4]
		if `p4_`outcome'' == 0.000 {
			local p4_`outcome' "<0.001"
		}
		
		local i = `i'+1
	}

	post `tabla2' ("Q1") ("`q1'") ("")  ("`q1'") ("")  ("`q1'") ("")
	post `tabla2' ("Q2") ("`q2_sppb'") ("`p2_sppb'")  ("`q2_vel'") ("`p2_vel'") ("`q2_grip'") ("`p2_grip'")
	post `tabla2' ("Q3") ("`q3_sppb'") ("`p3_sppb'")  ("`q3_vel'") ("`p3_vel'") ("`q3_grip'") ("`p4_grip'")
	post `tabla2' ("Q4") ("`q4_sppb'") ("`p4_sppb'")  ("`q4_vel'") ("`p4_vel'") ("`q4_grip'") ("`p4_grip'")
	
	
	//Cuartiles continuo
	local i = 1

	foreach outcome_var in ewgsop2_perform_sppbw0 ewgsop2_perform_velw0 ewgsop2_strength_gripw0{
		local outcome : word `i' of `outcomes'
		logistic `outcome_var' log`var'4 w17sexo edadw0 w17fuma w17educa_3cat w17imc3
		matrix b = r(table)
		
		local pt_`outcome' : di %4.3f b[4,1]
		if `pt_`outcome'' == 0.000 {
			local pt_`outcome' "<0.001"
		}
		
		local i = `i'+1

	}
	
	post `tabla2' ("P for trend") ("") ("`pt_sppb'")  ("") ("`pt_vel'")  ("") ("`pt_grip'")
	
}


	

postclose `tabla2'

preserve

use tabla2_temp.dta, clear

list

asdoc list, title(Tabla 2 Multivariate logistic regression analysis of Log-transformed metals for the prevalence of each studied outcome) save(tabla2.doc) replace

restore
shell del "tabla2_temp.dta"
