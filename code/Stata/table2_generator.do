local root "C:\Users\a.santos\OneDrive - UAM\Escritorio\UAM_Sarcopenia\Functional-Decline-ENRICA-BKMR" //Introduce here your path to the repository
cd "`root'\data"

use "bd_metales_deteriorommii.dta", replace


drop if missing(w17vis_enf) //No blood sample was extracted
drop if missing(Al, Co, Cr, Cu, Fe, Mg, Mn, Mo, Ni, Pb, Se, V, Zn)
drop if missing(w17sexo, edadw0, w17fuma, w17educa_3cat, w17imc3, alcohol4, w17xmedas, w17modvigpa_mets)
drop if w17IR_DEGREES==. | w17IR_DEGREES>=5 //We drop patients with severe kidney chronic disease or missing as we cannot trust those measurements
drop if missing(ewgsop2_strength_gripw0, ewgsop2_strength_chairw0, calfcircumf_dicotw0, ewgsop2_perform_sppbw0, ewgsop2_perform_velw0, sarcopenia4)

tempname table2

postfile `table2' str28 a ///
					str20 Grip_strength ///
					str8 b ///
                    str20 Chair_stand ///
					str8 c ///
                    str20 Calf_circunference ///
					str8 d ///
		            str20 SPPB ///
					str8 e ///
		            str20 Gait_speed ///
					str8 f ///
		            str20 Sarcopenia ///
					str8 g ///
                    using table2_temp.dta, replace



post `table2' ("") ("OR (95% CI)") ("P-value")  ("OR (95% CI)") ("P-value")  ("OR (95% CI)") ("P-value") ("OR (95% CI)") ("P-value")  ("OR (95% CI)") ("P-value")  ("OR (95% CI)") ("P-value")

foreach var in Al Co Cr Cu Fe Mg Mn Mo Ni Pb Se V Zn {
	capture drop log`var' log`var'4
	gen log`var'=log(`var')
	xtile log`var'4 = log`var', nq(4)	
	
	*Here we spcifiy the desired outcomes to be evaluated. In case of needing to check any other outcome, add the it to the for loop on line 43 and a custom name to outcome_names in the same position
	local outcomes grip ///
                     chair ///
                     calf ///
					 sppb ///
					 vel ///
					 sarcopenia
					 
					 
	//Log continuo
	local i = 1
	
	foreach outcome_var in ewgsop2_strength_gripw0 ewgsop2_strength_chairw0 calfcircumf_dicotw0 ewgsop2_perform_sppbw0 ewgsop2_perform_velw0 sarcopenia2 { 
		
		local outcome : word `i' of `outcomes'
		logistic `outcome_var' log`var' w17sexo edadw0 w17fuma w17educa_3cat w17imc3 medas4 alcohol4 pa4
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
	
	post `table2' ("Log `var'")  ("`or_grip'") ("`p_grip'")  ("`or_chair'") ("`p_chair'") ("`or_calf'") ("`p_calf'") ("`or_sppb'") ("`p_sppb'")  ("`or_vel'") ("`p_vel'") ("`or_sarcopenia'") ("`p_sarcopenia'")
	
	
	//Cuartiles
	local q1 = "1 (Ref)"
	local i = 1
	
	foreach outcome_var in ewgsop2_strength_gripw0 ewgsop2_strength_chairw0 calfcircumf_dicotw0 ewgsop2_perform_sppbw0 ewgsop2_perform_velw0 sarcopenia2{
		
		local outcome : word `i' of `outcomes'
		logistic `outcome_var' i.log`var'4 w17sexo edadw0 w17fuma w17educa_3cat w17imc3 medas4 alcohol4 pa4
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

	post `table2' ("Q1") ("`q1'") ("")  ("`q1'") ("")  ("`q1'") ("") ("`q1'") ("")  ("`q1'") ("")  ("`q1'") ("")
	post `table2' ("Q2") ("`q2_grip'") ("`p2_grip'")  ("`q2_chair'") ("`p2_chair'") ("`q2_calf'") ("`p2_calf'") ("`q2_sppb'") ("`p2_sppb'")  ("`q2_vel'") ("`p2_vel'") ("`q2_sarcopenia'") ("`p2_sarcopenia'")
	post `table2' ("Q3") ("`q3_grip'") ("`p3_grip'")  ("`q3_chair'") ("`p3_chair'") ("`q3_calf'") ("`p3_calf'") ("`q3_sppb'") ("`p3_sppb'")  ("`q3_vel'") ("`p3_vel'") ("`q3_sarcopenia'") ("`p3_sarcopenia'")
	post `table2' ("Q4") ("`q4_grip'") ("`p4_grip'")  ("`q4_chair'") ("`p4_chair'") ("`q4_calf'") ("`p4_calf'") ("`q4_sppb'") ("`p4_sppb'")  ("`q4_vel'") ("`p4_vel'") ("`q4_sarcopenia'") ("`p4_sarcopenia'")
	
	
	//Cuartiles continuo
	local i = 1

	foreach outcome_var in ewgsop2_strength_gripw0 ewgsop2_strength_chairw0 calfcircumf_dicotw0 ewgsop2_perform_sppbw0 ewgsop2_perform_velw0 sarcopenia2{
		local outcome : word `i' of `outcomes'
		logistic `outcome_var' log`var'4 w17sexo edadw0 w17fuma w17educa_3cat w17imc3 medas4 alcohol4 pa4
		matrix b = r(table)
		
		local pt_`outcome' : di %4.3f b[4,1]
		if `pt_`outcome'' == 0.000 {
			local pt_`outcome' "<0.001"
		}
		
		local i = `i'+1

	}
	
	post `table2' ("P for trend") ("") ("`pt_grip'")  ("") ("`pt_chair'")  ("") ("`pt_calf'") ("") ("`pt_sppb'")  ("") ("`pt_vel'")  ("") ("`pt_sarcopenia'")
	
}


	

postclose `table2'

preserve

use table2_temp.dta, clear

asdoc list, title(Table 2 Multivariate logistic regression analysis of Log-transformed metals for the prevalence of each studied outcome) save(../tables/table2.doc) replace

restore
shell del "table2_temp.dta"
