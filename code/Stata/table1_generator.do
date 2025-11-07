local root "" //Introduce here your path to the repository
cd "`root'\Functional-Decline-ENRICA-BKMR"

use "data/bd_metales_deteriorommii.dta", replace

local matrix = "blood" //Select the desired matrix 

if "`matrix'" == "serum"{
	drop if w17IR_DEGREES==. | w17IR_DEGREES>=5 //We firstly drop patients with severe kidney chronic disease or missing as we cannot trust those measurements
	drop if missing(Al, Co, Cr, Cu, Fe, Mg, Mn, Mo, Ni, Pb, Se, V, Zn)
	drop if missing(w17sexo, edadw0, w17fuma, w17educa_3cat, w17imc3, w17cv, w17dai_hypertension, w17dai_diabetes)
}

if "`matrix'" == "blood"{
	drop if w17IR_DEGREES==. | w17IR_DEGREES>=5
	drop if missing(Cd_imput, Hg_whb, Mn_whb, Pb_whb, Se_whb)
	drop if missing(w17sexo, edadw0, w17fuma, w17educa_3cat, w17imc3, w17cv, w17dai_hypertension, w17dai_diabetes)
}


//Here we spcifiy the desired outcome to be evaluated. In case of needing to check any other outcome, add the variable to study_outcome and a custom name to outcome_names in the same position
local study_outcomes ewgsop2_perform_sppbw0 ///
                     ewgsop2_perform_velw0 ///
                     ewgsop2_strength_gripw0
					 
local outcome_names SPPB ///
                     Gait_Speed ///
                     Grip_Strength

forvalues i = 1/3 {
    
    local study_outcome : word `i' of `study_outcomes'
    local outcome_name : word `i' of `outcome_names'
    
    display "`outcome_name'"



preserve
drop if `study_outcome'==.
	
tempname tabla1

postfile `tabla1' str28 Variables ///
					str20 Total ///
                    str20 High_`outcome_name' ///
                    str20 Low_`outcome_name' ///
					str8 P_value ///
                    using tabla1_temp.dta, replace

tab `study_outcome', matcell(freqs)
count
local total = r(N)
local controls = freqs[1,1]
local cases = freqs[2,1]
local porc_control : di %2.1f `controls'/`total'*100
local porc_caso : di %2.1f `cases'/`total'*100

post `tabla1' ("") ("n=`total'") ("n=`controls' (`porc_control'%)") ("n=`cases' (`porc_caso'%)") ("")

//Sexo
tab w17sexo `study_outcome', chi2
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "Gender, n(%)"

post `tabla1' ("`parameter'") ("") ("") ("") ("`p_val'")

local study_labels Male ///
             Female


forvalues i = 1/2{
	tab w17sexo `study_outcome', col matcell(freqs)
	local n_control = freqs[`i',1]
	local n_caso = freqs[`i',2]
	local n_total = `n_control' + `n_caso'
	local porc_control : di %2.1f `n_control'/`controls' * 100
	local porc_caso : di %2.1f `n_caso'/`cases' * 100
	local porc_total : di %2.1f `n_total'/`total' * 100
	
	local stat_control = "`n_control' (`porc_control'%)"
	local stat_caso = "`n_caso' (`porc_caso'%)"
	local stat_total = "`n_total' (`porc_total'%)"
	
	local parameter : word `i' of `study_labels'
	local parameter = subinstr("`parameter'", "_", " ", .)



	
	post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("")

}

//Edad
tabstat edadw0, stat(mean sd) save
local mean_total : display %4.2f r(StatTotal)[1,1]
local sd_total : display %4.2f r(StatTotal)[2,1]
local stat_total = "`mean_total' (`sd_total')"

tabstat edadw0, by(`study_outcome') stat(mean sd) save
local mean_control : di %4.2f r(Stat1)[1,1]
local sd_control : di %4.2f r(Stat1)[2,1]
local stat_control = "`mean_control' (`sd_control')"

local mean_caso : di %4.2f r(Stat2)[1,1]
local sd_caso : di %4.2f r(Stat2)[2,1]
local stat_caso = "`mean_caso' (`sd_caso')"

ttest edadw0, by(`study_outcome')
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "Age (years) mean (sd)"
post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("`p_val'")


//Nivel de estudios
tab w17educa_3cat `study_outcome', chi2
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "Education, n(%)"

post `tabla1' ("`parameter'") ("") ("") ("") ("`p_val'")

local study_labels <Secundary ///
             Secundary ///
             >Secundary


forvalues i = 1/3{
	tab w17educa_3cat `study_outcome', col matcell(freqs)
	local n_control = freqs[`i',1]
	local n_caso = freqs[`i',2]
	local n_total = `n_control' + `n_caso'
	local porc_control : di %2.1f `n_control'/`controls' * 100
	local porc_caso : di %2.1f `n_caso'/`cases' * 100
	local porc_total : di %2.1f `n_total'/`total' * 100
	
	local stat_control = "`n_control' (`porc_control'%)"
	local stat_caso = "`n_caso' (`porc_caso'%)"
	local stat_total = "`n_total' (`porc_total'%)"
	
	local parameter : word `i' of `study_labels'
	local parameter = subinstr("`parameter'", "_", " ", .)



	
	post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("")

}

//BMI
tab w17imc3 `study_outcome', chi2
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "BMI, n(%)"

post `tabla1' ("`parameter'") ("") ("") ("") ("`p_val'")

local study_labels <25 ///
             25-30 ///
             >=30


forvalues i = 1/3{
	tab w17imc3 `study_outcome', col matcell(freqs)
	local n_control = freqs[`i',1]
	local n_caso = freqs[`i',2]
	local n_total = `n_control' + `n_caso'
	local porc_control : di %2.1f `n_control'/`controls' * 100
	local porc_caso : di %2.1f `n_caso'/`cases' * 100
	local porc_total : di %2.1f `n_total'/`total' * 100
	
	local stat_control = "`n_control' (`porc_control'%)"
	local stat_caso = "`n_caso' (`porc_caso'%)"
	local stat_total = "`n_total' (`porc_total'%)"
	
	local parameter : word `i' of `study_labels'
	local parameter = subinstr("`parameter'", "_", " ", .)



	
	post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("")

}

//Smoking
tab w17fuma `study_outcome', chi2
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "Smoking status, n(%)"

post `tabla1' ("`parameter'") ("") ("") ("") ("`p_val'")

local study_labels Never ///
             Ex-smoker ///
             Current


forvalues i = 1/3{
	tab w17fuma `study_outcome', col matcell(freqs)
	local n_control = freqs[`i',1]
	local n_caso = freqs[`i',2]
	local n_total = `n_control' + `n_caso'
	local porc_control : di %2.1f `n_control'/`controls' * 100
	local porc_caso : di %2.1f `n_caso'/`cases' * 100
	local porc_total : di %2.1f `n_total'/`total' * 100
	
	local stat_control = "`n_control' (`porc_control'%)"
	local stat_caso = "`n_caso' (`porc_caso'%)"
	local stat_total = "`n_total' (`porc_total'%)"
	
	local parameter : word `i' of `study_labels'
	local parameter = subinstr("`parameter'", "_", " ", .)



	
	post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("")

}

//Enfermedad cardiovascular
tab w17cv `study_outcome', chi2
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "Cardiovascular disease, n(%)"

post `tabla1' ("`parameter'") ("") ("") ("") ("`p_val'")

local study_labels No ///
             Yes


forvalues i = 1/2{
	tab w17cv `study_outcome', col matcell(freqs)
	local n_control = freqs[`i',1]
	local n_caso = freqs[`i',2]
	local n_total = `n_control' + `n_caso'
	local porc_control : di %2.1f `n_control'/`controls' * 100
	local porc_caso : di %2.1f `n_caso'/`cases' * 100
	local porc_total : di %2.1f `n_total'/`total' * 100
	
	local stat_control = "`n_control' (`porc_control'%)"
	local stat_caso = "`n_caso' (`porc_caso'%)"
	local stat_total = "`n_total' (`porc_total'%)"
	
	local parameter : word `i' of `study_labels'
	local parameter = subinstr("`parameter'", "_", " ", .)



	
	post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("")

}

//Diabetes mellitus
tab w17dai_diabetes `study_outcome', chi2
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "Diabetes mellitus, n(%)"

post `tabla1' ("`parameter'") ("") ("") ("") ("`p_val'")

local study_labels No ///
             Yes


forvalues i = 1/2{
	tab w17dai_diabetes `study_outcome', col matcell(freqs)
	local n_control = freqs[`i',1]
	local n_caso = freqs[`i',2]
	local n_total = `n_control' + `n_caso'
	local porc_control : di %2.1f `n_control'/`controls' * 100
	local porc_caso : di %2.1f `n_caso'/`cases' * 100
	local porc_total : di %2.1f `n_total'/`total' * 100
	
	local stat_control = "`n_control' (`porc_control'%)"
	local stat_caso = "`n_caso' (`porc_caso'%)"
	local stat_total = "`n_total' (`porc_total'%)"
	
	local parameter : word `i' of `study_labels'
	local parameter = subinstr("`parameter'", "_", " ", .)



	
	post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("")

}


//Hipertensión
tab w17dai_hypertension `study_outcome', chi2
local p_val : di %4.3f r(p)
if `p_val' == 0.000{
	local p_val = "<0.001"
}

local parameter = "Hypertension, n(%)"

post `tabla1' ("`parameter'") ("") ("") ("") ("`p_val'")

local study_labels No ///
             Yes


forvalues i = 1/2{
	tab w17dai_hypertension `study_outcome', col matcell(freqs)
	local n_control = freqs[`i',1]
	local n_caso = freqs[`i',2]
	local n_total = `n_control' + `n_caso'
	local porc_control : di %2.1f `n_control'/`controls' * 100
	local porc_caso : di %2.1f `n_caso'/`cases' * 100
	local porc_total : di %2.1f `n_total'/`total' * 100
	
	local stat_control = "`n_control' (`porc_control'%)"
	local stat_caso = "`n_caso' (`porc_caso'%)"
	local stat_total = "`n_total' (`porc_total'%)"
	
	local parameter : word `i' of `study_labels'
	local parameter = subinstr("`parameter'", "_", " ", .)



	
	post `tabla1' ("`parameter'") ("`stat_total'") ("`stat_control'") ("`stat_caso'") ("")

}


restore

postclose `tabla1'

preserve

use tabla1_temp.dta, clear

list

asdoc list, title(Tabla 1 `outcome_name'_`matrix') save(tables/tabla1_`outcome_name'_`matrix'.doc) replace

restore
shell del "tabla1_temp.dta"

}
