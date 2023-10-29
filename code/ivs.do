/* 
This file combines the European and World Values Survey, removes values that are not related to gender.

Official documentation on creating the Integrated Values Survey (i.e. combining the World Values Survey, and the European Values Survey) is available here: 
https://europeanvaluesstudy.eu/methodology-data-documentation/integrated-values-surveys-ivs-1981-2021/
https://europeanvaluesstudy.eu/methodology-data-documentation/integrated-values-surveys-ivs-1981-2021/data-and-documentation-ivs-1981-2021/ 
*/

	* For merging heritage and individual characteristics. *
use "intermediate_data/dd1.dta", clear
replace c_code_n = 688 if country_code == "SRB"
save "intermediate_data/dd100.dta", replace

			* * * Individual Characteristics (WVS data) * * * 
use "raw_data/Integrated_values_surveys_1981-2021.dta", clear

g wave = .
forvalues i = 1/7{
replace wave = `i' if S002 == `i'
}
replace wave = 1 if S002EVS == 1
replace wave = 2 if S002EVS == 2
replace wave = 4 if S002EVS == 3
replace wave = 5 if S002EVS == 4
replace wave = 7 if S002EVS == 5

* Waves 1-4 are dated and not globally representative.
keep if wave >= 5

* Recode missing values. 
ds, has(type numeric)
foreach var in `r(varlist)'{
	qui replace `var' = . if `var' == .a | `var' == .b | `var' == .c | `var' == .d | `var' == .e
}

ds, has(type string)
foreach var in `r(varlist)'{
qui replace `var' = "" if `var' == ".a" | `var' == ".b" | `var' == ".c" | `var' == ".d" | `var' == ".e"
}

rename S003 country
rename S018 weight
g inequality = 11 - E035
rename E037 government

* Misogyny
* NB - C001_01 replaced C001 in both the EVS and WVS. The IVS has already recoded this. F199 is unavailable in the EVS. 
pca D059 E233 D060 C001 D078 F120
predict gender, score

* Categorical:
rename G026 m_immigrant
replace m_immigrant = 1 if V002 == 0
replace m_immigrant = 0 if V002 == 1

rename G027 f_immigrant
replace f_immigrant = 1 if V001 == 0
replace f_immigrant = 0 if V001 == 1

* Immigrants can't take wvs in China: 
replace m_immigrant = 0 if country == 156
replace f_immigrant = 0 if country == 156

egen parent_immigrant = rowmax(f_immigrant m_immigrant)

g immigrant = .
replace immigrant = 0 if X002_02 == 1
replace immigrant = 1 if X002_02 == 0
replace immigrant = 0 if G027A == 1
replace immigrant = 1 if G027A == 2
replace immigrant = 0 if country == 156

rename G027B citizen
replace citizen = 1 if G005 == 1
replace citizen = 0 if G005 == 0

rename V097EF f_occupation
replace f_occupation = . if f_occupation == 66

* Too much missing data for X052 (sector of work).

rename F025 religion
g male = .
replace male = 1 if X001 == 1
replace male = 0 if X001 == 2
drop X001 
rename X003 age

* no_children and hh_size are given in EVS as 'greater than 5,6' respectively.
rename X011 no_children
replace no_children = 5 if no_children > 5
rename X013 hh_size
replace hh_size = 6 if hh_size > 6

* EVS has 4 categories, WVS only 2; to be consistent across sources, I've grouped living with inlaws and living with parents.
rename X026 parents_cohabit
replace parents_cohabit = 1 if X026_01 == 2 | X026_01 == 3 | X026_01 == 4
replace parents_cohabit = 0 if X026_01 == 1 

* Chief_earner (X040) is not available in the EVS

* Employment.
rename X028 employment
g unemployed = 0 if !missing(employment)
replace unemployed = 1 if employment == 7

* Defining married = 1 if (i) married, (ii) living together as married or (iii) widowed
g married = 0 if !missing(X007)
replace married = 1 if X007 == 1 | X007 == 2 | X007 == 5 
drop X007

* continuous:
rename V004RF f_educ
rename V004RM m_educ
rename W002R s_educ
rename X025R education

egen p_educ= rowmax(f_educ m_educ)

* X049a is from the EVS, and X049 for the WVS. X049A != X049a, the categories are different, (20,000 - 50,000 vs 25,000 - 100,000).
rename X049a population_EVS
rename X049 population

replace population = 3 if population_EVS == 1
replace population = 1.5 if population_EVS == 2
replace population = 5.5 if population_EVS == 3
replace population = 7 if population_EVS == 4
replace population = 8 if population_EVS == 5
drop population_EVS

* Urban/Rural
rename X050C urban
mdesc urban 
* too much missing; so drop
drop urban

g income = .
forvalues i = 1/10{
replace income = `i' if X047_WVS == `i' | X047CS == `i' | X047_EVS == `i'
}

* Majority status.                     
g w = weight
gen counter = w
replace counter = . if X051 == .

* Minorities within a state: 
egen count_eth_state = total(counter), by (X051 X048WVS S002)
egen count_state = total(counter), by (X048WVS S002)
gen eth_pc_state = count_eth_state / count_state

* Nothing magic about the 50% cutoff, so make splits at 5,15, ... , 85,95 percentile levels. 
g ethnicity_state = .
local j = 0
forvalues i = 5(10)105{
	qui replace ethnicity_state = `j' if eth_pc_state <= `i'/100 & eth_pc_state >= `j'/100
	local j = `i'
}

replace ethnicity_state = . if missing(X048WVS)

* Minorities within a country: 
egen count_eth_country = total(counter), by (X051 country S002)
egen count_country = total(counter), by (country S002)
gen eth_pc_country = count_eth_country / count_country

g ethnicity_country = .
local j = 0
forvalues i = 5(10)105{
	qui replace ethnicity_country = `j' if eth_pc_country <= `i'/100 & eth_pc_country >= `j'/100
	local j = `i'
}

* EVS doesn't have ethnicity.
replace ethnicity_state = . if S001 == 1
replace ethnicity_country = . if S001 == 1
drop counter count_eth_country count_country eth_pc_country count_eth_state count_state eth_pc_state

* Religious majorities
g counter = w
replace counter = . if religion == .

* State level: Too many missing state markers. 
* Country level: 
egen count_eth_country = total(counter), by (religion country S002)
egen count_country = total(counter), by (country S002)
gen eth_pc_country = count_eth_country / count_country

g religion_country = .
local j = 0
forvalues i = 5(10)105{
	qui replace religion_country = `j' if eth_pc_country <= `i'/100 & eth_pc_country >= `j'/100
	local j = `i'
}

drop counter count_eth_country count_country eth_pc_country 

* Minority status can't be continuous since ethnicity is missing in EVS.
g minority = .
replace minority = 0 if !missing(religion_country) | !missing(ethnicity_state) | !missing(ethnicity_country) | !missing(immigrant)
replace minority = 1 if religion_country <= 50 | ethnicity_country <= 50 | ethnicity_state <= 50 | immigrant == 1

* I can't use these guys anymore, no EVS data: 
drop ethnicity_state ethnicity_country

* Only 650 Jews from 370,000 obs, so add to 'other religion'.
tab religion 
replace religion = 9 if religion == 4

* Keep only the variables that I want available in R
keep S001 D059 E233 D060 C001 D078 F199 F120 weight wave country gender inequality government m_immigrant f_immigrant parent_immigrant immigrant citizen religion employment male parents_cohabit married education population income age no_children hh_size religion_country minority 

foreach var in weight wave country religion employment male married education population income age no_children hh_size religion_country minority{
	drop if missing(`var')
}

drop if missing(gender) & missing(inequality) & missing(government)

foreach var in government inequality gender{
* Record the wave that the latest data is from, for the EVS
g wave_evs = wave if !missing(`var') & S001 == 1
bysort country: egen wave_max_evs = max(wave_evs)

g wave_wvs = wave if !missing(`var') & S001 == 2
bysort country: egen wave_max_wvs = max(wave_wvs)

replace wave_max_evs = 0 if missing(wave_max_evs)
replace wave_max_wvs = 0 if missing(wave_max_wvs)

* Create a dummy equal to 1 if the wvs data is more recent thatn the evs data
gen `var'_wave_wvs = 0
replace `var'_wave_wvs = 1 if wave_max_wvs >= wave_max_evs & wave_max_wvs == wave

gen `var'_wave_evs = 0
replace `var'_wave_evs = 1 if wave_max_evs > wave_max_wvs & wave_max_evs == wave

* Replace it as 0 if missing data
replace `var'_wave_wvs = 0 if missing(`var')
replace `var'_wave_evs = 0 if missing(`var')

egen `var'_wave = rowmax(`var'_wave_evs `var'_wave_wvs)

drop wave_evs wave_max_evs wave_wvs wave_max_wvs `var'_wave_wvs `var'_wave_evs
}

bysort country: egen counter=count(weight) if gender_wave == 1 | inequality_wave ==1 | government_wave ==1

rename country c_code_n
merge m:1 c_code_n using "intermediate_data/dd100"
keep if _merge == 3
drop _merge gender // Calculated within R. 
save "intermediate_data/ivs.dta", replace
