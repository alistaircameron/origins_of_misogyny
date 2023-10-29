/*  This file takes the original files containing heritage variables, and makes them consistent across sources (country naming etc) */

di "Hello!"

* This list of WB codes created from: https://wits.worldbank.org/WITS/wits/WITSHELP/Content/Codes/Country_Codes.htm Accessed 16.7.2021
use "raw_data/dd0.dta", clear

* Drop world, euro union, unspecified, western sahara, fm viet Rp/Dr
drop if c_code_n == 918 | c_code_n == 0 | c_code_n == 898 | c_code_n == 732 | c_code_n == 866 | c_code_n == 868

* It is common to mistakenly label Romania with ROU
replace country_code = "ROM" if country_code == "ROU"
replace country_code = "SRB" if country_code == "SER"
save "intermediate_data/dd1.dta", replace


* Name: FED
* https://www.sciencedirect.com/science/article/pii/S1567134817302460#s0055 
* Fedderke et al. 2017 Genetic adaptation to historical pathogen burdens

use "raw_data/Disease_Genes.dta", clear
rename code country_code
sort country_code
duplicates report country_code
duplicates example country_code
* Two islands given the same identifier, remove them. 
drop if country_code == "CHI"

label variable abslat "abs latitude" 
label variable uvr "UV radiation" 
label variable diamond "no of domesticable species" 
label variable meanelev "mean elevation" 
label variable rough "roughness of terrain" 
label variable avprec "average precipitation" 
label variable avtemp "average temperature" 
label variable sdtemp   "SD temperature" 
label variable dwater    "distance to water" 
label variable mt810kbc "mean of temperature in early holocene" 
label variable sd810kbc "sd of temp in early holocene" 
label variable pdiv_aa "genetic diversity - acenstry adjuted"
label variable p10kbc "pop size 12k years ago" 
label variable pd10kbc "pop density 12k years ago" 
label variable p_tropical "percent keppen-geiger tropical" 
label variable p_montane "percent keppen-geiger montane" 
label variable p_temperate   "percent keppen-geiger temperate" 
label variable p_continental     "percent keppen-geiger continental" 
label variable p_dry "percent keppen-geiger dry" 
label variable origtime "economic length of human occupation" 
label variable abslong  "absolute longitute" 
label variable climate_gini "variability of cilmate in country koppen-geiger" 
label variable size "continental landmass" 
label variable avelev "average elevation"
label variable acp1_a "something genetic"
label variable acp1_b "something genetic"
label variable il6 "something genetic"
label variable il10 "something genetic"
label variable faah "something genetic"
label variable sallele "something genetic"
label variable hla "something genetic"
label variable rhdneg "something genetic"
label variable rhdhetero "something genetic"
label variable gdist_plu "genetic diversity"
label variable gdist_w "genetic diversity"
label variable axis "orientaion diamond"

drop id country africa europe asia oceania americas meanelev_sqr v52 v53 sd810kbc_sqr origtime_sqr avelev

* Add a database identifer. 
rename * FED_=
rename *country_code country_code

replace country_code = "ROM" if country_code == "ROU"
replace country_code = "SRB" if country_code == "YUG"
* Because of it's history, the former Yugoslav countries are difficult to get data for, everyone codes it slightly differently. I have chosen to use Yugoslav data for Serbia, and not include the rest. This reflects what is available as deep determinants, and as WVS data. 
save "intermediate_data/dd2.dta", replace


* Name: AG13
* Ashraf & Galor AER 2013. 
* The "Out of Africa" Hypothesis, Human Genetic Diversity, and Comparative Economic Development
* Data & replication files available here: https://www.openicpsr.org/openicpsr/project/112588/version/V1/view
* refer to Section F of the Online Appendix of the paper for detailed variable definitions and sources: https://assets.aeaweb.org/asset-server/articles-attachments/aer/data/feb2013/20100971_app.pdf


use "raw_data/ashraf_galor_AER_2013.dta", clear

* Dropping variables that aren't heritage variables (ie. are from the recent past) religion is from 1980
drop id trust articles gdppc2000 mdist_london mdist_london_sqr mdist_tokyo mdist_tokyo_sqr mdist_mexico mdist_mexico_sqr aerial aerial_sqr aerial_aa aerial_aa_sqr fstdist1500_uk fstdistwtd_usa elevavg elevavg2 rough_sqr socinf democ xconst school uvdamage continent region ln_pop1 ln_pop1000 ln_pop1500 ln_pd1 ln_pd1000 ln_pd1500 ln_ur1500 ln_gdppc2000 ln_adiv ln_adiv_sqr ln_pdiv ln_pdiv_sqr ln_pdiv_aa ln_pdiv_aa_sqr ln_frontdist1 ln_frontdist1000 ln_frontdist1500 ln_yst ln_yst_aa ln_yst1500 ln_submode1000 ln_area_ar ln_abslat ln_arable ln_suitavg ln_soilsuit ln_temp ln_precip country pprotest pcatholic pmuslim pother height weight pdiv_sqr adiv_sqr pdiv_aa_sqr pdivhmi_sqr pdivhmi_aa_sqr ahomo_sqr phomo_sqr phomo_aa_sqr mdist_hgdp_sqr mdist_addis_sqr mdist_addis_aa_sqr hmicost_hgdp_sqr hmicost_addis_sqr hmicost_addis_aa_sqr efrac peuro pnative cleanlim cleanpd1 cleanpd1000 cleanpd1500 cleanur1500 cleancomp cleangdp cleantrust cleanarticles cleanhibbs cleanskin cleanheight cleanweight cleanlimfst cleanpd1500fst cleancompfst cleangdptrust cleangdparticles conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 conley_const skinreflectance size axis plants animals

* these are for 1995
drop kgatr kgatrstr kgatemp malfal
drop adiv
rename pdiv_aa pdiv_1500_aa
rename pdiv pdiv_1500

* the 'homo' variables are just `1 - hetero_variables'
drop ahomo phomo phomo_aa 

* limited sample, they have mdist_abbis which is full sample
drop mdist_hgdp hmicost_hgdp

* linear transformations of yst, will give same rankings in ML as yst.
drop yst1500 yst1500_scaled

* There are some variables which are both aa'd (ancestry adjustment'ed) and not aa'd. Drop the aa'd version since I later do it myself.
drop *_aa
rename code country_code
rename * AG13_=
rename *country_code country_code

replace country_code = "ROM" if country_code == "ROU"
replace country_code = "SRB" if country_code == "YUG"
save "intermediate_data/dd3.dta", replace



* Name: PD
* Rethinking the relationship between pronoun-drop and individualism with Bayesian multilevel models
* https://academic.oup.com/jole/article/2/2/188/3091421

import delimited using "raw_data/pronoundrop_joined01.csv", clear
rename iso_code country_code
keep country_code language_family drop 

replace country_code = "DEU" if country_code == "GER"
replace country_code = "ROM" if country_code == "ROU"

rename * PD_=
rename *country_code country_code

save "intermediate_data/dd4.dta", replace

* Name: ASR
* Traditional agricultural practices and the sex ratio today - Alesina et al. 2018

use "raw_data/Alesina_sex_ratio.dta", clear
rename isocode country_code
rename plow plough
keep country_code plough settlement plow_positive_crops plow_negative_crops animals
label variable settlement "economic complexity"
rename settlement economic_complexity

ds
foreach x in `r(varlist)' {
rename `x' `x'_aa
}

rename country_code_aa country_code
rename * ASR_=
rename *country_code country_code

replace country_code = "ROM" if country_code == "ROU"
replace country_code = "SRB" if country_code == "YUG"
save "intermediate_data/dd5.dta", replace

* Name: ENK
* https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/JX1OIU
* Kinship, Cooperation, and the Evolution of Moral Systems
* Benjamin Enke
* The Quarterly Journal of Economics, Volume 134, Issue 2, May 2019, Pages 953–1019, https://doi.org/10.1093/qje/qjz001 


use "raw_data/CountryData.dta", clear

* malaraiindex and malfal are different
rename isocode country_code
keep popd1500 popd1600 popd1700 popd1800 urbfrac1500 urbfrac1600 urbfrac1700 urbfrac1800  country_code colony_esp colony_gbr colony_fra colony_prt colony_oeu malariaindex frac_native kinship_score cath1900 muslim1900 cont_ssa cont_mena cont_eca cont_sas cont_eap cont_nam cont_lac malfal small_scale

* On page 1012, Enke says that populations are not ancestry ajusted. Just the ea variables are ancestry adjusted. Kinship_score is derived from the EA, therefore is adjusted.
foreach x in frac_native kinship_score cath1900 muslim1900 malfal malariaindex cont_ssa cont_mena cont_eca cont_sas cont_eap cont_nam cont_lac small_scale {
	rename `x' `x'_aa
}

replace country_code = "ROM" if country_code == "ROU"
rename * Enk_=
rename *country_code country_code

save "intermediate_data/dd6.dta", replace


/*
https://academic.oup.com/qje/article/134/2/953/5288003

To aggregate to country level, follow the procedure of Enke QJE 2019, explained in A.2 of their supplementary material.

Essentially, its an unweighted average of the ethnic groups in that country. Then, use the Putterman & Weil migration matrix to ancestry-adjust. 

Ancestry adjustment matrix from here: https://sites.google.com/brown.edu/louis-putterman/world-migration-matrix-1500-2000
*/

* firstly, clean up the migration matrix 
import excel "raw_data/putterman_weils_migration_matrix.xls", firstrow clear
drop update wbname
xpose, clear v
drop in 1

forvalues i = 1/`=_N' {
	rename v`i' `=_varname[`i']'
}

rename _varname country_code
replace country_code = upper(country_code)
order country_code
// rename yug srb
// replace country_code = "SRB" if country_code == "YUG"
save "intermediate_data/migration_matrix.dta", replace

* Now, use the Ethnographic Atlas + other data (still from Enke's QJE paper)
use "raw_data/EAShort.dta", clear

* The following gets the mean for each variable by isocode, then drops everthing.
collapse (mean) clan inheritance_land_patrilineal inheritance_prop_patrilineal small_scale hierlocal_village rel_agriculture CSIPost1500Ex distance_mutation tsi loyalty_local s_vio_other s_vio_community diff_violence, by(isocode)

rename isocode country_code
replace country_code = "ROM" if country_code == "ROU"
replace country_code = "SRB" if country_code == "YUG"
save "intermediate_data/dd14.dta", replace




* Name: Har
* Jeffrey Sachs (and others) at Harvard's Centre for International Development have put together a large number of datasets on geographic determinants of growth. Many papers then use these data. So, this should be the primary source. 
* https://dataverse.harvard.edu/dataverse/cid?q=&types=dataverses%3Adatasets&sort=dateSort&order=desc&page=1


* % cultivated land in given climate type. 
use "raw_data/agric.dta", clear
rename wbcode country_code
drop cultmaf cultmam cultmaw cultmbs cultmbw cultmcf cultmcs cultmcw cultmdf cultmdw cultme cultmh wcultmaf wcultmam wcultmaw wcultmbs wcultmbw wcultmcf wcultmcs wcultmcw wcultmdf wcultmdw wcultme wcultmh country wcultcaf wcultcaw wcultcam wcultcbs wcultcbw wcultccf wcultccs wcultccw wcultcdf wcultcdw wcultce wcultch

replace country_code = "ROM" if country_code == "ROU"
replace country_code = "YUG" if country_code == "SBR"

rename * Har_=
rename *country_code country_code
save "intermediate_data/dd7.dta", replace


* % land area of a country with given disease in 1950
* They also have % popn in areas with a given disease. To the extent that populations move across time, I prefer to use % land area of a country with a given disease. 
use "raw_data/disarea.dta", clear

rename wbcode country_code
drop disa23 disa22 country disa21
replace country_code = "ROM" if country_code == "ROU"
rename * Har_=
rename *country_code country_code

save "intermediate_data/dd8.dta", replace




* % land area and % 1995 population in Koeppen-Geiger climatic zones
use "raw_data/kgzones.dta", clear

rename wbcode country_code
drop country
replace country_code = "ROM" if country_code == "ROU"
rename * Har_=
rename *country_code country_code

save "intermediate_data/dd9.dta", replace


* distance to coast etc
use "raw_data/physfact_rev.dta", clear
rename wbcode country_code
drop country pop95 pdenpavg cen_c cen_cr
* dropping cen_c & cen_cr because we have mean values in this same dataset which I think are more representative of an individual's ability to access waterways.
replace country_code = "ROM" if country_code == "ROU"
rename * Har_=
rename *country_code country_code

save "intermediate_data/dd10.dta", replace



* Gini & GDP per capita
* World Bank data, not used in the final analysis. 

import delimited using "raw_data/gini_worldbank.csv", clear
rename countrycode country_code
egen gini_18 = rowmean(*2017* *2018* *2019*)
keep country_code gini_18
replace country_code = "ROM" if country_code == "ROU"
save "intermediate_data/dd11.dta", replace

import delimited using "raw_data/worldbank_gdp.csv", clear
keep v2 v64 
rename v2 country_code 
rename v64 gdp_pc_19
drop in 1/3
drop in 260/266
replace country_code = "ROM" if country_code == "ROU"
save "intermediate_data/dd12.dta", replace




* Name: CWG
* Was the Wealth of Nations Determined in 1000 BC? - Diego Comin; William Easterly; Erick Gong
* Paper & data here: https://www.aeaweb.org/articles?id=10.1257/mac.2.3.65

use "raw_data/primitive_aejmacro.dta", clear

order iso3
sort iso3
drop if missing(wbname)
replace iso3 = "GBR" if wbname == "United Kingdom"
replace iso3 = "RUS" if wbname == "Russia"
replace iso3 = "MDA" if wbname == "Moldova"
replace iso3 = "GMB" if wbname == "Gambia"
replace iso3 = "KAZ" if wbname == "Kazakhstan"
replace iso3 = "COG" if wbname == "Congo, Rep."
replace iso3 = "YUG" if wbname == "Serbia and Montenegro"

drop ly2002 a4 c4 t4 mw4 tr4 lpop* tropical dist2 larable landlocked eu af as am oc u1000 u0 country countries pop1000 latitude distequat year cc clusc clus1000 ccode1000

* drop the average of the sectors
drop tr*

* this is the same as mt3, c3... so drop it.
drop mt1500c c1500c


rename a1 tech_1000bc
rename a2 tech_0ad
rename a3 tech_1500ad

rename c1 communication_1000bc
rename c2 communication_0ad
rename c3 communication_1500ad

rename t1 transport_1000bc
rename t2 transport_0ad
rename t3 transport_1500ad

rename mt1 military_1000bc
rename mt2 military_0ad
rename mt3 military_1500ad

rename mw1 industry_1000bc
rename mw2 industry_0ad
rename mw3 industry_1500ad

rename mt1mig military_1000bc_mig_adj
rename mt2mig military_0ad_mig_adj
rename mt3mig military_1500ad_mig_adj

rename mw1mig industry_1000bc_mig_adj
rename mw2mig industry_0ad_mig_adj
rename mw3mig industry_1500ad_mig_adj

rename a1mig tech_1000bc_mig_adj
rename a2mig tech_0ad_mig_adj
rename a3mig tech_1500ad_mig_adj

rename c1mig communication_1000bc_mig_adj
rename c2mig communication_0ad_mig_adj
rename c3mig communication_1500ad_mig_adj

rename t1mig transport_1000bc_mig_adj
rename t2mig transport_0ad_mig_adj
rename t3mig transport_1500ad_mig_adj

drop weu chi india
rename arab arab_empire
replace arab_empire = 0 if missing(arab_empire)

drop wbname
rename iso3 country_code

label variable colony "=1 if colonised"

* drop the migration adjusted variables, because I will do the adjustment myself. 
drop *_mig_adj

replace country_code = "ROM" if country_code == "ROU"
replace country_code = "SRB" if country_code == "YUG"
rename * Har_=
rename *country_code country_code
save "intermediate_data/dd13.dta", replace





* https://academic.oup.com/restud/article/83/4/1334/2223564?login=true#supplementary-dat 
* Climate and the Emergence of Global Income Differences. Thomas Barnebeck Andersen, Carl-Johan Dalgaard, Pablo Selaya
* UV data. 

use "raw_data/uvr_crosscountry.dta", clear
keep countrycode avgUV 
rename countrycode country_code
rename avgUV avg_uv_exposure

replace country_code = "ROM" if country_code == "ROU"
save "intermediate_data/dd15.dta", replace



* GapMinder Life Expectancy Data. https://www.gapminder.org - accessed 26.10.23
* They compile data from a number of different sources to get the longest, most complete dataset on life expectancy that I can find.

import excel "raw_data/GapMinder_Life_Expectancy.xlsx", clear sheet(data-lex-in-columns)
drop in 1/3
drop KR KS

destring G, replace
destring L, replace

ds, has(type string)

* rename + drop predictions
qui ds, has(type numeric)
foreach v of varlist `r(varlist)' {
	if `v'[1] >= 2020 drop `v'
}

qui ds, has(type numeric)
foreach v of varlist `r(varlist)' {
    rename `v' y_`=`v'[1]'
}

* Turn the strings into upper case. 
qui ds, has(type string)
foreach v of varlist `r(varlist)' {
	replace `v' = strupper(`v')
    rename `v' `=`v'[1]'
}

drop in 1
drop if missing(GEO)
drop in 198/203

* Finally, you can get some natural disaster, war, or Spanish Flu or something like this to cause a temporary blip in your mortality data. So I take a moving average; I then drop everything else and say good riddance to this dataset. 
egen life_expectancy_1800 = rowmean(y_1800 y_1801 y_1802 y_1803 y_1804)

keep GEO life_expectancy_1800
drop if missing(life_expectancy_1800)

rename GEO country_code

replace country_code = "ROM" if country_code == "ROU"
save "intermediate_data/dd16.dta", replace


* Name: NP
* This data is well documented here: https://diegopuga.org/data/rugged/#country and was prepared for: Nathan Nunn and Diego Puga 'Ruggedness: The blessing of bad geography in Africa', published in the Review of Economics and Statistics 94(1), February 2012: 20-36

use "raw_data/rugged_data.dta", clear
drop rgdp* cont* q_rule_law gemstones lon lat country

g slave_ex_pc = slave_exports / pop_1400

* Assume that if you're missing slave-market distance in this dataset, it's because you were not involved in the supply side of this market. As such, replace missing with largest possible number (since regression trees then split them off).
foreach var in dist_slavemkt_atlantic dist_slavemkt_indian dist_slavemkt_saharan dist_slavemkt_redsea{
replace `var' = 100 if missing(`var')
}

drop isonum
rename isocode country_code
replace country_code = "ROM" if country_code == "ROU"
rename * NP_=
rename *country_code country_code

save "intermediate_data/dd17.dta", replace



* Name: SBB
* note that here, it's not all his own data, he grabs some from various sources, as outlined on p.52 of the stuff on the online appendix: https://www.science.org/doi/10.1126/science.aau5141 (see, in particular p.15)
* I haven't gone to the actual sources of the data; may be worth doing at some point. 
* authors: Jonathan F. Schulz*, Duman Bahrami-Rad, Jonathan P. Beauchamp, Joseph Henrich
* Title: The Church, intensive kinship, and global psychological variation
* replication files from: https://datadryad.org/stash/dataset/doi:10.5061/dryad.2rbnzs7hs 


use "raw_data/alldata.dta", clear

keep js_code ct_code country KII fs_complexityIND_wop fs_cousin_pref_cont_est2 fs_polygyny_cont fs_polygyny_cont_std2 fs_localdom_est fs_lineage fs_clan fs_year ChurchExpWest ChurchExpEast ft_CombinedParasiteStress du_caloricsui du_oat_rain du_rye_rain nu_rugged irri_impact5 

rename js_code country_code 
drop ct_code country
rename * SBB_=
rename *country_code country_code

replace country_code = "ROM" if country_code == "ROU"
save "intermediate_data/dd18.dta", replace


* Ethnographic Atlas Data.
* Followiong Nunn and Giuliano on the ancestry adjustment. This seems to be the accepted way of doing this. They also exted the EA into Europe.
* Nunn provides replication data: https://scholar.harvard.edu/nunn/pages/data-0 

use "raw_data/Ancestral_Characteristics_Database_Extended_Eastern_Europe_Siberia_and_WES_Version_1_1.dta", clear

* drop stuff that isn't EA
drop v102 v104 v106 KG_code_11 KG_code_12 KG_code_13 KG_code_14 KG_code_21 KG_code_22 KG_code_26 KG_code_27 KG_code_31 KG_code_32 KG_code_33 KG_code_34 KG_code_35 KG_code_36 KG_code_37 KG_code_38 KG_code_39 KG_code_41 KG_code_42 KG_code_43 KG_code_44 KG_code_45 KG_code_46 KG_code_47 KG_code_48 KG_code_49 KG_code_50 KG_code_51 KG_code_52 KG_code_61 KG_code_62 avgrug dist_coast

* In the original paper https://scholar.harvard.edu/files/nunn/files/giuliano_nunn_ehdr_2018_0.pdf Giuliano & Nunn show how to aggregate V6, V25, V30, V33, V43
* Start here: https://d-place.org/contributions/EA

* Note that I replace data as missing if 80% of data is missing.

* A measure of intensity of gathering, hunting, fishing etc: 
local y = 1
foreach x in gathering hunting fishing animal_husbandry agriculture {
	g `x'_ea = .
	replace `x'_ea = v`y'_grp1 * 0 + v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3 + v`y'_grp5 * 4 + v`y'_grp6 * 5 + v`y'_grp7 * 6 + v`y'_grp8 * 7 + v`y'_grp9 * 8
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}


* Now, do the pastoralism variable of Anke Becker: On the Economic Origins of Restrictions on Women's Sexuality. Becker's code is not currently available, so have to recreate it.
egen herd_animal = rowtotal(v40_grp4 v40_grp5 v40_grp6 v40_grp7 v40_grp8)
replace herd_animal = herd_animal/ (1-v40_grp1)
g pastoralism_ea = herd_animal*animal_husbandry_ea
drop herd_animal

* I want a measure of setlement complexity, note that there are some missing variables, so, unlike the above, the denom is now 1 - %missing: 
local y = 30
foreach x in settlement_complexity {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3 + v`y'_grp5 * 4 + v`y'_grp6 * 5 + v`y'_grp7 * 6 + v`y'_grp8 * 7 + v`y'_grp9 * 8) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

local y = 33
foreach x in jurisdictional_hierarchy {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3 + v`y'_grp5 * 4 + v`y'_grp6 * 5) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

* same as jurisdictional_hierarchy, but for within a local community
local y = 32
foreach x in local_juris_hier {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

forvalues x = 2/8{
	g marriage_`x' = v6_grp`x' / (1-v6_grp1)
	replace marriage_`x' = . if v6_grp1 >= 0.8
	drop v6_grp`x'
}
drop v6_grp1

* v6 is 'primary' marriage practices and v7 is 'alternate' marriage practices
drop v7_*

* these are also considered 'alternate' rather than 'primary' or 'prevailing' practices; so I drop them: 
drop v13_* v14_* 

* Fraction of a popn who have no preference for cousin marriage over non-cousin marriage
g cousin_marriage_ea = (1 - (v25_grp1 + v25_grp16) ) / ( 1 - v25_grp1)
replace cousin_marriage_ea = . if v25_grp1 >= 0.8
drop v25*

* Variables similar to v25 that i remove (in part, because I use Enke's kinship score): 
drop v23* v24* v26*

* For Lineages and Marriage Types I follow: https://doi.org/10.1111/tops.12430
* Lineages:
g patrilineal_ea = v43_grp2 / (1 - v43_grp1)
g matrilineal_ea = v43_grp4 / (1 - v43_grp1)
g bilateral_ea = (v43_grp5 + v43_grp7) / (1 - v43_grp1)
g ambilineal_ea = (v43_grp3 + v43_grp6) / (1 - v43_grp1)
g mixed_descent_ea = (v43_grp8) / (1 - v43_grp1)

foreach x in patrilineal_ea matrilineal_ea bilateral_ea ambilineal_ea mixed_descent_ea{
	replace `x' = . if v43_grp1 >= 0.8
}
drop v43*

* Marriage Types
g endogamous_ea = (v15_grp2 + v15_grp3) / (1 - v15_grp1)
g agamous_ea = (v15_grp4) / (1 - v15_grp1)
g exogamous_ea = (v15_grp5 + v15_grp6 + v15_grp7) / (1 - v15_grp1)

foreach x in endogamous_ea agamous_ea exogamous_ea {
	replace `x' = . if v15_grp1 >= 0.8
}
drop v15*

*endogamous - marry within your group,clan etc. agamous - no norms about marrying within our outside your group. 

g wifes_group_ea = (v11_grp4) / (1 - v11_grp1)
g husbands_group_ea = (v11_grp2) / (1 - v11_grp1)

foreach x in wifes_group_ea husbands_group_ea {
	replace `x' = . if v11_grp1 >= 0.8
}
drop v11*

g irrigated_agriculture_ea = v28_grp7 / (1 - v28_grp1)
replace irrigated_agriculture_ea = . if v28_grp1 >= 0.8

local y = 28
foreach x in agricultural_intensity {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3 + v`y'_grp5 * 4 + v`y'_grp6 * 5 + v`y'_grp7 * 6) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

local y = 31
foreach x in townsize {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3 + v`y'_grp5 * 4 + v`y'_grp6 * 5 + v`y'_grp7 * 6 + v`y'_grp8 * 7 + v`y'_grp9 * 8) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

* Sex differnces in particular occupations: v44 to v54.
g unequal_gathering_ea = (v50_grp4 + v50_grp5 ) / (1 - v50_grp1 - v50_grp8 - v50_grp9) 
replace unequal_gathering_ea = . if v50_grp1 >= 0.8

* no community where hunting is shared equally
* g unql_hunting_ea = v51
local y = 52
foreach x in unql_fishing unql_animalhusbandry unql_agriculture {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp4 + v`y'_grp5 ) / (1 - v`y'_grp1 - v`y'_grp8 - v`y'_grp9)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	local y = `y' + 1
}

forvalues i = 44/54{
	drop v`i'_*
}

* v55/v65 are specialisation in a given occupation. 
* so, for metalwork, it's basically 'were there blacksmiths in the society?', and the answer is: missing -> missing; craft.specialisation -> yes; industrial.specialisation -> yes; task absent or occupation absent -> no (ie. it wasn't a specialist profession, or the profession didn't exist)
* We prefer the extensive margin - 'did the task exist' and that's already captured in v44:v54.

forvalues i = 55/65{
	drop v`i'_*
}

* Principal type of crop cultivated.
drop v29_*

* Housing materials: 
forvalues i = 79/89{
	drop v`i'_*
}

* Drop some geographic (91,92), climatic (95,96) and linguistic (97) variables - partly coz they're missing lots of data and partly because they're caputred by more modern stuff. 
drop v91_* v92_* v95_* v96_* v97_* 


* Ranking democratic succession 
local y = 94
foreach x in democratic_succession {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp8 * 1 + v`y'_grp9 * 3 + v`y'_grp10 * 2 + v`y'_grp11 * 2) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

* Poitical Integration
local y = 90
foreach x in political_integration {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp2 * 0 + v`y'_grp3 * 1 + v`y'_grp4 * 2 + v`y'_grp5 * 3 + v`y'_grp6 * 4 + v`y'_grp7 * 5 + v`y'_grp8 * 6) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

* Female premarital sex norms
local y = 78
foreach x in premarital_sex {
	g `x'_ea = .
	replace `x'_ea = (v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3 + v`y'_grp5 * 4 + v`y'_grp6 * 5 + v`y'_grp7 * 6) / (1 - v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 1
}

* Inheritance Rules
local y = 74
foreach x in land non_land {
	g no_`x'_property_rights_ea = v`y'_grp2 / (1 - v`y'_grp1)
	g patri_`x'_inherit_ea = .
	replace patri_`x'_inherit_ea = (v`y'_grp3 * 1 + v`y'_grp4 * 2 + v`y'_grp5 * 3 + v`y'_grp6 * 4 + v`y'_grp7 * 5 + v`y'_grp8 * 6) / (1 - v`y'_grp1 - v`y'_grp2)
	replace no_`x'_property_rights_ea = . if v`y'_grp1 >= 0.8
	replace patri_`x'_inherit_ea = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 2
}

* Equal inheritance
local y = 75
foreach x in land non_land {
	g equal_`x'_inheritance = v`y'_grp2 / (1 - v`y'_grp1- v`y'_grp6)
	replace equal_`x'_inheritance = . if v`y'_grp1 >= 0.8
	drop v`y'_*
	local y = `y' + 2
}


* Data for 71 is not useable. 
drop v71*

g slavery_ea = (1 - v70_grp2 - v70_grp1) / (1 - v70_grp1)
replace slavery_ea = . if v70_grp1 >= 0.8
drop v70*

* Political Succession; 72 is straight forward, v73 is the type of succession.
g matrilineal_politics_ea = v72_grp3 / (1-v72_grp1 - v72_grp9)
g patrilineal_politics_ea = v72_grp2 / (1-v72_grp1 - v72_grp9)

foreach x in matrilineal_politics_ea patrilineal_politics_ea {
	replace `x' = . if v72_grp1 >= 0.8
}

drop v73* v72*

* Class differentiation. 
*drop secondary features
drop v67* v69*
local y = 66 
foreach x in class_absent_66 class_absent_68 {
	g `x'_ea = v`y'_grp2 / (1-v`y'_grp1)
	replace `x'_ea = . if v`y'_grp1 >= 0.8
	local y = `y' + 2
} 

g class_wealth_ea =  v66_grp3 / (1 - v66_grp1)
g class_ethni_ea =  v66_grp4 / (1 - v66_grp1)
g class_elite_ea =  v68_grp4 / (1 - v68_grp1)
replace class_elite_ea = . if v68_grp1 >= 0.8

foreach x in class_ethni_ea class_wealth_ea {
	replace `x' = . if v66_grp1 >= 0.8
}
drop v68* v66*

* Dominant subsistence economy activity (this is derived from varirabels 1 through 6; see above)
drop v42*

* Milking of domestic animals
g milking_ea = v41_grp3 / (1 - v41_grp1)
replace milking_ea = . if v41_grp1 >= 0.8
drop v41*

* Domestic animals
g domestic_animals_ea = 1 - ( v40_grp2 / (1 - v40_grp1))
replace domestic_animals_ea = . if v40_grp1 >= 0.8
drop v40*

* Aboriginal plough use
g plough_ea = v39_grp4 / (1 - v39_grp1)
replace plough_ea = . if v39_grp1 >= 0.8
drop v39*

* male circumcision 
drop v37*

* segragation of males at puberty
drop v38*

* Postpartum sex taboo
local y = 36
foreach x in sex_taboo_ea {
	g `x' = (v`y'_grp2 * 1 + v`y'_grp3 * 2 + v`y'_grp4 * 3 + v`y'_grp5 * 4 + v`y'_grp6 * 5 + v`y'_grp7 * 6) / (1 - v`y'_grp1)
	replace `x' = . if v`y'_grp1 >= 0.8
	drop v`y'_*
}

* Games with winners and losers
g games_ea = 1 - (v35_grp2 / (1 - v35_grp1))
replace games_ea = . if v35_grp1 >= 0.8
drop v35*

* Moral god
g moral_god_ea = v34_grp5 / (1 - v34_grp1)
replace moral_god_ea = . if v34_grp1 >= 0.8
drop v34*

* kin terms for cousin. 
* could be interesting; too much missing data; dropped. 
drop v27*

* Domestic organisation
g nuclear_family_ea = v8_grp2 / (1 - v8_grp1)
replace nuclear_family_ea = . if v8_grp1 >= 0.8
g sororal_marriage_ea = ( v9_grp4 + v9_grp5 ) / (1 - v9_grp1)
replace sororal_marriage_ea = . if v9_grp1 >= 0.8
drop v8_* v9_*

* Marital Residence
* v10 is marital residence in the first few years post marriage. and essentially it doesn't provide extra info from v12 - enduring marital residence. So I use v12. Arguably, I should use both, in the vein of 'no loss from not doing so'.
drop v10_*

g ambilocal_ea = v12_grp3 / (1 - v12_grp1)
g matrilocal_ea = (v12_grp10 + v12_grp6) / (1 - v12_grp1)
g patrilocal_ea = (v12_grp5 + v12_grp9 + v12_grp11) / (1 - v12_grp1)
g neolocal_ea = v12_grp7 / (1 - v12_grp1)
g avunculocal_ea = (v12_grp2 + v12_grp4 ) / (1 - v12_grp1)
g not_local_ea = v12_grp8 / (1 - v12_grp1)

foreach x in ambilocal_ea matrilocal_ea patrilocal_ea neolocal_ea avunculocal_ea not_local_ea {
	replace `x' = . if v12_grp1 >= 0.8
}

drop v12_*

* organisation of clans
g clans_ea = (v16_grp2 + v16_grp3) / (1 - v16_grp1)
replace clans_ea = . if v16_grp1 >= 0.8
drop v16_*

* cognatic descent system
* exogamous ramages - exogamous = outside of family/clan/in group; ramage = descent group
g exogamous_ramages_ea = v21_grp6 / (1 - v21_grp1)
replace exogamous_ramages_ea = . if v21_grp1 >= 0.8
g unilineal_descent_ea = v21_grp8 / (1 - v21_grp1)
replace unilineal_descent_ea = . if v21_grp1 >= 0.8

g kindred_descent_ea = (v22_grp2 * 1 + v22_grp3 * 2) / (1 - v22_grp1 - v22_grp4)
replace kindred_descent_ea = . if v22_grp1 >= 0.8
drop v21_* v22_*

* Lineages
g single_patri_lineage = v17_grp4 / (1 - v17_grp1)
replace single_patri_lineage = . if v17_grp1 >= 0.8
g single_matri_lineage = v19_grp4 / (1 - v19_grp1)
replace single_matri_lineage = . if v19_grp1 >= 0.8

drop v17* v18* v19* v20*

* Add _ea to remaining variables: 
foreach x in marriage_2 marriage_3 marriage_4 marriage_5 marriage_6 marriage_7 marriage_8 single_patri_lineage single_matri_lineage equal_land_inheritance equal_non_land_inheritance{
	rename `x' `x'_ea
}

rename isocode country_code
replace country_code = "ROM" if country_code == "ROU"
replace country_code = "SRB" if country_code == "YUG"
save "intermediate_data/dd19.dta", replace
* Ancestry Adjustment: the EA data is already ancestor adjusted by Giuliano Nunn
