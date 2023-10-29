						/* This file ancestry adjusts. */


* Merging files
use "intermediate_data/dd1.dta", clear
forvalues i = 2/19 {
merge 1:1 country_code using "intermediate_data/dd`i'"
drop _merge
}


/*
Ancestry Adjustment. 
	1. merge all the files once, drop everything ending in _aa (ie. already ancestry adjusted). 
	2. do the ancestry adjustment. save.
	3. redo the merge. keep just the features i don't want to adjust.
	4. merge the unadjusted, with the saved, adjusted features. 
*/

* Step 1. 
drop *climate *legor_uk *legor_fr *legor_so *legor_ge *legor_sc *africa *europe *asia *oceania *americas *oecd *opec *western *wb_ssa *wb_mena *wb_eca *wb_sas **wb_eap *wb_nam *wb_lac *language_family *drop *colony_esp *colony_gbr *colony_fra *colony_prt *colony_oeu *mineuro *majeuro *colony *arab_empire gini_18 *gdp_pc_19 *country *c_code_n *_aa *_ea

* Naming is consistent across the two datasets, except for:
drop if country_code == "YUG"
replace country_code = "YUG" if country_code == "SRB"
merge 1:1 country_code using "intermediate_data/migration_matrix"
keep if _merge == 3
drop _merge

* Force the ordering of columns & rows to be the same.
sort country_code
order country_code afg	ago	alb	are	arg	arm	aus	aut	aze	bdi	bel	ben	bfa	bgd	bgr	bhr	bih	blr	blz	bol	bra	btn	bwa	caf	can	che	chl	chn	civ	cmr	cog	col	com	cpv	cri	cub	cyp	cze	deu	dnk	dom	dza	ecu	egy	eri	esp	est	eth	fin	fji	fra	gab	gbr	geo	gha	gin	gmb	gnb	gnq	grc	gtm	guy	hkg	hnd	hrv	hti	hun	idn	ind	irl	irn	irq	isl	isr	ita	jam	jor	jpn	kaz	ken	kgz	khm	kor	kwt	lao	lbn	lbr	lby	lka	lso	ltu	lux	lva	mar	mda	mdg	mex	mkd	mli	mlt	mmr	mng	moz	mrt	mus	mwi	mys	nam	ner	nga	nic	niu	nld	nor	npl	nzl	oan	omn	pak	pan	per	phl	png	pol	pri	prk	prt	pry	qat	rom	rus	rwa	sau	sdn	sen	sgp	sle	slv	som	stp	svk	svn	swe	swz	syr	tcd	tgo	tha	tjk	tkm	tmp	ton	tto	tun	tur	tza	uga	ukr	ury	usa	uzb	vct	ven	vnm	wsm	yem	yug	zaf	zar	zmb	zwe


* Remove country_codes with no data. 
drop cil

* Make a local of all the variable names and exclude country_codes: 
ds
local vl = "`r(varlist)'"
local exclude "country_code afg	ago	alb	are	arg	arm	aus	aut	aze	bdi	bel	ben	bfa	bgd	bgr	bhr	bih	blr	blz	bol	bra	btn	bwa	caf	can	che	chl	chn	civ	cmr	cog	col	com	cpv	cri	cub	cyp	cze	deu	dnk	dom	dza	ecu	egy	eri	esp	est	eth	fin	fji	fra	gab	gbr	geo	gha	gin	gmb	gnb	gnq	grc	gtm	guy	hkg	hnd	hrv	hti	hun	idn	ind	irl	irn	irq	isl	isr	ita	jam	jor	jpn	kaz	ken	kgz	khm	kor	kwt	lao	lbn	lbr	lby	lka	lso	ltu	lux	lva	mar	mda	mdg	mex	mkd	mli	mlt	mmr	mng	moz	mrt	mus	mwi	mys	nam	ner	nga	nic	niu	nld	nor	npl	nzl	oan	omn	pak	pan	per	phl	png	pol	pri	prk	prt	pry	qat	rom	rus	rwa	sau	sdn	sen	sgp	sle	slv	som	stp	svk	svn	swe	swz	syr	tcd	tgo	tha	tjk	tkm	tmp	ton	tto	tun	tur	tza	uga	ukr	ury	usa	uzb	vct	ven	vnm	wsm	yem	yug	zaf	zar	zmb	zwe"

local vl: list vl - exclude

foreach feature in `vl' {
	qui g `feature'_aa = .
forvalues i = 1/`=_N'{
	* i + 1 because we have a country_code column. 
	scalar a = `i' + 1
	qui ds
	local var : word `=a' of `r(varlist)'
	* weighted average:
	asgen blip = `feature', w(`var')
	qui replace `feature'_aa = blip in `i'
	
	g not_missing = !missing(`feature')
	egen ttl = total(`var'*not_missing)

	* Drop if the too much missing data (we chose 80%) for a country. 
	qui replace `feature'_aa = . in `i' if ttl < 0.8
	drop blip not_missing ttl
}
drop `feature'
}
save "intermediate_data/deep_dets.dta", replace

use "intermediate_data/dd1.dta", clear
forvalues i = 2/19 {
merge 1:1 country_code using "intermediate_data/dd`i'"
drop _merge
}

* Keep just the variables we haven't ancestry-adjusted.
keep AG13_climate *legor_uk *legor_fr *legor_so *legor_ge *legor_sc AG13_africa AG13_europe AG13_asia AG13_oceania AG13_americas AG13_oecd AG13_opec AG13_western AG13_wb_ssa AG13_wb_mena AG13_wb_eca AG13_wb_sas AG13_wb_eap AG13_wb_nam AG13_wb_lac PD_language_family PD_drop *cath1900* *muslim1900* *cont_ssa* *cont_mena* *cont_eca* *cont_sas* *cont_eap* *cont_nam* *cont_lac* *malfal* *colony_esp* *colony_gbr* *colony_fra* *colony_prt* *colony_oeu* *mineuro *majeuro Har_colony *arab_empire gini_18 gdp_pc_19 *_aa *_ea country c_code_n country country_code c_code_n

* Merge with the ancestry-adjusted variables (as before: Naming is consistent across the two datasets, except for:)
* expand 2 if country_code == "YUG"
drop if country_code == "YUG"
replace country_code = "YUG" if country_code == "SRB"
merge 1:1 country_code using "intermediate_data/deep_dets"

drop afg ago alb are arg arm aus aut aze bdi bel ben bfa bgd bgr bhr bih blr blz bol bra btn bwa caf can che chl chn civ cmr cog col com cpv cri cub cyp cze deu dnk dom dza ecu egy eri esp est eth fin fji fra gab gbr geo gha gin gmb gnb gnq grc gtm guy hkg hnd hrv hti hun idn ind irl irn irq isl isr ita jam jor jpn kaz ken kgz khm kor kwt lao lbn lbr lby lka lso ltu lux lva mar mda mdg mex mkd mli mlt mmr mng moz mrt mus mwi mys nam ner nga nic niu nld nor npl nzl oan omn pak pan per phl png pol pri prk prt pry qat rom rus rwa sau sdn sen sgp sle slv som stp svk svn swe swz syr tcd tgo tha tjk tkm tmp ton tto tun tur tza uga ukr ury usa uzb vct ven vnm wsm yem yug zaf zar zmb zwe


* Ethiopia occurs twice, once including, and once excluding Eritrea, keep just one.
drop if c_code == 230
replace country = "Ethiopia" if country == "Ethiopia (excludes Eritrea)"

* Finishing the Serbia inconsistencies. 
replace country_code = "SRB" if country_code == "YUG"
replace c_code = 688 if country_code == "SRB"

sort c_code
drop _merge
save "intermediate_data/deep_determinants.dta", replace
