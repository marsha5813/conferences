
* read in the data
import delim using "../tweets_classified_geocoded.csv"

* fix fips
drop if fips=="NA"
destring fips, replace
drop if fips==.
count

* Destring numeric vars
foreach var of varlist prop* {
	replace `var' = "" if `var' == "NA"
	replace `var' = "" if `var' == "Inf"
	destring `var', replace
}

* Bring in religious geographic data
merge m:1 fips using "../../data_RCMS\U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).dta", nogen keepusing(totadh evanadh mprtadh cathadh POP2010 totrate evanrate mprtrate cathrate totcng evancng mprtcng cathcng)

* bring in county controls
merge m:1 fips using "../../data_county data\countydata2.dta", nogen keepusing(b79aa_2010 a08aa_2010 crime_ucr_modindex_2000 gini_rpme_2010)
rename b79aa_2010 median_income
gen propmale2010 = a08aa_2010/POP2010
gen crimerate2000 = crime_ucr_modindex_2000/POP2010


****************************
* Variable transformations
****************************


* tweets per county
gen id = _n
egen pickfips = tag(fips)
egen ntweets = count(id), by(fips)
sum ntweets if pickfips, detail
gen keepcounty = 1 if ntweets>10
label var ntweets "Number of tweets in the county"
label var keepcounty "County has >10 tweets"

* Pick one county, only including counties with more than 10 tweets
egen pickone = tag(fips)
replace pickone = 0 if keepcounty!=1

* Versions of DVs
recode nb_probpos(0/.9499999 = 0) (.95/1 = 1), gen(ispos) // positive tweet indicator

* County-level variables
bysort fips: egen meansent = mean(nb_sent) // county mean of nb_sent

local emotions "anger anticipation trust surprise sadness joy disgust fear"
foreach i of local emotions {
	bysort fips: egen mean`i' = mean(prop`i')
}



* Log the proportion variables
foreach var of varlist prop* {
	cap drop ln`var'
	gen ln`var' = ln(`var'+1)
}


* Religious population shares
local stubs "tot evan mprt cath"
foreach i of local stubs {
	gen `i'_ps = `i'adh/POP2010
	label var `i'_ps "`i' county popultion share"
	gen pct_`i' = `i'_ps*100
}

* Highly evangelical county
gen highevan = .
replace highevan = 1 if evan_ps>.3
replace highevan = 0 if evan_ps<=.3
replace highevan = . if evan_ps==.

* Standardize variables
foreach var of varlist prop* pct* {
	egen std`var' = std(`var')
}

* Create different versions of fips for later merges
gen strfips = string(fips)
gen zero = "0"
replace zero = "" if fips>9999
gen strfips2 = zero + strfips
drop strfips zero
rename strfips2 strfips
gen statefips = substr(strfips,1,2)
bysort statefips: egen statemeansent = mean(nb_sent) // state mean of nb_sent

* Create state-level versions of variables
egen tagstate = tag(statefips)
ssc install _gwtmean

local emotions "anger anticipation trust surprise sadness joy disgust fear"
foreach i of local emotions {
	cap drop statemean`i'
	by statefips: egen statemean`i' = mean(prop`i')
}
* State versions of control variables
foreach var of varlist pct* crimerate2000 propmale2010 median_income gini_rpme_2010 POP2010 {
	cap drop state_`var'
	by statefips: egen state_`var' = wtmean(`var'), weight(POP2010)
}

* State versions of controls, not weighted by population
foreach var of varlist pct* crimerate2000 propmale2010 median_income gini_rpme_2010 POP2010 {
	cap drop state_`var'_nwt
	by statefips: egen state_`var'_nwt = mean(`var')
}



* State versions of focal predictors
local stubs "totadh evanadh mprtadh cathadh"
foreach i of local stubs {
	cap drop `i'*ps
	cap drop state_`i'
	gen `i'_ps = `i'/POP2010
	bysort statefips: egen state_`i'_ps = wtmean(`i'_ps), weight(POP2010)
}
by statefips: egen state_pop = wtmean(POP2010), weight(POP2010)
by statefips: egen temp = mean(POP2010)


bysort statefips: egen state_ispos = mean(ispos)


* Standardize state variables
foreach var of varlist statemean* state_pct* state_crime* state_prop* state_median* *gini* state_*_ps {
	cap drop std`var'
	egen std`var' = std(`var')
}


* Save the data
save tweets_8-10-17_version.dta, replace

****************************
* Prepare data for mapping
****************************

* Tweet-level csv file
export delim using "../asr-2017/mapdata/tweet-level-data.csv"

* County-level excel file
* THIS IS A DESTRUCTIVE COMMAND
keep if pickone==1
export excel using "../asr-2017/mapdata/county-level-data", firstrow(variables) replace

* State-level excel file
* THIS IS A DESTRUCTIVE COMMAND
keep if tagstate
export excel using "../asr-2017/mapdata/state-level-data", firstrow(variables) replace
save statedata.dta, replace

****************************
* Analyses
****************************

* Data viz

ssc install scheme-burd, replace
ssc install aaplot

* State-level mean probability(positive)
twoway (lfit stdstatemeansent stdstate_pct_tot if tagstate, lw(medthick)) ///
(lfit stdstatemeansent stdstate_pct_evan if tagstate, lw(medthick)) ///
(lfit stdstatemeansent stdstate_pct_mprt if tagstate, lw(medthick)) ///
(lfit stdstatemeansent stdstate_pct_cath if tagstate, lw(medthick)), ///
ti("State mean p(positive) tweet classification") yti("zcore(p(positive tweet))") xti("zscore(religious population share)") ///
legend(order(1  "% Total affil." 2 "% Evangelical" 3 "% Mainline" 4 "% Catholic")) scheme(burd4)
graph export "../asr-2017/lfit_statemeansent.png"

* State-level mean proportion fear words per tweet
twoway (lfit stdstatemeanfear stdstate_totadh_ps if tagstate, lw(medthick)) ///
(lfit stdstatemeanfear stdstate_evanadh_ps if tagstate, lw(medthick)) ///
(lfit stdstatemeanfear stdstate_mprtadh_ps if tagstate, lw(medthick)) ///
(lfit stdstatemeanfear stdstate_cathadh_ps if tagstate, lw(medthick)), ///
ti("State mean proportion fear words per tweet") yti("zcore(proportion fear words)") xti("zscore(religious population share)") ///
legend(order(1  "% Total affil." 2 "% Evangelical" 3 "% Mainline" 4 "% Catholic")) scheme(burd4)
graph export "../asr-2017/lfit_statemeanfear.png", replace


* State-level mean proportion fear words per tweet
twoway (lfit stdstatemeananger stdstate_totadh_ps if tagstate, lw(medthick)) ///
(lfit stdstatemeananger stdstate_evanadh_ps if tagstate, lw(medthick)) ///
(lfit stdstatemeananger stdstate_mprtadh_ps if tagstate, lw(medthick)) ///
(lfit stdstatemeananger stdstate_cathadh_ps if tagstate, lw(medthick)), ///
ti("State mean proportion angry words per tweet") yti("zcore(proportion anger-related words)") xti("zscore(religious population share)") ///
legend(order(1  "% Total affil." 2 "% Evangelical" 3 "% Mainline" 4 "% Catholic")) scheme(burd4)
graph export "../asr-2017/lfit_statemeananger.png"




* Regressions

* Define controls
global controls "state_crimerate2000 state_propmale2010 state_median_income state_gini_rpme_2010 h7v001"

* State-level regressions predicting sentiment, mean probability that tweet is positive
reg statemeansent state_evanadh if tagstate, beta
est store sentm1

reg statemeansent state_evanadh $controls if tagstate, beta
est store sentm2

esttab sent* using "estimates/8-13-17/sentiment.html", replace ///
se nonum b(2) constant noobs nogaps alignment(center) nomti wide ///
order(state_evanadh) ///
coeflabels(state_evanadh "State percent evangelical" ///
state_crimerate2000 "Crime rate" ///
state_propmale2010 "Proportion male" ///
state_median_income "Median income" ///
state_gini_rpme_2010 "Gini index" ///
h7v001 "State population" ///
_cons "Intercept") ///
scalars("N States" "r2 R<sup>2</sup>" ) 













