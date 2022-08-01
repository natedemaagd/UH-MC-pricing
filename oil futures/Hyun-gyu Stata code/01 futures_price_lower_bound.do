*************************************************
* Create the lower bound of futures prices 
* by using implied volatility of Brent Crude oil futures 
* Data collected from the following site
*	*Futures prices 
* 	https://www.cmegroup.com/trading/energy/crude-oil/brent-crude-oil.html
* 	*implied volatility
*	https://www.barchart.com/futures/quotes/CBV18/options/oct-18
*	the information of implied volatility availabe until next 4 years
*	If the current year is 2020, the implied volatility information is avaiable until 2024 
*************************************************

*Data collected on Aug/05/2018 
*insheet using "$raw_data/Brent_crude_oil_futures_price_implied_volatility_08052018_CME_barchart.csv", clear
*Data collected on Feb/05/2018
*insheet using "$raw_data/Brent_crude_oil_futures_price_implied_volatility_on_020518_barchart.csv", clear 
 
*Data collected on Apr/04/2020
insheet using "$raw_data/Brent_futures_price_IV_040420.csv", clear

*Rename the variable 
ren last price
ren iv imp_vol
ren price b_price


*Create time variable 
gen double time=monthly(contract, "M20Y")
scalar max_time=r(max)
drop if time>max_time

/*
twoway ///
(line imp_vol time if inrange(time, 726, 778), color(red)) ///
(line iv_may_5 time if inrange(time, 726, 778), color(blue)) ///
(line iv_may_5_gap time if inrange(time, 726, 778), color(orange)), ///
ytitle(" percent(%)") ///
xlabel(726(12)778, format(%tmCY))  ///
graphregion(color(white)) ///
legend(off) ///
text(85 727 "Implied volatility (April 2020)", color(red) place(e))  ///
text(65 730 "Implied volatility (May 2020)", color(blue) place(e))  ///
text(15 730 "Difference of Implied volatility" "between months (May 2020)", color(orange) place(e)) 
graph export "$graphs_for_report/implied_volatility_May_5_2020.pdf"
*/

*Set implied volatility is the same as the last month volatility if the site provides no information 
replace imp_vol=imp_vol[_n-1] if imp_vol==.

*Generate lower bound of futures prices 
gen lb=b_price-(b_price*(imp_vol/100))



******************
* Add futures prices in August 2019 for comparison

preserve
	*Data collected on August/26/2019
	insheet using "$raw_data/Brent_crude_oil_futures_price_implied_volatility_082319_CME_barchart.csv", clear

	*Rename the variable 
	ren last price_prev
	ren iv imp_vol_prev
	ren price b_price_prev

	*Create time variable 
	gen double time=monthly(contract, "M20Y")
	sum time
	scalar max_time=r(max)
	drop if time>max_time
	
	*Set implied volatility is the same as the last month volatility if the site provides no information 
	replace imp_vol_prev=imp_vol_prev[_n-1] if imp_vol_prev==.

	*Generate lower bound of futures prices 
	gen lb_prev=b_price_prev-(b_price_prev*(imp_vol_prev/100))

	
	tempfile a 
	save `a.dta', replace
restore

****************************
merge 1:1 time using `a.dta'
drop _merge

*****************************
* Add historical brent crude oil prices
preserve
	insheet using "$raw_data/POILBREUSDM_Aug_2019.csv", clear
	gen double time=monthly(date, "M20Y")
	ren poilbreusdm price
	keep time price
	tempfile brent_crude_price
	save "`brent_crude_price.dta'", replace 
restore

merge 1:1 time using "`brent_crude_price.dta'"
drop _merge

gen date=dofm(time)
gen month=month(date)
gen year=year(date)

sort time


*Graph for comparison
preserve
	keep if time>=636
	twoway ///
	(line price time, lw(medthick)) ///
	(line b_price time, lc(green*1.3) lw(medthick)) ///
	(line lb time, lc(orange*1.3) lw(medthick)) ///
	(line b_price_prev time, lw(medthick) lp(dash) lc(green*0.5)) ///
	(line lb_prev time, lw(medthick) lp(dash) lc(orange*0.5)),  ///
	graphregion(color(white)) ///
	xline(725, lp(dash) lc(black)) ///
	legend(off) ///
	xtitle("")  ///
	ytitle("Dollars per Barrel") ///
	xlabel(636(24)829, format(%tmCY))  ///
	xscale(r(636 873))  ///
	text(130 638 "Past Brent Crude Oil Price", color(gs4) place(e)) ///
	text(60 800 "Mid scenario (Aug 2019)", color(green*0.7) place(e)) ///
	text(41 800 "Low scenario (Aug 2019)", color(orange*0.7) place(e)) ///
	text(48 805 "Mid scenario (Apr 2020)", color(green*1.3) place(e)) /// 
	text(25 805 "Low scenario (Apr 2020)", color(orange*1.3) place(e))
	*graph expor "BCO_price_comparison_Aug_2019_Mar_2020.pdf", replace

restore 

*Drop variables about 2018 
drop *_prev

end

*Save the file 
save "$processed_data/futures_price_lower_bound.dta", replace


end
