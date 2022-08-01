******************************************************
* Future Crude Oil prices
* Combine crude oil prices from EIA, CME, Barchart
* Extend the price 2025 to 2030 
* Assume that prices are flat in real term after 2025 for CME and NYMEX 
*****************************************************

*Load Brent futures prices
insheet using "$raw_data/Brent_crude_oil_futures_price_implied_volatility_08052018_CME_barchart.csv", clear

gen double time=monthly(contract, "M20Y")
ren last futures 
keep contract futures time




*Set time variables
*Expand tiem to 2039 December
tsset time
drop if time>791
tsappend, add(168)

gen date=dofm(time)
gen month=month(date)
gen year=year(date)

*Add EIA Forecast (nominal prices)
gen EIA_forecast=179 if year==2040
	replace EIA_forecast=152 if year==2035	
	replace EIA_forecast=128 if year==2030	
	replace EIA_forecast=104 if year==2025
	replace EIA_forecast=43 if year==2016
	replace EIA_forecast=52 if year==2015
 
 
*linear Interporlation of EIA's crude oil price forecast between 2017 and 2024
*2017 to 2024
forval i=1/8 {
	replace EIA_forecast=43+((104-43)*`i'/9) if year==2016+`i'
}

forval i=1/4 {
	replace EIA_forecast=104+((128-104)*`i'/5) if year==2025+`i'
}
forval i=1/4 {
	replace EIA_forecast=128+((152-128)*`i'/5) if year==2030+`i'
}
forval i=1/4 {
	replace EIA_forecast=152+((179-152)*`i'/5) if year==2035+`i'
}

*Add lower bound 
merge 1:1 time using "$processed_data\futures_price_lower_bound.dta"
drop _merge
drop b_price imp_vol

*Set future value is flat in the real term (after 2025 December)
foreach var of var futures lb  {
	replace `var'=`var'[87]*(1+$inf)^(year-2023) if year>2023
}
*
*--------------------------------------------------------------------------
*Add brent crude oil prices 
preserve
insheet using "$raw_data/brent_crude_oil_FRED.csv", clear 

*Convert time variable 
gen double time_d=date(date, "MDY") 
*generate month variable 
gen time=mofd(time_d)
ren dcoilbrenteu price 
keep time price
order time price
tempfile price
save `price.csv', replace 
restore 

append using `price.csv'

*--------------------------------------------------------------------------


*Generate real term (2016 dollars) of EIA forecast, future and lower bound prices 
local variable EIA_forecast futures lb
foreach var of local variable {
	gen r_`var'=. 
	forval i=0/24 {
		replace r_`var'=`var'/((1+$inf)^`i') if year==2016+`i'
	}
	label var r_`var' "real 2016 dollars"
}
*

drop date month year
gen date=dofm(time)
gen month=month(date)
gen year=year(date)


*-----------------------------------------------------------------------
*Graph
*-----------------------------------------------------------------------
*Graph crude oil and future crude oil prices 

keep if year>=1999
twoway ///
(line price time, lc(navy)) ///
(line lb time,  lc(blue*1.5)) ///
(line futures time, lc(orange*1.1)) ///
(line EIA_forecast time if time>=696, lc(red*1.3) lp(dash)) , ///
xlabel(468(48)971, format(%tmCY)) xtitle("") ///
graphregion(color(white))  ///
ytitle("$/bbl")  ///
legend(off)	///
xline(705, lc(black) lp(dash)) ///
text(160 550 "Brent crude oil price", place(e) color(navy)) ///
text(100 800 "EIA Forecast", color(red*1.5) place(e)) ///
text(65 800 "Futures Price", color(orange*1.1) place(e)) ///
text(40 750 "Lower Bound of Futures Price", color(blue*1.5) place(e)) 

graph export "$word_figure/past_and_future_crude_oil_prices.png", replace


*Graph future crude oil prices (real, 2016 dollars, annual inflation rate: 2.1%)
twoway ///
(line r_EIA_forecast time if time>=696, lc(red*1.5) ) ///
(line r_futures time if time>=696, lc(blue*1.5) ) ///
(line r_lb time if time>=696, lc(green*1.5) ), ///
graphregion(color(white))   ///
legend(off) ///
xtitle("")  ///
xscale(r(680 971)) ///
xline(696, lc(black) lp(dash)) ///
xlabel(696(49)971, format(%tmCY)) ///
ytitle("2016 dollars / bbl") ///
text(100 800 "EIA Forecast", color(red*1.5) place(e)) ///
text(51 800 "Futures Price", color(blue*1.5) place(e)) ///
text(40 800 "Lower Bound of Futures Price", color(green*1.5) place(e)) 
graph export "future_r_crude_oil_price.png", replace




save "$processed_data/crude_oil_forecast.dta", replace
end



 
