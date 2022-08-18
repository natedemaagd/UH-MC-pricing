******************************************************
* Future Crude Oil prices
* Combine crude oil prices from EIA, barchart
* Extend the price 2025 to 2045 
* Assume that prices are flat in real term after 2029  
*****************************************************
 
*Load historical oil prices and computed lower bound 
use "$processed_data/futures_price_lower_bound.dta", clear
set more off 

*Add brent crude oil price 
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



*set dataset as time series
tsset time
*extend the timeseries to 2045 December
tsappend, add(201)
drop date month year
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
	replace EIA_forecast=210 if year==2045
 
 
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

forval i=1/4 {
	replace EIA_forecast=179+((210-179)*`i'/5) if year==2040+`i'
}




*Extand the futures price and low price to 2045 
*foreach var of var futures_price_CME b_price lb  {
foreach var of var b_price lb  {
	replace `var'=`var'[339]*(1+$inf)^(year-2028) if time>818
}




twoway ///
(line price time, lc(gs8) lw(medthick))  ///
(line b_price time,  lc(orange*1.1) lw(medthick)) ///
(line lb time,  lc(blue*1.5) lw(medthick) ) ///
(line EIA_forecast time if time>=715, lc(red*1.3) lp(dash) lw(medthick)) , ///
xlabel(519(48)1031, format(%tmCY)) xtitle("") ///
graphregion(color(white))  ///
ytitle("$/bbl")  ///
legend(off)	///
xline(715, lc(black) lp(dash)) ///
text(160 550 "Brent crude oil price", place(e) color(gs8)) ///
text(130 735 "EIA forecast", color(red*1.3) place(e)) ///
text(75 735 "Brent crude oil futures price", color(orange*1.1) place(e)) ///
text(35 720 "Lower bound of futures price", color(blue*1.5) place(e)) 
*graph export "BCO_price_forecast.png", replace

*obtain crude oil prices for analysis 
preserve
ren b_price futures
keep time year month EIA_forecast futures lb price
save "$processed_data/crude_oil_forecast_april_2018.dta", replace 
keep if year>=2020 
keep year month EIA_forecast futures lb
outsheet using "~/Dropbox/UH_energy_project/inputs_outputs/original_inputs/oil_price_2020_2045.csv" , comma nolabel replace
restore
end

**************************************************
* Convert oil price from nominal to 2019 dollars
**************************************************
*add inflation from the excel that Matthias shared (May, 11, 2020)
preserve
insheet using "$raw_data/inflation_from_Matthias.csv", clear
tempfile a 
save `a.dta'
restore 
merge n:1 year using `a.dta'
drop _merge


*Discount different oil price projections to 2019 dollar term
preserve
foreach var of varlist EIA_forecast b_price lb {
	gen `var'_2019=`var'/(inflation^(year-2019))

}


preserve
keep if inrange(year,2020,2045)
ren EIA_forecast_2019 high
ren b_price_2019 mid
ren lb_2019 low
keep year month high mid low
sort year month *
order year month high mid low
outsheet using "$processed_data/crude_oil_price_projections_2020_2045_2019dollar.csv" , comma nolabel replace
restore 





end



 
