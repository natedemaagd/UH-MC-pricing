
# this script replicates Hyun-Gyu's oil futures script originially written in Stata.
# stata do file: crude_oil_forecast_HECO_CME_lower_bound_Aug052018.do

library(readxl); library(lubridate); library(tidyverse); library(zoo)
library(haven)




##### load and format data #####

# load Hyun-Gyu's final data for comparison
dat_HyunGyu <- read_dta("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Oil prices - Hyun-Gyu/crude_oil_forecast.dta")

# load futures data
datRaw <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/Brent_crude_oil_futures_price_implied_volatility_08052018_CME_barchart.csv.xlsx")
datRaw$contract <- as.Date(datRaw$contract)
colnames(datRaw)[colnames(datRaw) == 'contract'] <- 'date'

# load lower bound data - see futures_price_lower_bound.do
datLowerBound <- read_dta("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Oil prices - Hyun-Gyu/futures_price_lower_bound.dta")
datLowerBound$date <- as.Date(paste0(datLowerBound$year, '-',
                                     datLowerBound$month, '-01'))
datLowerBound$imp_vol <- datLowerBound$year <- datLowerBound$month <- NULL
datLowerBound <- datLowerBound[,colSums(is.na(datLowerBound)) < nrow(datLowerBound)]  # remove columns of NA

# load brent crude oil FRED
datBrentFred <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/brent_crude_oil_FRED.csv")
datBrentFred$date <- as.Date(datBrentFred$DATE,format = '%m/%d/%Y')
datBrentFred$DATE  <- NULL
datBrentFred <- datBrentFred[c('date', 'DCOILBRENTEU')]




##### merge and format data #####

# create data.frame of EIA crude oil price forecast
datEIA <- data.frame(year = 2015:2040,
                     EIA_forecast = NA)
datEIA$EIA_forecast[datEIA$year == 2040] <- 179
datEIA$EIA_forecast[datEIA$year == 2035] <- 152
datEIA$EIA_forecast[datEIA$year == 2030] <- 128
datEIA$EIA_forecast[datEIA$year == 2025] <- 104
datEIA$EIA_forecast[datEIA$year == 2016] <- 43
datEIA$EIA_forecast[datEIA$year == 2015] <- 52

# linearly interpolate EIA forecasted prices
datEIA$EIA_forecast <- na.approx(datEIA$EIA_forecast)

# create data.frame with expanded timeline to merge all data
dat <- data.frame(date = seq.Date(as.Date('2015-01-01'),
                                  as.Date('2040-01-01'),
                                  'month'))
dat$month <- day(dat$date)
dat$year <- year(dat$date)

# merge futures data and EIA data to expanded timeline data
dat <- left_join(dat, datRaw, 'date')
dat <- left_join(dat, datEIA, 'year')
rm(datRaw, datEIA)

# merge lower bound data with main data  ### WHERE DOES THIS COME FROM ???
dat <- left_join(dat, datLowerBound, 'date')
rm(datLowerBound)

# add 2% inflation to price variables
vec_vars <- c('last', 'IV', 'b_price', 'price')
for(v in 1:length(vec_vars)){
  dat[,v] <- dat[,v] * (1.02)^(dat$year-2023)
}
rm(vec_vars)



