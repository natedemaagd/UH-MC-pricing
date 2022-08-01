
# *************************************************
# * Create the lower bound of futures prices 
# * by using implied volatility of Brent Crude oil futures 
# * Data collected from the following site
# *	*Futures prices 
# * 	https://www.cmegroup.com/trading/energy/crude-oil/brent-crude-oil.html
# * 	*implied volatility
# *	https://www.barchart.com/futures/quotes/CBV18/options/oct-18
# *	the information of implied volatility available until next 4 years
# *	If the current year is 2020, the implied volatility information is available until 2024 
# *************************************************

library(zoo); library(dplyr); library(lubridate)

# load comparison data (Hyun-gyu's output for this script)
datHyunGyu <- haven::read_dta("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Oil prices - Hyun-Gyu/futures_price_lower_bound.dta")

# load data
dat201802 <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/Brent_crude_oil_futures_price_implied_volatility_on_020518_barchart.csv")
dat201804 <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/Brent_futures_price_IV_040420.csv")
dat201808 <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/Brent_crude_oil_futures_price_implied_volatility_08052018_CME_barchart.csv")

# create time variable
dat201802$time <- as.Date(paste0(dat201802$Contract, '-01'), format = '%y-%b-%d')
dat201804$time <- as.Date(paste0(dat201804$contract, '-01'), format = '%y-%b-%d')
dat201808$time <- as.Date(paste0(dat201808$Contract, '-01'), format = '%b-%y-%d')

# combine data
dat <- rbind(as.matrix(dat201802), as.matrix(dat201804), as.matrix(dat201808))
dat <- as.data.frame(dat)
rm(dat201802, dat201804, dat201808)

# rename variables
colnames(dat) <- c('contract', 'price', 'imp_vol', 'time')
dat$b_price <- dat$price

# format variables
dat$b_price <- as.numeric(dat$b_price)
dat$imp_vol <- as.numeric(dat$imp_vol)
dat$time <- as.Date(dat$time)
dat <- dat[order(dat$time),]

# remove NA times and old data (that is updated by more recent data downloads)
dat <- dat[!is.na(dat$time),]
dat <- dat[!duplicated(dat$time, fromLast = TRUE),]

# set implied volatility is the same as the last month volatility if the site provides no information
dat$imp_vol <- na.locf(dat$imp_vol)

# generate lower bound of futures price
dat$lb <- with(dat,
               b_price-(b_price*(imp_vol/100))
               )




# ##### add futures price in Aug 2019 for comparison #####
# 
# # load data
# dat2019 <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/Brent_crude_oil_futures_price_implied_volatility_082319_CME_barchart.csv")
# 
# # rename variables
# colnames(dat2019)[colnames(dat2019) == 'last'] <- 'price_prev'
# colnames(dat2019)[colnames(dat2019) == 'IV'] <- 'imp_vol_prev'
# colnames(dat2019)[colnames(dat2019) == 'price'] <- 'b_price_prev'
# colnames(dat2019)[colnames(dat2019) == 'contract'] <- 'contract_prev'
# dat2019$b_price_prev <- dat2019$price_prev
# 
# # format date
# dat2019$time <- as.Date(paste0(dat2019$contract, '-01'), format = '%b-%y-%d')
# 
# # set implied volatility is the same as the last month volatility if the site provides no information
# dat2019$imp_vol_prev <- na.locf(dat2019$imp_vol_prev)
# 
# # generate lower bound of futures price
# dat2019$lb_prev <- with(dat2019,
#                         b_price_prev-(b_price_prev*(imp_vol_prev/100))
# )
# 
# # merge with main data.frame
# dat <- left_join(dat, dat2019, 'time')
# 
# rm(dat2019)




##### add historical brent crude oil prices #####

# load data
dat_brent <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/POILBREUSDM_Aug_2019.csv")

# format variables
dat_brent$time <- as.Date(paste0(dat_brent$DATE, '-01'), format = '%b-%y-%d')
dat_brent$price <- dat_brent$POILBREUSDM
dat_brent$POILBREUSDM <- dat_brent$DATE <- NULL

# merge data
dat$price <- NULL
dat <- left_join(dat, dat_brent, 'time')
rm(dat_brent)

# add month and year variables
dat$date <- dat$time
dat$month <- month(dat$date)
dat$year <- year(dat$date)




##### final formatting #####

# reorder columns to match Hyun-gyu's
dat <- dat[c('contract', 'b_price', 'imp_vol', 'time', 'lb', 'price', 'date', 'month', 'year')]
dat <- dat[order(dat$date),]

# save
saveRDS(dat,
        file = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Oil prices - Hyun-Gyu/futures_price_lower_bound.rds')
