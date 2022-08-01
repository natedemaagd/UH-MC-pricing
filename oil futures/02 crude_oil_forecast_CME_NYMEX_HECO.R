
# ******************************************************
# * Future Crude Oil prices
# * Combine crude oil prices from EIA, barchart
# * Extend the price 2025 to 2045 
# * Assume that prices are flat in real term after 2029  
# *****************************************************

library(tidyverse); library(lubridate); library(zoo)

# load historical oil prices and computed lower bound
dat <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Oil prices - Hyun-Gyu/futures_price_lower_bound.rds")

# # add brent crude oil price - ALREADY DONE IN 01
# datBrent <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/POILBREUSDM_Aug_2019.csv")
# datBrent$DATE2 <- paste0('01-', datBrent$DATE)
# datBrent$DATE2 <- as.Date(datBrent$DATE2, format = '%d-%b-%y')
# datBrent <- data.frame(date = datBrent$DATE2,
#                        price = datBrent$POILBREUSDM)
# dat <- left_join(dat, datBrent, 'date')
# rm(datBrent)

# extend data to Dec 2045
min_date <- min(dat$date)
newDatEndDate <- min_date %m-% months(1)
max_date <- max(dat$date)
newDatStartDate <- max_date %m+% months(1)
dat2 <- data.frame(date = c(seq.Date(as.Date('2015-01-01'),
                                     newDatEndDate,
                                     'month'),
                            seq.Date(newDatStartDate,
                                   as.Date('2045-12-01'),
                                   'month')),
                   contract = NA,
                   b_price = NA,
                   imp_vol = NA,
                   lb = NA,
                   price = NA)
dat2$year <- year(dat2$date)
dat2$month <- month(dat2$date)
dat2$time <- dat2$date
dat <- rbind(dat, dat2)
dat <- dat[order(dat$date),]
rm(dat2, min_date, newDatEndDate, max_date, newDatStartDate)

# add EIA forecast (nominal prices)
dat$EIA_forecast <- NA
dat$EIA_forecast[dat$year == 2015] <-  52
dat$EIA_forecast[dat$year == 2016] <-  43
dat$EIA_forecast[dat$year == 2025] <- 104
dat$EIA_forecast[dat$year == 2030] <- 128
dat$EIA_forecast[dat$year == 2035] <- 152
dat$EIA_forecast[dat$year == 2040] <- 179
dat$EIA_forecast[dat$year == 2045] <- 210

# linear interpolation of EIA's crude oil price
dat$EIA_forecast <- na.approx(dat$EIA_forecast)

# extend the futures price and lower bound to 2045, 2% annual inflation
rownames(dat) <- 1:nrow(dat)
last_row_with_price <- 172
for(t in last_row_with_price:nrow(dat)){
  dat$b_price[[t]] <-
    dat$b_price[[last_row_with_price-1]] * (1.02 ^ (dat$year[[t]] - dat$year[[last_row_with_price]]-1))
  dat$lb[[t]] <-
    dat$lb[[last_row_with_price-1]] * (1.02 ^ (dat$year[[t]] - dat$year[[last_row_with_price]]-1))
}
rm(t)

# obtain crude oil prices for analysis
dat2 <- dat
colnames(dat2)[colnames(dat2) == 'b_price'] <- 'futures'
dat2 <- dat2[c('time', 'date', 'year', 'month', 'EIA_forecast', 'futures', 'lb', 'price')]
saveRDS(dat2,
        file = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Oil prices - Hyun-Gyu/02 oil_price_2020_2045.rds')
rm(dat2)




##### convert oil price from nominal to 2019 dollars #####

# add inflation from the excel that Matthias shared (May 11, 2020)
dat_inflation <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/Oil futures retrieved 2022-07-01/inflation_from_Matthias.csv")
colnames(dat_inflation) <- c('year', 'inflation')

# merge data
dat <- left_join(dat, dat_inflation, 'year')
rm(dat_inflation)

# discount oil price projections to 2019 dollars
dat$EIA_forecast_2019 <- with(dat, EIA_forecast / (inflation ^ (year - 2019)))
dat$b_price_2019 <- with(dat, b_price / (inflation ^ (year - 2019)))
dat$lb_2019 <- with(dat, lb / (inflation ^ (year - 2019)))

# format data for saving
dat2 <- dat[dat$year %in% 2020:2045,]
colnames(dat2)[colnames(dat2) == 'EIA_forecast_2019'] <- 'high'
colnames(dat2)[colnames(dat2) == 'b_price_2019'] <- 'mid'
colnames(dat2)[colnames(dat2) == 'lb_2019'] <- 'low'
dat2 <- dat2[c('date', 'year', 'month', 'high', 'mid', 'low')]

# save
saveRDS(dat2, file = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Oil prices - Hyun-Gyu/02 crude_oil_price_projections_2020_2045_2019dollar.rds')
