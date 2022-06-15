
# this script finds the same marginal cost-based bills from script 03, but adds additional PV production 

library(lubridate); library(dplyr); library(stringr); library(ggplot2); library(ggnewscale)
library(readxl); library(zoo)
Sys.setenv(TZ='HST')




##### load and format historical data #####

# load MC and load data
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/00_smartMeterData.R")
rm(list = ls()[!(ls() == 'mcHeco')])  # keep only lambda/marginal cost
dat_DSpricing <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/UH/01_constructed_bills_under_DS_schedule.rds')
dat_UHdemand <- readxl::read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/UH/UH Demand 2017 - 2021.xlsx")
mcHeco <- mcHeco[order(mcHeco$date_time),]

# fix UH demand column names and timezone
time_seq <- as.character(seq(from=as.POSIXct("2012-01-01 00:00", tz="HST"), 
                             to=as.POSIXct("2012-01-01 23:45", tz="HST"), by="15 min"))
time_seq <- substr(time_seq, 12, 16)
dat_colnames <- c('date', time_seq)
dat_colnames <- gsub(':', '', dat_colnames)
colnames(dat_UHdemand) <- dat_colnames
dat_UHdemand$date <- lubridate::force_tz(dat_UHdemand$date, tzone = 'HST')
rm(dat_colnames, time_seq)

# 15-min UH demand missing at 2019-02-28 00:00:00 --> fill with mean of previous and subsequent 15-min values
dat_UHdemand[dat_UHdemand$date == as.POSIXct('2019-02-28'), '0000'] <- mean(c(11340.00, 11417.04))

# load marginal cost pricing data - rename objects for clarity
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/UH/02_marginal_cost_pricing.Rdata")
dat_UHbill_daily_MCpricing <- dat_UHbill_daily; rm(dat_UHbill_daily)
dat_UHbill_monthly_MCpricing <- dat_UHbill_monthly; rm(dat_UHbill_monthly)
dat_UHbill_daily_MCpricing$weekID <- NULL

# merge previous week load-weighted MC to marginal cost data
mcHeco <- mcHeco %>%  # create date variable to merge prior-week MC
  mutate(date = as.Date(date_time, tz = 'HST'),
         .after = date_time)
mcHeco <- left_join(mcHeco, dat_UHbill_daily_MCpricing[c('date', 'mc_prevWeekLoadWtd')], 'date')




##### subtract solar PV production from demand #####

# load PV production data and format datetime column for merging with demand data
dat_PV <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/UH/Solar production/historicalProduction15min.csv")
colnames(dat_PV) <- c('siteTime', 'elkorProduction_kWh', 'sitePerformanceEstimate_kWh')
dat_PV <- dat_PV %>%
  mutate(datetime = as.POSIXct(siteTime, format = "%m-%d-%Y %H:%M:%OS"),
         .after = siteTime)
dat_PV$siteTime <- NULL

# clean PV production data
dat_PV[dat_PV < 0 & !is.na(dat_PV)] <- 0  # production can't be negative
dat_PV$hour <- hour(dat_PV$datetime)  # create hour variable
dat_PV$elkorProduction_kWh[dat_PV$hour %in% c(0:6, 20:23) & !is.na(dat_PV$elkorProduction_kWh)] <- 0  # production can't be positive when sun isn't out
dat_PV$hour <- NULL
dat_PV[is.na(dat_PV$elkorProduction_kWh) & dat_PV$datetime >= as.POSIXct('2019-07-30 17:15:00'), 'elkorProduction_kWh'] <-
  dat_PV[is.na(dat_PV$elkorProduction_kWh) & dat_PV$datetime >= as.POSIXct('2019-07-30 17:15:00'), 'sitePerformanceEstimate_kWh']  # replace NA values with estimated performance data (missing days), but only after data gathering started

# PV data has missing timestamps. Fill them in with 0
dat_PV2 <- data.frame(datetime = seq.POSIXt(min(dat_PV$datetime), max(dat_PV$datetime), by = '15 min'))
dat_PV2$datetime <- as.POSIXct(dat_PV2$datetime,  tz = 'HST')
dat_PV2 <- left_join(dat_PV2, dat_PV, 'datetime')
dat_PV <- dat_PV2; rm(dat_PV2); gc()
dat_PV[is.na(dat_PV$elkorProduction_kWh) & dat_PV$datetime >= as.POSIXct('2019-07-30 17:15:00'), 'elkorProduction_kWh'] <- 0  # fill any remaining NAs with 0 since, if data are entirely missing, we don't even have an estimate at that timestamp


# convert UH load matrix into vector
dat_UHdemand_15min <- data.frame(datetime = rep(dat_UHdemand$date, each = 24*4),
                                 load_kW_mean15min = as.vector(t(dat_UHdemand[2:ncol(dat_UHdemand)])))
dat_UHdemand_15min$datetime <- as.POSIXct(paste0(dat_UHdemand_15min$datetime, ' ',
                                                 substr(colnames(dat_UHdemand)[2:ncol(dat_UHdemand)], 1, 2), ':',
                                                 substr(colnames(dat_UHdemand)[2:ncol(dat_UHdemand)], 3, 4)))
rm(dat_UHdemand)

# merge PV production data to UH demand
dat_UHdemand_15min <- left_join(dat_UHdemand_15min, dat_PV, 'datetime')

# simulate demand with additional PV production, based on merged PV data
dat_UHdemand_15min$consumption_kWh_pv1MW <- dat_UHdemand_15min$load_kW_mean15min/4 # convert average kW load to total kWh consumption in each 15-minute interval - existing 1MW PV system
dat_UHdemand_15min <- dat_UHdemand_15min %>%
  mutate(consumption_kWh_pv2MW = consumption_kWh_pv1MW -  elkorProduction_kWh,     # subtract production to simulate adding additional 1 MW PV
         consumption_kWh_pv5MW = consumption_kWh_pv1MW - (elkorProduction_kWh*4))  # subtract production * 4 to simulate a 5 MW PV system




##### format hourly data and calculate hourly average load by year #####

# calculate hourly consumption from 15-min data
dat_UHdemand_15min$hourID <- rep(1:(nrow(dat_UHdemand_15min)/4), each=4)  # hour group for summing
dat_UHdemand_hourly = with(dat_UHdemand_15min,
                           data.frame(datetime  = datetime[!duplicated(hourID)],  # unique hours
                                      consumption_kWh_pv1MW = aggregate(consumption_kWh_pv1MW, list(hourID), sum)[,2],
                                      consumption_kWh_pv2MW = aggregate(consumption_kWh_pv2MW, list(hourID), sum)[,2],
                                      consumption_kWh_pv5MW = aggregate(consumption_kWh_pv5MW, list(hourID), sum)[,2]))

# merge marginal costs to demand
colnames(mcHeco) <- c('datetime', 'date', 'mc_dollarsPerMWh', 'mcPrevWkLoadWtd_dollarsPerMWh')
dat_UHdemand_hourly <- left_join(dat_UHdemand_hourly, mcHeco[c('datetime', 'mc_dollarsPerMWh', 'mcPrevWkLoadWtd_dollarsPerMWh')], 'datetime')

# create year, month, day, hour variables
dat_UHdemand_hourly <- cbind(dat_UHdemand_hourly,
                             with(dat_UHdemand_hourly,
                                  data.frame(year = year(datetime),
                                             month = month(datetime),
                                             day = day(datetime),
                                             hour = hour(datetime))))

# calculate hourly average kWh for each year and each PV scenario - used later for baselines under GP tariff
dat_UHdemand_hourlyAvg <- with(dat_UHdemand_hourly, aggregate(list(consumption_kWh_pv1MW, consumption_kWh_pv2MW, consumption_kWh_pv5MW), list(year, hour), mean, na.rm  = TRUE))
colnames(dat_UHdemand_hourlyAvg) <- c('year', 'hour', 'consumption_kWh_pv1MW', 'consumption_kWh_pv2MW', 'consumption_kWh_pv5MW')
dat_UHdemand_hourlyAvg <- dat_UHdemand_hourlyAvg[with(dat_UHdemand_hourlyAvg, order(year, hour)),]  # reorder rows accding to date

# plot average hourly consumption by PV scenario
plotdat <- with(dat_UHdemand_hourly, data.frame(value = c(aggregate(consumption_kWh_pv1MW, list(hour), mean, na.rm  = TRUE)[,2],
                                                          aggregate(consumption_kWh_pv2MW, list(hour), mean, na.rm  = TRUE)[,2],
                                                          aggregate(consumption_kWh_pv5MW, list(hour), mean, na.rm  = TRUE)[,2]),
                                                scenario = rep(c('1 MW (current)', '2 MW', '5 MW'), each = 24),
                                                hour = 0:23))

ggplot(data = plotdat, aes(x = hour, y = value/1000, color = scenario)) +
  geom_line(size = 1.3) +
  labs(x = 'Hour of day', y = 'Hourly consumption (MWh)', color = 'PV system') +
  theme(text = element_text(size = 14))
ggsave(filename = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/04_UH average hourly consumption by PV scenario.png",
       dpi = 300, height = 4,  width = 6)




# convert kWh consumption under each PV scenario to mean 15-min kW load
colnames(dat_UHdemand_15min)[colnames(dat_UHdemand_15min) == 'load_kW_mean15min'] <- 'mean15minLoad_kW_pv1MW'
dat_UHdemand_15min <- dat_UHdemand_15min %>%
  mutate(mean15minLoad_kW_pv2MW = consumption_kWh_pv2MW * 4,
         mean15minLoad_kW_pv5MW = consumption_kWh_pv5MW * 4,
         .after = mean15minLoad_kW_pv1MW)

# save data
saveRDS(dat_UHdemand_15min[c('datetime',
                             'mean15minLoad_kW_pv1MW', 'mean15minLoad_kW_pv2MW', 'mean15minLoad_kW_pv5MW',
                             'consumption_kWh_pv1MW',  'consumption_kWh_pv2MW',  'consumption_kWh_pv5MW')],
        file = c('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/04 demand under simulated solar production.rds'))
