
# this script simulates UH bills under Georgia Power MC pricing, using various base years as baselines

library(lubridate); library(dplyr); library(stringr); library(ggplot2)
Sys.setenv(TZ='HST')




##### load data #####

# load data
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

# 15-min UH demand missing at2019-02-28 00:00:00 --> fill with mean of previous and subsequent 15-min values
dat_UHdemand[dat_UHdemand$date == as.POSIXct('2019-02-28'), '0000'] <- mean(c(11340.00, 11417.04))




##### data setup #####

# convert UH load matrix into vector
dat_UHdemand_15min <- data.frame(datetime = rep(dat_UHdemand$date, each = 24*4),
                                 load_kW_mean15min = unlist(dat_UHdemand[2:ncol(dat_UHdemand)]))
dat_UHdemand_15min$datetime <- as.POSIXct(paste0(dat_UHdemand_15min$datetime, ' ',
                                          substr(colnames(dat_UHdemand)[2:ncol(dat_UHdemand)], 1, 2), ':',
                                          substr(colnames(dat_UHdemand)[2:ncol(dat_UHdemand)], 3, 4)))
rm(dat_UHdemand)

# calculate hourly consumption from 15-min data
dat_UHdemand_15min$hourID <- rep(1:(nrow(dat_UHdemand_15min)/4), each=4)  # hour group for summing
dat_UHdemand_hourly = with(dat_UHdemand_15min,
                           data.frame(datetime = datetime[!duplicated(hourID)],  # unique hours
                                      kWh      = aggregate(load_kW_mean15min, list(hourID), sum)[,2]/4))  # sum 15-min load data by hour, and divide by 4 to get kWh for each hour
rm(dat_UHdemand_15min)

# merge marginal costs to demand
colnames(mcHeco) <- c('datetime', 'mc_dollarsPerMWh')
dat_UHdemand_hourly <- left_join(dat_UHdemand_hourly, mcHeco, 'datetime')

# create year, month, day, hour variables
dat_UHdemand_hourly <- cbind(dat_UHdemand_hourly,
                             with(dat_UHdemand_hourly,
                                  data.frame(year = year(datetime),
                                             month = month(datetime),
                                             day = day(datetime),
                                             hour = hour(datetime))))

# rearrange data.frame so it aligns month-day-hour kWh usage across years
dat_UHdemand_hourly_splitByYear <- split(dat_UHdemand_hourly, dat_UHdemand_hourly$year)  # split data by year
for(i in 1:length(dat_UHdemand_hourly_splitByYear)){
  
  # rename kWh and MC variables pasted with year i
  colnames(dat_UHdemand_hourly_splitByYear[[i]])[colnames(dat_UHdemand_hourly_splitByYear[[i]]) == 'kWh'] = paste0('kWh_', names(dat_UHdemand_hourly_splitByYear)[[i]])
  colnames(dat_UHdemand_hourly_splitByYear[[i]])[colnames(dat_UHdemand_hourly_splitByYear[[i]]) == 'mc_dollarsPerMWh'] = paste0('mc_dollarsPerMWh_', names(dat_UHdemand_hourly_splitByYear)[[i]])
  
  # keep only relevant columns, reorder
  dat_UHdemand_hourly_splitByYear[[i]] <- dat_UHdemand_hourly_splitByYear[[i]][,c(5:7,2:3)]
}; rm(i)

dat_UHdemand_hourly <- Reduce(function(x, y) left_join(x, y, by = c('month', 'day', 'hour')), dat_UHdemand_hourly_splitByYear[c(4,1:3,5)])  # combine, using 2020 as base year since it has leap year
dat_UHdemand_hourly <- dat_UHdemand_hourly[c('month', 'day', 'hour',
                                             'kWh_2017', 'kWh_2018', 'kWh_2019', 'kWh_2020', 'kWh_2021',
                                             'mc_dollarsPerMWh_2017', 'mc_dollarsPerMWh_2018', 'mc_dollarsPerMWh_2019', 'mc_dollarsPerMWh_2020', 'mc_dollarsPerMWh_2021')]  # reorder years
rm(dat_UHdemand_hourly_splitByYear, mcHeco)
gc()




##### simulate Georgia Power bills using different baseline years #####

source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/03_deviations_from_baseline_charged_at_MC.R")

# baseline year 2018
dat_UHdemand_hourly <- devFromBaseline(current_year = 2019,  baseline_year = 2018)
dat_UHdemand_hourly <- devFromBaseline(current_year = 2020,  baseline_year = 2018)

# baseline year 2019
dat_UHdemand_hourly <- devFromBaseline(current_year = 2018,  baseline_year = 2019)
dat_UHdemand_hourly <- devFromBaseline(current_year = 2020,  baseline_year = 2019)

# baseline year 2020
dat_UHdemand_hourly <- devFromBaseline(current_year = 2018,  baseline_year = 2020)
dat_UHdemand_hourly <- devFromBaseline(current_year = 2019,  baseline_year = 2020)

rm(devFromBaseline)




##### merge with marginal cost bill data #####

# load marginal cost pricing data - rename objects for clarity
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/UH/02_marginal_cost_pricing.Rdata")
dat_UHbill_daily_MCpricing <- dat_UHbill_daily; rm(dat_UHbill_daily)
dat_UHbill_monthly_MCpricing <- dat_UHbill_monthly; rm(dat_UHbill_monthly)
dat_UHbill_daily_MCpricing$weekID <- NULL

# convert simulated data back into time series
dat_UHdemand_hourly_ts <- with(dat_UHdemand_hourly, list(data.frame(year = 2018,
                                                                    month = month,
                                                                    day = day,
                                                                    hour = hour,
                                                                    mc_dollarsPerMWh = mc_dollarsPerMWh_2018,
                                                                    kWh_deviationFrom2018_sameHour = 0,
                                                                    kWh_deviationFrom2019_sameHour = kWh_2018deviationFrom2019,
                                                                    kWh_deviationFrom2020_sameHour = kWh_2018deviationFrom2020,
                                                                    chargeDollars_deviationFrom2018_sameHour = 0,
                                                                    chargeDollars_deviationFrom2019_sameHour = chargeDollars_2018deviationFrom2019,
                                                                    chargeDollars_deviationFrom2020_sameHour = chargeDollars_2018deviationFrom2020),
                                                         data.frame(year = 2019,
                                                                    month = month,
                                                                    day = day,
                                                                    hour = hour,
                                                                    mc_dollarsPerMWh = mc_dollarsPerMWh_2019,
                                                                    kWh_deviationFrom2018_sameHour = kWh_2019deviationFrom2018,
                                                                    kWh_deviationFrom2019_sameHour = 0,
                                                                    kWh_deviationFrom2020_sameHour = kWh_2019deviationFrom2020,
                                                                    chargeDollars_deviationFrom2018_sameHour = chargeDollars_2019deviationFrom2018,
                                                                    chargeDollars_deviationFrom2019_sameHour = 0,
                                                                    chargeDollars_deviationFrom2020_sameHour = chargeDollars_2019deviationFrom2020),
                                                         data.frame(year = 2020,
                                                                    month = month,
                                                                    day = day,
                                                                    hour = hour,
                                                                    mc_dollarsPerMWh = mc_dollarsPerMWh_2020,
                                                                    kWh_deviationFrom2018_sameHour = kWh_2020deviationFrom2018,
                                                                    kWh_deviationFrom2019_sameHour = kWh_2020deviationFrom2019,
                                                                    kWh_deviationFrom2020_sameHour = 0,
                                                                    chargeDollars_deviationFrom2018_sameHour = chargeDollars_2020deviationFrom2018,
                                                                    chargeDollars_deviationFrom2019_sameHour = chargeDollars_2020deviationFrom2019,
                                                                    chargeDollars_deviationFrom2020_sameHour = 0)))
dat_UHdemand_hourly_ts <- do.call(rbind, dat_UHdemand_hourly_ts)
dat_UHdemand_hourly_ts <- dat_UHdemand_hourly_ts[complete.cases(dat_UHdemand_hourly_ts),]  # remove 2/29 in non-leap years
dat_UHdemand_hourly_ts$datetime <- with(dat_UHdemand_hourly_ts, as.POSIXct(paste0(year, '-',
                                                                                  str_pad(month, 2, pad = '0'), '-',
                                                                                  str_pad(day, 2, pad = '0'), ' ',
                                                                                  str_pad(hour, 2, pad = '0'), ':00:00')))

# aggregate simulated hourly data to daily and format
dat_UHdemand_daily_ts <- aggregate(dat_UHdemand_hourly_ts[,c("kWh_deviationFrom2018_sameHour",           "kWh_deviationFrom2019_sameHour",           "kWh_deviationFrom2020_sameHour",
                                                             "chargeDollars_deviationFrom2018_sameHour", "chargeDollars_deviationFrom2019_sameHour", "chargeDollars_deviationFrom2020_sameHour")],
                                   list(dat_UHdemand_hourly_ts$year, dat_UHdemand_hourly_ts$month, dat_UHdemand_hourly_ts$day), sum)
colnames(dat_UHdemand_daily_ts) <- gsub("_sameHour", "_sameDay", colnames(dat_UHdemand_daily_ts))  # change column names to say day instead of hour now that it's aggregated
colnames(dat_UHdemand_daily_ts)[1:3] <- c('year', 'month', 'day')  # rename aggregation groups
dat_UHdemand_daily_ts <- dat_UHdemand_daily_ts[with(dat_UHdemand_daily_ts, order(year, month, day)), ]  # order data by date
dat_UHdemand_daily_ts$date <- as.POSIXct(with(dat_UHdemand_daily_ts, paste(year, month, day, sep= '-')))  # add date variable

# merge data with MC pricing data
dat_UHdemand_daily_ts <- left_join(dat_UHbill_daily_MCpricing, dat_UHdemand_daily_ts, 'date')
dat_UHdemand_daily_ts <- dat_UHdemand_daily_ts[complete.cases(dat_UHdemand_daily_ts),]




##### adjust DS pricing bills based on charge/credit from load deviations and MC #####

# aggregate charges/credits by year-month
dat_UHdemand_monthly_ts <- aggregate(dat_UHdemand_daily_ts[c('kWh_deviationFrom2018_sameDay', 'kWh_deviationFrom2019_sameDay', 'kWh_deviationFrom2020_sameDay',
                                                             'chargeDollars_deviationFrom2018_sameDay', 'chargeDollars_deviationFrom2019_sameDay', 'chargeDollars_deviationFrom2020_sameDay')],
                                     list(dat_UHdemand_daily_ts$year_month), sum)
colnames(dat_UHdemand_monthly_ts)[colnames(dat_UHdemand_monthly_ts) == 'Group.1'] <- c('year_month')

# merge DS pricing bills
dat_UHdemand_monthly_ts <- left_join(dat_UHdemand_monthly_ts, dat_DSpricing[c('year_month', 'totalBill_dollars_constructed')], 'year_month')
colnames(dat_UHdemand_monthly_ts)[colnames(dat_UHdemand_monthly_ts) == 'totalBill_dollars_constructed'] <- 'totalBill_DSpricing_dollars'

# adjust DS bill based on MC-based charge/credit for all baseline years
dat_UHdemand_monthly_ts$totalBill_DSpricing_dollars_MCdeviation2018 <- with(dat_UHdemand_monthly_ts, totalBill_DSpricing_dollars + chargeDollars_deviationFrom2018_sameDay)
dat_UHdemand_monthly_ts$totalBill_DSpricing_dollars_MCdeviation2019 <- with(dat_UHdemand_monthly_ts, totalBill_DSpricing_dollars + chargeDollars_deviationFrom2019_sameDay)
dat_UHdemand_monthly_ts$totalBill_DSpricing_dollars_MCdeviation2020 <- with(dat_UHdemand_monthly_ts, totalBill_DSpricing_dollars + chargeDollars_deviationFrom2020_sameDay)

# merge monthly MC-based charges (real-time and backward-looking)
dat_UHdemand_monthly_ts <- left_join(dat_UHdemand_monthly_ts, dat_UHbill_monthly_MCpricing[c('year_month', 'dollars_mc', 'dollars_mc_prevWeekLoadWtd')], 'year_month')



##### plot data #####

# remove months with missing baseline bill
dat_UHdemand_monthly_ts <- dat_UHdemand_monthly_ts[!is.na(dat_UHdemand_monthly_ts$totalBill_DSpricing_dollars),]

# add date variable for plotting purposes
dat_UHdemand_monthly_ts$date <- as.Date(paste0(dat_UHdemand_monthly_ts$year_month, '-01'))

# melt data
plotdat <- data.frame(value = with(dat_UHdemand_monthly_ts, c(totalBill_DSpricing_dollars,                  # DS pricing, base
                                                              totalBill_DSpricing_dollars_MCdeviation2018,  # DS pricing, deviations from 2018 charged/credited accd to MC
                                                              totalBill_DSpricing_dollars_MCdeviation2019,  # DS pricing, deviations from 2019 charged/credited accd to MC
                                                              totalBill_DSpricing_dollars_MCdeviation2020,  # DS pricing, deviations from 2020 charged/credited accd to MC
                                                              dollars_mc,                                   # MC pricing (no fixed charge included)
                                                              dollars_mc_prevWeekLoadWtd)                   # MC pricing, previous-week load-weighted marginal cost (no fixed charge included)
                                                              ),
                      pricingStructure = c(rep('DS pricing',                         times = nrow(dat_UHdemand_monthly_ts)),  # DS pricing, base
                                           rep('DS pricing',                         times = nrow(dat_UHdemand_monthly_ts)),  # DS pricing, deviations from 2018 charged/credited accd to MC
                                           rep('DS pricing',                         times = nrow(dat_UHdemand_monthly_ts)),  # DS pricing, deviations from 2019 charged/credited accd to MC
                                           rep('DS pricing',                         times = nrow(dat_UHdemand_monthly_ts)),  # DS pricing, deviations from 2020 charged/credited accd to MC
                                           rep('MC pricing',                         times = nrow(dat_UHdemand_monthly_ts)),  # MC pricing (no fixed charge included)
                                           rep('MC pricing, previous week load-wtd', times = nrow(dat_UHdemand_monthly_ts))   # MC pricing, previous-week load-weighted marginal cost (no fixed charge included)
                                           ),
                      baseYear = c(rep('NA', times = nrow(dat_UHdemand_monthly_ts)),    # DS pricing, base
                                   rep('2018', times = nrow(dat_UHdemand_monthly_ts)),  # DS pricing, deviations from 2018 charged/credited accd to MC
                                   rep('2019', times = nrow(dat_UHdemand_monthly_ts)),  # DS pricing, deviations from 2019 charged/credited accd to MC
                                   rep('2020', times = nrow(dat_UHdemand_monthly_ts)),  # DS pricing, deviations from 2020 charged/credited accd to MC
                                   rep('NA', times = nrow(dat_UHdemand_monthly_ts)),    # MC pricing (no fixed charge included)
                                   rep('NA', times = nrow(dat_UHdemand_monthly_ts))     # MC pricing, previous-week load-weighted marginal cost (no fixed charge included)
                                   ),
                      date = as.Date(paste0(dat_UHdemand_monthly_ts$year_month,  '-01'))
                      )

# if baseline year is same as current year, change value to NA
plotdat[plotdat$baseYear == as.character(year(plotdat$date)), 'value'] <- NA

# plot
ggplot() +
  geom_line(data = plotdat[plotdat$pricingStructure == 'DS pricing'                         & plotdat$baseYear != 'NA',], aes(x = date, y = value/1e6,      color = baseYear), size = 1.6, linetype = 'solid') +     # deviations from baseline year (solid colored lines)
  geom_line(data = plotdat[plotdat$pricingStructure == 'DS pricing'                         & plotdat$baseYear == 'NA',], aes(x = date, y = value/1e6),     color = 'black',   size = 3.0, linetype = 'solid') +     # baseline DS pricing (solid black line)
  geom_line(data = plotdat[plotdat$pricingStructure == 'MC pricing'                         & plotdat$baseYear == 'NA',], aes(x = date, y = (value/1e6)+1), color = 'black',   size = 1.6, linetype = 'longdash') +  # real-time MC pricing w/ $1 million fixed charge
  geom_line(data = plotdat[plotdat$pricingStructure == 'MC pricing, previous week load-wtd' & plotdat$baseYear == 'NA',], aes(x = date, y = (value/1e6)+1), color = 'black',   size = 1.6, linetype = 'dotted') +    # previous week load-wtd MC pricing w/ $1 million fixed charge
  labs(x = NULL, y = 'Monthly bill (Million $)', color = 'Baseline year') +
  geom_vline(xintercept = as.Date(c('2019-01-01', '2020-01-01')), linetype = 'longdash') +
  theme(text = element_text(size = 20), legend.position = c(0.9,0.9))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03_monthly DS pricing with charges and credits from deviations from base year.png',
       dpi = 300, height = 6, width = 10)
