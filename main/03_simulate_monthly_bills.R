
# this script simulates UH bills under Georgia Power MC pricing, using various years as baselines

library(lubridate); library(dplyr); library(stringr); library(ggplot2); library(ggnewscale)
library(readxl)
Sys.setenv(TZ='HST')




##### load and format historical data #####

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

# 15-min UH demand missing at 2019-02-28 00:00:00 --> fill with mean of previous and subsequent 15-min values
dat_UHdemand[dat_UHdemand$date == as.POSIXct('2019-02-28'), '0000'] <- mean(c(11340.00, 11417.04))




##### format hourly data and calculate hourly average load by year #####

# convert UH load matrix into vector
dat_UHdemand_15min <- data.frame(datetime = rep(dat_UHdemand$date, each = 24*4),
                                 load_kW_mean15min = as.vector(t(dat_UHdemand[2:ncol(dat_UHdemand)])))
dat_UHdemand_15min$datetime <- as.POSIXct(paste0(dat_UHdemand_15min$datetime, ' ',
                                          substr(colnames(dat_UHdemand)[2:ncol(dat_UHdemand)], 1, 2), ':',
                                          substr(colnames(dat_UHdemand)[2:ncol(dat_UHdemand)], 3, 4)))
rm(dat_UHdemand)

# calculate hourly consumption from 15-min data
dat_UHdemand_15min$hourID <- rep(1:(nrow(dat_UHdemand_15min)/4), each=4)  # hour group for summing
dat_UHdemand_hourly = with(dat_UHdemand_15min,
                           data.frame(datetime = datetime[!duplicated(hourID)],  # unique hours
                                      kWh      = aggregate(load_kW_mean15min, list(hourID), sum)[,2]/4))  # sum 15-min load data by hour, and divide by 4 to get kWh for each hour

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

# calculate hourly average kWh for each year - used for baselines under GP tariff
dat_UHdemand_hourlyAvg <- with(dat_UHdemand_hourly, aggregate(kWh, list(year, hour), mean))
colnames(dat_UHdemand_hourlyAvg) <- c('year', 'hour', 'kWh')
dat_UHdemand_hourlyAvg <- dat_UHdemand_hourlyAvg[with(dat_UHdemand_hourlyAvg, order(year, hour)),]  # reorder rows accding to date




##### create 15-min baseline loads by year - for billing demand/demand charge #####

# split datetime variable in to year, month, day, hour, min
dat_UHdemand_15min <- dat_UHdemand_15min %>%
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime),
         minute = minute(datetime),
         .after = datetime)
dat_UHdemand_15min$hourID <- NULL  # get rid of previous hour variable

# initialize data.frame to store peak loads for each month
dat_UHdemand_peak15min <- data.frame(date = seq.Date(as.Date('2018-01-01'), as.Date('2020-12-31'), by = 'month'),
                                     peakLoadPrevMonth_kW = NA,
                                     peakLoadPrevYear_kW = NA)

# (A) for each month, get max load over previous month
for(m in 1:nrow(dat_UHdemand_peak15min)){
  dat_UHdemand_peak15min$peakLoadPrevMonth_kW[[m]] <-  max(dat_UHdemand_15min[with(dat_UHdemand_15min, month == month(dat_UHdemand_peak15min$date[[m]] - months(1)) &  # subtract one month from the current month and keep month of that date
                                                                                                       year  == year(dat_UHdemand_peak15min$date[[m]]  - months(1))),  # subtract one month from the current month and keep year of that date
                                                                              'load_kW_mean15min'])
}

# (B) for each month, get max load over previous year
for(m in 1:nrow(dat_UHdemand_peak15min)){
  dat_UHdemand_peak15min$peakLoadPrevYear_kW[[m]]  <-  max(dat_UHdemand_15min[dat_UHdemand_15min$datetime %in% seq.POSIXt(from = as.POSIXct(paste(year(dat_UHdemand_peak15min$date[[m]] - months(12)),  # sequence of days, starting with beginning of month 12 months ago (m-12)
                                                                                                                                                  month(dat_UHdemand_peak15min$date[[m]] - months(12)),
                                                                                                                                                  '1 00:00:00',
                                                                                                                                                  sep = '-'),
                                                                                                                                            tz = 'HST'),
                                                                                                                          to   = as.POSIXct(paste0(year(dat_UHdemand_peak15min$date[[m]] - months(1)), '-',   # sequence of days, ending with end of month 1 month ago (m-1)
                                                                                                                                                   month(dat_UHdemand_peak15min$date[[m]] - months(1)), '-',
                                                                                                                                                   days_in_month(as.Date(paste(year(dat_UHdemand_peak15min$date[[m]] - months(1)),
                                                                                                                                                                               month(dat_UHdemand_peak15min$date[[m]] - months(1)), '1',
                                                                                                                                                                               sep = '-'))),
                                                                                                                                                   ' 23:59:59'),
                                                                                                                                            tz = 'HST'),
                                                                                                                          by = '15 min'), 'load_kW_mean15min'])
}

# (C): find mean of (A) and (B)
dat_UHdemand_peak15min$billing_demand_kW <- rowMeans(dat_UHdemand_peak15min[c("peakLoadPrevMonth_kW", "peakLoadPrevYear_kW")])

# baseline billing demand will be mean of (C) by year
dat_UHdemand_baseline_peakDemand <- with(dat_UHdemand_peak15min ,aggregate(billing_demand_kW, list(year(date)), mean))
colnames(dat_UHdemand_baseline_peakDemand) <- c('year', 'billing_demand_kW')




##### create hourly and monthly baseline loads by year #####

# initialize year's worth of datetime data for each baseline year
dat_UHdemand_baseline_hourly <- data.frame(date = seq.POSIXt(as.POSIXct('2018-01-01 00:00'), as.POSIXct('2020-12-31 23:59'), by = 'hour'))
dat_UHdemand_baseline_hourly$year  <- year(dat_UHdemand_baseline_hourly$date)
dat_UHdemand_baseline_hourly$month <- month(dat_UHdemand_baseline_hourly$date)
dat_UHdemand_baseline_hourly$day   <- day(dat_UHdemand_baseline_hourly$date)
dat_UHdemand_baseline_hourly$hour  <- hour(dat_UHdemand_baseline_hourly$date)

# merge avg hourly data to baseline
dat_UHdemand_baseline_hourly <- left_join(dat_UHdemand_baseline_hourly, dat_UHdemand_hourlyAvg, c('year', 'hour'))

# remove leap days from baseline (???)
dat_UHdemand_baseline_hourly <- dat_UHdemand_baseline_hourly[!(month(dat_UHdemand_baseline_hourly$date) == 2 & day(dat_UHdemand_baseline_hourly$date) == 28),]

# baseline data: aggregate hourly to monthly (note: bill will only be function of number of days in the month and the year's avg daily use. This is averaged out below.)
dat_UHdemand_baseline_monthly <- with(dat_UHdemand_baseline_hourly, aggregate(kWh, list(year, month), sum))
colnames(dat_UHdemand_baseline_monthly) <- c('year', 'month', 'kWh')
dat_UHdemand_baseline_monthly <- dat_UHdemand_baseline_monthly[with(dat_UHdemand_baseline_monthly, order(year, month)),]  # reorder rows accding to date




##### calculate charges for baseline years using DS tariff #####

# calculate mean monthly loads for each baseline year: don't want baseline load to depend on number of days in the month so take mean of the monthly values within the baseline years
dat_UHdemand_baseline_monthly <- with(dat_UHdemand_baseline_monthly, aggregate(kWh, list(year), mean))
colnames(dat_UHdemand_baseline_monthly) <- c('year', 'kWh')

# load function that calculates total baseline bill for each month in the chosen baseline year
source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/03f_baseline_load_DSbill_calculator.R")

# define baselines
baseline_years  <- 2018:2020
baseline_months <- 1:12

# use sourced function to calculate monthly DS bills for baseline loads (fixed charge before load deviation adjustment)
dat_baselineMonthlyBill_dollars <- matrix(data = NA, nrow = length(baseline_years), ncol = length(baseline_months))  # initiate matrix to store values
rownames(dat_baselineMonthlyBill_dollars) <- baseline_years
colnames(dat_baselineMonthlyBill_dollars) <- baseline_months

for(y in 1:length(baseline_years)){
  for(m in 1:length(baseline_months)){
    
    dat_baselineMonthlyBill_dollars[[y,m]] <- ds_bill_calculator(baseline_year  = baseline_years[[y]],
                                                                 baseline_month = baseline_months[[m]])
    
  }
}

# convert matrix of baseline charges to a data.frame
dat_baselineMonthlyBill_dollars <- as.data.frame(t(dat_baselineMonthlyBill_dollars))
colnames(dat_baselineMonthlyBill_dollars) <- paste0('baseline', colnames(dat_baselineMonthlyBill_dollars))

rm(ds_bill_calculator)




##### calculate deviations from baseline and their MC-based charges/credits #####

# load function that calculates hour-by-hour deviations from baseline loads, and charges them according to real-time marginal costs
source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/03f_deviations_from_baseline_charged_at_MC.R")

# reformat hourly mean load data for merging with actual load data
dat_UHdemand_hourlyAvg <- data.frame(hour         = 0:23,
                                     kWh_2017mean = dat_UHdemand_hourlyAvg[dat_UHdemand_hourlyAvg$year == 2017, 'kWh'],
                                     kWh_2018mean = dat_UHdemand_hourlyAvg[dat_UHdemand_hourlyAvg$year == 2018, 'kWh'],
                                     kWh_2019mean = dat_UHdemand_hourlyAvg[dat_UHdemand_hourlyAvg$year == 2019, 'kWh'],
                                     kWh_2020mean = dat_UHdemand_hourlyAvg[dat_UHdemand_hourlyAvg$year == 2020, 'kWh'])

# merge hourly mean load to hourly data
dat_UHdemand_hourly <- left_join(dat_UHdemand_hourly, dat_UHdemand_hourlyAvg, 'hour')
rm(dat_UHdemand_hourlyAvg)

# run deviation function for baseline years 2018, 2019, 2020
dat_UHdemand_hourly <- devFromBaseline(baseline_year = 2018)
dat_UHdemand_hourly <- devFromBaseline(baseline_year = 2019)
dat_UHdemand_hourly <- devFromBaseline(baseline_year = 2020)

rm(devFromBaseline)




##### merge data with marginal cost bill data #####

# load marginal cost pricing data - rename objects for clarity
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/UH/02_marginal_cost_pricing.Rdata")
dat_UHbill_daily_MCpricing <- dat_UHbill_daily; rm(dat_UHbill_daily)
dat_UHbill_monthly_MCpricing <- dat_UHbill_monthly; rm(dat_UHbill_monthly)
dat_UHbill_daily_MCpricing$weekID <- NULL

# aggregate simulated hourly data to daily and format
dat_UHdemand_daily <- aggregate(dat_UHdemand_hourly[,c("kWh_deviationFrom2018",           "kWh_deviationFrom2019",           "kWh_deviationFrom2020",
                                                       "chargeDollars_deviationFrom2018", "chargeDollars_deviationFrom2019", "chargeDollars_deviationFrom2020")],
                                   list(dat_UHdemand_hourly$year, dat_UHdemand_hourly$month, dat_UHdemand_hourly$day), sum)
colnames(dat_UHdemand_daily)[1:3] <- c('year', 'month', 'day')  # rename aggregation groups
dat_UHdemand_daily <- dat_UHdemand_daily[with(dat_UHdemand_daily, order(year, month, day)), ]  # order data by date
dat_UHdemand_daily$date <- as.POSIXct(with(dat_UHdemand_daily, paste(year, month, day, sep= '-')))  # add date variable

# merge data with MC pricing data
dat_UHdemand_daily <- left_join(dat_UHbill_daily_MCpricing, dat_UHdemand_daily, 'date')
dat_UHdemand_daily <- dat_UHdemand_daily[complete.cases(dat_UHdemand_daily),]




##### adjust DS pricing bills based on charge/credit from load deviations and MC #####

# aggregate charges/credits by year-month
dat_UHdemand_monthly <- aggregate(dat_UHdemand_daily[c('kWh_deviationFrom2018',           'kWh_deviationFrom2019',           'kWh_deviationFrom2020',
                                                       'chargeDollars_deviationFrom2018', 'chargeDollars_deviationFrom2019', 'chargeDollars_deviationFrom2020')],
                                  list(dat_UHdemand_daily$year_month), sum)
colnames(dat_UHdemand_monthly)[colnames(dat_UHdemand_monthly) == 'Group.1'] <- c('year_month')

# add GP tariff-based baseline fixed charge (calculated for each year)
dat_UHdemand_monthly <- dat_UHdemand_monthly[year(as.Date(paste0(dat_UHdemand_monthly$year_month, '-01'))) != 2017,]  # remove 2017 data since we don't have a baseline (requires 2016 data)
dat_UHdemand_monthly$chargeDollars_fixedCharge_baseline2018 <- dat_baselineMonthlyBill_dollars$baseline2018
dat_UHdemand_monthly$chargeDollars_fixedCharge_baseline2019 <- dat_baselineMonthlyBill_dollars$baseline2019
dat_UHdemand_monthly$chargeDollars_fixedCharge_baseline2020 <- dat_baselineMonthlyBill_dollars$baseline2020

# adjust baseline charge with monthly MC-based charge/credit
dat_UHdemand_monthly$totalBill_dollars_GPbaseline2018 <- with(dat_UHdemand_monthly, chargeDollars_fixedCharge_baseline2018 + chargeDollars_deviationFrom2018)
dat_UHdemand_monthly$totalBill_dollars_GPbaseline2019 <- with(dat_UHdemand_monthly, chargeDollars_fixedCharge_baseline2019 + chargeDollars_deviationFrom2019)
dat_UHdemand_monthly$totalBill_dollars_GPbaseline2020 <- with(dat_UHdemand_monthly, chargeDollars_fixedCharge_baseline2020 + chargeDollars_deviationFrom2020)

# merge DS pricing bills
dat_UHdemand_monthly <- left_join(dat_UHdemand_monthly, dat_DSpricing[c('year_month', 'totalBill_dollars_constructed')], 'year_month')
colnames(dat_UHdemand_monthly)[colnames(dat_UHdemand_monthly) == 'totalBill_dollars_constructed'] <- 'totalBill_dollars_DSpricing'

# merge monthly MC-based charges (real-time and backward-looking)
dat_UHdemand_monthly <- left_join(dat_UHdemand_monthly, dat_UHbill_monthly_MCpricing[c('year_month', 'dollars_mc', 'dollars_mc_prevWeekLoadWtd')], 'year_month')
colnames(dat_UHdemand_monthly)[colnames(dat_UHdemand_monthly) == 'dollars_mc'] <- 'totalBill_dollars_MargCostRTP'
colnames(dat_UHdemand_monthly)[colnames(dat_UHdemand_monthly) == 'dollars_mc_prevWeekLoadWtd'] <- 'totalBill_dollars_MargCostPrevWeekLoadWtd'




##### plot time series data #####

# remove months with missing baseline bill
dat_UHdemand_monthly <- dat_UHdemand_monthly[!is.na(dat_UHdemand_monthly$totalBill_dollars_DSpricing),]

# add date variable for plotting purposes
dat_UHdemand_monthly$date <- as.Date(paste0(dat_UHdemand_monthly$year_month, '-01'))
dat_UHdemand_monthly <- dat_UHdemand_monthly %>%
  mutate(date = as.Date(paste0(year_month, '-01')),
         .after = year_month)

# melt DS data and create factor levels
plotdat_DS <- data.frame(value = with(dat_UHdemand_monthly, c(totalBill_dollars_DSpricing,
                                                              totalBill_dollars_GPbaseline2018,
                                                              totalBill_dollars_GPbaseline2019,
                                                              totalBill_dollars_GPbaseline2020)),
                         baseYear = c(rep(c('Actual bill', '2018', '2019', '2020'), each = nrow(dat_UHdemand_monthly))),
                         date = rep(dat_UHdemand_monthly$date, times = 4))
plotdat_DS$baseYear <- factor(plotdat_DS$baseYear, levels = c('Actual bill', '2018', '2019', '2020'))

# melt MC data and create factor levels
plotdat_MC <- data.frame(value = with(dat_UHdemand_monthly, c(totalBill_dollars_MargCostRTP, totalBill_dollars_MargCostPrevWeekLoadWtd)),
                         pricingStructure = rep(c('Real-time', 'Prior week\nload-weighted'), each = nrow(dat_UHdemand_monthly)),
                         date = rep(dat_UHdemand_monthly$date, times = 2))
plotdat_MC$pricingStructure <- factor(plotdat_MC$pricingStructure, levels = c('Real-time', 'Prior week\nload-weighted'))

# in MC data, add various fixed charges
plotdat_MC <- rbind(plotdat_MC, plotdat_MC, plotdat_MC)
plotdat_MC$value <- plotdat_MC$value + rep(c(500000, 750000, 1000000), each = nrow(dat_UHdemand_monthly))
plotdat_MC$fixedCharge <- rep(c('$500k', '$750k', '$1000k'), each = nrow(dat_UHdemand_monthly))
plotdat_MC$fixedCharge <- factor(plotdat_MC$fixedCharge, levels = c('$500k', '$750k', '$1000k'))

# plot DS pricing
ggplot(data = plotdat_DS[plotdat_DS$baseYear != '2018',], aes(x = date, y = value/1e6, color = baseYear)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c('black', scales::hue_pal()(3)), name = 'Baseline year') +
  geom_line(data = plotdat_DS[plotdat_DS$baseYear == 'Actual bill',], aes(x = date, y = value/1e6), size = 2, color = 'black') +  # plot actual bills again so it's on top and made with thicker line
  labs(x = NULL, y = 'Monthly bill (million $)') +
  theme(text = element_text(size = 20))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03_monthly DS pricing with charges and credits from deviations from base year.png',
       dpi = 300, height = 6, width = 11)

# plot MC pricing
ggplot() +
  
  # plot MC pricing
  geom_line(data = plotdat_MC, aes(x = date, y = value/1e6, color = fixedCharge, linetype = pricingStructure), size = 1.2) +
  scale_color_discrete(name = 'Fixed charge', guide = guide_legend(order = 3)) +
  scale_linetype_manual(name = 'Pricing structure', values = c('longdash', 'dotted'), guide = guide_legend(order = 2)) +
  
  # new scale
  new_scale('linetype') +
  
  # plot DS pricing for comparison
  geom_line(data = plotdat_DS[plotdat_DS$baseYear == 'Actual bill',], aes(x = date, y = value/1e6, linetype = 'Actual bill (DS)'),
            size = 2, alpha = 0.5) +
  scale_linetype_manual(name = NULL, values = 'solid', guide = guide_legend(order = 1)) +
  
  # plot settings
  labs(x = NULL, y = 'Monthly bill (million $)') +
  theme(text = element_text(size = 20))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03_monthly MC pricing compared to DS.png',
       dpi = 300, height = 6, width = 11)




##### for comparison of loads between years, plot monthly kWh usage #####

# melt data
plotdat_kWh <- dat_DSpricing[c('date', 'billing_demand_kWh')]
plotdat_kWh$month <- month(plotdat_kWh$date)
plotdat_kWh$year  <- year(plotdat_kWh$date)
plotdat_kWh <- reshape2::melt(plotdat_kWh[c('month', 'year', 'billing_demand_kWh')], id.vars = c('month', 'year'))
plotdat_kWh <- plotdat_kWh[!(plotdat_kWh$year %in% c(2017, 2021)),]

# plot
ggplot(data = plotdat_kWh) +
  geom_line(aes(x = month, y = value/1000, color = as.character(year)), size = 2) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  labs(x = 'Month of year', y = 'Monthly MWh', color = 'Year') +
  theme(text = element_text(size = 20))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03_monthly MWh loads.png',
       dpi = 300, height = 6, width = 11)

