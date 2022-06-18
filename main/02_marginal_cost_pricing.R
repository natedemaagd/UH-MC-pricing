
Sys.setenv(TZ='HST')

######################################################################
#### LINE 71 LIMITS DATA - IF WE GET NEW UPDATED DATA REMOVE THIS ####
######################################################################


library(ggplot2); library(tidyverse)

# load data
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/00_smartMeterData.R")
rm(list = ls()[!(ls() == 'mcHeco')])  # keep only lambda/marginal cost
dat_DSpricing <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/UH/01_constructed_bills_under_DS_schedule.rds')
dat_UHdemand <- readxl::read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/UH/UH Demand 2017 - 2021.xlsx")
mcHeco <- mcHeco[order(mcHeco$date_time),]
gc()

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

# create week ID for each time period in overall time sequence
time_seq <- data.frame(time_seq = seq.POSIXt(as.POSIXct(paste0(min(dat_UHdemand$date), ' 00:00:00', tz = 'HST')), as.POSIXct(paste0(max(dat_UHdemand$date), ' 23:45:00', tz = 'HST')), by = '15 min'),
                       weekID   = NA)
for(i in seq(1, nrow(time_seq), 672)){
  time_seq$weekID[[i]] <- (i+671)/672
}
time_seq$weekID <- zoo::na.locf(time_seq$weekID)
  
# for each row (day) of demand, get lambdas - WILL RETURN ERROR FOR 2021-01-01 AND ON - NO MC DATA FROM HECO FOR THIS
dat_UHbill_daily <- data.frame(date                       = dat_UHdemand$date,
                               weekID                     = rep(NA, times = nrow(dat_UHdemand)),
                               demand_kwh                 = rep(NA, times = nrow(dat_UHdemand)),
                               mc_prevWeekLoadWtd         = rep(NA, times = nrow(dat_UHdemand)),
                               dollars_mc                 = rep(NA, times = nrow(dat_UHdemand)),
                               dollars_mc_prevWeekLoadWtd = rep(NA, times = nrow(dat_UHdemand)))
for(i in 1:nrow(dat_UHbill_daily)){
  
  # get demand sequence for day i
  dat_day_i <- dat_UHdemand[i,]
  
  # create time sequence for day i
  timeseq <- data.frame(date_time = as.POSIXct(paste0(dat_day_i$date, ' ',
                                                      substr(colnames(dat_day_i[2:ncol(dat_day_i)]), 1, 2), ':',
                                                      substr(colnames(dat_day_i[2:ncol(dat_day_i)]), 3, 4)),
                                               tz = 'HST'))
  
  # get lambdas for timeseq
  lambdas <- mcHeco[mcHeco$date_time %in% timeseq$date_time,]
  
  # merge lambdas to timeseq (CHECK IF THIS SHOULD BE AT HALF HOUR POINT)
  timeseq <- dplyr::left_join(timeseq, lambdas, 'date_time')
  
  # interpolate 15-min values and fill NAs at the end
  timeseq$mc <- zoo::na.approx(timeseq$mc, na.rm = FALSE)
  last_mc <- timeseq[!is.na(timeseq$mc),][nrow(timeseq[!is.na(timeseq$mc),]), 'mc']
  timeseq$mc[is.na(timeseq$mc)] <- last_mc
  
  # add demand values
  timeseq$demand_kw <- t(dat_day_i[1, 2:ncol(dat_day_i)])[,1]
  
  # calculate kWh consumed and MC charge
  dat_UHbill_daily$demand_kwh[[i]] <- sum(timeseq$demand_kw) / 4
  dat_UHbill_daily$dollars_mc[[i]] <- sum(timeseq$mc * timeseq$demand_kw) / 1000 / 4
  
  # add week ID
  dat_UHbill_daily$weekID[[i]] <-  time_seq[time_seq$time_seq == dat_UHbill_daily$date[[i]], 'weekID']
  
  # find previous week load-wtd mean MC
  if(i >= 8){
    
    # get week ID of previous week and make time sequence for that week
    week_i_minus_1 <- dat_UHbill_daily$weekID[[i]] - 1
    timeseq_week_i_minus1 <- data.frame(date_time = seq.POSIXt(as.POSIXct(paste0(min(dat_UHbill_daily[dat_UHbill_daily$weekID == week_i_minus_1 & !is.na(dat_UHbill_daily$weekID), 'date']), ' 00:00:00')),
                                                               as.POSIXct(paste0(max(dat_UHbill_daily[dat_UHbill_daily$weekID == week_i_minus_1 & !is.na(dat_UHbill_daily$weekID), 'date']), ' 23:45:00')),
                                                               by = '15 min'))
    # get lambdas for the previous-week time sequence
    lambdas <- mcHeco[mcHeco$date_time %in% timeseq_week_i_minus1$date_time,]
    
    # merge lambdas to the time sequence
    timeseq_week_i_minus1 <- dplyr::left_join(timeseq_week_i_minus1, lambdas, 'date_time')
    
    # interpolate 15-min values and fill NAs at the end
    timeseq_week_i_minus1$mc <- zoo::na.approx(timeseq_week_i_minus1$mc, na.rm = FALSE)
    last_mc <- timeseq_week_i_minus1[!is.na(timeseq_week_i_minus1$mc),][nrow(timeseq_week_i_minus1[!is.na(timeseq_week_i_minus1$mc),]), 'mc']
    timeseq_week_i_minus1$mc[is.na(timeseq_week_i_minus1$mc)] <- last_mc
    
    # add demand values
    demand_kw <- dat_UHdemand[as.Date(dat_UHdemand$date) %in% as.Date(timeseq_week_i_minus1$date_time),]
    demand_kw <- demand_kw[1:(nrow(demand_kw)-1), 2:ncol(demand_kw)]
    timeseq_week_i_minus1$demand_kw <- unlist(demand_kw)
    
    # calculate previous week load-weighted mean MC and corresponding daily bill
    mc_wtd_prevWeek <- with(timeseq_week_i_minus1, weighted.mean(mc, demand_kw))
    dat_UHbill_daily$dollars_mc_prevWeekLoadWtd[[i]] <- sum(mc_wtd_prevWeek * timeseq$demand_kw) / 1000 / 4
    dat_UHbill_daily$mc_prevWeekLoadWtd[[i]] <- mc_wtd_prevWeek
  }
}
rm(dat_day_i, demand_kw, lambdas, timeseq, timeseq_week_i_minus1, time_seq, i, last_mc, mc_wtd_prevWeek, week_i_minus_1)

# sum kWh usage and bill by month
dat_UHbill_daily$year_month <- substr(dat_UHbill_daily$date, 1, 7)
dat_UHbill_monthly <- aggregate(dat_UHbill_daily[c('demand_kwh', 'dollars_mc', 'dollars_mc_prevWeekLoadWtd')], list(dat_UHbill_daily$year_month), sum, na.rm = TRUE)
colnames(dat_UHbill_monthly)[[1]] <- 'year_month'

# plot monthly difference between marginal cost and previous-week-load-weighted marginal cost as 
ggplot(data = dat_UHbill_monthly[-c(1, 44:48),]) +
  geom_line(aes(x = as.Date(paste0(year_month, '-15')), y = (dollars_mc_prevWeekLoadWtd - dollars_mc)/(mean(dollars_mc_prevWeekLoadWtd - dollars_mc)))) +
  labs(x = NULL, y = '(Previous week load-weighted MC charge) - (MC charge)\nas proportion of mean difference') +
  annotate(geom = 'text', x = as.Date('2020-06-15'), y = -30, label = paste0('Mean diff. = ', round((mean(dat_UHbill_monthly$dollars_mc_prevWeekLoadWtd - dat_UHbill_monthly$dollars_mc)),2))) +
  theme(text = element_text(size = 16))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/02_monthly_MC_difference_as_proportion_of_mean_difference.png',
       height = 6, width = 8)




##### merge actual bills and determine monthly revenue deficit compared to DS schedule #####

# merge actual bills to MC data
dat_UHbill_monthly <- dplyr::left_join(dat_UHbill_monthly, dat_DSpricing[c('year_month', 'totalBill_dollars_constructed')])

# LIMIT DATA TO BEFORE DEC 2020 - CHANGE IF NEW DATA OBTAINED
dat_UHbill_monthly <- dat_UHbill_monthly[12:41,]

# calculate dollar amount required to generate same revenue as billing under DS schedule (constructed bill)
dat_UHbill_monthly$dollars_revenueDeficit <- dat_UHbill_monthly$totalBill_dollars_constructed - dat_UHbill_monthly$dollars_mc

# plot time series of revenue deficit ()
ggplot(data = dat_UHbill_monthly) +
  geom_line(aes(x = as.Date(paste0(year_month, '-15')),
                y = dollars_revenueDeficit/1e6)) +
  labs(x = NULL, y = 'Monthly revenue deficit (Million $)')




##### by year, calculate fixed monthly charge required to generate same revenue as DS schedule #####

# split data by year
dat_UHbill_monthly_byYear <- split(dat_UHbill_monthly, substr(dat_UHbill_monthly$year_month, 1, 4))

# get total revenue under DS schedule for each year
yearly_revenue_DS <- sapply(dat_UHbill_monthly_byYear, function(df) sum(df$totalBill_dollars_constructed))

# get total revenue from MC pricing for each year
yearly_revenue_MConly <- sapply(dat_UHbill_monthly_byYear, function(df) sum(df$dollars_mc))

# get yearly deficit from MC pricing compared to DS pricing and calculate monthly fixed charge that makes up the difference
yearly_deficit <- yearly_revenue_DS - yearly_revenue_MConly
monthly_fixed_charge_by_year <- c()
for(t in 1:length(dat_UHbill_monthly_byYear)){
  monthly_fixed_charge_by_year[[t]] <- yearly_deficit[[t]] / nrow(dat_UHbill_monthly_byYear[[t]])
}
names(monthly_fixed_charge_by_year) <- names(dat_UHbill_monthly_byYear)
rm(t, yearly_deficit, dat_UHbill_monthly_byYear, yearly_revenue_DS, yearly_revenue_MConly)




##### SD of difference by week and month #####

# get SD of difference by week, add week, then plot
difference_SD_by_week <- aggregate(dat_UHbill_daily$dollars_mc_prevWeekLoadWtd - dat_UHbill_daily$dollars_mc, list(dat_UHbill_daily$weekID), sd)
colnames(difference_SD_by_week) <- c('weekID', 'MC_difference_SD')
weekDates <- dat_UHbill_daily[!duplicated(dat_UHbill_daily$weekID), c('weekID', 'date')]
difference_SD_by_week <- dplyr::left_join(difference_SD_by_week, weekDates, 'weekID')
ggplot(data = difference_SD_by_week) +
  geom_line(aes(x = date, y = MC_difference_SD)) +
  labs(x = NULL, y = 'Weekly SD of difference ($)') +
  annotate(geom = 'text', x = as.POSIXct('2018-01-01'), y = 5000, label = paste0('Mean SD = $', round(mean(difference_SD_by_week$MC_difference_SD, na.rm = TRUE))))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/02_weekly_SD_of_MC_difference.png')
rm(difference_SD_by_week, weekDates)

# get SD of difference by month, add month, then plot
difference_SD_by_month <- aggregate(dat_UHbill_daily$dollars_mc_prevWeekLoadWtd - dat_UHbill_daily$dollars_mc, list(dat_UHbill_daily$year_month), sd)
colnames(difference_SD_by_month) <- c('year_month', 'MC_difference_SD')
monthDates <- dat_UHbill_daily[!duplicated(dat_UHbill_daily$year_month), c('year_month', 'date')]
difference_SD_by_month <- dplyr::left_join(difference_SD_by_month, monthDates, 'year_month')
ggplot(data = difference_SD_by_month) +
  geom_line(aes(x = date, y = MC_difference_SD)) +
  labs(x = NULL, y = 'Monthly SD of difference ($)') +
  annotate(geom = 'text', x = as.POSIXct('2018-01-01'), y = 4000, label = paste0('Mean SD = $', round(mean(difference_SD_by_month$MC_difference_SD, na.rm = TRUE))))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/02_monthly_SD_of_MC_difference.png')
rm(difference_SD_by_month, monthDates)




##### plot - MC sample days #####

# keep only 2020 data
mcHeco$date <- as.Date(mcHeco$date_time)
mcHeco2020 <- mcHeco[mcHeco$date >= as.Date('2020-01-01'),]

# find mean MC by hour
mcHeco2020$hour <- lubridate::hour(mcHeco2020$date_time)
mc_mean_byHour <- aggregate(mcHeco2020$mc, list(mcHeco2020$hour), mean, na.rm = TRUE)
colnames(mc_mean_byHour) <- c('hour', 'mc')

# separate MC data by day
mcHeco2020_byDay <- split(mcHeco2020, mcHeco2020$date)

# for each day, find difference between min and max MC, and SD
mc_difference_byDay <- sapply(mcHeco2020_byDay, function(df) abs(max(df$mc) - min(df$mc)))
mc_SD_byDay <- sapply(mcHeco2020_byDay, function(df) sd(df$mc) )

# sample MC days - get "interesting" days for plotting; found by min/max/SD 
days_to_sample <- c(mcHeco2020[which.max(mcHeco2020$mc), 'date'],
                    names(mcHeco2020_byDay)[[which.min(mc_difference_byDay)]],
                    names(mcHeco2020_byDay)[mc_SD_byDay == sort(mc_SD_byDay,decreasing = TRUE)[[40]]])

# get all hours of MC data for these sample days
dat_MCsample <- rbind(mcHeco2020[as.Date(mcHeco2020$date_time) == days_to_sample[[1]],],
                      mcHeco2020[as.Date(mcHeco2020$date_time) == days_to_sample[[2]],],
                      mcHeco2020[as.Date(mcHeco2020$date_time) == days_to_sample[[3]],])
                      #mcHeco2020[as.Date(mcHeco2020$date_time) == days_to_sample[[4]],])

# combine data, create hour variable, and create plot labels
dat_MCsample <- plyr::rbind.fill(dat_MCsample, mc_mean_byHour)
dat_MCsample$hourOfDay = 0:23
dat_MCsample <- dat_MCsample[order(dat_MCsample$date),]
dat_MCsample$label <- as.character(dat_MCsample$date)
dat_MCsample$label[is.na(dat_MCsample$label)] <- '2020 mean'

# adjust labels so it's clear what they represent (high, low, variable MC)
dat_MCsample$label <- with(dat_MCsample, ifelse(label == '2020-04-01', '2020-04-01 (Day with high MC variablility)',
                                         ifelse(label == '2020-08-20', '2020-08-20 (Day with low MC)',
                                         ifelse(label == '2020-12-30', '2020-12-30 (Day with high MC)',
                                                label))))

# plot sample MC
ggplot(data = dat_MCsample) +
  geom_line(aes(x = hourOfDay, y = mc/1000, color = label), size = 1.4) +
  labs(x = 'Hour of day', y = '$/kWh', color = 'Date') +
  theme(text=element_text(size = 16), legend.position = 'bottom') +
  scale_x_continuous(limits = c(0,24), breaks = seq(0,24,2), labels = seq(0,24,2)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
  #geom_hline(yintercept = mean(mcHeco2020$mc), linetype = 'longdash', size = 1.5)
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/02_hourly mc.png',
       height = 6, width = 8)




##### plot - mean demand-weighted MC against previous week demand-weighted MC #####

# create data.frame
dat_wtdMC <- data.frame(wtdMC_currentWeek = dat_UHbill_monthly$dollars_mc_prevWeekLoadWtd[2:nrow(dat_UHbill_monthly)],
                        wtdMC_prevWeek    = dat_UHbill_monthly$dollars_mc_prevWeekLoadWtd[1:(nrow(dat_UHbill_monthly)-1)])

# plot
ggplot(data = dat_wtdMC, aes(x = wtdMC_prevWeek, y = wtdMC_currentWeek)) +
  geom_point()




##### plot - mean demand-weighted MC against previous week demand-weighted MC #####

# create data.frame of UH loads (vectorized)
dat_UHdemand_vectorized <- data.frame(date_time = seq.POSIXt(min(dat_UHdemand$date), max(dat_UHdemand$date)+lubridate::days(1), by = '15 min'))
dat_UHdemand_vectorized$kW <- c(unlist(dat_UHdemand[2:ncol(dat_UHdemand)]), NA)

# merge MC to load, linearly interpolate NAs (hourly to 15-min)
dat_UHdemand_vectorized <- dplyr::left_join(dat_UHdemand_vectorized, mcHeco, 'date_time')
dat_UHdemand_vectorized$mc <- zoo::na.approx(dat_UHdemand_vectorized$mc, na.rm = FALSE)

# determine week of each date_time
dat_UHdemand_vectorized$year_week <- paste(lubridate::year(dat_UHdemand_vectorized$date_time), lubridate::isoweek(dat_UHdemand_vectorized$date_time), sep = '-')

# load-weighted mean MC by week
dat_UHdemand_vectorized <- dat_UHdemand_vectorized %>%
  group_by(year_week) %>%
  mutate(weekly_loadWeighted_mc = weighted.mean(mc, kW))

# get unique year-weeks to plot, get lagged MC value
dat_plot <- dat_UHdemand_vectorized[!duplicated(dat_UHdemand_vectorized$year_week), c('year_week', 'weekly_loadWeighted_mc')]
dat_plot$prevWeek_loadWeighted_mc <- c(NA, dat_plot$weekly_loadWeighted_mc[-length(dat_plot$weekly_loadWeighted_mc)])


# plot data
ggplot(data = dat_plot, aes(x = prevWeek_loadWeighted_mc/1000, y = weekly_loadWeighted_mc/1000)) +
  geom_point(alpha= 0.6, size = 2.3) +
  labs(x = 'Previous week load-wtd mean MC ($/kWh)', y = 'Current week load-wtd mean MC ($/kWh)') +
  theme(text = element_text(size = 20))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/02_weekly load-wtd MC vs previous week load-wtd MC.png',
       dpi = 300, height = 8, width = 9.1)




##### save data #####

save(dat_UHbill_daily, dat_UHbill_monthly, file = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/UH/02_marginal_cost_pricing.Rdata")
