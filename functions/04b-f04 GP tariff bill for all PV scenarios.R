
# this script calculates total monthly bill under a Georgia Power-like tariff for each PV scenario
  # NOTE: bills charged according to prior week load-weighted MC

GP_bill_calculator <- function(){
  
  require(tidyverse)
  
  # load monthly baseline charge data
    # dollar amount charged according to DS tariff using constant baseline load curve calculated using consumption data in indicated year
    # NOTE: all baseline charges use only existing 1 MW PV system
  dat_baselineMonthlyBill_dollars <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/03 GP baseline monthly charges.rds")
  
  # load hourly baseline loads
  dat_baselineHourlyConsumption <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/03 GP baseline hourly loads.rds")
  # dat_baselineHourlyConsumption <- rbind(dat_baselineHourlyConsumption,
  #                                        data.frame(date = as.POSIXct(seq.POSIXt(from = as.POSIXct('2018-02-28 00:00:00'),
  #                                                                                to = as.POSIXct('2018-02-28 23:59:59'),
  #                                                                                by = '1 hour')),
  #                                                   year = 2018, month = 2, day = 28, hour = 0:23,
  #                                                   kWh = dat_baselineHourlyConsumption[as.Date(dat_baselineHourlyConsumption$date) == as.Date('2018-02-27'),
  #                                                                                       'kWh']))  # missing data for 2018-02-28: fill with 02-27 data
  # dat_baselineHourlyConsumption <- dat_baselineHourlyConsumption[with(dat_baselineHourlyConsumption,
  #                                                                     order(date)),]
  
  # add month, day, hour, minute variables to consumption data to match baseline consumption
  dat_consumption15min <- dat_consumption15min %>%
    mutate(year   = year(datetime),
           month  = month(datetime),
           day    = day(datetime),
           hour   = hour(datetime),
           .after = datetime)
  
  # aggregate 15-min data to hourly
  dat_consumptionHourly <- with(dat_consumption15min, aggregate(dat_consumption15min[,c(paste0("consumption_kWh_", scenarios))],
                                                                list(year, month, day, hour),
                                                                sum))
  colnames(dat_consumptionHourly)[1:4] <- c('year', 'month', 'day', 'hour')
  dat_consumptionHourly <- dat_consumptionHourly[with(dat_consumptionHourly, order(year, month, day, hour)),]
  dat_consumptionHourly <- dat_consumptionHourly[!(dat_consumptionHourly$month == 2 & dat_consumptionHourly$day == 29),]
  
  # merge baseline loads to hourly consumption data.frame
  for(i in 2018:2020){
    dat_consumptionHourly <- left_join(dat_consumptionHourly,
                                       dat_baselineHourlyConsumption[dat_baselineHourlyConsumption$year == i,
                                                                     c('month', 'day', 'hour', 'kWh')],
                                       by = c('month', 'day', 'hour'))
    colnames(dat_consumptionHourly)[colnames(dat_consumptionHourly) == 'kWh'] <- paste0('kWh_baseline', i)
  }
  
  # calculate kWh deviations for all PV scenarios and baseline years
  for (s in 1:length(scenarios)){
    for(y in 2018:2020){
      dat_consumptionHourly[,paste0('kWh_deviation_', scenarios[[s]], '_baseline', y)] <-
        dat_consumptionHourly[,paste0('consumption_kWh_', scenarios[[s]])] - 
        dat_consumptionHourly[,paste0('kWh_baseline', y)]
    }
  }
  
  # merge MC to hourly data - use previous week load-weighted MC
  mc_hourly <- with(dat_MC, aggregate(mcPrevWkLoadWtd_dollarsPerMWh,
                                      list(year(datetime), month(datetime),
                                           day(datetime), hour(datetime)),
                                      mean))
  colnames(mc_hourly) <- c('year', 'month', 'day', 'hour', 'mcPrevWkLoadWtd_dollarsPerMWh')
  dat_consumptionHourly <- left_join(dat_consumptionHourly, mc_hourly,
                                     by = c('year', 'month', 'day', 'hour'))
  
  # for each PV scenario and baseline year, charge deviations according to MC
  for(s in 1:length(scenarios)){
    for(y in 2018:2020){
      dat_consumptionHourly[,paste0('deviationChargeDollars_', scenarios[[s]], '_baseline', y)] <-
        dat_consumptionHourly[,paste0('kWh_deviation_', scenarios[[s]], '_baseline', y)] *
        dat_consumptionHourly$mcPrevWkLoadWtd_dollarsPerMWh/1000
    }
  }
  
  # aggregate consumption deviation charges by year-month
  dat_deviationChargesMonthly <- with(dat_consumptionHourly,
                                      aggregate(dat_consumptionHourly[grep('deviationChargeDollars_', colnames(dat_consumptionHourly))],
                                                list(year, month),
                                                sum, na.rm = TRUE))
  colnames(dat_deviationChargesMonthly)[1:2] <- c('year', 'month')
  dat_deviationChargesMonthly <- with(dat_deviationChargesMonthly,
                                      dat_deviationChargesMonthly[order(year, month),])
  
  # use baseline charge and deviation charge to calculate total bill under GP tariff for each baseline year and scenario
  dat_GPbill <- left_join(dat_baselineMonthlyBill_dollars, dat_deviationChargesMonthly,
                          by = c('year', 'month'))
  dat_GPbill[dat_GPbill == 0] <- NA
  for(y in 2019:2020){
    for(s in 1:length(scenarios)){
      dat_GPbill[,paste0('totalBillDollars_GPpricingBaseline', y, '_', scenarios[[s]])] <- 
        dat_GPbill[paste0('baseline', y)] +
        dat_GPbill[paste0('deviationChargeDollars_', scenarios[[s]], '_baseline',  y)]
    }
  }
  dat_GPbill$date <- with(dat_GPbill,
                          as.Date(paste0(year, '-', month, '-01')))
  dat_GPbill <- dat_GPbill[,c(ncol(dat_GPbill), grep('totalBillDollars', colnames(dat_GPbill)))]

  
  # merge GP bills to master monthly data
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_GPbill, 'date')
  
  return(dat_consumptionMonthly)
  
}