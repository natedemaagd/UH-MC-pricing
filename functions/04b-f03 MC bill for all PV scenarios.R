
# this script is used by script 04b to calculate the monthly bill under true MC pricing for all PV scenarios

MC_bill_calculator <- function(fixedCharge_dollars){
  
  require(tidyverse); require(zoo)
  Sys.setenv(tz ='HST')
  
  # merge MC data to 15-min consumption data
  colnames(dat_MC)[colnames(dat_MC) == 'date_time'] <- 'datetime'
  dat_consumptionAndMC <- left_join(dat_consumption15min, dat_MC,  'datetime')
  
  # interpolate MC from hourly to 15-min
  dat_consumptionAndMC <- dat_consumptionAndMC %>%
    mutate(mc_dollarsPerMWh = na.approx(dat_consumptionAndMC$mc_dollarsPerMWh,   na.rm = FALSE),
           mcPrevWkLoadWtd_dollarsPerMWh = na.approx(dat_consumptionAndMC$mcPrevWkLoadWtd_dollarsPerMWh, na.rm = FALSE))
  
  # for each PV scenario, calculate 15-minute charges per kWh using MCs
  for(i in 1:length(scenarios)){
    dat_consumptionAndMC[,paste0('dollarsMC_', scenarios[[i]])] <-
      dat_consumptionAndMC$mc_dollarsPerMWh * dat_consumptionAndMC[,paste0('consumption_kWh_', scenarios[[i]])]  / 1000  # MC is charged by MWh and consumption is in kWh
  }
  for(i in 1:length(scenarios)){
    dat_consumptionAndMC[,paste0('dollarsMCprevWeekLoadWtd_', scenarios[[i]])] <-
      dat_consumptionAndMC$mcPrevWkLoadWtd_dollarsPerMWh * dat_consumptionAndMC[,paste0('consumption_kWh_', scenarios[[i]])] / 1000
  }
  
  # sum MC charges by month and format resulting data.frame
  dat_monthlyMC <- aggregate(dat_consumptionAndMC[,c(paste0('dollarsMC_', scenarios),
                                                     paste0('dollarsMCprevWeekLoadWtd_', scenarios))],
                             list(dat_consumptionAndMC$yearmonth), sum)
  dat_monthlyMC <- dat_monthlyMC %>%
    mutate(date = as.Date(paste0(Group.1, '-1')),
           .before = Group.1)
  dat_monthlyMC$Group.1 <- NULL
  dat_monthlyMC <- dat_monthlyMC[order(dat_monthlyMC$date),]
  
  # add fixed charge
  for(i in 1:length(scenarios)){
    dat_monthlyMC[,paste0('totalBillDollars_MCpricing_', scenarios[[i]])] <-
      dat_monthlyMC[,paste0('dollarsMC_', scenarios[[i]])] + fixedCharge_dollars
  }
  for(i in 1:length(scenarios)){
    dat_monthlyMC[,paste0('totalBillDollars_MCprevWeekLoadWtdPricing_', scenarios[[i]])] <-
      dat_monthlyMC[,paste0('dollarsMCprevWeekLoadWtd_', scenarios[[i]])] + fixedCharge_dollars
  }
  
  # merge monthly MC bill 
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly,
                                      dat_monthlyMC[,c('date', paste0('totalBillDollars_MCpricing_', scenarios),
                                                       paste0('totalBillDollars_MCprevWeekLoadWtdPricing_', scenarios))],
                                      'date')
  
  return(dat_consumptionMonthly)
  
}