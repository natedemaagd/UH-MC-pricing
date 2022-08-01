
# This script calculates DS bills under each PV system scenario (existing 1 MW, along with 2 MW and 5 MW systems), for use with script 04b.
# Script is generalized in the event additional PV scenarios are added in other scripts in the future.

library(lubridate)
library(tidyverse)




##### determine monthly billing demand (kW) #####

# provide a column x from `dat_consumption15min` data.frame and output monthly kW billing demand for demand charge
billing_demand_kW <- function(){
  
  # get max load for the month, format resulting data
  kW_maxLoad_lastMonth <- with(dat_consumption15min,
                               aggregate(list(mean15minLoad_kW_pv1MW, mean15minLoad_kW_pv2MW, mean15minLoad_kW_pv5MW, mean15minLoad_kW_pv17MW),
                                         list(yearmonth), max, na.rm =  TRUE))  # find max load over each month
  colnames(kW_maxLoad_lastMonth) <- c('yearmonth', 'max15minLoad_prev1mo_pv1MW', 'max15minLoad_prev1mo_pv2MW', 'max15minLoad_prev1mo_pv5MW', 'max15minLoad_prev1mo_pv17MW')  # rename columns in new data.frame
  kW_maxLoad_lastMonth <- kW_maxLoad_lastMonth[with(kW_maxLoad_lastMonth,
                                                    order(as.Date(paste0(yearmonth, '-1')))),]  # order rows in data by date
  kW_maxLoad_lastMonth <- kW_maxLoad_lastMonth %>% 
    mutate_if(is.numeric, ~ replace_na(., 0) %>% 
                replace(., is.infinite(.), NA))  # replace Inf with NA (missing load data)
  
  # for each month, get max load from previous 11 months, then format into data.frame
  kW_maxLoad_last11mo <- list()
  for(i in 2:ncol(kW_maxLoad_lastMonth)){
    kW_maxLoad_last11mo[[i-1]] <- list()
    for(m in 12:nrow(kW_maxLoad_lastMonth)){
      kW_maxLoad_last11mo[[i-1]][[m-11]] <- max(kW_maxLoad_lastMonth[(m-11):m, i], na.rm = TRUE)
    }
    names(kW_maxLoad_last11mo[[i-1]]) <- kW_maxLoad_lastMonth[12:nrow(kW_maxLoad_lastMonth), 'yearmonth']
  }
  kW_maxLoad_last11mo <- lapply(kW_maxLoad_last11mo, unlist)
  kW_maxLoad_last11mo <- as.data.frame(cbind(kW_maxLoad_lastMonth[12:nrow(kW_maxLoad_lastMonth), 'yearmonth'],
                                             do.call('cbind', kW_maxLoad_last11mo)))
  colnames(kW_maxLoad_last11mo) <- c('yearmonth', 'max15minLoad_prev11mo_pv1MW', 'max15minLoad_prev11mo_pv2MW', 'max15minLoad_prev11mo_pv5MW', 'max15minLoad_prev11mo_pv17MW')
  for(i in 2:ncol(kW_maxLoad_last11mo)){ kW_maxLoad_last11mo[,i] <- as.numeric(kW_maxLoad_last11mo[,i]) }
  kW_maxLoad_last11mo <- kW_maxLoad_last11mo %>% 
    mutate_if(is.numeric, ~ replace_na(., 0) %>% 
                replace(., is.infinite(.), NA))  # replace Inf with NA (missing load data)
  
  # merge max loads from last month and last 11 months
  kW_maxLoad <- left_join(kW_maxLoad_last11mo, kW_maxLoad_lastMonth, 'yearmonth')
  rm(kW_maxLoad_last11mo, kW_maxLoad_lastMonth)
  
  # get unique pv scenarios for calculating demand charge by pv scenario
  pv_scenarios <- strsplit(colnames(kW_maxLoad)[2:ncol(kW_maxLoad)], split = '_')
  pv_scenarios <- unique(sapply(pv_scenarios, function(v) v[[3]]))
  
  # for each scenario, find max{prior month load, mean(prior month load, prior 11 month load)}
  kW_maxLoad_byScenario <- list()  # split data by scenario
  for(i in 1:length(pv_scenarios)){
    kW_maxLoad_byScenario[[i]] <- kW_maxLoad[,c(1, grep(pv_scenarios[[i]], colnames(kW_maxLoad)))]
  }
  for(i in 1:length(kW_maxLoad_byScenario)){
    kW_maxLoad_byScenario[[i]]$billingDemand_kW <- c()
    for(m in 1:nrow(kW_maxLoad_byScenario[[i]])){
      kW_maxLoad_byScenario[[i]]$billingDemand_kW[[m]] <- max(c(kW_maxLoad_byScenario[[i]][m,3],
                                                                mean(c(kW_maxLoad_byScenario[[i]][m,3], kW_maxLoad_byScenario[[i]][m,2]))))
    }
  }
  for(i in 1:length(kW_maxLoad_byScenario)){
    kW_maxLoad_byScenario[[i]]$billingDemand_kW <- unlist(kW_maxLoad_byScenario[[i]]$billingDemand_kW)
  }
  names(kW_maxLoad_byScenario) <- pv_scenarios
  
  # merge billing demand to monthly consumption data
  dat_billingDemand <- data.frame(yearmonth = kW_maxLoad_byScenario[[1]]$yearmonth)
  for(i in 1:length(pv_scenarios)){
    dat_billingDemand[,i+1] <- kW_maxLoad_byScenario[[i]]$billingDemand_kW
  }
  colnames(dat_billingDemand)[2:ncol(dat_billingDemand)] <- paste0('billingDemand_kW_', pv_scenarios)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_billingDemand, 'yearmonth')
  
  return(dat_consumptionMonthly)
  
}