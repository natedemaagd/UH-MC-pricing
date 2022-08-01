
# this script uses the billing demand values calculated in the previous function (04b-f01) to calculate a bill under the current DS
# pricing schedule, for all PV scenarios

DS_bill_calculator <- function(){
  
  require(readxl); require(tidyverse)
  source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/dataframe_date_format.R")
  
  # load historical charges data
  dat_cc <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_ds/customer_charge.xlsx")  # fixed customer charge
  dat_dc <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_ds/demand_charge.xlsx")  # demand charge
  dat_ecrc <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/all_schedules/ecrf.xlsx")  # Energy Costs: accounts for energy cost adjustment (prior to 2019) and energy cost recovery (2019 and on)
  dat_ppac <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_ds/purchase_power_adjustment.xlsx")  # Purchased Power Adjustment Clause
  dat_rbap <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/all_schedules/rba_rate_adjustment.xlsx")  # Revenue Balancing Account Provision
  #dat_irpcrp <- read_xlsx()  # Integrated Resource Planning Cost Recovery Provision
  #dat_pwrFactor <- read_xlsx()  # Power Factor (reactive power)
  dat_pbfs <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_ds/pbf_surcharge.xlsx")  # Public Benefits Fund Surcharge
  dat_reicrp <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_r/reicrp.xlsx")  # Renewable Energy Infrastructure Cost Recovery Provision ###### CAN'T FIND DS VALUES
  dat_gif <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_ds/green_infrastructure_fee.xlsx")  # Green Infrastructure Fee Surcharge
  
  # reformat monthly consumption date variable for merging with charges data
  dat_consumptionMonthly$yearmonth <- with(dat_consumptionMonthly,
                                            paste0(year(date), '-',
                                                   str_pad(month(date), 2, 'left', '0')))
  colnames(dat_consumptionMonthly)[colnames(dat_consumptionMonthly) == 'yearmonth'] <- 'year_month'
  
  # get all PV scenarios
  scenarios <- colnames(dat_consumptionMonthly)[grep('consumption_kWh_', colnames(dat_consumptionMonthly))]
  scenarios <- sapply(strsplit(scenarios, '_'), function(x) x[[3]])
  
  # calculate charges
  dat_cc <- date_format(dat_cc)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_cc, 'year_month')
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('customerChargeDollars_', scenarios[[i]])] <- dat_consumptionMonthly$dollars_per_month
  }
  dat_consumptionMonthly$dollars_per_month <- NULL
  
  dat_dc <- date_format(dat_dc)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_dc, 'year_month')
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('demandChargeDollars_', scenarios[[i]])] <- 
      dat_consumptionMonthly[,paste0('billingDemand_kW_', scenarios[[i]])] * dat_consumptionMonthly$dollars_per_kW
  }
  dat_consumptionMonthly$dollars_per_kW <- NULL
  
  dat_ecrc$month <- str_pad(dat_ecrc$month, width = 2, side = 'left', pad = 0)
  dat_ecrc$year_month <- paste(dat_ecrc$year, dat_ecrc$month, sep = '-')
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_ecrc, 'year_month')
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('EcrcDollars_', scenarios[[i]])] <- 
      dat_consumptionMonthly[,paste0('consumption_kWh_', scenarios[[i]])] * dat_consumptionMonthly$final_cents_per_kwh / 100
  }
  dat_consumptionMonthly[c('county', 'year', 'month', 'energy_charge_cents_per_kwh', 'energy_cost_adjustment_factor_cents_per_kwh', 'energy_cost_recovery_factor_cents_per_kwh', 'final_cents_per_kwh')] <- NULL
  
  dat_ppac <- date_format(dat_ppac)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_ppac, 'year_month')
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('PpacDollars_', scenarios[[i]])] <- 
      dat_consumptionMonthly[,paste0('consumption_kWh_', scenarios[[i]])] * dat_consumptionMonthly$cents_per_kwh / 100
  }
  dat_consumptionMonthly$cents_per_kwh <- NULL
  
  dat_rbap <- date_format(dat_rbap)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_rbap, 'year_month')
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('RbapDollars_', scenarios[[i]])] <- 
      dat_consumptionMonthly[,paste0('consumption_kWh_', scenarios[[i]])] * dat_consumptionMonthly$cents_per_kwh / 100
  }
  dat_consumptionMonthly$cents_per_kwh <- NULL
  
  # irpcrp
  
  dat_pbfs <- date_format(dat_pbfs)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_pbfs, 'year_month')
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('PbfsDollars_', scenarios[[i]])] <- 
      dat_consumptionMonthly[,paste0('consumption_kWh_', scenarios[[i]])] * dat_consumptionMonthly$cents_per_kwh / 100
  }
  dat_consumptionMonthly$cents_per_kwh <- NULL
  
  dat_reicrp <- date_format(dat_reicrp)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_reicrp, 'year_month')
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('ReicrpDollars_', scenarios[[i]])] <- 
      dat_consumptionMonthly[,paste0('consumption_kWh_', scenarios[[i]])] * dat_consumptionMonthly$cents_per_kwh / 100
  }
  dat_consumptionMonthly$cents_per_kwh <- NULL
  
  dat_gif <- date_format(dat_gif)
  dat_consumptionMonthly <- left_join(dat_consumptionMonthly, dat_gif, 'year_month')
  dat_consumptionMonthly$gifDollars <- 
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('GifDollars_', scenarios[[i]])] <- dat_consumptionMonthly$dollars_per_month
  }
  dat_consumptionMonthly$dollars_per_month <- NULL
  
  # calculate total bill under each PV scenario
  for(i in 1:length(scenarios)){
    dat_consumptionMonthly[,paste0('totalBillDollars_DSpricing_', scenarios[[i]])] <-
      rowSums(dat_consumptionMonthly[,grep(paste0('Dollars_', scenarios[[i]]), colnames(dat_consumptionMonthly))])
  }
  
  # clean up data.frame
  dat_consumptionMonthly[,grep('customerCharge', colnames(dat_consumptionMonthly))] <- NULL
  dat_consumptionMonthly[,grep('Ppac', colnames(dat_consumptionMonthly))] <- NULL
  dat_consumptionMonthly[,grep('Rbap', colnames(dat_consumptionMonthly))] <- NULL
  dat_consumptionMonthly[,grep('Pbfs', colnames(dat_consumptionMonthly))] <- NULL
  dat_consumptionMonthly[,grep('Reicrp', colnames(dat_consumptionMonthly))] <- NULL
  dat_consumptionMonthly[,grep('Gif', colnames(dat_consumptionMonthly))] <- NULL
  
  
  return(dat_consumptionMonthly)
  
}

