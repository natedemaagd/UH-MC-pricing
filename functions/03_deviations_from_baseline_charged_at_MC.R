
devFromBaseline <- function(current_year, baseline_year){
  
  # add column of deviation from baseline use
  dat_UHdemand_hourly[,paste0('kWh_', current_year, 'deviationFrom', baseline_year)] <-
    dat_UHdemand_hourly[paste0('kWh_', current_year)] - dat_UHdemand_hourly[paste0('kWh_', baseline_year)]
  
  # add column that calculates charge or credit based on deviation and present-time MC
  dat_UHdemand_hourly[,paste0('chargeDollars_', current_year, 'deviationFrom', baseline_year)] <-
    dat_UHdemand_hourly[paste0('kWh_', current_year, 'deviationFrom', baseline_year)]*(dat_UHdemand_hourly[paste0('mc_dollarsPerMWh_', current_year)]/1000)
  
  return(dat_UHdemand_hourly)
}
