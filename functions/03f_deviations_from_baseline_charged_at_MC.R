
devFromBaseline <- function(baseline_year){
  
  # add column of deviation from baseline use
  dat_UHdemand_hourly[,paste0('kWh_deviationFrom', baseline_year)] <-
    dat_UHdemand_hourly$kWh - dat_UHdemand_hourly[paste0('kWh_', baseline_year, 'mean')]
  
  # add column that calculates charge or credit based on deviation and present-time MC
  dat_UHdemand_hourly[,paste0('chargeDollars_deviationFrom', baseline_year)] <-
    dat_UHdemand_hourly[paste0('kWh_deviationFrom', baseline_year)]*(dat_UHdemand_hourly$mc_dollarsPerMWh/1000)
  
  return(dat_UHdemand_hourly)
}
