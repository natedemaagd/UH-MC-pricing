
# this script charges deviations from a baseline consumption value (kWh) according PRIOR-WEEK LOAD-WEIGHTED MARGINAL COST

devFromBaseline <- function(baseline_year){
  
  # add column of deviation from baseline use
  dat_UHdemand_hourly[,paste0('kWh_deviationFrom', baseline_year)] <-
    dat_UHdemand_hourly$kWh - dat_UHdemand_hourly[paste0('kWh_', baseline_year, 'mean')]
  
  # add column that calculates charge or credit based on deviation and present-time MC
  dat_UHdemand_hourly[,paste0('chargeDollars_deviationFrom', baseline_year)] <-
    dat_UHdemand_hourly[paste0('kWh_deviationFrom', baseline_year)]*(dat_UHdemand_hourly$mcPrevWkLoadWtd_dollarsPerMWh/1000)  # change to mc_dollarsPerMWh if real-time pricing desired
  
  return(dat_UHdemand_hourly)
}
