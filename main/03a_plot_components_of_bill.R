
# title of section in this script corresponds to title in 03_simulate_monthly_bills.R file
# plots use final dataset created in the indicated section




##### find peak loads over previous month and previous 12 months for billing demand/demand charge #####

# get rid of data we won't use as baseline years
plotdat <- dat_UHdemand_peak15min[!(dat_UHdemand_peak15min$year %in% c(2017, 2018, 2021)),]

# melt data
plotdat <- with(plotdat, data.frame(date = seq.Date(as.Date('2019-01-01'), as.Date('2020-12-31'), 'month'),
                                    value = c(peakLoadPrevMonth_kW, peakLoadPrev12Months_kW, billing_demand_kW),
                                    label = rep(c('Peak load, previous month', 'Peak load, previous year', 'Billing demand'), each = nrow(plotdat))))

# plot
ggplot(data = plotdat) +
  geom_line(aes(x = date, y = value, color = label), size = 1.3) +
  geom_point(aes(x = date, y = value, color = label), size = 3) +
  labs(x = NULL, y = 'Peak load (kW)', color = NULL, caption = 'Billing demand is mean of previous month peak load and previous year peak load') +
  theme(text = element_text(size = 20), legend.position = c(0.2, 0.2))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03a_peak load previous month and previous year.png',
       dpi = 300, height = 6, width = 11)

rm(plotdat)




##### format hourly data and calculate hourly average load by year #####

# plot
ggplot(data = dat_UHdemand_hourlyAvg[dat_UHdemand_hourlyAvg$year %in% 2019:2020,]) +
  geom_line(aes(x = hour, y = kWh, color = as.character(year)), size = 1.3) +
  labs(x = 'Hour of day', y = 'Mean consumption (kWh)', color = 'Year') +
  theme(text = element_text(size = 20))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03a_mean consumption by hour of day.png',
       dpi = 300, height = 6, width = 11)




##### calculate baseline monthly charges using DS tariff #####

# create data
plotdat <- data.frame(date = rep(seq.Date(as.Date('2018-01-01'), as.Date('2020-12-31'), 'month'), times = 2),
                      baselineCharge_dollars = with(dat_baselineMonthlyBill_dollars, c(baseline2019, baseline2020)),
                      baselineYear = rep(c(2019,2020), each = length(dat_baselineMonthlyBill_dollars$baseline2019)))

# plot
ggplot(data = plotdat) +
  geom_line(aes(x = date, y = baselineCharge_dollars/1e6, color = as.character(baselineYear)), size = 1.3) +
  labs(x = 'Month', y = 'Baseline charge (Million $)', color = 'Baseline year') +
  theme(text = element_text(size = 20))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03a_baseline monthly charges by baseline year.png',
       dpi = 300, height = 6, width = 11)

# plot raw energy prices for comparison
dat_ecrc <- dat_ecrc <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/all_schedules/ecrf.xlsx")  # Energy Costs: accounts for energy cost adjustment (prior to 2019) and energy cost recovery (2019 and on)
ggplot(data = dat_ecrc[dat_ecrc$year %in% 2018:2020,]) +
  geom_line(aes(x = as.Date(paste(year, month, '1', sep = '-')), y = final_cents_per_kwh), size = 1.3) +
  labs(x = NULL, y = 'ECRC (Cents per kWh)', color = 'Baseline year') +
  theme(text = element_text(size = 20))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03a_ECRC historical prices.png',
       dpi = 300, height = 6, width = 11)




# ##### adjust DS pricing bills based on charge/credit from load deviations and MC #####
# 
# # melt kW deviation data
# plotdat <- data.frame(date = rep(dat_UHdemand_monthly[year(dat_UHdemand_monthly$date) %in% c(2019,2020),'date'], times = 2),
#                       value = with(dat_UHdemand_monthly[year(dat_UHdemand_monthly$date) %in% c(2019,2020),], c(kWh_deviationFrom2019, kWh_deviationFrom2020)),
#                       name = rep(c('Deviation from\n2019 baseline', 'Deviation from\n2020 baseline'), each = nrow(dat_UHdemand_monthly[year(dat_UHdemand_monthly$date) %in% c(2019,2020),])))
# 
# # plot
# ggplot(data = plotdat) +
#   geom_line(aes(x = date, y = value, color = name), size = 1.3) +
#   labs(x = NULL, y = 'Monthly aggregated kWh', color = NULL) +
#   theme(text = element_text(size = 20))
# ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/03a_monthly aggregated kWh deviation from baselines.png',
#        dpi = 300, height = 6, width = 11)





