
# this script calculates bills under existing DS pricing, true MC pricing, and Georgia Power-based pricing using consumption under simulated additional solar projects
  # NOTE: 1 MW PV project is existing capacity and used as comparison

library(lubridate); library(zoo); library(tidyverse); library(readxl);  library(ggplot2)

# load 15-min MC and consumption data
dat_MC <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/03 marginal cost data.rds')
dat_consumption15min <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/04 demand under simulated solar production.rds')

# create monthly data.frame
dat_consumption15min <- dat_consumption15min %>%
  mutate(yearmonth = paste(year(dat_consumption15min$datetime),
                           month(dat_consumption15min$datetime),
                           sep = '-'),
         .after = datetime) # create year-month variable for grouping

dat_consumptionMonthly <- with(dat_consumption15min,
                               aggregate(list(consumption_kWh_pv1MW, consumption_kWh_pv2MW,
                                              consumption_kWh_pv5MW, consumption_kWh_pv17MW),
                                         list(yearmonth), sum))
colnames(dat_consumptionMonthly) <- c('yearmonth', 'consumption_kWh_pv1MW', 'consumption_kWh_pv2MW',
                                      'consumption_kWh_pv5MW', 'consumption_kWh_pv17MW')
dat_consumptionMonthly <- dat_consumptionMonthly %>%
  mutate(date = as.Date(paste0(yearmonth, '-01')),
         .before = yearmonth)  # date column for ordering rows
dat_consumptionMonthly <- dat_consumptionMonthly[order(dat_consumptionMonthly$date),]

# define PV scenarios
scenarios <- c('pv1MW', 'pv2MW', 'pv5MW', 'pv17MW')




##### calculate bills under DS pricing #####

# add billing demand kW to each month for each PV scenario
source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/04b-f01 billing demand for all PV scenarios.R")
dat_consumptionMonthly <- billing_demand_kW()  # warnings returned for missing values - okay since we don't have PV data for entire billing history
rm(billing_demand_kW)

# calculate DS bill under each PV scenario
source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/04b-f02 DS bill for all PV scenarios.R")
dat_consumptionMonthly <- DS_bill_calculator()
rm(DS_bill_calculator, date_format)
gc()




##### calculate bills under true MC pricing #####

# add total MC bill to data using chosen fixed charge
source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/04b-f03 MC bill for all PV scenarios.R")
dat_consumptionMonthly <- MC_bill_calculator(fixedCharge_dollars = 965000)
rm(MC_bill_calculator)




##### calculate bills under Georgia Power tariff #####

# calculate bills according to GP tariff (deviations from baseline load charged at PRIOR WEEK LOAD-WEIGHTED MARGINAL COST)
source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/04b-f04 GP tariff bill for all PV scenarios.R")
dat_consumptionMonthly <- GP_bill_calculator()
rm(GP_bill_calculator)

# save data
saveRDS(dat_consumptionMonthly,
        file = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Intermediate/04b monthly bills under various PV scenarios - no battery.rds')




##### plot bills - DS and MC #####

# melt data
plotdat <- dat_consumptionMonthly[!is.na(dat_consumptionMonthly$totalBillDollars_DSpricing_pv1MW),]  # keep only range with original DS bills
plotdat <- with(plotdat, data.frame(value = c(totalBillDollars_DSpricing_pv1MW, totalBillDollars_DSpricing_pv2MW, totalBillDollars_DSpricing_pv5MW, totalBillDollars_DSpricing_pv17MW,
                                              totalBillDollars_MCpricing_pv1MW, totalBillDollars_MCpricing_pv2MW, totalBillDollars_MCpricing_pv5MW, totalBillDollars_MCpricing_pv17MW)/1e6,
                                    scenario = rep(c('1 MW', '2 MW', '5 MW', '17 MW'), each = nrow(plotdat)),
                                    pricingStructure = rep(c('DS', 'Prior-week MC'), each = nrow(plotdat)*length(scenarios)),
                                    date = plotdat$date))
plotdat$scenario <- factor(plotdat$scenario, levels = c('1 MW', '2 MW', '5 MW', '17 MW'))
plotdat <- plotdat[!(plotdat$pricingStructure == 'Prior-week MC' & plotdat$date == max(plotdat$date)),]

# plot
ggplot() +
  geom_line(data = plotdat,
            aes(x = date, y = value, color = pricingStructure, linetype = scenario),
            size = 1.3, alpha = 0.7) +
  labs(color = 'Pricing\nstructure',  linetype = 'PV capacity',
       x = NULL, y = 'Monthly bill (million $)') +
  scale_linetype_manual(values = c('solid', 'longdash', 'dashed', 'dotted')) +
  geom_line(data = plotdat[plotdat$scenario == '1 MW' & plotdat$pricingStructure == 'DS',],
            aes(x = date, y = value), color = scales::hue_pal()(3)[[1]], alpha = 0.7, size = 2) +
  scale_x_date(limits = c(min(plotdat$date), '2021-01-01')) +
  theme(text = element_text(size = 15), legend.key.size=unit(2,"lines"))

ggsave("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/04b-fig01 DS and prior-week MC bills under various PV scenarios.png",
       dpi = 300, height = 6, width = 11)




##### plot bills - DS and GP #####

# melt data
plotdat <- dat_consumptionMonthly[!is.na(dat_consumptionMonthly$totalBillDollars_DSpricing_pv1MW),]  # keep only range with original DS bills
plotdat <- with(plotdat, data.frame(value = c(totalBillDollars_DSpricing_pv1MW, totalBillDollars_DSpricing_pv2MW, totalBillDollars_DSpricing_pv5MW,  totalBillDollars_DSpricing_pv17MW,
                                              totalBillDollars_GPpricingBaseline2019_pv1MW, totalBillDollars_GPpricingBaseline2019_pv2MW, totalBillDollars_GPpricingBaseline2019_pv5MW, totalBillDollars_GPpricingBaseline2019_pv17MW,
                                              totalBillDollars_GPpricingBaseline2020_pv1MW, totalBillDollars_GPpricingBaseline2020_pv2MW, totalBillDollars_GPpricingBaseline2020_pv5MW, totalBillDollars_GPpricingBaseline2020_pv17MW)/1e6,
                                    scenario = rep(c('1 MW', '2 MW', '5 MW', '17 MW'), each = nrow(plotdat)),
                                    pricingStructure = rep(c('DS', 'GP baseline 2019', 'GP baseline 2020'), each = nrow(plotdat)*length(scenarios)),
                                    date = plotdat$date))
plotdat$scenario <- factor(plotdat$scenario, levels = c('1 MW', '2 MW', '5 MW', '17 MW'))

#plotdat <- plotdat[plotdat$pricingStructure != 'GP baseline 2019',]  # plot only GP 2020 baseline

# plot
ggplot() +
  geom_line(data = plotdat,
            aes(x = date, y = value, color = pricingStructure, linetype = scenario),
            size = 1.3, alpha = 0.7) +
  labs(color = 'Pricing\nstructure',  linetype = 'PV capacity', x = NULL, y = 'Monthly bill (million $)') +
  scale_linetype_manual(values = c('solid', 'longdash', 'dashed', 'dotted')) +
  geom_line(data = plotdat[plotdat$scenario == '1 MW' & plotdat$pricingStructure == 'DS',],
            aes(x = date, y = value), color = scales::hue_pal()(3)[[1]], alpha = 0.7, size = 2) +
  theme(text = element_text(size = 15), legend.key.size=unit(2,"lines"))

ggsave("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/04b-fig02 DS and GP bills under various PV scenarios.png",
       dpi = 300, height = 6, width = 10)
