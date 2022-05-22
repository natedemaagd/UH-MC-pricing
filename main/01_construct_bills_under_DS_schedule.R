
Sys.setenv(TZ='HST')

library(ggplot2); library(readxl); library(dplyr)
source('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/UH/UH MC pricing/functions/dataframe_date_format.R')
setwd('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/')

# load actual bills
dat_actual_bills <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/UH/monthly_UH_bill_2018_2019.xlsx")

# load UH power use data
dat_15min <- readxl::read_xlsx("UH/UH Demand 2017 - 2021.xlsx")

# fix column names
time_seq <- as.character(seq(from=as.POSIXct("2012-01-01 00:00", tz="HST"), 
                             to=as.POSIXct("2012-01-01 23:45", tz="HST"), by="15 min"))
time_seq <- substr(time_seq, 12, 16)
dat_colnames <- c('date', time_seq)
dat_colnames <- gsub(':', '', dat_colnames)
colnames(dat_15min) <- dat_colnames
rm(dat_colnames, time_seq)




##### create billing demand data.frame #####

# split data by year-month
dat_15min$year_month <- substr(as.character(dat_15min$date),1,7)
dat_15min_yearmonth <- split(dat_15min, dat_15min$year_month)

# get max 15-min load in each year-month
billing_demand <- data.frame(year_month = unique(dat_15min$year_month),
                             max_15min_kW = sapply(dat_15min_yearmonth, function(df) max(df[2:(ncol(df)-1)], na.rm = TRUE)))
rm(dat_15min_yearmonth)

# get max 15-min load for preceding 11 months
billing_demand$max_15min_kW_preceding11mo <- NA
for(i in 12:nrow(billing_demand)){
  
  billing_demand$max_15min_kW_preceding11mo[[i]] <- max(billing_demand$max_15min_kW[(i-11):i])
  
}
rm(i)

# get mean of current month and max previous 11 months
billing_demand$mean_currentMonth_and_maxPreceding11months_kW <- rowMeans(billing_demand[,-1])

# billed demand: current month 15-min max or mean(current month, max of previous 11 months), whichever is higher. Must be at least 300 kW
billing_demand$billing_demand_kW <- apply(billing_demand[,c('max_15min_kW', 'mean_currentMonth_and_maxPreceding11months_kW')], 1, max)
billing_demand$billing_demand_kW <- ifelse(!is.na(billing_demand$billing_demand_kW) & billing_demand$billing_demand_kW < 300, 300, billing_demand$billing_demand_kW)  # ensure billing demand is at least 300 kW

# calculate kWh from 15-min kW
dat_daily_kwh <- data.frame(year_month = dat_15min$year_month, kwh = rowSums(dat_15min[,2:(ncol(dat_15min)-1)]))
dat_monthly_kwh <- aggregate(dat_daily_kwh$kwh, list(dat_daily_kwh$year_month), sum, na.rm = TRUE)
billing_demand$billing_demand_kWh <- dat_monthly_kwh$x/4  # divide by 4 since data in 15-min (1/4 hour) intervals
rm(dat_daily_kwh, dat_monthly_kwh)

# remove incomplete cases (i.e. rows where we don't have previous year data to calculate demand charge)
billing_demand <- billing_demand[complete.cases(billing_demand),]

# calculate non-fuel energy charge
billing_demand$nonfuelEnergyCharge_dollars <- billing_demand$billing_demand_kW * 0.018886  ####### FIND




##### calculate charges #####

# load charges data
dat_cc <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_ds/customer_charge.xlsx")  # fixed customer charge
dat_dc <- read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_ds/demand_charge.xlsx")  # demand charge
dat_ecrc <- read_xlsx("Rate_Data/all_schedules/ecrf.xlsx")  # Energy Costs: accounts for energy cost adjustment (prior to 2019) and energy cost recovery (2019 and on)
dat_ppac <- read_xlsx("Rate_Data/schedule_ds/purchase_power_adjustment.xlsx")  # Purchased Power Adjustment Clause
dat_rbap <- read_xlsx("Rate_Data/all_schedules/rba_rate_adjustment.xlsx")  # Revenue Balancing Account Provision
#dat_irpcrp <- read_xlsx()  # Integrated Resource Planning Cost Recovery Provision
#dat_pwrFactor <- read_xlsx()  # Power Factor (reactive power)
dat_pbfs <- read_xlsx("Rate_Data/schedule_ds/pbf_surcharge.xlsx")  # Public Benefits Fund Surcharge
dat_reicrp <- read_xlsx("Rate_Data/schedule_r/reicrp.xlsx")  # Renewable Energy Infrastructure Cost Recovery Provision ###### CAN'T FIND DS VALUES
dat_gif <- read_xlsx("Rate_Data/schedule_ds/green_infrastructure_fee.xlsx")  # Green Infrastructure Fee Surcharge

# calculate charges
dat_cc <- date_format(dat_cc)
billing_demand <- left_join(billing_demand, dat_cc, 'year_month')
colnames(billing_demand)[length(colnames(billing_demand))] <- 'customerCharge_dollars'

dat_dc <- date_format(dat_dc)
billing_demand <- left_join(billing_demand, dat_dc, 'year_month')
billing_demand$demandCharge_dollars <- billing_demand$billing_demand_kW * billing_demand$dollars_per_kW
billing_demand <- billing_demand[-(ncol(billing_demand)-1)]

dat_ecrc$month <- str_pad(dat_ecrc$month, width = 2, side = 'left', pad = 0)
dat_ecrc$year_month <- paste(dat_ecrc$year, dat_ecrc$month, sep = '-')
billing_demand <- left_join(billing_demand, dat_ecrc, 'year_month')
billing_demand$ecrc_dollars <- billing_demand$billing_demand_kWh * billing_demand$final_cents_per_kwh / 100
billing_demand[c('county', 'year', 'month', 'energy_charge_cents_per_kwh', 'energy_cost_adjustment_factor_cents_per_kwh', 'energy_cost_recovery_factor_cents_per_kwh', 'final_cents_per_kwh')] <- NULL

dat_ppac <- date_format(dat_ppac)
billing_demand <- left_join(billing_demand, dat_ppac, 'year_month')
billing_demand$ppac_dollars <- billing_demand$billing_demand_kWh * billing_demand$cents_per_kwh / 100
billing_demand <- billing_demand[-(ncol(billing_demand)-1)]

dat_rbap <- date_format(dat_rbap)
billing_demand <- left_join(billing_demand, dat_rbap, 'year_month')
billing_demand$rbap_dollars <- billing_demand$billing_demand_kWh * billing_demand$cents_per_kwh / 100
billing_demand <- billing_demand[-(ncol(billing_demand)-1)]

# irpcrp

dat_pbfs <- date_format(dat_pbfs)
billing_demand <- left_join(billing_demand, dat_pbfs, 'year_month')
billing_demand$pbfs_dollars <- billing_demand$billing_demand_kWh * billing_demand$cents_per_kwh / 100
billing_demand <- billing_demand[-(ncol(billing_demand)-1)]

dat_reicrp <- date_format(dat_reicrp)
billing_demand <- left_join(billing_demand, dat_reicrp, 'year_month')
billing_demand$reicrp_dollars <- billing_demand$billing_demand_kWh * billing_demand$cents_per_kwh / 100
billing_demand <- billing_demand[-(ncol(billing_demand)-1)]

dat_gif <- date_format(dat_gif)
billing_demand <- left_join(billing_demand, dat_gif, 'year_month')
billing_demand$gif_dollars <- billing_demand$dollars_per_month
billing_demand <- billing_demand[-(ncol(billing_demand)-1)]

# calculate total bill and plot
billing_demand$totalBill_dollars_constructed <- with(billing_demand, customerCharge_dollars + nonfuelEnergyCharge_dollars + demandCharge_dollars + ecrc_dollars + ppac_dollars + rbap_dollars + pbfs_dollars + reicrp_dollars + gif_dollars)

ggplot(data = billing_demand[-c(1:11),]) + geom_line(aes(x = as.Date(paste0(year_month, '-15')), y = totalBill_dollars_constructed/1e6)) +
  labs(x = NULL, y = 'Total bill (mil $)')

# merge actual bills to constructed bills, then find difference b/w the bills
billing_demand$date <- as.Date(paste0(billing_demand$year_month, '-01'))
billing_demand <- dplyr::left_join(billing_demand, dat_actual_bills[c('date', 'bill_dollars')], by = 'date')
colnames(billing_demand)[colnames(billing_demand) == 'bill_dollars'] <- 'totalBill_dollars_actual'
billing_demand$bill_constructed_minus_actual <- billing_demand$totalBill_dollars_constructed - billing_demand$totalBill_dollars_actual
ggplot(data = billing_demand[!is.na(billing_demand$totalBill_dollars_actual),]) +
  geom_line(aes(x = date, y = bill_constructed_minus_actual/totalBill_dollars_actual*100)) +
  labs(x = NULL, y = 'Calculated bill % error')




##### find average dollars per kWh for all charges #####

# create table of averages
billing_demand_avgs <- data.frame(component = c('Customer charge', 'Nonfuel energy charge', 'Demand charge', 'Energy cost recovery clause', 'Purchased power adjustment clause', 'Revenue balancing account provision', 'Integrated resource planning cost recovery provision',
                                                'Public benefits fund surcharge', 'Renewable energy infrastructure cost recovery provision', 'Green infrastructure fee surcharge'),
                                  mean_charge = c(mean(billing_demand$customerCharge_dollars, na.rm = TRUE),
                                                  mean(billing_demand$nonfuelEnergyCharge_dollars, na.rm = TRUE),
                                                  mean(billing_demand$demandCharge_dollars, na.rm = TRUE),
                                                  mean(billing_demand$ecrc_dollars, na.rm = TRUE),
                                                  mean(billing_demand$ppac_dollars, na.rm = TRUE),
                                                  mean(billing_demand$rbap_dollars, na.rm = TRUE),
                                                  NA,  # irpcrp
                                                  mean(billing_demand$pbfs_dollars, na.rm = TRUE),
                                                  mean(billing_demand$reicrp_dollars, na.rm = TRUE),
                                                  mean(billing_demand$gif_dollars, na.rm = TRUE)))

saveRDS(billing_demand, file = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/UH/01_constructed_bills_under_DS_schedule.rds')




##### plots - peak load last month and last year #####

# find peak consumption last month and last year
peakDayLastMonth <- dat_15min$date[[1431+which.max(matrixStats::rowMaxs(as.matrix(dat_15min[1432:nrow(dat_15min),-c(1,ncol(dat_15min))])))]]
peakDayLastYear  <- dat_15min$date[[1095+which.max(matrixStats::rowMaxs(as.matrix(dat_15min[1096:nrow(dat_15min),-c(1,ncol(dat_15min))])))]]

# extract load profiles
loadProfile_peakDayLastMonth <- data.frame(time = colnames(dat_15min[2:(ncol(dat_15min)-1)]),
                                           load = as.vector(t(dat_15min[dat_15min$date == peakDayLastMonth, 2:(ncol(dat_15min)-1)])))
loadProfile_peakDayLastYear  <- data.frame(time = colnames(dat_15min[2:(ncol(dat_15min)-1)]),
                                           load = as.vector(t(dat_15min[dat_15min$date == peakDayLastYear,  2:(ncol(dat_15min)-1)])))

# combine peak data into one data.frame
loadProfile_peaks <- data.frame(time = c(loadProfile_peakDayLastMonth$time, loadProfile_peakDayLastYear$time),
                                load = c(loadProfile_peakDayLastMonth$load, loadProfile_peakDayLastYear$load),
                                day  = c(rep('2021-06-03', times = nrow(loadProfile_peakDayLastMonth)),
                                         rep('2020-10-02',  times = nrow(loadProfile_peakDayLastYear))))

# plot
ggplot(data = loadProfile_peaks) +
  geom_line(aes(x = as.numeric(time), y = load, group = day, color = day), size = 1) +
  labs(x = 'Time of day', y = 'Load (kW)', color = NULL) +
  theme(text = element_text(size = 16),
        legend.position = c(0.55, 0.2))
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01_UH_example loads.png',
       width = 8, height = 5)
