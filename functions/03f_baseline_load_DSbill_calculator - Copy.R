
# this script calculates the bill under DS pricing for baseline loads - for use with the Georgia Power bill simulator script: 03_simulate_monthly_bills.R

# calculation of baseline bills uses latest recorded value for each charge (published by HECO monthly: https://www.hawaiianelectric.com/billing-and-payment/rates-and-regulations/energy-cost-filings)

# function to calculate bill for all months in the baseline year
ds_bill_calculator <- function(baseline_year){
  
  # load all HECO DS schedule fixed and variable charge data
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
  
  # define total consumption (kWh) and peak load (kW)
  kWh_total   = dat_UHdemand_baseline_monthly[dat_UHdemand_baseline_monthly$year == baseline_year, 'kWh']
  kW_peakLoad = dat_UHdemand_peak15minLoad[dat_UHdemand_peak15minLoad$year == baseline_year, 'billing_demand_kW']
  
  # create vector of all charges
  charges_list_dollars <- list(
    
    # divide by 100 when charged cents per kWh to get total dollars
    
    # customer charge: fixed monthly
    customer_charge = dat_cc$dollars_per_month[[nrow(dat_cc)]],
    
    # demand charge: per kW, based on billing demand (peak load as calculated in dat_UHdemand_peak15minLoad)
    demand_charge = kW_peakLoad * dat_dc$dollars_per_kW[[nrow(dat_dc)]],
    
    # energy cost recovery: per kWh
    ecrc =  kWh_total * dat_ecrc$final_cents_per_kwh[[nrow(dat_ecrc)]]/100,
    
    # purchase power adjustment clause: per kWh
    ppac = kWh_total * dat_ppac$cents_per_kwh[[nrow(dat_ppac)]]/100,
    
    # RBA rate adjustment: per kWh
    rbap = kWh_total * dat_rbap$cents_per_kwh[[nrow(dat_rbap)]]/100,
    
    # public benefits fund surcharge: per kWh
    pbfs = kWh_total * dat_pbfs$cents_per_kwh[[nrow(dat_pbfs)]]/100,
    
    # renewable energy infrastructure surcharge
    reicrp = kWh_total * dat_reicrp$cents_per_kwh[[nrow(dat_reicrp)]]/100,
    
    # green infrastructure fee: fixed monthly
    gif = dat_gif$dollars_per_month[[nrow(dat_gif)]]
  )
  
  # sum across all charges to get total bill for each month in the baseline year
  return(Reduce('+', charges_list_dollars))
  
}
