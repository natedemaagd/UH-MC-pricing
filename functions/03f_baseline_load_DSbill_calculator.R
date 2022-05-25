
# this script calculates the bill under DS pricing for baseline loads - for use with the Georgia Power bill simulator script: 03_simulate_monthly_bills.R

# calculation of baseline bills uses latest recorded value for each charge (published by HECO monthly: https://www.hawaiianelectric.com/billing-and-payment/rates-and-regulations/energy-cost-filings)

# function to calculate bill for all months in historical data
ds_bill_calculator <- function(baseline_year, current_year, current_month){
  
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
  
  # define total consumption (kWh) and peak load (kW) based on baseline year
  kWh_total   = dat_UHdemand_baseline_monthly[dat_UHdemand_baseline_monthly$year == baseline_year, 'kWh']
  kW_peakLoad = dat_UHdemand_baseline_peakDemand[dat_UHdemand_baseline_peakDemand$year == baseline_year, 'billing_demand_kW']
  
  # create vector of all charges
  charges_list_dollars <- list(
    
    ### Charges based on baseline kWh consumption and kW peak load and current prices. Divide by 100 when price given as cents per kWh
    
    # customer charge: fixed monthly
    customer_charge = unlist(dat_cc[with(dat_cc, year == current_year & month == current_month), 'dollars_per_month']),
    
    # demand charge: per kW, based on billing demand (peak load as calculated in dat_UHdemand_peak15minLoad)
    demand_charge = kW_peakLoad * unlist(dat_dc[with(dat_dc, year == current_year & month == current_month), 'dollars_per_kW']),
    
    # energy cost recovery: per kWh
    ecrc =  kWh_total * unlist(dat_ecrc[with(dat_ecrc, year == current_year & month == current_month), 'final_cents_per_kwh'])/100,
    
    # purchase power adjustment clause: per kWh
    ppac = kWh_total * unlist(dat_ppac[with(dat_ppac, year == current_year & month == current_month), 'cents_per_kwh'])/100,
    
    # RBA rate adjustment: per kWh
    rbap = kWh_total * unlist(dat_rbap[with(dat_rbap, year == current_year & month == current_month), 'cents_per_kwh'])/100,
    
    # public benefits fund surcharge: per kWh
    pbfs = kWh_total * unlist(dat_pbfs[with(dat_pbfs, year == current_year & month == current_month), 'cents_per_kwh'])/100,
    
    # renewable energy infrastructure surcharge
    reicrp = kWh_total * unlist(dat_reicrp[with(dat_reicrp,year == current_year & month == current_month), 'cents_per_kwh'])/100,
    
    # green infrastructure fee: fixed monthly
    gif = dat_gif[with(dat_gif, year == current_year & month == current_month), 'dollars_per_month']
  )
  
  # sum across all charges to get total bill for each month in the baseline year
  return(unlist(Reduce('+', charges_list_dollars)))
  
}
