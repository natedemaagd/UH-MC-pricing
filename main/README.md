Descriptions of script files

* 01_construct_bills_under_DS_schedule.R - constructs historical monthly bills for UH under HECO's DS pricing schedule, using historical prices and demand
* 02_marginal_cost_pricing.R - calculates charges for historical UH demand under marginal cost pricing using FERC data
  * calculates under real-time pricing and backward-looking previous week load-weighted MC pricing
* 03_simulate_monthly_bills.R - simulates UH bills under Georgia Power MC pricing, using various years as baselines
