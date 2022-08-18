
*********************************************************************************
*Organize the master file 
* Last update: 07/12/2018
*********************************************************************************

*Run these before running individual do files 
 
*set directory (for mac)
cd "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/stata_code"


*raw data
global raw_data "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/Raw_data"
*Processed data
global processed_data "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/Processed_data"
*Figure
global figure "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/Figure/RecentFigures"
global word_figure "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/Figure/Figures_for_word"
global graphs_for_report "~/Dropbox/UH_energy_project/graphs_for_report"
*set directory for table 
global table "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/table"
*set global to matthias system lambda estimation results
global est_md "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/energy_sources/"

*set global to UH-Switch model input_output folder
global UHSwitch "~/Dropbox/UH_energy_project/inputs_outputs/original_inputs"

*set global to compute HECO's rent and UH's credit
global processed_data "~/Google Drive/SwitchUH/Hyungyu/Energy Project (UH)/Processed_data"
global est_hourly_price "~/Dropbox/UH_energy_project/psip_costs/outputs"

global path_to_data "~/Dropbox/UH_energy_project/inputs_outputs"
global processed_data_2020 "~/Dropbox/UH_energy_project/inputs_outputs/processed_inputs"
global baseline_high "00L_EC2_NEC0_PV0_PPA0_CG0_WO0_VRM0"
global baseline_mid "00L_EC1_NEC0_PV0_PPA0_CG0_WO0_VRM0"
global baseline_low "00L_EC3_NEC0_PV0_PPA0_CG0_WO0_VRM0"



*set inflation and future electricity consumption change
global inf 0.02  //rate of inflation 
global fall 0.006  //Rate of decreasing future electricity consumption

*set discount rate at 3% to calculate present valued average electricity bill 
global dis 0.01 //discount rate 

end

*****************************************************************
*Start analysis
*****************************************************************

*--------------------------------------------------------------
* 1. Preparation of data set 
*--------------------------------------------------------------

* clean up UH billing data 
* graph for the UH billing structure and each component 
* generate the dataset "UH_electricity_bill_history_2000_2019"
do data_preparation_energy charge_ECAC_RBA_PPAC_1

*old
* generate the dataset "UH_electricity_bill_history_1999_2017"
do data_preparation_energy charge_ECAC_RBA_PPAC


*Prepare brent crude oil futures price and estimate lower bound 
* by using implied volatility (data set is collected from bar chart 
* website: https://www.barchart.com/futures/quotes/CBJ18/options/apr-18
do futures_price_lower_bound
 
*Combine crude oil forecasts and lower bound 
*crude oil price collected in April
do crude_oil_forecast_CME_NYMEX_HECO //2018 April
*do crude_oil_forecast_HECO_CME_lower_bound_Aug052018
*note: 
	*source for futures price: https://www.cmegroup.com/trading/energy/crude-oil/brent-crude-oil.html
	*srouce for brent crude oil prices: https://fred.stlouisfed.org/series/DCOILBRENTEU


brent_crud_oil_price_1999_2018
*---------------------------------------------------------------------------------------
* 2. Analyzing the average variable cost and construct the UH bill 
* without PV Investment 
*---------------------------------------------------------------------------------------

*2.1.1 enery charge
do energy_charge_fold_PV_cost_non_ec_1 //create a energy charge that includes PV costs by using updated non-ec and dc on August 17,2018 
*Analyze average variable cost of the power plants under HECO's grid by using the similar method that Karl did. 
*Predict future energy charge (=average variable cost)
*Generate a graph illustrating the rate of energy charge under different future crude oil price forecast 
*The variable costs from PV is no loger part of future energy charge. It is folded into the non-energy charge 
*Generate following data sets 
* "$processed_data/future_energy_charge_EIA_brent_lb_PSIP_lb.dta", 
*updated: 09/04/2018
 
*2.1.2 energy charge II
do energy_charge_2
*This script illustrates the case that the part of purchased power costs from the utility-scale PVs were charged through
* energy charge ( about 90%)
* updated: 01/15/2020

do energy_charge_3
* In this script, the case of estimating energy charge with the opimized utility-scale PV is included
 
*Compare the trend of avatement costs, energy charge and predicted energy charge
do analyzing_avoided_cost

*2.2 non-energy charge schedule  (2015-2038)
*non energy charge 
do non_energy_charge_DS_schedule_add_solar_cost_1 /
*Note: create a non-energy charge that includes PV costs by using updated non-ec and dc on August 17,2018 

* Schedule: DS schedule 
* Total non-energy charge is expected to rise by 5% in each year 
* Total electricity consumption of the DS schedule is expected to increase as PSIP 2016 predicted 
* 30% grid defection is also included 
*reference
*do non_energy_charge_DS_schedule_2 // used the same set up in the excel file but the results are different. So I used the results from excel
*do non_energy_charge_DS_schedule_1 // the way calculate the rates of non-energy and demand charge separately. 
*do non_energy_charge_DS_schedule 

* 2.3 Future electricity bill of the UH
*Combine estimated energy and non-energy charge 
*Create the trend graph for future electricity bill
do future_UH_ann_ec_non_ec_2018_2038_norminal // UH bill under 9.5, 7.5 5.5 PPA and futures prices updated on Aug/16/2018 results are nominal 


*reference
*do future_UH_ann_ec_non_ec_2018_2038_1 // UH bill under 9.5, 7.5 5.5 PPA and futures prices updated on Aug/5/2018
*do future_UH_ann_ec_non_ec_2018_2038



*---------------------------------------------------------------------------------------
*3. Analysis of virtual rider M + W
*---------------------------------------------------------------------------------------

do Analysis_of_rider_M_1 // virtual rider M with 11.5, 9.5 amd 7.5 PPA with 5MW/20MWh battery and 10MW PV in west oahu, without campus investment
do TEB_SQ_campus_inv_vrm // virtual rider M with an investment on PV and battery 

*Reference
*do Analysis_of_rider_M


*--------------------------------------------------------------------------------------------------------------------------------
*4. Combine the results of estimating total electricity bill (without campus investment, 
*	with and without rider M+W
*---------------------------------------------------------------------------------------

*Create a graph showing the trend of enrgy, non-energy charge and total electricity bill
*Create a present valued average monthly electricity bill ("$table\dis_predicted_total_bill_status_quo_sleeve_w_wo_PV.csv")
do Combine_results_of_all_scenarios_nominal //combine the results of status-quo sleeve_nominal 
do Combine_results_of_all_scenarios // combine the results of status-quo sleeve and rider M convert all results to real from nominal 


total_ebill_status_quo_sleeve_no_battery_nominal
*reference: combine the results of past estimation and Imelda's SWITCH model
*do Combine_results_from_HECO_and_Imelda


*--------------------------------------------------------------------------------------------------------------------------------
* 5. Estimation of HECO's rent under each scenario (period: 2020-2039) 
* Do file needs to be updated after deciding whether we are using 765 (selected days) or 777 ( all days) for the average marginal
* costs serving UH 
*--------------------------------------------------------------------------------------------------------------------------------
do HECO_rent_SQ_1 // HECO's rent under SQ contract with updated hourly MC
do HECO_rent_SQ_w_inv_camp //HECO's rent under SQ contract with campus investment
do HECO_rent_SQ_w_inv_camp_def //HECO's rent under SQ contract with campus investment and 30% defection
do HECO_rent_SL_no_comp_inv //HECO's rent under sleeve contract without compus investment (update: 9/18/2018)
do HECO_rent_SL_no_comp_inv_low_30 //HECO's rent under sleeve contract without compus investment
do HECO_rent_SL_w_comp_inv //HECO's rent under sleeve contract with compus investment
do HECO_rent_SL_w_comp_inv_low_30 //HECO's rent under sleeve contract with compus investment and 30% defection


* 5.1 Estimating HECO's rent for the final report in 2020 June
do Cleanning_estimated_hourly_prices // Make the hourly price results from Matthias to be able to use 
do HECO_rent_SQ_2020 // HECO's rent under status quo scenario with houlry prices 
do HECO_rent_SQ_w_inv_camp //HECO's rent with campus investment
do HECO_rent_w_comp_inv_GT_2020 //HECO's rent with campus investment and green tariff project
do obtain_weight //obtain weight for each year 
do HECO_avoided_cost_w_on_campus_inv_1 //Obtain HECO's avoided cost with campus investment
do prepare_energy_output_from_switch // compile optimized utility-scale PV from Oahu model

*Need to run "HECO_avoided_cost_w_on_campus_inv_1.do" to activate the global "$mid_campus_inv" 
do Estimate_HECO_Rent_UH_Credit //Estimate HECO's rent and UH's credits related to Green Tariff Project
*Not use below do file to estimate Credit
do Estimate_HECO_Rent_UH_Credit_VRM //Estimate HECO's rent and UH's credits related to Green Tariff Project with VRM

*Referece
*do HECO_rent_SQ // HECO's rent under SQ contract, marginal costs are not based on year 2007

* Supporting 'do' file in estimating HECO's rent 
Preapare_net_demand_sq_baseline //status quo with PV and battery investment on campus
Prepare_net_demand_sleeve_camp_in
Prepare_net_demand
 
 

*--------------------------------------------------------------------------------------------------------------------------------
* 6. Compile the results from SWITCH model
*--------------------------------------------------------------------------------------------------------------------------------
do TEB_SQ_campus_inv // contains the results of both SQ and SL with campus inv
do TEB_SL_camp_inv



*--------------------------------------------------------------------------------------------------------------------------------
* 7. Analyzing the scenario for Real Time Pricing   (RTP)  
*--------------------------------------------------------------------------------------------------------------------------------
do TEB_Real_Time_Pricing  // without campus investment
do TEB_RTP_w_camp_inv // with campus investment, the results of estimation from the UHM SWTICH model


*--------------------------------------------------------------------------------------------------------------------------------
*8. Adjusting years of each data to match years of MC (2007) and load(2014) by using the FERC data
*   Using the methods that Matthias suggest by musing mean and standard deviation 
*-------------------------------------------------------------------------------------------------------------------------------- 
Updated_MC_hourly_Oahu_model_2020_2039

*--------------------------------------------------------------------------------------------------------------------------------
* 9. Reference do file 
* Each do file is needed for the analysis. However, it does not sorted into one specific section.
*-------------------------------------------------------------------------------------------------------------------------------- 
*Estimate energy charge by using the total PV generation from the Oahu model
do energy_charge_3

*Prepare Oahu model output (2020/06/15)  
do prepare_energy_output_from_switch 
*This do file prepares and organizes the outputs from Oahu model, 2020

 
*Compile 15 min electricity load of the UH from 2010-2015
*create data set named "$processed_data/UH_15min_load.dta"
do Preparation_UH_15_load

* Prepare system lambda reported by HECO
* System lambda is collected from FERC (system lambda and hourly demand) 
do system_lambda_1 

* Compute variable cost of the UH by using compiled 15min electricity consumption 
* of the UH and system labmda from FERC 
* Compare it to the historical electricity bill of the UH 
* Generate the fixed charge from the real time pricing (RTP) scenario 
do lambda_UH_consumption 

*Compile capacity factor and estimate the possible electricity generate in west oahu by uing histroical data 
*compare the estimation to the historical UH electricity bill (2010-2015)
*the main purpose is to find out HECO's cost with sleeve contract
do HECO_cost_with_sleeve


* Clean up future marginal cost calculated by Matthias 
* Generate the graph that compare system lambda weighed marginal cost and energy charge 
do Future_marginal_cost_matthias


*Generate the variables for the table presenting present valued average monthly bill 
* and components of each scenario 
do graph_for_ebill_summary


*Generate the bart chart illustrating all of the results 
do sum_results_bar_chart

*Generating the graph showing energy charge, system labmda weighted by UH load and WO load
do Graph_of_energy_charge_system_lambda_weighted_UH_WO

 
*Compared the results of UH and HECO
do comparison_forecasted_charges_HECO_UH


*Calculate the growth of non-energy charge by using past revenue and energy charge
Recovering_past_non_energy_charge


*----------------------------------------------------------------------------
* Case that HECO transfer soloar cost from energy charge to non-energy charge 
do non_energy_charge_DS_schedule_add_solar_cost

*combine energy and non-energy charge
do future_uh_bill_with_cost_PV

*A case that HECO adopt utility-scale PV earlier than its Plan in PSIP,2016
do non_energy_charge_HECO_adopt_PV_early
*----------------------------------------------------------------------------

*Prepare demand weighted system lambda
do prepare_demand_weighted_system_lambda 

*Fit system lambda by using brent crude oil price
do Fit_system_lambda_brent_crude_oil

*Obtain energy, non-energy and demand charges as Miles requested
obtain_energy_non_energy_demand_charge_per_kWh


*compare the results of SWITCH and without SWTICH
do RTP_comparison
do comparison_all_day_selected_day

