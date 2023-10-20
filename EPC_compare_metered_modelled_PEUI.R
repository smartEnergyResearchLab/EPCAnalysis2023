# June 2022, Jessica Few, jessica.few@ucl.ac.uk

# This script runs the comparison between metered annual PEUI and EPC modelled PEUI
# results of this analysis are published in this paper: https://doi.org/10.1016/j.enbuild.2023.113024
# load packages and data from EPC_data_analysis_preparation.R file

library(ggplot2)
library(dplyr)
library(reshape)

processed_data_loc = ""
plot_data = read.csv(paste(processed_data_loc, "Metered_combined_dt.csv", sep = ""))

# load plot_functions
source("")

#save loc 
save_outputs_loc = ""


##### generate figures for analysis

# simple plot to give an idea of where the bulk of the sample lies
ggplot() + 
  geom_point(data = plot_data, aes(x = energyConsumptionCurrent + appliance_peui_eleccook,y =total_primary/totalFloorArea, color = currentEnergyRatingMerge)) +
  geom_abline(aes(intercept = 0, slope = 1)) + 
  xlim(0,1000) + ylim(0, 1000)

# histogram of the difference between metered and modelled
figure = ggplot(plot_data, aes(x = metered_primary_eui - epc_tot_peui)) +
  geom_histogram(position= 'dodge') +
  xlab("Number of dwellings")  + ylab(bquote('Difference in PEUI'))#+ 
  #coord_cartesian(ylim = c(10, 500), xlim = c(-400, 400), expand = FALSE)
  
print(figure)
# linear model of metered against modeled
linear_model = lm(metered_primary_eui ~ epc_tot_peui, data = plot_data)
summary(linear_model)

linear_model = lm(metered_primary_eui ~ epc_tot_peui*currentEnergyRatingMerge, data = plot_data)
summary(linear_model)


#### 

res = mean_plot_by_1group(plot_data, currentEnergyRatingMerge, "EPC Energy Efficiency Rating", epc_tot_peui)


line_plot_for_exporting_single_group(plot_data)

line_plot_for_exporting_diff_basic(plot_data, currentEnergyRatingMerge, "EPC band")  

line_plot_for_exporting_diff_basic(plot_data, new_home_epc, "Reason for EPC")  

line_plot_for_exporting_diff_basic(plot_data, set_point_2, "Thermostat set point")  

line_plot_for_exporting_diff_basic(plot_data, num_occupants, "Number of occupants")  

line_plot_for_exporting_diff_basic(plot_data, match_sap_occupancy, "SAP occupant assumptions")  

