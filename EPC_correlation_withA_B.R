# June 2022, Jessica Few, jessica.few@ucl.ac.uk

# This script analyses the correlation between homes that have EPC rating A or B and 
# other building or sociodemographic variables
# results of this analysis are published in this paper: https://doi.org/10.1016/j.enbuild.2023.113024

# load data
plot_data_loc = ""
plot_data = read.csv(paste(plot_data_loc, 'Metered_combined_dt.csv', sep = ''))
# save loc 
save_outputs_loc = ""


plot_data$currentEnergyRatingAB = 'A and B'
plot_data[plot_data$currentEnergyRatingMerge != 'A and B', c('currentEnergyRatingAB')] = 'C to G'

test = plot_data

factor_cols = c("currentEnergyRatingAB", "builtForm",
"energyTariff", "extensionCount", "fixedLightingOutletsCount",
"flatTopStorey","floorDescription",
"glazedArea", "glazedType",
"hotwaterDescription","hotWaterEnergyEff",
"lightingDescription","lightingEnergyEff", "localAuthority",
"lowEnergyFixedLightCount", "lowEnergyLighting",
"mainheatcEnergyEff","mainheatcontDescription",
"mainheatDescription","mainheatEnergyEff", "mechanicalVentilation",
"multiGlazeProportion", "numberHabitableRooms", "numberHeatedRooms",
"numberOpenFireplaces","propertyType", "roofDescription",
"roofEnergyEff","secondheatDescription",
"tenure","transactionType",
"wallsDescription", "wallsEnergyEff", "windowsDescription","windowsEnergyEff",
"Region","IMD_quintile",
"building_age","num_occupants", "set_point_2",
"has_unheated_spaces","managing_financially",
"heating_control_timer", "heating_control_temp_setting", "heating_control_smart",
"heating_control_manual",
"heating_control_any",
"floor_area_banded_midpoint", "lodgement_year",
'A10', 'A13_01', 'A13_02', 
'A14', 'A1501', 'A1502', 'B10_sum')



test = test[, factor_cols]
# make all the missing data NAs
test[(!is.na(test$tenure)) & (test$tenure == 'NO DATA!'), 'tenure'] = NA
test[(!is.na(test$tenure)) & (test$tenure == 'unknown'), 'tenure'] = NA

test[(!is.na(test$mechanicalVentilation)) & (test$mechanicalVentilation == 'NO DATA!'), 'mechanicalVentilation'] = NA

test[(!is.na(test$glazedType)) & (test$glazedType == 'INVALID!'), 'glazedType'] = NA
test[(!is.na(test$glazedType)) & (test$glazedType == 'NO DATA!'), 'glazedType'] = NA
test[(!is.na(test$glazedType)) & (test$glazedType == 'not defined'), 'glazedType'] = NA

test[(!is.na(test$glazedArea)) & (test$glazedArea == 'NO DATA!'), 'glazedArea'] = NA

test[(!is.na(test$energyTariff)) & (test$energyTariff == 'INVALID!'), 'energyTariff'] = NA
test[(!is.na(test$energyTariff)) & (test$energyTariff == 'Unknown'), 'energyTariff'] = NA

test[(!is.na(test$builtForm)) & (test$builtForm == 'NO DATA!'), 'builtForm'] = NA

fisher_results = data.frame()

for (col in factor_cols){
  print(col)
  test_table = table(test[,col], test[,'currentEnergyRatingAB'])
  fisher = fisher.test(test_table, simulate.p.value = TRUE)
  fisher_this_col = data.frame(var_name = c(col), p_val = c(fisher$p.value), alternative = c(fisher$alternative))
  fisher_results = rbind(fisher_results, fisher_this_col)
}

# save the results in a csv for a table in the appendix of the EPC paper
write.csv(fisher_results, paste(save_outputs_loc, "Fisher_results_bandAB_vs_CtoG.csv", sep = ""), row.names = FALSE)













