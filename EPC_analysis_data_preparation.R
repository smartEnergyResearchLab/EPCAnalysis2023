# June 2022, Jessica Few, jessica.few@ucl.ac.uk

# This script is for generating a subset of annual energy use data created for 
# the SERL stats report, which can then be used in to compare against the 
# EPC primary energy use on a by-household basis
# results of this analysis are published in this paper: https://doi.org/10.1016/j.enbuild.2023.113024

library(dplyr)
library(reshape)

# First load the EPC, SERL survey, participant info from SERL observatory edition 4
# and PV exporter PUPRN, annual energy use from annual report analysis 2022

observatory_loc = ""
epc = read.csv(paste(observatory_loc, "serl_epc_data_edition04.csv", sep = ""))
serl_survey = read.csv(paste(observatory_loc, "serl_survey_data_edition04.csv", sep = ""))
participant = read.csv(paste(observatory_loc, "serl_participant_summary_edition04.csv", sep = ""))

pv_exporter_loc = ""
# this is a list of the PUPRN that exported electricity to the grid in 2021, from annual report processing.
pv_exporters = read.csv(paste(pv_exporter_loc, "Elec_2021_list_of_exporter_puprns.csv", sep = ""), header = FALSE)

annual_energy_loc = ""
annual_energy = read.csv(paste(annual_energy_loc, "Annual_report_sm_annual_mean_daily_consumption_2021.csv", sep = ""))

save_data_loc = ""
thermostat_blog_loc = ""

#####
# combine some relevant data
epc = merge(epc, participant[c("PUPRN", "Region", "IMD_quintile")], by = "PUPRN")

# join pv, use electricity exporters
names(pv_exporters) = c("PUPRN")
pv_exporters['has_PV'] = 'Yes'
epc = merge(epc, pv_exporters, by = "PUPRN", all.x = TRUE)
epc$has_PV[is.na(epc$has_PV)] = "No"

# create another column for those where the public EPC says there's PV 
logic = (epc$epcVersion == 'England and Wales') & (epc$photoSupply > 0)
# create a separate column for those with PV that the EPC knows about.
epc$epc_knows_PV = 'No'
epc$epc_knows_PV[logic] = 'Yes'

# scottish photosupply variable
# this line finds the first digit in the photosupply variable for scottish
# data and asks if it's greater than 0. This number will either represent peak power
# or % roof area covered. 
logic = (epc$epcVersion == 'Scotland') & (as.numeric(gsub(".*?([0-9]+).*", "\\1", epc$photoSupply)) > 0)
logic[is.na(logic)] = FALSE
epc$epc_knows_PV[logic] = 'Yes'

# find homes with non-metered energy 
# first create a new variable to make the central heating survey questions easy to interpret
serl_survey['boiler_type'] = NA
logic = (serl_survey$A3_sum == 1) & (serl_survey$A302 == 1)
serl_survey[logic,'boiler_type'] = 'Gas boiler'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A303 == 1)
serl_survey[logic,'boiler_type'] = 'Electric storage radiators'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A304 == 1)
serl_survey[logic,'boiler_type'] = 'Electric radiators'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A305 == 1)
serl_survey[logic,'boiler_type'] = 'Other electric'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A306 == 1)
serl_survey[logic,'boiler_type'] = 'Oil'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A307 == 1)
serl_survey[logic,'boiler_type'] = 'Solid fuel'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A308 == 1)
serl_survey[logic,'boiler_type'] = 'Biomass'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A309 == 1)
serl_survey[logic,'boiler_type'] = 'District or community'
logic = (serl_survey$A3_sum == 1) & (serl_survey$A310 == 1)
serl_survey[logic,'boiler_type'] = 'Other'
logic = (serl_survey$A3_sum > 1) & (serl_survey$A302 == 1)
serl_survey[logic,'boiler_type'] = 'Gas boiler plus other'
logic = (serl_survey$A3_sum > 1) & (serl_survey$A302 == 0)
serl_survey[logic,'boiler_type'] = 'Other mix'
logic = (serl_survey$A3_sum == 0) | ((serl_survey$A3_sum == 1) & (serl_survey$A301 == 1))
serl_survey[logic,'boiler_type'] = 'None'

# gas heating or not?
serl_survey['gas_heating'] = 'Not gas'
logic = (serl_survey['boiler_type'] == 'Gas boiler') | (serl_survey['boiler_type'] == 'Gas boiler plus other')
serl_survey[logic, 'gas_heating'] = 'Gas'

serl_survey['elec_heating'] = 'Not elec'
logic = (serl_survey['boiler_type'] == 'Electric storage radiators') | 
  (serl_survey['boiler_type'] == 'Electric radiators') | (serl_survey['boiler_type'] == 'Other electric')
serl_survey[logic, 'elec_heating'] = 'Elec'

# is the main heating metered?
serl_survey['main_heating'] = 'Metered'
logic = (serl_survey$A306 == 1) | (serl_survey$A307 == 1) |
  (serl_survey$A308 == 1) | (serl_survey$A309 == 1) | 
  (serl_survey$A310 == 1) | (serl_survey$A301 == 1 & serl_survey$A3_sum == 1) # take none as unmetered if it's the only option selected, if any other options are selected just use those
serl_survey[logic, 'main_heating'] = 'Unmetered'

# is the secondary heating metered
serl_survey['secondary_heating'] = 'None' # strictly none or no answer
logic = (serl_survey$A7 == 1)
serl_survey[logic, 'secondary_heating'] = 'Unmetered'
logic = (serl_survey$A7 == 2)
serl_survey[logic, 'secondary_heating'] = 'Metered'

# is any of the heating unmetered
serl_survey['any_unmetered'] = 'Metered'
logic = (serl_survey$secondary_heating == 'Unmetered') | (serl_survey$main_heating == 'Unmetered')
serl_survey[logic, 'any_unmetered'] = 'Unmetered'

# building age (from survey)
serl_survey['building_age'] = NA
serl_survey[serl_survey$B9 == 1, 'building_age'] = 'Before 1900'
serl_survey[serl_survey$B9 == 2, 'building_age'] = '1900 - 1929'
serl_survey[serl_survey$B9 == 3, 'building_age'] = '1930 - 1949'
serl_survey[serl_survey$B9 == 4, 'building_age'] = '1950 - 1975'
serl_survey[serl_survey$B9 == 5, 'building_age'] = '1976 - 1990'
serl_survey[serl_survey$B9 == 6, 'building_age'] = '1991 - 2002'
serl_survey[serl_survey$B9 == 7, 'building_age'] = '2003 onwards'
serl_survey[(serl_survey$B9 == -1) | (serl_survey$B9 == -2), 'building_age'] = 'Unknown or no answer'

# is there an ev?
serl_survey['has_EV'] = NA
serl_survey[serl_survey$C5 == 1, 'has_EV'] = 'Yes'
serl_survey[serl_survey$C5 == 2, 'has_EV'] = 'No'
serl_survey[(serl_survey$C5 == -1) | (serl_survey$C5 == -2), 'has_EV'] = 'Unknown or no answer'

# Number of occupants
serl_survey['num_occupants'] = NA
logic = (serl_survey$C1_new >= 1) & (serl_survey$C1_new <= 5)
serl_survey[logic,'num_occupants'] = serl_survey[logic,'C1_new']
logic = (serl_survey$C1_new == 6) | (serl_survey$C1_new == 7)
serl_survey[logic,'num_occupants'] = '6 - 7'
logic = (serl_survey$C1_new <=0) | (serl_survey$C1_new > 7)
serl_survey[logic,'num_occupants'] = 'No data or > 7'

serl_survey['set_point_1'] = NA
# we have quite a lot of NA in the set point field, change these all to no answer / doesn't apply
serl_survey[is.na(serl_survey$A5_degC), 'set_point_1'] = 'Unknown or not applicable'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC < 18.5), 'set_point_1'] = 'Less than 17.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 17.5) & 
              (serl_survey$A5_degC < 18.5), 'set_point_1'] = '17.5 to 18.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 18.5) & 
              (serl_survey$A5_degC < 19.5), 'set_point_1'] = '18.5 to 19.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 19.5) & 
              (serl_survey$A5_degC < 20.5), 'set_point_1'] = '19.5 to 20.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 20.5) & 
              (serl_survey$A5_degC < 21.5), 'set_point_1'] = '20.5 to 21.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 21.5) & 
              (serl_survey$A5_degC < 22.5), 'set_point_1'] = '21.5 to 22.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 22.5) 
            & (serl_survey$A5_degC < 23.5), 'set_point_1'] = '22.5 to 23.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 23.5), 'set_point_1'] = '23.5 or above'

# do a 2 degree range as well
serl_survey['set_point_2'] = NA
serl_survey[is.na(serl_survey$A5_degC), 'set_point_2'] = 'Unknown or not applicable'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC < 17.5), 'set_point_2'] = 'Less than 17.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 17.5) & 
              (serl_survey$A5_degC < 19.5), 'set_point_2'] = '17.5 to 19.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 19.5) & 
              (serl_survey$A5_degC < 21.5), 'set_point_2'] = '19.5 to 21.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 21.5) & 
              (serl_survey$A5_degC < 23.5), 'set_point_2'] = '21.5 to 23.5'
serl_survey[!is.na(serl_survey$A5_degC) & (serl_survey$A5_degC >= 23.5), 'set_point_2'] = '23.5 or above'

serl_survey['has_unheated_spaces'] = NA
serl_survey[serl_survey$A11 == -2, 'has_unheated_spaces'] = 'No answer'
serl_survey[serl_survey$A11 == 1, 'has_unheated_spaces'] = 'Has unheated space'
serl_survey[serl_survey$A11 == 2, 'has_unheated_spaces'] = 'All space heated'

serl_survey['heating_control_timer'] = NA
serl_survey[serl_survey$A401 == -9, 'heating_control_timer'] = 'Not applicable'
serl_survey[serl_survey$A401 == 0, 'heating_control_timer'] = 'No'
serl_survey[serl_survey$A401 == 1, 'heating_control_timer'] = 'Yes'

serl_survey['heating_control_temp_setting'] = NA
serl_survey[serl_survey$A402 == -9, 'heating_control_temp_setting'] = 'Not applicable'
serl_survey[serl_survey$A402 == 0, 'heating_control_temp_setting'] = 'No'
serl_survey[serl_survey$A402 == 1, 'heating_control_temp_setting'] = 'Yes'

serl_survey['heating_control_smart'] = NA
serl_survey[serl_survey$A403 == -9, 'heating_control_smart'] = 'Not applicable'
serl_survey[serl_survey$A403 == 0, 'heating_control_smart'] = 'No'
serl_survey[serl_survey$A403 == 1, 'heating_control_smart'] = 'Yes'

serl_survey['heating_control_manual'] = NA
serl_survey[serl_survey$A403 == -9, 'heating_control_manual'] = 'Not applicable'
serl_survey[serl_survey$A403 == 0, 'heating_control_manual'] = 'No'
serl_survey[serl_survey$A403 == 1, 'heating_control_manual'] = 'Yes'

serl_survey['heating_control_any'] = 'No'
serl_survey[(serl_survey$heating_control_timer == 'Yes') | (serl_survey$heating_control_temp_setting == 'Yes') |
              (serl_survey$heating_control_smart == 'Yes') | (serl_survey$heating_control_manual == 'Yes'),
            'heating_control_any'] = 'Yes'


serl_survey['managing_financially'] = NA 
serl_survey[serl_survey$D4 < 0, 'managing_financially'] = 'No answer'
serl_survey[serl_survey$D4 == 1, 'managing_financially'] = 'Living comfortably'
serl_survey[serl_survey$D4 == 2, 'managing_financially'] = 'Doing alright'
serl_survey[serl_survey$D4 == 3, 'managing_financially'] = 'Just about getting by'
serl_survey[serl_survey$D4 == 4, 'managing_financially'] = 'Finding it quite difficult'
serl_survey[serl_survey$D4 == 5, 'managing_financially'] = 'Finding it very difficult'

serl_survey['comfortably_warm'] = NA 
serl_survey[serl_survey$B7 < 0, 'comfortably_warm'] = 'No answer'
serl_survey[serl_survey$B7 == 1, 'comfortably_warm'] = 'Yes'
serl_survey[serl_survey$B7 == 2, 'comfortably_warm'] = 'No'


serl_survey['self_contained'] = NA 
serl_survey[serl_survey$B2 < 0, 'self_contained'] = 'No answer'
serl_survey[serl_survey$B7 == 1, 'self_contained'] = 'Yes'
serl_survey[serl_survey$B7 == 2, 'self_contained'] = 'No'


# recode the boiler_type variable so it matches the mainheat_recode
serl_survey$survey_heating = NA
serl_survey[serl_survey$boiler_type == 'Gas boiler', 'survey_heating'] = 'Gas'
serl_survey[grep(paste(c('Biomass', 'Oil', 'Solid fuel'), collapse = "|"), serl_survey$boiler_type), 'survey_heating'] = 'Oil, wood, coal, LPG'
serl_survey[serl_survey$boiler_type == 'District or community', 'survey_heating'] = 'Gas'
serl_survey[grep(paste(c('Gas boiler plus other', 'Other', 'Other mix'), collapse = "|"), serl_survey$boiler_type), 'survey_heating'] = 'Other, mix or unknown'
serl_survey[grep(paste(c('Elec', 'elec'), collapse = "|"), serl_survey$boiler_type), 'survey_heating'] = 'Electric'


merge_vars = c('PUPRN', 'boiler_type', 'gas_heating', 'main_heating', 
               'secondary_heating', 'any_unmetered', 'building_age', 
               'has_EV', 'num_occupants', 'set_point_1', 'set_point_2', 'has_unheated_spaces', 
               'managing_financially', 'elec_heating', 'heating_control_timer', 
               'heating_control_temp_setting', 'heating_control_smart', 
               'heating_control_manual', 'heating_control_any', 'comfortably_warm', 
               'C1_new', 'self_contained', 'survey_heating', 'A10', 'A13_01', 'A13_02', 
               'A14', 'A1501', 'A1502', 'B10_sum')

epc = merge(epc, serl_survey[merge_vars],
            by = "PUPRN", all.x = TRUE)

# merge A and B and F and G 
epc['currentEnergyRatingMerge'] = epc$currentEnergyRating
logic = (epc$currentEnergyRatingMerge == 'A') | (epc$currentEnergyRatingMerge == 'B')
epc[logic, 'currentEnergyRatingMerge'] = 'A and B'
logic = (epc$currentEnergyRatingMerge == 'F') | (epc$currentEnergyRatingMerge == 'G')
epc[logic, 'currentEnergyRatingMerge'] = 'F and G'


# create floor area bands
epc['floor_area_banded'] = epc$totalFloorArea
logic = epc$totalFloorArea < 50
epc[logic, 'floor_area_banded'] = 'Less than 50'
logic = epc$totalFloorArea >= 50
epc[logic, 'floor_area_banded'] = '50 to 100'
logic = epc$totalFloorArea >= 100
epc[logic, 'floor_area_banded'] = '100 to 150'
logic = epc$totalFloorArea >= 150
epc[logic, 'floor_area_banded'] = '150 to 200'
logic = epc$totalFloorArea >= 200
epc[logic, 'floor_area_banded'] = 'More than 200'


# create SAP occupancy and agreement with survey
# SAP 2012 page 196 / table 1B in formulae and tables
# if TFA < 13.9, N =1, else, N = 1+ 1.76*(1-exp(-0.000349*(TFA-13.9)^2)) + 0.0013*(TFA-13.9)

# 1+1.76*(1-exp(-0.000349*(FA-13.9)^2))+0.0013*(FA-13.9)

epc['sap_occupants'] = 1+1.76*(1-exp(-0.000349*(epc$totalFloorArea-13.9)^2))+0.0013*(epc$totalFloorArea-13.9)
epc[(epc$C1_new == -2) | is.na(epc$C1_new), "C1_new"] = NA
epc$C1_new = as.numeric(epc$C1_new)
epc['diff_occupants'] = epc$sap_occupants - epc$C1_new
epc['diff_occupants_moreless'] = 'Unknown'
epc[(abs(epc$diff_occupants)<=0.5) & !is.na(epc$diff_occupants), "diff_occupants_moreless"] = 'Agree'
epc[epc$diff_occupants<=-0.5 & !is.na(epc$diff_occupants), "diff_occupants_moreless"] = 'Survey more than SAP'
epc[epc$diff_occupants>0.5 & !is.na(epc$diff_occupants), "diff_occupants_moreless"] = 'Survey less than SAP'


# calculate appliance use from SAP2012 manual
# lighting (page 83 - 84)
epc$Eb = 59.73*((epc$totalFloorArea*epc$sap_occupants)^0.4714)
epc$C1_lighting = 1-0.5*(epc$lowEnergyLighting/100)
# if low energy lighting is nan, make it 0, i.e. assume there is no low energy lighting
epc[is.na(epc$C1_lighting), "C1_lighting"] = 1
# let's assume GL> 0.095 so C2 = 0.96
C2 = 0.96
epc$EL = epc$Eb*epc$C1_lighting*C2

days_in_months = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
epc$EL_final = 0
for (month in 1:12)
{
  print(head(epc$EL_final))
  epc$EL_final = epc$EL_final + epc$EL*(1+0.5*cos(2*pi*(month-0.2)/12))*days_in_months[month]/365
}

# calculate electrical appliance use
epc$EA = 207.8*(epc$totalFloorArea*epc$sap_occupants)^0.4714
epc$EA_final = 0
for (month in 1:12)
{
  print(head(epc$EA_final))
  epc$EA_final = epc$EA_final + epc$EA*(1+0.157*cos(2*pi*(month-1.78)/12))*days_in_months[month]/365
}

# cooking - not really clear if this is monthly or annual energy use
epc$Gc = 35+7*epc$sap_occupants

epc$appliance_energy = epc$Gc + epc$EA_final

epc$appliance_peui_gascook = (1.22*epc$Gc + 3.07*(epc$EA_final))/epc$totalFloorArea
epc$appliance_peui_eleccook = (3.07*epc$Gc + 3.07*(epc$EA_final))/epc$totalFloorArea

epc$appl_ratio = epc$appliance_peui_gascook / (epc$energyConsumptionCurrent + epc$appliance_peui_gascook)

# create a column summarising the epc main heat description so we can compare it
# to the survey response

epc['mainheat_recode'] = NA
epc[grep(paste(c('gas', 'Gas', 'nwy prif gyflenwad'), collapse = "|"), epc$mainheatDescription), 'mainheat_recode'] = 'Gas'
table(epc[grep(paste(c('gas', 'Gas', 'nwy prif gyflenwad'), collapse = "|"), epc$mainheatDescription), 'mainheatDescription'])
# we have some which are mains gas community scheme... think these should probably be classified as community.
# they'll get changed below

epc[grep(paste(c('elec', 'Elec'), collapse = "|"), epc$mainheatDescription), 'mainheat_recode'] = 'Electric'
table(epc[grep(paste(c('elec', 'Elec'), collapse = "|"), epc$mainheatDescription), 'mainheatDescription'])
# we have some which have LPG boilers and radiators and electric underfloor. Will catch this in the other group

epc[grep(paste(c(' oil', 'wood', 'coal', 'anthracite', 'LPG', 'smokeless fuel'), collapse = "|"), epc$mainheatDescription), 'mainheat_recode'] = 'Oil, wood, coal, LPG'
table(epc[grep(paste(c(' oil', 'wood', 'coal', 'anthracite', 'LPG', 'smokeless fuel'), collapse = "|"), epc$mainheatDescription), 'mainheatDescription'])

epc[grep('Community', epc$mainheatDescription), 'mainheat_recode'] = 'Community'
table(epc[grep('Community', epc$mainheatDescription), 'mainheatDescription'])

table(epc[is.na(epc$mainheat_recode), 'mainheatDescription'])
# some with SAP05:Main-Heating... can see from mainFuel variable these are split between 
# gas and electric, but does suggest that these EPCs have been filled in well. Let's just leave as unknown
epc[is.na(epc$mainheat_recode), 'mainheat_recode'] = 'Other, mix or unknown'
epc[epc$mainheatDescription == 'Boiler and radiators, LPG, Electric underfloor heating', 'mainheat_recode'] = 'Other, mix or unknown'
epc[epc$mainheatDescription == 'Community scheme, wood pellets and mains gas', 'mainheat_recode'] = 'Other, mix or unknown'


# for people that didn't fill in the survey, fill their rows with unknown
for (survey_var in merge_vars)
{
  epc[is.na(epc[, survey_var]), survey_var] = 'Unknown or no answer'
}

# check how well the survey responses and EPCs agree in terms of heating type
table(epc[, c('boiler_type', 'mainheat_recode')])

table(epc[, c('survey_heating', 'mainheat_recode')])
prop.table(table(epc[, c('survey_heating', 'mainheat_recode')]), margin = 1)

epc$C1_new_numeric = epc$C1_new
epc[epc$C1_new == "Unknown or no answer", "C1_new_numeric"] = NA
epc$C1_new_numeric = as.numeric(epc$C1_new_numeric)
epc$floor_area_banded_midpoint = NA
epc[epc$floor_area_banded == "Less than 50", "floor_area_banded_midpoint"] = 25
epc[epc$floor_area_banded == "50 to 100", "floor_area_banded_midpoint"] = 75
epc[epc$floor_area_banded == "100 to 150", "floor_area_banded_midpoint"] = 125
epc[epc$floor_area_banded == "150 to 200", "floor_area_banded_midpoint"] = 175
epc[epc$floor_area_banded == "More than 200", "floor_area_banded_midpoint"] = 225


##### add energy use data from annual report 2021 data

# primary energy factors from SAP2012 table 12
elec_factor = 3.07
gas_factor = 1.22

annual_energy$total_primary = annual_energy$Clean_elec_net_kWh_d_mean*elec_factor + annual_energy$Clean_gas_kWh_d_mean*gas_factor
# add homes with electric only heating
logic = (annual_energy$PUPRN %in% epc[epc$elec_heating == 'Elec', 'PUPRN']) &
  (annual_energy$PUPRN %in% epc[epc$any_unmetered == 'Metered', 'PUPRN'])
annual_energy[logic, c('total_primary')] = annual_energy[logic, c('Clean_elec_net_kWh_d_mean')]*elec_factor

annual_energy$total_primary = annual_energy$total_primary*365 # to make annual primary consumption from SERL Data

# use Oct 2022 price cap to calculate total price, standard variable tariff
elec_price_cap = 0.34 # pounds/kWh
gas_price_cap = 0.103 # pounds/kWh
annual_elec_standing_charge = 0.46*365
annual_gas_standing_charge = 0.28*365

annual_energy$energy_use_cost = 365*annual_energy$Clean_elec_net_kWh_d_mean*elec_price_cap + 365*annual_energy$Clean_gas_kWh_d_mean*gas_price_cap
annual_energy$total_cost = annual_energy$energy_use_cost + annual_elec_standing_charge + annual_gas_standing_charge
# add homes with electric only heating
logic = (annual_energy$PUPRN %in% epc[epc$elec_heating == 'Elec', 'PUPRN']) &
  (annual_energy$PUPRN %in% epc[epc$any_unmetered == 'Metered', 'PUPRN'])
annual_energy[logic, c('energy_use_cost')] = 365*annual_energy[logic, c('Clean_elec_net_kWh_d_mean')]*elec_price_cap
annual_energy[logic, c('total_cost')] = annual_energy[logic, c('energy_use_cost')] + annual_elec_standing_charge


# Take all days, not just weekends or weekdays
annual_energy = annual_energy[annual_energy$weekday_weekend == 'both', ]

# merge total_primary with epc
epc = merge(epc, annual_energy[c("PUPRN", "total_primary", "total_cost", "energy_use_cost",
                                 "Clean_elec_net_kWh_d_mean", "Clean_gas_kWh_d_mean")], by = "PUPRN")
epc$metered_primary_eui= epc$total_primary/epc$totalFloorArea

# process the lodgement data so we can plot different groups
epc$lodgementDate_str = epc$lodgementDate
epc$lodgementDate = as.Date(epc$lodgementDate, "%d/%m/%Y")
epc$lodgement_year = format(epc$lodgementDate, format = "%Y")

epc$lodgement_year_banded = NA
# solid wall u value was updated in RdSAP 9.93 in November 2017 so separating before and after. 
# primary energy changed in 2209, 2012 and will change in SAP10. 
epc[epc$lodgement_year == '2008', 'lodgement_year_banded'] = 'Before 2009'
epc[grep(paste(c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017'), collapse = "|"), epc$lodgement_year), 'lodgement_year_banded'] = '2009-2018'
epc[grep(paste(c('2018', '2019', '2020', '2021', '2022'), collapse = "|"), epc$lodgement_year), 'lodgement_year_banded'] = '2018 onwards'

epc['new_home_epc'] = 'Other EPC reason'
epc[epc$transactionType == 'new dwelling', 'new_home_epc'] = 'New home'

#####
# save this merged dataset for analysis of energy consumption 
write.csv(epc, paste(save_data_loc, 'Merged_epc_survey_2021_energy_use.csv', sep = ""), row.names = FALSE)
# load file if we're not adding new columns
epc = read.csv(paste(save_data_loc, "Merged_epc_survey_2021_energy_use.csv", sep = ""))

# load the processed data
epc = read.csv(paste(save_data_loc, "Merged_epc_survey_2021_energy_use.csv", sep = ""))


#####
# choose only a relevant subset of data to continue
plot_data = epc[!is.na(epc$metered_primary_eui), ]
plot_data = plot_data[plot_data$totalFloorArea >20 & plot_data$totalFloorArea < 500, ]

plot_data = plot_data[plot_data$any_unmetered == 'Metered', ]

plot_data = plot_data[(plot_data$has_PV == 'No'), ]

plot_data = plot_data[(plot_data$has_EV == 'No'), ]

plot_data = plot_data[(plot_data$Region != 'SCOTLAND'), ]

# self-contained homes
plot_data = plot_data[(plot_data$self_contained == 'Yes'), ]
# remove electrically heated homes
plot_data = plot_data[plot_data$elec_heating == 'Not elec', ]

# don't take cases where the epc doesn't say gas but we have gas data
logic = (plot_data$mainheat_recode != 'Gas') & (!is.na(plot_data$Clean_gas_kWh_d_mean))
plot_data = plot_data[!logic,]
# only take cases where the survey and the epc agree over the heating
logic = plot_data$mainheat_recode == plot_data$survey_heating
plot_data = plot_data[logic,]

plot_data$IMD_quintile = as.factor(plot_data$IMD_quintile)
plot_data$epc_tot_peui = plot_data$energyConsumptionCurrent + plot_data$appliance_peui_eleccook

# only take post SAP 2012 data. SAP2012 came in in April / end of July 2014 for England/Wales
# take data from Wales from September 2014 and from England from May onward
logic = plot_data$Region == 'Wales' & plot_data$lodgementDate > as.Date('2014-08-31')
logic = logic | (plot_data$Region != 'Wales' & plot_data$lodgementDate > as.Date('2014-04-30'))
plot_data = plot_data[logic,]

#occupancy - 21 deg or above, all space normally heated, not financially 
# struggling, SAP occupancy agreement, normally comfortably warm
logic = plot_data$set_point_1 == '20.5 to 21.5'
sum(logic) 
logic = logic & (plot_data$has_unheated_spaces == "All space heated")
sum(logic)
logic = logic & (plot_data$managing_financially %in% 
                   c("Doing alright", "Just about getting by", "Living comfortably"))
sum(logic)
logic = logic & (((abs(plot_data$diff_occupants) < 0.5) & !is.na(plot_data$diff_occupants)))
sum(logic)
logic = logic & (plot_data$comfortably_warm == "Yes" & !is.na(plot_data$comfortably_warm)) 
sum(logic)

plot_data['match_sap_occupancy'] = 'Do not match'
plot_data[logic, 'match_sap_occupancy'] = 'Match'


# save prepared data for further analysis
write.csv(plot_data, paste(save_data_loc, 'Metered_combined_dt.csv', sep = ""), row.names = FALSE)
