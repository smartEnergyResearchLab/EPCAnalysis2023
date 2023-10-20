# June 2022 - Jessica Few, jessica.few@ucl.ac.uk

# This script is for comparing the SAP assumed weather to 2022 weather for 
# SERL Observatory participants with an EPC
# results of this analysis are published in this paper: https://doi.org/10.1016/j.enbuild.2023.113024


# load some libraries
library(ggplot2)
library(dplyr)
library(reshape)

# load the data

observatory_loc = ""
participant = read.csv(paste(observatory_loc, "serl_participant_summary_edition04.csv", sep = ""))

energy_loc = ""
annual_energy = read.csv(paste(energy_loc, "Annual_report_sm_annual_mean_daily_consumption_2021.csv", sep = ""))
monthly_energy_orig = read.csv(paste(energy_loc, "Annual_report_sm_monthly_mean_daily_consumption_wind_solar2021.csv", sep = ""))

# take all days, not only weekend or weekday
monthly_energy = monthly_energy_orig[monthly_energy_orig$weekday_weekend == 'both', ]

processed_epc_data_loc = ""
epc_merge_orig =  read.csv(paste(processed_epc_data_loc, "Merged_epc_survey_2021_energy_use.csv", sep = ""))
epc_merge = epc_merge_orig[is.na(epc_merge_orig$metered_primary_eui), ]

sap_temp = read.csv(paste(processed_epc_data_loc, "SAP_temperatures.csv", sep = ""))
sap_temp = dplyr::rename(sap_temp, Region_sap = Region)
save_data_loc = ""

# rearrange data so have monthly mean temperatures per region 

monthly_energy = merge(monthly_energy_orig, participant[c("PUPRN", "Region", "IMD_quintile")], by = "PUPRN")

# only homes with enough energy data report the mean external temperature...
# let's just take homes that have an EPC and enough energy data for an annual 
# energy use measurement

monthly_energy = monthly_energy[monthly_energy[,c('PUPRN')] %in% epc_merge[, c('PUPRN')],]

monthly_region_temp = monthly_energy %>% group_by(Region, month_local_time) %>% 
  summarise_at(vars(temp_weighted_C), funs(mean(., na.rm= TRUE)))

monthly_temp = monthly_energy %>% group_by(month_local_time) %>% 
  summarise_at(vars(temp_weighted_C, solar_weighted_C), funs(mean(., na.rm= TRUE)))


dict_to_match = c("South East England"="SOUTH EAST", "South West England"="SOUTH WEST", 
                  "Midlands"="WEST MIDLANDS", "North West England"="NORTH WEST", 
                  "North East England"="YORKSHIRE", "East Pennines" = "EAST MIDLANDS", 
                  "East Anglia" = "EAST OF ENGLAND", "Wales" = "WALES", 
                  "West Scotland" = "SCOTLAND", "Thames" = "GREATER LONDON", 
                  "Borders" = "NORTH EAST")

sap_temp$Region = dict_to_match[sap_temp$Region_sap]

monthly_region_temp = merge(monthly_region_temp, sap_temp, by = c("Region", "month_local_time"))

plot_data = melt(monthly_region_temp[,c('Region', 'SAP_temp', 'month_local_time', 'Region_sap', 'temp_weighted_C')], 
                 id.vars = c('Region', 'Region_sap', 'month_local_time'))
ggplot(plot_data, aes(x = month_local_time, y = value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(vars(Region))


# create a plot of the UK mean external temperature for SAP and for SERL in 2021
monthly_temp = merge(monthly_temp, sap_temp[sap_temp$Region_sap == 'UK average', ], by = 'month_local_time')

plot_data = melt(monthly_temp[c('month_local_time', 'temp_weighted_C', 'SAP_temp')], 
                 id.vars = c('month_local_time'))

temp_2021_figure = ggplot(plot_data, aes(x = month_local_time, y = value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_viridis_d(begin = 0.1, end = 0.5, option = 'viridis', name = '', labels = c('SERL average in 2021', 'SAP UK average')) +
  #scale_fill_discrete() + 
  xlab('Month') + ylab(expression(paste('Mean external temperature (', ~degree, 'C)', sep=''))) +
  theme_light() +
  coord_cartesian(ylim = c(0, 17.5), xlim = c(0.5, 12.5), expand = FALSE) +
  theme(legend.position = 'none') + scale_x_continuous(breaks = 1:12)
print(temp_2021_figure)


# plot for GHI
plot_data = melt(monthly_temp[c('month_local_time', 'solar_weighted_C', 'SAP_GHI')], 
                 id.vars = c('month_local_time'))

ghi_2021_figure = ggplot(plot_data, aes(x = month_local_time, y = value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_viridis_d(begin = 0.1, end = 0.5, option = 'viridis', name = '', labels = c('SERL average in 2021', 'SAP UK average')) +
  xlab('Month') + ylab(expression(paste('Mean global horizontal irradiance (W/', m^2, ')', sep=''))) +
  theme_light() +
  coord_cartesian(ylim = c(0, 225), xlim = c(0.5, 12.5), expand = FALSE) +
  theme(legend.position = 'none') + scale_x_continuous(breaks = 1:12)
legend_GHI = get_legend(ghi_2021_figure + theme(legend.position = c(1,0.6), legend.direction = 'horizontal', 
                                                legend.justification = 'center', 
                                                legend.box.just = 'bottom'))
print(ghi_2021_figure)


# a part a and b plot
figure_combined = plot_grid(temp_2021_figure, ghi_2021_figure, legend_GHI, rel_heights = c(1, .1)) 
print(figure_combined)

monthly_temp %>% dplyr::rename(solar_weighted = solar_weighted_C, Region_serl = Region)
monthly_temp$Region_serl = 'Average'


mean(monthly_region_temp$temp_weighted_C)
mean(monthly_region_temp$SAP_temp)
mean(monthly_temp$solar_weighted_C)
mean(monthly_temp$SAP_GHI)

# check for October - March
monthly_temp_oct_mar = monthly_temp[(monthly_temp$month_local_time<4) | (monthly_temp$month_local_time>9), ]
mean(monthly_temp_oct_mar$temp_weighted_C)
mean(monthly_temp_oct_mar$SAP_temp)
mean(monthly_temp_oct_mar$solar_weighted_C)
mean(monthly_temp_oct_mar$SAP_GHI)



