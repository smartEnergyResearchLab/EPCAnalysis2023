# June 2022, Jessica Few, jessica.few@ucl.ac.uk

# define some functions we can use to plot the data later
# results of this analysis are published in this paper: https://doi.org/10.1016/j.enbuild.2023.113024

library(viridisLite)
library(cowplot)

order_categoricals = c("A and B", "C", "D", "E", "F and G",
                       "Less than 17.5", "17.5 to 18.5","18.5 to 19.5", "19.5 to 20.5", 
                       "20.5 to 21.5", "21.5 to 22.5", "22.5 to 23.5",
                       "17.5 to 19.5", "19.5 to 21.5", "21.5 to 23.5", "23.5 or above",
                       "Less than 50", "50 to 100", "100 to 150","150 to 200","More than 200",
                       "Before 1900", "1900 - 1929", "1930 - 1949",  "1950 - 1975", 
                       "1976 - 1990", "1991 - 2002", "2003 onwards",
                       "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                       "2016", "2017", "2018", "2019", "2020", "2021",
                       "2008-2009", "2010-2012", "2013-2015", "2016-2018", "2019-2021",
                       "Before 2009", "2009-2014", "2009-2018", "2015 onwards", "2018 onwards",
                       "Elec", "Not elec", 
                       "Yes", "No",
                       "Match", "Do not match",
                       "1", "2", "3", "4", "5", ">=5", ">=6", '6 - 7',
                       "Very Good", "Good", "Average", "Poor", "Very poor", 
                       "Survey less than SAP", "Agree", "Survey more than SAP",
                       "New home",'assessment for green deal', 'ECO assessment', 'FiT application',
                       'following green deal', 'RHI application', "Other EPC reason",
                       "Unknown or not applicable", "Unknown or no answer",
                       "total_suppressed_for_SDC", 'No data or > 7')

sdc_safe_quantile = function(data, quantile){
  abs_diff = abs(data - quantile(data, quantile))
  new_data = data[order(abs_diff)]
  result = mean(head(new_data, 10))
  return(result)
}


bar_chart_freq = function(data, seg_var_1, seg_var_2){
  grouping_freq = count(data, {{seg_var_1}}, {{seg_var_2}})
  ggplot(grouping_freq, aes(fill = {{seg_var_2}}, y = n, x = {{seg_var_1}})) +
    geom_bar(position = 'stack', stat = "identity")
}

line_plot_by_group_with_scatter = function(data, seg_var_1, epc_model_var){
  ggplot() + 
    geom_point(data = data, aes(x = {{epc_model_var}},y =metered_primary_eui, 
                                color = {{seg_var_1}}), alpha = I(0.1)) +
    geom_smooth(data = data, aes(x = {{epc_model_var}},y =metered_primary_eui, 
                                 color = {{seg_var_1}}), alpha = 0.2, method= 'lm', 
                formula = y~x) +
    geom_abline(aes(intercept = 0, slope = 1))
  
}


get_group_model_summary = function(plot_data, seg_var, seg, model = 'diff'){
  
  plot_data_group = plot_data %>% filter({{seg_var}} == seg)
  if (model == 'diff'){
    group_model = lm((metered_primary_eui - epc_tot_peui) ~ epc_tot_peui, plot_data_group)
  }
  else{
    group_model = lm(metered_primary_eui ~ epc_tot_peui, plot_data_group)
  }
  
  group = c(seg)
  intercept = c(summary(group_model)$coefficients[1])
  gradient_epc_tot_peui = c(summary(group_model)$coefficients[2])
  p_val_intercept = c(summary(group_model)$coefficients[7])
  p_val_gradient = c(summary(group_model)$coefficients[8])
  conf_lower_grad = confint(group_model)[2]
  conf_upper_grad = confint(group_model)[4]
  conf_lower_int = confint(group_model)[1]
  conf_upper_int = confint(group_model)[3]
  adj_r_squared = c(summary(group_model)$adj.r.squared)
  rmse = sqrt(c(crossprod(group_model$residuals))/length(group_model$residuals))
  n_obs = c(nrow(plot_data_group))
  mean_10_lowest_epc_peui = plot_data_group %>% slice_min(epc_tot_peui, n= 10) %>% summarise(mean_10_lowest_epc_peui = mean(epc_tot_peui))
  mean_10_highest_epc_peui = plot_data_group %>% slice_max(epc_tot_peui, n= 10) %>% summarise(mean_10_highest_epc_peui = mean(epc_tot_peui))
  
  
  group_model_summary = data.frame(model, group, intercept, conf_lower_int, conf_upper_int, 
                                   p_val_intercept, 
                                   gradient_epc_tot_peui, conf_lower_grad, conf_upper_grad, 
                                   p_val_gradient, 
                                   adj_r_squared, rmse, mean_10_lowest_epc_peui, mean_10_highest_epc_peui,
                                   n_obs)
  # if there are exactly 10 observations in a group, then make the range between highest and lowest peui = 50
  group_model_summary[group_model_summary$n_obs == 10, 'mean_10_lowest_epc_peui'] = group_model_summary[group_model_summary$n_obs == 10, 'mean_10_lowest_epc_peui'] - 25
  group_model_summary[group_model_summary$n_obs == 10, 'mean_10_highest_epc_peui'] = group_model_summary[group_model_summary$n_obs == 10, 'mean_10_lowest_epc_peui'] + 25
  
  
  return(group_model_summary)
}

get_all_groups_summary = function(plot_data, seg_var, model = 'diff'){
  
  all_groups_summary = data.frame()
  for (group in unique((plot_data %>% select({{seg_var}}))[,1])){
    group_model_summary = get_group_model_summary(plot_data, {{seg_var}}, group, model)
    all_groups_summary = rbind(all_groups_summary, group_model_summary)
  }
  return(all_groups_summary)
}

line_plot_for_exporting_diff_basic = function(plot_data, seg_var, title_string){
  all_groups_summary_diff = get_all_groups_summary(plot_data, {{seg_var}})
  save_name_stem = paste("Regression peui diff against epc peui by ", title_string, sep = "")
  
  line_plot_data_diff = data.frame()
  for (group_val in unique((plot_data %>% select({{seg_var}}))[,1])){
    group_summary = all_groups_summary_diff %>% filter(group == group_val)
    print(group_val)
    print(group_summary)
    x1 = group_summary$mean_10_lowest_epc_peui
    x2 = group_summary$mean_10_highest_epc_peui
    y1 = group_summary$gradient_epc_tot_peui*x1 + group_summary$intercept
    y2 = group_summary$gradient_epc_tot_peui*x2 + group_summary$intercept
    groups = c(group_val, group_val)
    epc_tot_peui = c(x1, x2)
    metered_peui = c(y1, y2)
    line_plot_data_group = data.frame(groups, epc_tot_peui, metered_peui)
    line_plot_data_diff = rbind(line_plot_data_diff, line_plot_data_group)
  }
  
  line_plot_data_diff = line_plot_data_diff %>% mutate(across(groups, factor, levels = order_categoricals))
  # don't plot the suppressed or don't know responses
  line_plot_data_diff = filter(line_plot_data_diff,(groups!= 'Unknown or no answer') & 
                            (groups!= 'Unknown or not applicable')& (groups!= 'No data or > 7'))
  line_plot_data_diff$groups = factor(line_plot_data_diff$groups, levels = order_categoricals, ordered = TRUE)
  
  
  if (length(unique((plot_data %>% select({{seg_var}}))[,1])) == 2){
   cmap_begin = 0.1
   cmap_end = 0.5
  }
  else {
    cmap_begin = 0
    cmap_end = 0.9
  }
  
  figure_diff = ggplot() + 
    theme_light() +
    geom_abline(aes(intercept = 0, slope = 0), alpha= 0.5) +
    geom_smooth(data = line_plot_data_diff, aes(x = epc_tot_peui,y =metered_peui, 
                                           color = groups, linetype = groups), alpha = 0.2, method= 'lm', 
                formula = y~x, se = F) +
    scale_colour_viridis_d(begin = cmap_begin, end = cmap_end, option = 'viridis') + 
    scale_linetype_manual(values = rep(c('solid', 'twodash'), length(unique(line_plot_data_diff$groups))/2 + 1)) + 
    coord_cartesian(ylim = c(-400, 200), xlim = c(100, 700), expand = FALSE) +
    xlab(~paste("EPC modelled PEUI (kWh/ year/ ", m^2, ")")) + 
    ylab(~paste(Delta, 'PEUI (kWh/ year/ ', m^2, ')')) +
    labs(colour = title_string, linetype = title_string) + 
    theme(legend.position = 'none' , plot.margin = margin(5, 10, 0, 0))
  
  
  #### get data for the metered against modelled plot
  all_groups_summary_basic = get_all_groups_summary(plot_data, {{seg_var}}, model = 'basic')
  save_name_stem = paste("Regression metered against epc peui by ", title_string, sep = "")
  all_models_summary = rbind(all_groups_summary_basic, all_groups_summary_diff)
  write.csv(all_models_summary, paste(save_outputs_loc, save_name_stem, " - summary data.csv", sep = ""), row.names = FALSE)
  
  line_plot_data_basic = data.frame()
  for (group_val in unique((plot_data %>% select({{seg_var}}))[,1])){
    group_summary = all_groups_summary_basic %>% filter(group == group_val)
    print(group_val)
    print(group_summary)
    x1 = group_summary$mean_10_lowest_epc_peui
    x2 = group_summary$mean_10_highest_epc_peui
    y1 = group_summary$gradient_epc_tot_peui*x1 + group_summary$intercept
    y2 = group_summary$gradient_epc_tot_peui*x2 + group_summary$intercept
    groups = c(group_val, group_val)
    epc_tot_peui = c(x1, x2)
    metered_peui = c(y1, y2)
    line_plot_data_group = data.frame(groups, epc_tot_peui, metered_peui)
    line_plot_data_basic = rbind(line_plot_data_basic, line_plot_data_group)
  }
  
  
  line_plot_data_basic = line_plot_data_basic %>% mutate(across(groups, factor, levels = order_categoricals))
  # don't plot the suppressed or don't know responses
  line_plot_data_basic = filter(line_plot_data_basic,(groups!= 'Unknown or no answer') & 
                                 (groups!= 'Unknown or not applicable')& (groups!= 'No data or > 7'))
  line_plot_data_basic$groups = factor(line_plot_data_basic$groups, levels = order_categoricals, ordered = TRUE)
  
  
  figure_basic = ggplot() + 
    theme_light() +
    geom_abline(aes(intercept = 0, slope = 1), alpha = 0.5) +
    geom_smooth(data = line_plot_data_basic, aes(x = epc_tot_peui,y =metered_peui, 
                                                color = groups, linetype = groups), alpha = 0.2, method= 'lm', 
                formula = y~x, se = F) +
    scale_colour_viridis_d(begin = cmap_begin, end = cmap_end, option = 'viridis') + 
    scale_linetype_manual(values = rep(c('solid', 'twodash'), length(unique(line_plot_data_basic$groups))/2 + 1)) + 
    coord_cartesian(ylim = c(100, 700), xlim = c(100, 700), expand = FALSE) +
    xlab(~paste("EPC modelled PEUI (kWh/ year/ ", m^2, ")")) + 
    ylab(bquote(PEUI~from~metered~energy~use~(kWh/year/m^2))) +
    labs(colour = title_string, linetype = title_string) +
    theme(legend.position = 'none', plot.margin = margin(5, 10, 0, 0))
    
  
  legend_basic = get_legend(figure_basic + theme(legend.position = c(1,0.5), legend.direction = 'horizontal', 
                                                  legend.justification = 'center', 
                                                  legend.box.just = 'bottom'))
  
  figure_combined = plot_grid(figure_basic, figure_diff, legend_basic, rel_heights = c(1, .15)) 
  print(figure_combined)
  fig_save_name = paste("Regressions metered and diff against epc peui by ", title_string, '.png', sep = "")
  ggsave(fig_save_name, path = save_outputs_loc, device = png,
                 width = 8, height = 5, units = "in", dpi = 450)
  
  
  return(all_models_summary)
  
}


line_plot_for_exporting = function(plot_data, seg_var, title_string){
  all_groups_summary = get_all_groups_summary(plot_data, {{seg_var}})
  save_name_stem = paste("Regression metered against epc peui by ", title_string, sep = "")
  write.csv(all_groups_summary, paste(save_outputs_loc, save_name_stem, " - summary data.csv", sep = ""), row.names = FALSE)
  
  line_plot_data = data.frame()
  for (group_val in unique((plot_data %>% select({{seg_var}}))[,1])){
    group_summary = all_groups_summary %>% filter(group == group_val)
    print(group_val)
    print(group_summary)
    x1 = group_summary$mean_10_lowest_epc_peui
    x2 = group_summary$mean_10_highest_epc_peui
    y1 = group_summary$gradient_epc_tot_peui*x1 + group_summary$intercept
    y2 = group_summary$gradient_epc_tot_peui*x2 + group_summary$intercept
    groups = c(group_val, group_val)
    epc_tot_peui = c(x1, x2)
    metered_peui = c(y1, y2)
    line_plot_data_group = data.frame(groups, epc_tot_peui, metered_peui)
    line_plot_data = rbind(line_plot_data, line_plot_data_group)
  }
  
  line_plot_data = line_plot_data %>% mutate(across(groups, factor, levels = order_categoricals))
  # don't plot the suppressed or don't know responses
  line_plot_data = filter(line_plot_data,(groups!= 'Unknown or no answer') & 
                            (groups!= 'Unknown or not applicable') & (groups!= 'No data or > 7'))
  
  if (length(unique((plot_data %>% select({{seg_var}}))[,1])) == 2){
    cmap_begin = 0.3
    cmap_end = 0.7
  }
  else {
    cmap_begin = 0
    cmap_end = 0.9
  }
  
  figure = ggplot() + 
    geom_smooth(data = line_plot_data, aes(x = epc_tot_peui,y =metered_peui, 
                                           color = groups, linetype = groups), alpha = 0.2, method= 'lm', 
                formula = y~x, se = F) +
    scale_colour_viridis_d(begin = cmap_begin, end = cmap_end, option = 'viridis') + 
    scale_linetype_manual(values = rep(c('solid', 'longdash'), length(unique(line_plot_data$groups))/2 + 1)) + 
    geom_abline(aes(intercept = 0, slope = 0)) +
    xlab(~paste("EPC modelled PEUI (kWh/ year/ ", m^2, ")")) + 
    ylab(~paste(Delta, 'PEUI (kWh/ year/ ', m^2, ')')) +
    labs(colour = title_string, linetype = title_string) + 
    theme_light()
  print(figure)
  ggsave(paste(save_name_stem, '.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 6, units = "in", dpi = 450)
  
  return(line_plot_data)
  
}



line_plot_for_exporting_single_group = function(plot_data){
  group_model = lm((metered_primary_eui - epc_tot_peui) ~ epc_tot_peui, plot_data)
  model = 'difference against epc'
  group = 'all'
  intercept = c(summary(group_model)$coefficients[1])
  gradient_epc_tot_peui = c(summary(group_model)$coefficients[2])
  p_val_intercept = c(summary(group_model)$coefficients[7])
  p_val_gradient = c(summary(group_model)$coefficients[8])
  conf_lower_grad = confint(group_model)[2]
  conf_upper_grad = confint(group_model)[4]
  conf_lower_int = confint(group_model)[1]
  conf_upper_int = confint(group_model)[3]
  adj_r_squared = c(summary(group_model)$adj.r.squared)
  rmse = sqrt(c(crossprod(group_model$residuals))/length(group_model$residuals))
  n_obs = c(nrow(plot_data))
  mean_10_lowest_epc_peui = plot_data %>% slice_min(epc_tot_peui, n= 10) %>% summarise(mean_10_lowest_epc_peui = mean(epc_tot_peui))
  mean_10_highest_epc_peui = plot_data %>% slice_max(epc_tot_peui, n= 10) %>% summarise(mean_10_highest_epc_peui = mean(epc_tot_peui))
  
  diff_model_summary = data.frame(model, group, intercept, conf_lower_int, conf_upper_int, 
                                   p_val_intercept, 
                                   gradient_epc_tot_peui, conf_lower_grad, conf_upper_grad, 
                                   p_val_gradient, 
                                   adj_r_squared, rmse, mean_10_lowest_epc_peui, mean_10_highest_epc_peui,
                                   n_obs)
  
  group_model = lm((metered_primary_eui) ~ epc_tot_peui, plot_data)
  model = 'metered against epc'
  group = 'all'
  intercept = c(summary(group_model)$coefficients[1])
  gradient_epc_tot_peui = c(summary(group_model)$coefficients[2])
  p_val_intercept = c(summary(group_model)$coefficients[7])
  p_val_gradient = c(summary(group_model)$coefficients[8])
  conf_lower_grad = confint(group_model)[2]
  conf_upper_grad = confint(group_model)[4]
  conf_lower_int = confint(group_model)[1]
  conf_upper_int = confint(group_model)[3]
  adj_r_squared = c(summary(group_model)$adj.r.squared)
  rmse = sqrt(c(crossprod(group_model$residuals))/length(group_model$residuals))
  n_obs = c(nrow(plot_data))
  mean_10_lowest_epc_peui = plot_data %>% slice_min(epc_tot_peui, n= 10) %>% summarise(mean_10_lowest_epc_peui = mean(epc_tot_peui))
  mean_10_highest_epc_peui = plot_data %>% slice_max(epc_tot_peui, n= 10) %>% summarise(mean_10_highest_epc_peui = mean(epc_tot_peui))
  
  basic_model_summary = data.frame(model, group, intercept, conf_lower_int, conf_upper_int, 
                                   p_val_intercept, 
                                   gradient_epc_tot_peui, conf_lower_grad, conf_upper_grad, 
                                   p_val_gradient, 
                                   adj_r_squared, rmse, mean_10_lowest_epc_peui, mean_10_highest_epc_peui,
                                   n_obs)
  all_model_summary = rbind(basic_model_summary, diff_model_summary)
  
  
  save_name_stem = "Regression metered against epc peui"
  write.csv(all_model_summary, paste(save_outputs_loc, save_name_stem, " - summary data.csv", sep = ""), row.names = FALSE)
  
  
  x1 = basic_model_summary$mean_10_lowest_epc_peui
  x2 = basic_model_summary$mean_10_highest_epc_peui
  y1 = basic_model_summary$gradient_epc_tot_peui*x1 + basic_model_summary$intercept
  y2 = basic_model_summary$gradient_epc_tot_peui*x2 + basic_model_summary$intercept
  epc_tot_peui = c(x1, x2)
  metered_peui = c(y1, y2)
  line_plot_data_group = data.frame(epc_tot_peui, metered_peui)

  figure_basic = ggplot() + theme_light() +
    geom_abline(aes(intercept = 0, slope = 1), alpha = 0.5) +
    geom_smooth(data = line_plot_data_group, aes(x = epc_tot_peui,y =metered_peui), alpha = 0.2, method= 'lm', 
                formula = y~x, se = F, color = viridis(3)[2]) +
    coord_cartesian(ylim = c(100, 700), xlim = c(100, 700), expand = FALSE) +
    #xlim(0, 850) + ylim(0,850) +
    #scale_colour_viridis_d() + 
    xlab("EPC modelled PEUI"~(kWh/ year/ m^2)) + 
    ylab(bquote(PEUI~from~metered~energy~use~(kWh/year/m^2))) +
    labs(colour = "") + theme(legend.position = 'none' , plot.margin = margin(5, 10, 0, 0))
  print(figure_basic)
  ggsave(paste(save_name_stem, '.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 6, units = "in", dpi = 450)
    
  x1 = diff_model_summary$mean_10_lowest_epc_peui
  x2 = diff_model_summary$mean_10_highest_epc_peui
  y1 = diff_model_summary$gradient_epc_tot_peui*x1 + diff_model_summary$intercept
  y2 = diff_model_summary$gradient_epc_tot_peui*x2 + diff_model_summary$intercept
  epc_tot_peui = c(x1, x2)
  metered_peui = c(y1, y2)
  line_plot_data_group = data.frame(epc_tot_peui, metered_peui)
  figure_diff = ggplot() + theme_light() +
    geom_abline(aes(intercept = 0, slope = 0), alpha= 0.5) +
    geom_smooth(data = line_plot_data_group, aes(x = epc_tot_peui,y =metered_peui), alpha = 0.2, method= 'lm', 
                formula = y~x, se = F, color = viridis(3)[2]) +
    coord_cartesian(xlim = c(100, 700), ylim = c(-400, 200), expand = FALSE) +
    #scale_colour_viridis_d() + 
    xlab(~paste("EPC modelled PEUI (kWh/ year/ ", m^2, ")")) + 
    ylab(~paste(Delta, 'PEUI (kWh/ year/ ', m^2, ')')) +
    labs(colour = "") + theme(legend.position = 'none' , plot.margin = margin(5, 10, 0, 0))
  print(figure_diff)
  ggsave(paste(save_name_stem, 'diff.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 6, units = "in", dpi = 450)
  
  figure_combined = plot_grid(figure_basic, figure_diff)
  print(figure_combined)
  ggsave(paste(save_name_stem, 'both subplots.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 4, units = "in", dpi = 450)
  
  return(all_model_summary)
  
  
}



box_plot_2groups = function(plot_data, seg_var, facet_var, title_var, epc_model_var){
  summary_data = plot_data %>%
    group_by({{facet_var}}, {{seg_var}}) %>%
    summarise(mean_epc_peui = mean({{epc_model_var}}),
              sd_epc_peui = sd({{epc_model_var}}),
              mean_serl_peui = mean(metered_primary_eui),
              sd_serl_peui = sd(metered_primary_eui),
              mean_diff_peui = mean(metered_primary_eui - {{epc_model_var}}), 
              sd_diff_peui = sd(metered_primary_eui - {{epc_model_var}}),
              mean_perc_diff_peui = 100*mean((metered_primary_eui - {{epc_model_var}})/{{epc_model_var}}), 
              sd_perc_diff_peui = 100*sd((metered_primary_eui - {{epc_model_var}})/{{epc_model_var}}),
              n_obs = n())
  print(summary_data)
  
  plot_data_reform = plot_data %>% mutate(across({{facet_var}}, factor, levels = order_categoricals))
  
  figure = ggplot(plot_data_reform, aes(x = {{seg_var}}))+
    geom_boxplot(aes(y = metered_primary_eui, color = "darkslategray"), width = 0.2, position = position_nudge(x = +0.1)) +
    geom_boxplot(aes(y = epc_tot_peui, color = "darkslategray4"), width = 0.2, position = position_nudge(x = -0.1)) +
    facet_wrap(vars({{facet_var}})) + theme(legend.position = 'bottom') +
    scale_color_manual(name = '', values = c("darkslategray4"="darkslategray4", 
                                             "darkslategray" = "darkslategray"), 
                       labels = c("EPC (incl appliances)", "SERL"))
  print(figure)
  
  
  
  figure = ggplot(plot_data_reform, aes(x = {{seg_var}}))+
    geom_boxplot(aes(y = metered_primary_eui - epc_tot_peui, color = "darkslategray"), width = 0.4, colour = 'deepskyblue4') +
    xlab("EPC Energy Efficiency Rating")  + ylab(bquote('Difference in Primary Energy Use Intensity (metered - SAP)'~(kWh/ year/ m^2))) +
    facet_wrap(vars({{facet_var}})) + theme(legend.position = 'bottom')
  
  print(figure)
  
}


mean_plot_by_1group = function(plot_data, seg_var, title_var, epc_model_var){
  plot_data_reform = plot_data %>%  group_by({{seg_var}}) %>%
    summarise(mean_epc_peui = mean({{epc_model_var}}),
              sd_epc_peui = sd({{epc_model_var}}),
              sterr_epc_peui = sd({{epc_model_var}})/sqrt(n()),
              mean_serl_peui = mean(metered_primary_eui),
              sd_serl_peui = sd(metered_primary_eui),
              sterr_serl_peui = sd(metered_primary_eui)/sqrt(n()),
              mean_diff_peui = mean(metered_primary_eui - {{epc_model_var}}), 
              sd_diff_peui = sd(metered_primary_eui - {{epc_model_var}}),
              sterr_diff_peui = sd(metered_primary_eui - {{epc_model_var}})/sqrt(n()),
              mean_perc_diff_peui = 100*mean((metered_primary_eui - {{epc_model_var}})/{{epc_model_var}}), 
              sd_perc_diff_peui = 100*sd((metered_primary_eui - {{epc_model_var}})/{{epc_model_var}}),
              sterr_perc_diff_peui = 100*sd((metered_primary_eui - {{epc_model_var}})/{{epc_model_var}})/sqrt(n()),
              n_obs = n())
  print(plot_data_reform)
  
  
  groups_less_than_10 = unique(plot_data_reform[plot_data_reform$n_obs<10,]$currentEnergyRatingMerge)

  logic = (plot_data_reform$n_obs < 10)
  n_supressed = sum(plot_data_reform[logic,]$n_obs)
  plot_data_reform = as_tibble(plot_data_reform)
  if (n_supressed <= 10) {
    group_logic = plot_data_reform$currentEnergyRatingMerge == group
    more_than_10_logic = plot_data_reform$n_obs > 10
    logic = logic + (plot_data_reform$n_obs == min(plot_data_reform[more_than_10_logic, 'n_obs']))
      
    n_supressed = sum(plot_data_reform[logic,]$n_obs)
    print(n_supressed)
  }
  plot_data_reform = plot_data_reform %>% filter(!logic)
  plot_data_reform = plot_data_reform %>% add_row({{seg_var}} := 'total_suppressed_for_SDC',
                                          n_obs = n_supressed)
  
  print(plot_data_reform)
  save_name_stem = "EPC band "
  write.csv(plot_data_reform, paste(save_outputs_loc, save_name_stem, " - summary data.csv", sep = ""), row.names = FALSE)
  
  
  # don't plot the suppressed or don't know responses
  plot_data_reform = filter(plot_data_reform, ({{seg_var}} != "total_suppressed_for_SDC") & 
                              ({{seg_var}}!= 'Unknown or no answer') & ({{seg_var}}!= 'Unknown or not applicable'))
  
  
  
  figure = ggplot(plot_data_reform, aes(x = {{seg_var}}))+ theme_light()+
    geom_pointrange(aes(y =mean_epc_peui, ymin = mean_epc_peui- sterr_epc_peui, 
                        ymax =mean_epc_peui+ sterr_epc_peui, color = "turquoise4"), position = position_nudge(x = -0.05), 
                    shape = 18) + 
    geom_pointrange(aes(y =mean_serl_peui, ymin = mean_serl_peui- sterr_serl_peui, 
                        ymax =mean_serl_peui+ sterr_serl_peui, color = "midnightblue"), position = position_nudge(x = 0.05), 
                    shape = 18) +
    scale_color_manual(name = '', values = c("turquoise4"="turquoise4", 
                                             "midnightblue" = "midnightblue"), 
                       labels = c("EPC", "SERL")) +
    xlab("EPC Energy Efficiency Rating")  + ylab(bquote('Primary Energy Use Intensity '~(kWh/ year/ m^2))) +
    theme(legend.position = 'bottom') + coord_cartesian(ylim = c(100, 700), xlim = c(0.5, 5.5), expand = FALSE)
  print(figure)
  ggsave(paste(save_name_stem, '- EPC and metered PEUI.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 6, units = "in", dpi = 450)
  
  
  
  figure = ggplot(plot_data_reform, aes(x = {{seg_var}}))+ theme_light() +
    geom_abline(slope = 0, intercept = 0, color = 'grey60', size = 0.5) +
    geom_pointrange(aes(y =mean_perc_diff_peui, ymin = mean_perc_diff_peui- sterr_perc_diff_peui, 
                        ymax =mean_perc_diff_peui+ sterr_perc_diff_peui, color = 'midnightblue'), 
                    shape = 18, show.legend = FALSE) +
    scale_color_manual(name = '', values = c("midnightblue" = "midnightblue"), labels = "") +
    xlab("EPC Energy Efficiency Rating")  + ylab(bquote('Percent Difference in Primary Energy Use Intensity (%)'))
  print(figure)
  ggsave(paste(save_name_stem, '- percent difference PEUI.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 6, units = "in", dpi = 450)

  return(plot_data_reform)
}


mean_plot_by_2groups = function(plot_data, seg_var, facet_var, title_var, epc_model_var){
  summary_data = plot_data %>%
    group_by({{facet_var}}, {{seg_var}}) %>%
    summarise(mean_epc_peui = mean({{epc_model_var}}),
              sd_epc_peui = sd({{epc_model_var}}),
              mean_serl_peui = mean(metered_primary_eui),
              sd_serl_peui = sd(metered_primary_eui),
              mean_diff_peui = mean(metered_primary_eui - ({{epc_model_var}})), 
              sd_diff_peui = sd(metered_primary_eui - ({{epc_model_var}})),
              mean_perc_diff_peui = 100*mean((metered_primary_eui - ({{epc_model_var}}))/{{epc_model_var}}), 
              sd_perc_diff_peui = 100*sd((metered_primary_eui - ({{epc_model_var}}))/{{epc_model_var}}),
              n_obs = n())
  
  
  # Approach to the SDC, get rid of within band groups < 10 and the next 
  # smallest band as well if the sum of < 10 groups is not > 10.
  # put in a row with all nans in the stats cols and suppressed_for_sdc in the 
  # seg_var column with the total N of the suppressed values in the n_obs col
  groups_less_than_10 = unique(summary_data[summary_data$n_obs<10,]$currentEnergyRatingMerge)
  for (group in groups_less_than_10){
    logic = (summary_data$n_obs < 10) & (summary_data$currentEnergyRatingMerge == group)
    n_supressed = sum(summary_data[logic,]$n_obs)
    summary_data = as_tibble(summary_data)
    logic = (summary_data$n_obs < 10) & (summary_data$currentEnergyRatingMerge == group)
    if (n_supressed <= 10) {
      group_logic = summary_data$currentEnergyRatingMerge == group
      more_than_10_logic = summary_data$n_obs > 10
      logic = logic + (summary_data$n_obs == min(summary_data[group_logic & more_than_10_logic, 'n_obs'])) &
        group_logic
      n_supressed = sum(summary_data[logic,]$n_obs)
    }
    summary_data = summary_data %>% filter(!logic)
    summary_data = summary_data %>% add_row({{seg_var}} := group, {{facet_var}} := 'total_suppressed_for_SDC',
                                            n_obs = n_supressed)
  }
  print(summary_data)
  # reorder the data for plotting 
  summary_data = summary_data %>% ungroup() %>% mutate(across({{facet_var}}, factor, levels = order_categoricals))
  # save the summary data
  save_name_stem = paste("EPC band and ", title_var, sep = "")
  write.csv(summary_data, paste(save_outputs_loc, save_name_stem, " - summary data.csv", sep = ""), row.names = FALSE)
  
  # don't plot the suppressed or don't know responses
  plot_data_reform = filter(summary_data, ({{facet_var}} != "total_suppressed_for_SDC") & 
                              ({{facet_var}}!= 'Unknown or no answer') & ({{facet_var}}!= 'Unknown or not applicable'))
  
  figure = ggplot(plot_data_reform, aes(x = {{seg_var}}))+
    geom_pointrange(aes(y =mean_epc_peui, ymin = mean_epc_peui- sd_epc_peui, 
                        ymax =mean_epc_peui+ sd_epc_peui, color = 'darkslategray4'), 
                    position = position_nudge(x = -0.05), shape = 18) + 
    geom_pointrange(aes(y =mean_serl_peui, ymin = mean_serl_peui- sd_serl_peui, 
                        ymax =mean_serl_peui+ sd_serl_peui, color = 'darkslategray'), 
                    position = position_nudge(x = 0.05), shape = 18) + 
    scale_color_manual(name = '', values = c("darkslategray4"="darkslategray4", 
                                             "darkslategray" = "darkslategray"), 
                       labels = c("EPC", "SERL"))+
    xlab("EPC Energy Efficiency Rating")  + ylab(bquote('Primary Energy Use Intensity '~(kWh/ year/ m^2))) +
    ggtitle(paste("Primary energy use intensity by", title_var)) +
    facet_wrap(vars({{facet_var}})) + theme(legend.position = 'bottom')
  print(figure)
  ggsave(paste(save_name_stem, '- EPC and metered PEUI.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 6, units = "in", dpi = 450)
  
  figure = ggplot(plot_data_reform, aes(x = {{seg_var}}))+
    geom_abline(slope = 0, intercept = 0, color = 'white', size = 1.5) + 
    geom_pointrange(aes(y =mean_perc_diff_peui, ymin = mean_perc_diff_peui- sd_perc_diff_peui, 
                        ymax =mean_perc_diff_peui+ sd_perc_diff_peui), color = 'darkslategray4', 
                    shape = 18) + 
    xlab("EPC Energy Efficiency Rating")  + ylab(bquote('Percent Difference in Primary Energy Use Intensity (%)')) +
    ggtitle(paste("Percent difference in primary energy use intensity by", title_var)) +
    facet_wrap(vars({{facet_var}})) + theme(legend.position = 'bottom')
  
  print(figure)
  ggsave(paste(save_name_stem, '- percent difference PEUI.png'), path = save_outputs_loc, device = png, 
         width = 8, height = 6, units = "in", dpi = 450)
  
  return(summary_data)
}