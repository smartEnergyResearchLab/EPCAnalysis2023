# June 2022, Despina Manouseli, d.manouseli@ucl.ac.uk. Jessica Few, jessica.few@ucl.ac.uk

#Significance tests for metered PEUI and EPC modelled PEUI
# results of this analysis are published in this paper: https://doi.org/10.1016/j.enbuild.2023.113024

library(ggplot2)
library(dplyr)
library(broom)
library(purrr)

save_data_loc2 = "P:/196860_SERLAnnual_Report/Working/JF/Data/Processed/EPC_Analysis/"
metered_comb2 = read.csv(paste(save_data_loc2, 'Metered_combined_dt_Scotland.csv', sep = ""))
save_output_loc = paste(save_data_loc2, "/Scotland/", sep = "")

#heating-metered homes
#the intention is to test whether differences in the two measures of consumption are significant statistically.

##Lets first observe how differences in data are distributed. Do they look normal?
#create Difference column: Positive values indicate that modeled
#demand is higher than measured demand (overestimation of demand)

metered_comb2$Diff=metered_comb2$metered_primary_eui-metered_comb2$epc_tot_peui
metered_comb2$perc_diff = 100*metered_comb2$Diff/metered_comb2$epc_tot_peui

# first do basic comparison of metered against modelled and perc difference
#are differences looking normally distributed for different bands?
g1<-ggplot(metered_comb2,aes(Diff,fill=currentEnergyRatingMerge))+
  geom_histogram()+
  xlab("Difference between modelled and measured EUI (kWh/year/m2)")+
  guides(fill=guide_legend(title="EPC Band"))+
  facet_wrap(~currentEnergyRatingMerge,scales='free')
g1
ggsave(paste('Difference between modeled and measured EUI - Hist_per_EPC.png'), path = save_data_loc2, device = png, 
       width = 8, height = 6, units = "in", dpi = 450)

#how many houses per EPC Band?
metered_comb2 %>% dplyr::count(currentEnergyRatingMerge)
#A&B should be tested using K-S 
#Source: Mishra et al. 2019.Descriptive Statistics and Normality Tests for Statistical Data

#perform shapiro tests for normality
metered_comb2$currentEnergyRatingMerge<-as.factor(metered_comb2$currentEnergyRatingMerge)

tests=metered_comb2 %>%
  group_by(currentEnergyRatingMerge) %>%
  do(test=shapiro.test(.$Diff))


#Kolmogorov SMirnov for A&B
dfAB<-metered_comb2[,c('currentEnergyRatingMerge','Diff')]
dfAB<-filter(dfAB,currentEnergyRatingMerge=='A and B' )
dfAB<-dfAB[,c('Diff')]
ks.test(dfAB,'pnorm')


#Lets produce summary stats for the difference per EPC band
library(plyr)

summary_stats<-ddply(metered_comb2,.(currentEnergyRatingMerge),summarise,mean=mean(Diff), median=median(Diff), sd=sd(Diff),min=min(Diff), max=max(Diff))
write.csv(summary_stats, paste(save_data_loc, 'Summary_Stats_for_Diff_per_EPC_Band2.csv', sep = ""), row.names = FALSE)

gbox<-ggplot(metered_comb2,aes(x=currentEnergyRatingMerge,y=Diff))+
  geom_boxplot()+
  xlab("Difference between modelled and measured EUI (kWh/year/m2)")
gbox
ggsave(paste('Difference between modeled and measured EUI - Box_per_EPC.png'), path = save_data_loc2, device = png, 
       width = 8, height = 6, units = "in", dpi = 450)
#Perform Paired T test for All bands 
df_all<-metered_comb2%>%
  do(tidy(t.test(.$epc_tot_peui,.$metered_primary_eui,
                 paired=TRUE, conf.level = 0.95)))
df_all
# save results
df_all = dplyr::rename(df_all, mean_difference = estimate, n = parameter)
df_all$n = df_all$n + 1 # the parameter variable was degrees of freedom, we just want to report n so add 1
write.csv(df_all, paste(save_output_loc, 'Paired_test_for_significant_diff_Scotland.csv', sep = ""), row.names = FALSE)

# also do a t-test for the percent difference 
df_all_perc<-metered_comb2%>%
  do(tidy(t.test(.$perc_diff,
                 conf.level = 0.95)))
df_all_perc
df_all_perc = dplyr::rename(df_all_perc, mean_perc_difference = estimate, n = parameter)
df_all_perc$n = df_all_perc$n + 1 # the parameter variable was degrees of freedom, we just want to report n so add 1
write.csv(df_all_perc, paste(save_output_loc, 'Paired_test_for_significant_perc_diff.csv', sep = ""), row.names = FALSE)



df_ttest_NEW<-metered_comb2%>%
  group_by(currentEnergyRatingMerge)%>%
  do(tidy(t.test(.$epc_tot_peui,.$metered_primary_eui,
                 mu=0,alt="two.sided",
                 paired=TRUE,
                 conf.level = 0.95 )))

df_ttest_NEW

# rename some columns to make easier for SDC checking
df_ttest_NEW = dplyr::rename(df_ttest_NEW, mean_difference = estimate, n = parameter)
df_ttest_NEW$n = df_ttest_NEW$n + 1 # the parameter variable was degrees of freedom, we just want to report n so add 1

write.csv(df_ttest_NEW, paste(save_output_loc, 'Paired_test_for_significant_diff_by_EPC_band.csv', sep = ""), row.names = FALSE)


df_wilcox_NEW<-metered_comb2%>%
filter( currentEnergyRatingMerge=="F and G")%>%
group_by(currentEnergyRatingMerge)%>%
do(tidy(wilcox.test(.$epc_tot_peui,.$metered_primary_eui,
paired=TRUE)))
df_wilcox_NEW

df_wilcox_NEW = subset(df_wilcox_NEW, select = -c(statistic))
df_wilcox_NEW$n = sum(metered_comb2$currentEnergyRatingMerge == 'F and G')

write.csv(df_wilcox_NEW, paste(save_output_loc, 'Wilcox_for_significant_diff_Bands_FG.csv', sep = ""), row.names = FALSE)


#Tests by new_home_epc variable
gbox<-ggplot(metered_comb2,aes(x=new_home_epc,y=Diff))+
  geom_boxplot()+
  xlab("Difference between modelled and measured EUI (kWh/year/m2)")
gbox
ggsave(paste('Difference between modeled and measured EUI - box_per_NEW_HOME.png'), path = save_data_loc2, device = png, 
       width = 8, height = 6, units = "in", dpi = 450)

#are differences looking normally distributed for the 2 categories?
g4<-ggplot(metered_comb2,aes(Diff,fill=new_home_epc))+
  geom_histogram()+
  xlab("Difference between modelled and measured EUI (kWh/year/m2)")+
  guides(fill=guide_legend(title="New Home EPC"))+
  facet_wrap(~new_home_epc,scales='free')
g4
ggsave(paste('Difference between modeled and measured EUI - Hist_per_NewHome.png'), path = save_data_loc2, device = png, 
       width = 8, height = 6, units = "in", dpi = 450)
#how many houses per EPC Band?

metered_comb2 %>% dplyr::count(new_home_epc)
df_ttest_NEW_home<-metered_comb2%>%
  group_by(new_home_epc)%>%
  do(tidy(t.test(.$epc_tot_peui,.$metered_primary_eui,
                 mu=0,alt="two.sided",
                 paired=TRUE,
                 conf.level = 0.95 )))

df_ttest_NEW_home

df_ttest_NEW_home = dplyr::rename(df_ttest_NEW_home, mean_difference = estimate, n = parameter)
df_ttest_NEW_home$n = df_ttest_NEW_home$n + 1 # the parameter variable was degrees of freedom, we just want to report n so add 1


write.csv(df_ttest_NEW_home, paste(save_output_loc, 'Paired_test_for_significant_Diff_newHome.csv', sep = ""), row.names = FALSE)

#Tests by match_sap_occupancy variable
gbox<-ggplot(metered_comb2,aes(x=match_sap_occupancy,y=Diff))+
  geom_boxplot()+
  xlab("Difference between modelled and measured EUI (kWh/year/m2)")
gbox

#are differences looking normally distributed for the 2 categories?
g4<-ggplot(metered_comb2,aes(Diff,fill=match_sap_occupancy))+
  geom_histogram()+
  xlab("Difference between modelled and measured EUI (kWh/year/m2)")+
  guides(fill=guide_legend(title="Match SAP occupancy"))+
  facet_wrap(~match_sap_occupancy,scales='free')
g4
#how many houses per group?

metered_comb2 %>% dplyr::count(match_sap_occupancy)
df_ttest_NEW_match_sap_occupancy<-metered_comb2%>%
  group_by(match_sap_occupancy)%>%
  do(tidy(t.test(.$epc_tot_peui,.$metered_primary_eui,
                 mu=0,alt="two.sided",
                 paired=TRUE,
                 conf.level = 0.95 )))

df_ttest_NEW_match_sap_occupancy = dplyr::rename(df_ttest_NEW_match_sap_occupancy, mean_difference = estimate, n = parameter)
df_ttest_NEW_match_sap_occupancy$n = df_ttest_NEW_match_sap_occupancy$n + 1 # the parameter variable was degrees of freedom, we just want to report n so add 1


write.csv(df_ttest_NEW_match_sap_occupancy, paste(save_output_loc, 'Paired_test_for_significant_Diff_match_occupancy.csv', sep = ""), row.names = FALSE)

