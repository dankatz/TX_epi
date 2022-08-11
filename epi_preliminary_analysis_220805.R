#This script contains primary data visualization and analysis.
#See assemble_all_data.R for the data that goes in to this script


library(dlnm)
library(splines)
library(MASS)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

#rm(list = ls())


### load in data ###################################################
opa_day_schoolchildren <- read_csv("Z:/THCIC/Katz/opa_day_ages_5_17_dist_25_2022-08-10.csv", guess_max = 8260)
opa_day_adult <- read_csv("Z:/THCIC/Katz/opa_day_ages_18_99_dist_25_2022-08-10.csv", guess_max = 8260)
opa_day_youngchildren <- read_csv("Z:/THCIC/Katz/opa_day_ages_0_4_dist_25_2022-08-10.csv", guess_max = 8260)

### fig 2: time series of each var ############################################################
names(opa_day)

#time series for ED visits: young children
pbir_global_mean_youngchild <- opa_day_youngchildren %>% #the average across all the study areas
  group_by(date) %>%
  summarize(pbir_global_mean = mean(pbir))

panel_ed_youngchild <-
  opa_day_youngchildren %>%
  ggplot(aes(x = date, y = pbir, col = NAB_station)) + theme_few() +
  geom_line(aes(x = date, y=rollmean((pbir ), 7, na.pad=TRUE)), alpha = 0.3) +
  geom_line(data = pbir_global_mean_youngchild, aes(x = date, y = rollmean(pbir_global_mean, 7, na.pad=TRUE)), col = "black") +
  coord_cartesian(ylim = c(0, 70)) +  
  scale_color_grey(name = "NAB station") +
  ylab("Asthma ED \n visits \n (per 1,000,000)") +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position= "none",
        axis.title.x=element_blank(), axis.text.x=element_blank())

#time series for ED visits: school aged children
pbir_global_mean <- opa_day_schoolchildren %>% #the average across all the study areas
  group_by(date) %>%
  summarize(pbir_global_mean = mean(pbir))
panel_ed <-
  opa_day_schoolchildren %>%
  ggplot(aes(x = date, y = pbir, col = NAB_station)) + theme_few() +
  geom_line(aes(x = date, y=rollmean((pbir ), 7, na.pad=TRUE)), alpha = 0.3) +
  geom_line(data = pbir_global_mean, aes(x = date, y = rollmean(pbir_global_mean, 7, na.pad=TRUE)), col = "black") +
  coord_cartesian(ylim = c(0, 55)) +  scale_color_grey() +
  ylab("Asthma ED \n visits \n (per 1,000,000)") +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position= "none",axis.title.x=element_blank(), axis.text.x=element_blank())

#time series for ED visits: adults
pbir_global_mean_adult <- opa_day_adult %>% #the average across all the study areas
  group_by(date) %>%
  summarize(pbir_global_mean = mean(pbir))

panel_ed_adult <-
  opa_day_adult %>%
  ggplot(aes(x = date, y = pbir, col = NAB_station)) + theme_few() +
  geom_line(aes(x = date, y=rollmean((pbir ), 7, na.pad=TRUE)), alpha = 0.3) +
  geom_line(data = pbir_global_mean_adult, aes(x = date, y = rollmean(pbir_global_mean, 7, na.pad=TRUE)), col = "black") +
  coord_cartesian(ylim = c(0, 18)) +  
  scale_color_grey(name = "NAB station") +
  ylab("Asthma ED \n visits \n (per 1,000,000)") +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position= "none",
        axis.title.x=element_blank(), axis.text.x=element_blank())

## time series for pollen
pol_global_mean <- opa_day %>% group_by(date) %>%
  summarize(cup_all_m_global = mean(cup_all_m, na.rm = TRUE),
            trees_m_global = mean(trees_m, na.rm = TRUE),
            pol_other_m_global = mean(pol_other_m, na.rm = TRUE))

panel_pol_cup <-  opa_day %>%
  ggplot(aes(x = date, y = cup_all_m + 1, col = NAB_station, group = NAB_station)) + theme_few() + scale_y_log10() +
  geom_line(aes(x = date, y=rollmean((cup_all_m + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station), alpha = 0.3) +
  ylab(expression(atop(pollen, (grains/m^3)))) +  scale_color_grey(name = "NAB station")+ #scale_color_discrete(name = "NAB station") +
  geom_line(data = pol_global_mean, aes(x = date, y = rollmean(cup_all_m_global + 1, 7, na.pad=TRUE), col = NA, group = NA), col = "black") +
  theme(legend.position= "none" ) + theme(axis.title.x=element_blank(), axis.text.x=element_blank())

panel_pol_trees <-  opa_day %>%
  ggplot(aes(x = date, y = trees_m + 1, col = NAB_station, group = NAB_station)) + theme_few() + scale_y_log10() +
  geom_line(aes(x = date, y=rollmean((trees_m + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station), alpha = 0.3) +
  ylab(expression(atop(pollen, (grains/m^3)))) + scale_color_grey(name = "NAB station")+ #scale_color_discrete(name = "NAB station") +
  geom_line(data = pol_global_mean, aes(x = date, y = rollmean(trees_m_global + 1, 7, na.pad=TRUE), col = NA, group = NA), col = "black") +
  theme(legend.position= "none" ) + theme(axis.title.x=element_blank(), axis.text.x=element_blank()) 

panel_pol_other <-  opa_day %>%
  ggplot(aes(x = date, y = pol_other_m + 1, col = NAB_station, group = NAB_station)) + theme_few() + scale_y_log10() +
  geom_line(aes(x = date, y=rollmean((pol_other_m + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station), alpha = 0.3) +
  ylab(expression(atop(pollen, (grains/m^3)))) + scale_color_grey(name = "NAB station")+ #scale_color_discrete(name = "NAB station") +
  geom_line(data = pol_global_mean, aes(x = date, y = rollmean(pol_other_m_global + 1, 7, na.pad=TRUE), col = NA, group = NA), col = "black") +
  theme(legend.position= "none" ) + theme(axis.title.x=element_blank(), axis.text.x=element_blank())


#time series for viruses
panel_vir <-
  opa_day %>% 
  group_by(date) %>% 
  summarize(RSV_d_perc_pos = mean(v_pos_prop_RSV_m, na.rm = TRUE) * 100,
         corona_d_perc_pos = mean(v_pos_prop_corona_m, na.rm = TRUE) * 100,
         rhino_d_perc_pos = mean(v_pos_prop_rhino_m, na.rm = TRUE) * 100,
         flu_d_perc_pos = mean(v_pos_prop_flu_m, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  # dplyr::select(date, RSV_d_perc_pos, corona_d_perc_pos, rhino_d_perc_pos, flu_d_perc_pos ) %>%
  pivot_longer(cols = c(RSV_d_perc_pos, corona_d_perc_pos , rhino_d_perc_pos, flu_d_perc_pos),
               names_to = "virus_type", values_to = "positive_tests") %>%
  # distinct() %>%
  #filter(date > mdy("10 - 31 - 2015")) %>%
  #arrange(virus_type, date) %>%
  ggplot(aes(x = date, y = positive_tests, color = virus_type)) + theme_few() +
  geom_step(size = 1) + #geom_point() +
  ylab(expression(atop("positive tests", "(%)"))) +
  scale_color_viridis_d(breaks = c("flu_d_perc_pos", "corona_d_perc_pos", "rhino_d_perc_pos", "RSV_d_perc_pos"), 
                        option = "viridis",
                        labels = c("Influenza" ,"Seasonal coronavirus", "Rhinovirus", "RSV"), name = "virus type") +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position= "none")#c(0.75, 0.75))

#putting together the time series into one plot
ts_panels <- cowplot::plot_grid(panel_ed_youngchild, panel_ed, panel_ed_adult,
                                panel_pol_cup, panel_pol_trees, panel_pol_other, panel_vir,
                                align = "v", ncol = 1, rel_heights = c(1, 1, 1, 1, 1, 1, 1),
                                labels = c("A) Young children ARED", "B) School aged children ARED" , "C) Adult ARED", "D) Cupressaceae pollen", "E) tree pollen",
                                           "F) other pollen","G) Viruses"),
                                label_size = 11,
                                label_x = 0.17, label_y = 0.8,
                                hjust = 0, vjust = 0)
ggsave(file = "Z:/THCIC/Katz/results/time_series_fig_220810.jpg", plot = ts_panels,
       height = 24, width = 18, units = "cm", dpi = 300)


# 
# 
# #comparing pollen to ED visits for asthma over time
# names(opa_day)
# panel_a <- opa_day %>%
# filter(date > ymd("2015 - 11 - 01")) %>%
# filter(NAB_station == "San Antonio A") %>%
#   ggplot(aes(x = date, y = n_cases)) + theme_few() +  
#   geom_point(alpha = 0.2)  +  ylab("number of asthma ED visits") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#   xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
#   geom_line(aes(x = date, y=rollmean(n_cases, 7, na.pad=TRUE)), color = "black")
# panel_b <- opa_day %>% 
#   filter(date > ymd("2015 - 11 - 01")) %>%
#   filter(NAB_station == "San Antonio A") %>%
#   ggplot(aes(x = date, y = log(ja + 1))) + theme_few() +  #scale_x_log10() + 
#   geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#   xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
#   geom_line(aes(x = date, y=rollmean(log(ja + 1), 7, na.pad=TRUE)), color = "black")
# panel_c <- opa_day %>% 
#   filter(date > ymd("2015 - 11 - 01")) %>%
#   filter(NAB_station == "San Antonio A") %>%
#   ggplot(aes(x = date, y = log(cup_other_rfint_log_mean + 1))) + theme_few() +  #scale_x_log10() + 
#   geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#   xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
#   geom_line(aes(x = date, y=rollmean(log(cup_other_rfint_log_mean + 1), 7, na.pad=TRUE)), color = "black")
# panel_d <- opa_day %>% 
#   filter(date > ymd("2015 - 11 - 01")) %>%
#   filter(NAB_station == "San Antonio A") %>%
#   ggplot(aes(x = date, y = log(trees + 1))) + theme_few() +  #scale_x_log10() + 
#   geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#   xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
#   geom_line(aes(x = date, y=rollmean(log(trees + 1), 7, na.pad=TRUE)), color = "black")
# panel_e <- opa_day %>% 
#   filter(date > ymd("2015 - 11 - 01")) %>%
#   filter(NAB_station == "San Antonio A") %>%
#   ggplot(aes(x = date, y = log(pol_other + 1))) + theme_few() +  #scale_x_log10() + 
#   geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#   xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
#   geom_line(aes(x = date, y=rollmean(log(pol_other + 1), 7, na.pad=TRUE)), color = "black")
# 
# cowplot::plot_grid(panel_a, panel_b, panel_c, panel_d, panel_e, nrow = 5, 
#                    labels = c("ED visits", "J. ashei", "other Cupressaceae", "trees", "other pollen"))
# 
#   
#   opa_day %>%
#     filter(date > ymd("2015 - 11 - 01")) %>%
#     filter(NAB_station == "Houston" | NAB_station == "Dallas" | NAB_station == "San Antonio A") %>%
#     ggplot(aes(x = date, y = n_cases, color = v_tests_pos_RSV )) + theme_few() +  facet_wrap(~NAB_station, scale = "free_y", ncol = 1) + #scale_x_log10() + 
#     geom_point()  +  ylab("number of asthma ED visits") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#     xlab("") + scale_color_viridis_c() +
#     geom_line(aes(x = date, y=rollmean(n_cases, 7, na.pad=TRUE)), color = "black")
#   
#   opa_day %>%
#     filter(date > ymd("2015 - 11 - 01")) %>%
#     filter(NAB_station == "Georgetown" ) %>%
#     ggplot(aes(x = date, y = tot_pol +1 )) + theme_bw() +  #facet_wrap(~NAB_station, scale = "free_y", ncol = 1) + #scale_x_log10() + 
#     geom_point()  +  ylab("airborne pollen (grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#     xlab("") + #+ scale_color_viridis_c(name = "max temp (C)") +
#     #geom_line(aes(x = date, y= tot_pol_m7 + 1)) +
#     scale_x_date(breaks = pretty_breaks(15)) +
#     scale_y_log10()
#   
#   opa_day %>%
#     filter(date > ymd("2015 - 11 - 01")) %>%
#     filter(NAB_station == "Houston" | NAB_station == "Dallas" | NAB_station == "San Antonio A") %>%
#     ggplot(aes(x = date, y = met_tmaxdegc )) + theme_few() +  facet_wrap(~NAB_station, scale = "free_y", ncol = 1) + #scale_x_log10() + 
#     #geom_point()  +  
#     ylab("temperature (C)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
#     #xlab("") + 
#     geom_line(aes(x = date, y=rollmean(n_cases, 3, na.pad=TRUE), col = log10(tot_pol + 1)), lwd = 2) +
#     geom_line(aes(x = date, y=rollmean(met_tmaxdegc, 7, na.pad=TRUE)), color = "red") +
#      scale_y_continuous("ED visits", sec.axis = sec_axis(~ . , name = "temperature")) +
#     theme(      axis.title.y.right = element_text(color = "red")) +
#     scale_color_viridis_c()
#   
# opa_day %>%
#   ggplot(aes(y = tot_pol, x = met_tmaxdegc, col = doy)) + theme_few() +  facet_wrap(~NAB_station) + scale_y_log10() +  #facet_wrap(~year, ncol = 1) + #
#   geom_point() + xlab("maximum temp (C)") + ylab("pollen grains per m3") + #geom_smooth(method = "lm", se = FALSE)  + 
#   scale_color_viridis_c(name = "day of year")#ylab("PBIR (asthma ED visits per 10,000 residents)")  
#   
# opa_day %>% ungroup() %>%
#   filter(NAB_station == "San Antonio A" | NAB_station == "Dallas" | NAB_station == "Houston") %>%
#   filter(date > ymd("2015-11-01")) %>%
#   filter(year == 2016) %>%
#   ggplot(aes(x = date, y = tot_pol_stan )) + theme_bw() +  facet_wrap(~NAB_station, ncol = 1) +#+ facet_wrap(~year, ncol = 1) + #+ scale_y_log10() + 
#   geom_line(aes(x = date , y=rollmean(log10(tot_pol + 1) * 25, 7, na.pad=TRUE)), color = "blue") +
#   geom_line(aes(x = date , y=rollmean(v_tests_pos_Rhinovirus * 0.3, 7, na.pad=TRUE)), color = "green") +
#   geom_line(aes(x = date, y=rollmean(pbir * 175, 7, na.pad=TRUE)), color = "red") +
#   coord_cartesian(ylim = c(0, 100)) +
#   scale_x_date(breaks = pretty_breaks(30)) +
#   ylab("variable scaled to 100")
# #ylab("PBIR (asthma ED visits per 10,000 residents)")  
# 
# test <- filter(opa_day, MSA == "San Antonio-New Braunfels")
# ccf(test$hits_day_adj_pollen, test$tot_pol)
# ccf(test$hits_day_adj_pollen, test$pbir_child)
# 
# opa_day %>% ungroup() %>% #unique(opa_day$NAB_station)
#   filter(date > ymd("2015-11-01")) %>%
#   #filter(year == 2017) %>%
#   #filter(NAB_station == "San Antonio B" | NAB_station == "San Antonio A" ) %>%
#   #filter(NAB_station == "Waco B" | NAB_station == "Waco A" ) %>%
#   filter(NAB_station == "Dallas" | NAB_station == "Flower Mound" ) %>%
#   #filter(NAB_station == "Houston" | NAB_station == "Dallas" | NAB_station == "San Antonio A" | NAB_station == "Georgetown") %>%
#   #filter(NAB_station == "Flower Mound" | NAB_station == "San Antonio B" | NAB_station == "San Antonio A" | NAB_station == "Georgetown") %>%
#   ggplot(aes(x = date, y = pbir * 500 , col = log10(tot_pol+ 1) )) + theme_bw() +  facet_wrap(~NAB_station, ncol = 1) +
#   scale_color_viridis_c() +
#   #geom_line( lwd = 2) + 
#   geom_line(aes(x = date - 10, y=rollmean(pbir * 1000, 7, na.pad=TRUE)), color = "red", lwd = 2) + 
#   geom_point(aes(x = date, y = 50 * log(tot_pol + 1)))+
#   geom_line(aes(x = date, y = v_tests_pos_Adenovirus), color = "pink")+
#   geom_line(aes(x = date, y = v_tests_pos_HMPV), color = "pink")+
#   geom_line(aes(x = date, y = v_tests_pos_Corona), color = "green")+
#   geom_line(aes(x = date, y = v_tests_pos_Rhinovirus), color = "blue")+
#   geom_line(aes(x = date, y = v_tests_pos_RSV), color = "goldenrod")+
#   geom_line(aes(x = date, y = v_tests_pos_Parainfluenza), color = "pink") 
#   
# opa_day %>%
# filter(NAB_station == "Dallas") %>%
# #filter(year == 2016) %>%
# ggplot(aes(x = date, y = pbir, col = (tot_pol + 1))) + geom_point(size = 2) + theme_bw() + 
#   scale_color_viridis_c(trans = "log10", name = "total pollen (g/m3)") +
#   scale_x_date(breaks = pretty_breaks(20)) + ylab("PBIR (Asthma ED visits per 10,000 residents)") +#scale_y_log10()  + 
#   geom_line(aes(x = date, y=rollmean(pbir, 14, na.pad=TRUE)), color = "red") 
#   #geom_line(aes(x = date, y=rollmean(log10(tot_pol + 1)/10, 3, na.pad=TRUE)), color = "blue") 
# 
# 
# # #overlap between San Antonio datasets?
# # filter(NAB_tx, county_name == "Bexar") %>%
# #   ggplot(aes(x = date, y = Cupressaceae, color = file)) + geom_point() + facet_wrap(~file, ncol = 1)
# # 
# # filter(NAB_tx, county_name == "Bexar") %>% 
# #   dplyr::select(date, file, Cupressaceae) %>%
# #   pivot_wider(names_from = file, values_from = Cupressaceae) %>%
# #   setNames(., c("date", "San_198", "San_219")) %>%
# #   ggplot(aes(x= San_198 + 1, y = San_219 + 1)) + geom_point() + theme_few() + scale_x_log10() + scale_y_log10() + geom_abline(slope = 1, lty = 2) + 
# #   xlab("Station 198 (pollen grains/m3)") + ylab("Station 219 (pollen grains/m3)") #+ geom_smooth(method = "lm")
# # 
# # filter(NAB_tx, county_name == "McLennan") %>% 
# #   dplyr::select(date, file, Cupressaceae) %>%
# #   pivot_wider(names_from = file, values_from = Cupressaceae) %>%
# #   setNames(., c("date", "Waco_105", "Waco_106")) %>%
# #   mutate(doy = yday(date)) %>%
# #   ggplot(aes(x= Waco_105 + 1, y = Waco_106 + 1, color = doy)) + geom_point() + theme_few() + scale_x_log10() + scale_y_log10() + geom_abline(slope = 1, lty = 2) + 
# #   xlab("Station 105 (pollen grains/m3)") + ylab("Station 106 (pollen grains/m3)") #+ geom_smooth(method = "lm")
# # 
# # 
# # NAB_tx %>% mutate(doy = yday(date),
# #                   year = year(date)) %>%
# #   ggplot(aes(x= doy, y = Cupressaceae + 1, color = year)) + geom_point(alpha = 0.5) + geom_line(aes(y=rollmean((Cupressaceae + 1), 14, na.pad=TRUE))) +
# #   facet_wrap(~file) + scale_y_log10() + 
# #   scale_color_viridis_c() + theme_few() 
# 
# filter(opa_day, county_name == "Dallas") %>% #, 
#      #  doy > 350 | doy < 50) %>%
# ggplot(aes(x = date, y = pbir_child, color = gtrend_common_cold)) + geom_point(alpha = 0.9, size = 2) + theme_few() +
#   #ylab("asthma-related ED visits per 100,000 person years in Travis County") +
#   ylab("asthma-related ED visits per 10,000 people per day") + 
#   #ylab("rhino virus modeled incidence") + 
#   scale_color_viridis_c() + #limits = c(0, 150) name = "var"
#   scale_x_date(breaks = scales::pretty_breaks(n = 20)) + #, limits = as.Date(c('2016-02-14','2016-05-01'))) +
#   geom_line(aes(x = date, y=rollmean(pbir_child, 14, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)




#### setting up analysis with a distributed lags model using dlnm package #######################################
#function for model selection with QAIC:
#NOTE: computing QAIC is kind of a pain, see discussions here:
#https://www.r-bloggers.com/2017/05/a-note-on-aic-scores-for-quasi-families-in-rstats/
#https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
#http://fisher.utstat.toronto.edu/reid/sta2201s/QUASI-POISSON.pdf
#Thankfully, Gasparrini includes a little snippet of code to do so for DLNM models:
#https://github.com/gasparrini/2013_gasparrini_BMCmrm_Rcodedata/blob/master/01.prep.R
# FUNCTION TO COMPUTE THE Q-AIC IN QUASI-POISSON MODELS:
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}


opa_day <- opa_day_schoolchildren
age_low <- 5
age_hi <- 17

#opa_day <- opa_day_adult
#opa_day <- opa_day_youngchildren

## prepare data for model ============================================================
#needs to be run for each age group
opa_day %>% #group_by(NAB_station) %>% 
  summarize(total_cases = sum(n_cases), PBIR_mean = mean(pbir)) #%>% ungroup() %>%  summarize( total_n = sum (total_cases))


ggplot(opa_day, aes( x = date, y = trees + 1))  + facet_wrap(~NAB_station) + theme_bw()+ scale_y_log10() + 
  geom_point(color = "black") +
  geom_point(aes(x = date, y = trees_m + 1), color = "red", size = 0.5) 

#str(data_for_model)
#names(opa_day)
#unique(opa_day$NAB_station)
data_for_model <- opa_day %>%
  filter(date > ymd("2015 - 10 - 01") & date < ymd("2021 - 01 - 01")) %>% 
  #filter(NAB_station == "San Antonio A" | NAB_station == "San Antonio B" ) %>% #unique(opa_day$NAB_station)
  #filter(NAB_station != "College Station" & NAB_station != "Waco A" & NAB_station != "Waco B") %>% 
  mutate(
    #log_child_pop = log(children_pop),
    #child_pop = children_pop,
    #log_adult_pop = log(adult_pop),
    log_agegroup_x_pop = log(agegroup_x_pop),
    NAB_station_n = as.numeric(as.factor(NAB_station)),
    #flu_d_perc_pos = flu_d_prop_pos *100,
    met_prcp_flag = ifelse(met_prcpmmday > 0, 1, 0),
    met_prcpmmday_l = log(met_prcpmmday + 1),
    met_prcpmmday_ls = scale(met_prcpmmday_l),
    met_sradWm2_s = scale(met_sradWm2),
    met_tmaxdegc_s = scale(met_tmaxdegc),
    met_tmindegc_s = scale(met_tmindegc),
    met_tavg_s = scale(met_tmaxdegc + met_tmindegc),
    met_vpPa_s = scale(met_vpPa)
  ) %>%
  dplyr::select(NAB_station, date, n_cases, pbir, 
                agegroup_x_pop, log_agegroup_x_pop, #child_pop, log_child_pop, adult_pop, log_adult_pop, 
                week_day,
                #cup_other_lm, #cup_other_lms, 
                cup_all_lm, cup_all_m,
                trees_lm, trees_m,
                pol_other_lm, pol_other_m,
              
                # v_pos_prop_rhino_m, v_pos_prop_rhino_m14#v_pos_rel_Rhinovirus_m, v_pos_rel_adj_Rhinovirus_m, v_pos_rel_adj_Rhinovirus_m14,  
                # # v_pos_rel_adj_Rhinovirus_m14l, 
                # # v_pos_rel_adj_Rhinovirus_m14r, v_pos_rel_adj_Rhinovirus_m21, 
                # # v_pos_rel_adj_Rhinovirus_m28, 
                # 
                # v_pos_prop_RSV_m, #v_pos_rel_RSV_m, v_pos_rel_adj_RSV_m, v_pos_rel_adj_RSV_m14, 
                # v_pos_rel_adj_RSV_m14l, v_pos_rel_adj_RSV_m14r, v_pos_rel_adj_RSV_m21,v_pos_rel_adj_RSV_m28,#names(nrevss_data4)
                # 
                # v_pos_prop_corona_m, #v_pos_rel_corona_m, v_pos_rel_adj_corona_m, v_pos_rel_adj_corona_m14, 
                # v_pos_rel_adj_corona_m14l, v_pos_rel_adj_corona_m14r, v_pos_rel_adj_corona_m21,v_pos_rel_adj_corona_m28,
                # 
                # v_pos_prop_flu_m, #v_pos_rel_flu_m, v_pos_rel_adj_flu_m,v_pos_rel_adj_flu_m14, v_pos_rel_adj_flu_m14l,
                # v_pos_rel_adj_flu_m14r, v_pos_rel_adj_flu_m21,v_pos_rel_adj_flu_m28,
                v_pos_prop_RSV_m, v_pos_prop_RSV_m14, v_pos_prop_RSV_m21, v_pos_prop_RSV_m28,
                v_pos_prop_rhino_m, v_pos_prop_rhino_m14,v_pos_prop_rhino_m21,v_pos_prop_rhino_m28,
                v_pos_prop_corona_m, v_pos_prop_corona_m14, v_pos_prop_corona_m21, v_pos_prop_corona_m28,
                v_pos_prop_flu_m, v_pos_prop_flu_m14, v_pos_prop_flu_m21, v_pos_prop_flu_m28,
                
                met_prcpmmday, met_sradWm2, met_tmaxdegc, met_tmindegc, met_vpPa, #hist(data_for_model$met_prcpmmday_ls)
                met_prcpmmday_l, met_prcpmmday_ls, met_sradWm2_s, met_tmaxdegc_s, met_tmindegc_s, met_vpPa_s, met_tavg_s, met_prcp_flag
  ) %>%
  arrange(NAB_station, date) %>%
  ungroup() %>%  #to avoid an error with filtering the complete cases on the next line: https://stackoverflow.com/questions/59603972/result-must-have-length-error-with-complete-cases 
  filter(complete.cases(.)) %>% 
  #filter(NAB_station != "San Antonio B") %>% 
  # filter(date < mdy("11-01-2017")) %>% cutting off the bad flu season makes the negative effect of flu NS
  # filter(NAB_station == "San Antonio A" | NAB_station == "San Antonio B" | NAB_station == "Georgetown" |
  #          NAB_station == "Waco A" | NAB_station == "Waco B" ) %>% 
  # filter(NAB_station == "Houston" | NAB_station == "Dallas" | NAB_station == "Flower Mound" |
  #          NAB_station == "College Station" ) %>% 
  #filter(   NAB_station != "San Antonio A" & NAB_station != "San Antonio B" ) %>% # NAB_station != "Waco A",   # NAB_station != "College Station") %>% 
  group_by(NAB_station) %>% 
  mutate(time = row_number(),
         all_pol_lm = log10(cup_all_m + trees_m + pol_other_m + 1)) %>% 
  mutate(    doy = yday(date),
             months = month(date),
             years = year(date))  
  # mutate(main_cup_season = case_when(date > ymd("2015 - 10 - 01") & date < ymd("2015 - 12 - 20") ~ 0,
  #                                    date > ymd("2015 - 12 - 19") & date < ymd("2016 - 03 - 15") ~ 1,
  #                                    date > ymd("2016 - 03 - 14") & date < ymd("2016 - 12 - 20") ~ 0,
  #                                    date > ymd("2016 - 12 - 19") & date < ymd("2017 - 03 - 15") ~ 1,
  #                                    date > ymd("2017 - 03 - 14") & date < ymd("2017 - 12 - 20") ~ 0,
  #                                    date > ymd("2017 - 12 - 19") & date < ymd("2018 - 03 - 15") ~ 1),
  #        season = case_when(date > ymd("2015 - 10 - 01") & date < ymd("2015 - 12 - 20") ~ "no",
  #                           date > ymd("2015 - 12 - 19") & date < ymd("2016 - 03 - 15") ~ "15 - 16",
  #                           date > ymd("2016 - 03 - 14") & date < ymd("2016 - 12 - 20") ~ "no",
  #                           date > ymd("2016 - 12 - 19") & date < ymd("2017 - 03 - 15") ~ "16 - 17",
  #                           date > ymd("2017 - 03 - 14") & date < ymd("2017 - 12 - 20") ~ "no",
  #                           date > ymd("2017 - 12 - 19") & date < ymd("2018 - 03 - 15") ~ "17"))  #ggplot(data_for_model, aes(x = date, y = main_cup_season)) + geom_point()
#data_for_model2 <- data_for_model
#data_for_model <- filter(data_for_model, main_cup_season == 1) %>% filter(season == "15 - 16")


# names(data_for_model) 
# summary(data_for_model)
# ggplot(data_for_model, aes( x = date, y = cup_all_lm )) + geom_point() + facet_wrap(~NAB_station) 

## SI: viral prevalence by health service region (3 wk moving average) ----------------------------------------
data_for_model %>% group_by(NAB_station, date) %>% 
  dplyr::select(contains("v_")) %>% 
  ungroup() %>% 
  rename(Rhinovirus = v_pos_prop_rhino_m21,
         Corona = v_pos_prop_corona_m21,
         RSV = v_pos_prop_RSV_m21,
         Influenza = v_pos_prop_flu_m21
  ) %>% 
  pivot_longer(cols = c(Rhinovirus, Corona, RSV, Influenza)) %>% 
  ggplot(aes( x = date, y = value, color = NAB_station)) + geom_step() + facet_wrap(~name, scales = "free_y", ncol = 1)  + theme_bw() +
  ylab("viral prevalence index (scaled positive tests per day)")


## dlnm model -----------------------------------------------------------------
## set up dlnm crossbasis object for use in glm
max_lag <- 4
cup_lag <- crossbasis(data_for_model$cup_all_lm, lag = max_lag, #log10 transformed & imputed pollen concentration for Cupressaceae
                      # argvar=list(fun = "lin"), #shape of response curve
                      # arglag = list(fun = "integer")) #shape of lag
argvar=list(fun = "poly", degree = 3), #shape of response curve
arglag = list(fun = "poly", degree = 3)) #shape of lag

trees_lag <- crossbasis(data_for_model$trees_lm, lag = max_lag, 
                        # argvar=list(fun = "lin"), #shape of response curve
                        # arglag = list(fun = "integer")) #shape of lag
argvar=list(fun = "poly", degree = 3), #shape of response curve
arglag = list(fun = "poly", degree = 3)) #shape of lag


pol_other_lag <- crossbasis(data_for_model$pol_other_lm, lag = max_lag, 
                            # argvar=list(fun = "lin"), #shape of response curve
                            # arglag = list(fun = "integer")) #shape of lag
argvar=list(fun = "poly", degree = 3), #shape of response curve
arglag = list(fun = "poly", degree = 3)) #shape of lag

#viruses in dlnm format for use with attrdl function for AR
rhino_lag <- crossbasis(data_for_model$v_pos_prop_rhino_m, lag = 0, #percent of tests positive for rhinovirus
                        # argvar=list(fun = "poly", degree = 3), #shape of response curve
                        # arglag = list(fun = "poly", degree = 3)) #shape of lag
                        argvar=list(fun = "lin"), arglag = list(fun = "lin")) #using a linear response

corona_lag <- crossbasis(data_for_model$v_pos_prop_corona_m, lag = 0, #percent of tests positive for rhinovirus
                         # argvar=list(fun = "poly", degree = 3), #shape of response curve
                         # arglag = list(fun = "poly", degree = 3)) #shape of lag
                         argvar=list(fun = "lin"), arglag = list(fun = "lin"))
rsv_lag <- crossbasis(data_for_model$v_pos_prop_RSV_m, lag = 0, #percent of tests positive for rhinovirus
                      # argvar=list(fun = "poly", degree = 3), #shape of response curve
                      # arglag = list(fun = "poly", degree = 3)) #shape of lag
                      argvar=list(fun = "lin"), arglag = list(fun = "lin"))

flu_lag <- crossbasis(data_for_model$v_pos_prop_flu_m, lag = 0, #percent of tests positive for influenza (separate dataset)
                      # argvar=list(fun = "poly", degree = 3), #shape of response curve
                      # arglag = list(fun = "poly", degree = 3)) #shape of lag
                      argvar=list(fun = "lin"), arglag = list(fun = "lin"))


#quasiposson glm with included variables
model1 <- glm(n_cases ~  #number of cases at a station on an observed day
                NAB_station + #effect of station
                offset(log(agegroup_x_pop)) +  #offset for the population of a study area
                cup_lag +  trees_lag  + pol_other_lag + #dlnm crossbasis for each pollen type
                #cup_all_lm + trees_lm + pol_other_lm +
                rhino_lag + corona_lag  + rsv_lag +  flu_lag +
                # rhino_lag  * trees_lag +#dlnm crossbasis for each virus type
                # cup_lag  *  cup_all_lm  +# met_vpPa +
                # trees_lag * trees_lm +
                # rhino_interaction_lag +
                # v_tests_pos_Rhinovirus_ms+ 
                # all_pol_lm *flu_lag +
                # met_vpPa +
                # met_prcpmmday_ls +
                # met_tavg_s +  #met_tmindegc_s + #met_vpPa +  
                # met_tmaxdegc_s +
                # met_tmindegc_s +
              # met_sradWm2_s + #
              # met_prcp_flag +
               ns(doy, df = 12) + #not including spline anymore
              #main_cup_season +
              week_day, #day of week term
              family = quasipoisson, #quasipoisson
              data = data_for_model)  
fqaic(model1)
#summary(model1)
#str(model1)

# include the 1-day lagged residual in the model
resid_model1 <- c(rep(NA, max_lag), residuals(model1, type = "deviance"))
#resid_model1 <- c(rep(NA, 0), residuals(model1, type = "deviance"))#for the model version when pollen isn't included
model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1))  #length(resid_model1) #length(residuals(model1, type = "deviance"))

# hist(model1$fitted.values, n = 100)
# hist(model2$fitted.values, n = 200)
# hist(data_for_model$n_cases, n = 100)


# ### model diagnotistic plots
# #deviance residuals over time
# data_for_model %>%
#   ungroup() %>%
#   mutate(resid = c(rep(NA, 15), residuals(model2, type = "deviance"))) %>%
#   ggplot(aes(x = date, y = resid)) + theme_bw() +
#   geom_point() + facet_wrap(~NAB_station) + ylab("Deviance residuals")
# 
# #partial autocorrelation plots of the deviance residuals
# pacf(residuals(model1, type = "deviance"), na.action=na.omit,main="From original model")
# pacf(residuals(model2, type = "deviance"), na.action=na.omit,main="From model adjusted for residual autocorrelation")
# 
# summary(model1)
# summary(model2)


### investigate residuals and time series from a model without pollen #############################
# some basic exploration of lags and correlations
# data_for_model %>% ungroup() %>%  
#   mutate(cuplag6 = cup_lag[,7]) %>% 
#   group_by(NAB_station) %>% 
#   # dplyr::select(pbir, cuplag6, NAB_station) %>% 
#   # filter(!is.na(cuplag6)) %>% 
#   summarize(correlation = cor(cuplag6, pbir))
# ggplot(aes(x = cuplag6, y = pbir)) + geom_point(alpha = 0.2) + facet_wrap(~NAB_station) + geom_smooth(se  = FALSE) + theme_bw()
# 
# str(cup_lag)
# 

# #data exploration figure for correlations between residuals and pollen
# test <- NA
# test2 <- NA
# for(i in 1:max_lag){
#   test <- data_for_model %>% ungroup() %>%
#     mutate(focal_pol_lag = cup_lag[,i]) %>%
#     mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
#     #mutate(nopol_resid = c(1, residuals(model2, type = "deviance"))) %>%  # model2 has the lagged residuals included
#     #mutate(months = month(date)) %>% filter(months == 12 | months == 1 | months == 2) %>% #filter(months != 1 & months != 2) %>%
#     #mutate(years = year(date)) %>% filter(years == 2017) %>%
#     group_by(NAB_station) %>%
#     dplyr::select(nopol_resid, focal_pol_lag, NAB_station) %>%
#     filter(!is.na(focal_pol_lag)) %>%
#     summarize(correlation = cor(focal_pol_lag, nopol_resid)) %>%
#     mutate(lag = i - 1)
#   if(i == 1){test2 <- test}
#   if(i > 1){test2 <- bind_rows(test, test2)}  #test2 <- test
# }
# test2 %>% ggplot(aes(x = lag, y = correlation, color = NAB_station)) + geom_line(lwd = 2) + theme_bw() + 
#   ylab("correlation between Cupressaceae pollen and residuals")

# #data exploration figure for correlations between residuals and pollen when model is only in Ja season
# resid_df <- NA
# resid_df2 <- NA
# 
# data_for_model2 <- dplyr::select(data_for_model2, date, NAB_station, cup_all_lm, trees_lm)
# for(i in 1:max_lag){
#   data_for_model3 <- mutate(data_for_model2, focal_pol_lag = lag(cup_all_lm, i))
#   resid_df <- left_join(data_for_model, data_for_model3) %>% ungroup() %>%
#     mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
#     #mutate(nopol_resid = c(1, residuals(model2, type = "deviance"))) %>%  # model2 has the lagged residuals included
#     #mutate(months = month(date)) %>% filter(months == 12 | months == 1 | months == 2) %>% #filter(months != 1 & months != 2) %>%
#     #mutate(years = year(date)) %>% filter(years == 2017) %>%
#     group_by(NAB_station) %>%
#     dplyr::select(nopol_resid, focal_pol_lag, NAB_station) %>%
#     filter(!is.na(focal_pol_lag)) %>%
#     summarize(correlation = cor(focal_pol_lag, nopol_resid)) %>%
#     mutate(lag = i - 1)
#   if(i == 1){resid_df2 <- resid_df}
#   if(i > 1){resid_df2 <- bind_rows(resid_df, resid_df2)}  #test2 <- test
# }
# resid_df2 %>% ggplot(aes(x = lag, y = correlation, color = NAB_station)) + geom_line(lwd = 2) + theme_bw() + 
#   ylab("correlation between Cupressaceae pollen and residuals")
# 
# #residuals vs pollen
# resid_df <- NA
# resid_df2 <- NA
# 
# for(i in 1:max_lag){
#   data_for_model3 <- mutate(data_for_model2, focal_pol_lag = lag(cup_all_lm, i))
#   resid_df <- left_join(data_for_model, data_for_model3) %>% ungroup() %>%
#     mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
#     group_by(NAB_station) %>%
#     filter(!is.na(focal_pol_lag))
#   if(i == 1){resid_df2 <- resid_df}
#   if(i > 1){resid_df2 <- bind_rows(resid_df, resid_df2)}  #test2 <- test
# }
# resid_df2_ts <- resid_df2 %>% 
#   mutate(doy = yday(date),
#          doy2 = case_when(doy > 300 ~ doy -365,
#                           doy < 301 ~ doy )) %>% 
#   filter(doy2 < 60)  
# resid_df2_ts %>% ggplot(aes(x = doy2, y = nopol_resid)) + geom_point(alpha = 0.002) + theme_bw() + 
#   geom_line(aes(y=rollmean(nopol_resid , 7, na.pad=TRUE)), lwd = 1) +
#   ylab("correlation between Cupressaceae pollen and residuals") + xlab("Julian day") + facet_grid(season~NAB_station) + 
#   geom_line(aes(y=rollmean(cup_all_lm, 7, na.pad=TRUE)), color = "red") +
#   geom_line(aes(y=rollmean(trees_lm, 7, na.pad=TRUE)), color = "blue")






# #data exploration figure for pollen vs residuals 
# test <- NA
# test2 <- NA
# for(i in 1:max_lag){
#   test <- data_for_model %>% ungroup() %>%
#     mutate(focal_pol_lag = cup_lag[,i]) %>%
#     mutate(nopol_resid = residuals(model1, type = "deviance")) %>%  #model1
#     #mutate(nopol_resid = c(1, residuals(model2, type = "deviance"))) %>%  # model2 has the lagged residuals included
#     mutate(months = month(date)) %>% #filter(months == 12 | months == 1 | months == 2) %>% #filter(months != 1 & months != 2) %>%
#     mutate(years = year(date)) %>% #filter(years == 2017) %>%
#     group_by(NAB_station) %>%
#     dplyr::select(nopol_resid, focal_pol_lag, NAB_station, months, years) %>%
#     filter(!is.na(focal_pol_lag)) %>%
#     #summarize(correlation = cor(focal_pol_lag, nopol_resid)) %>%
#     mutate(lag = i - 1)
#   if(i == 1){test2 <- test}
#   if(i > 1){test2 <- bind_rows(test, test2)}  #test2 <- test
# }
# 
# #


# resid_df2 %>% 
#   filter(!is.na(nopol_resid)) %>% 
#   filter(!is.na(focal_pol_lag)) %>% 
#   filter(lag == 28) %>% 
#   ggplot(aes(x = focal_pol_lag, y = nopol_resid,  color = months)) + geom_point() + theme_bw() + 
#   ylab("residual") + xlab("pollen concentration")+ facet_wrap(~NAB_station) + geom_smooth(method = "lm", se = FALSE)




#resid_explor <- bind_cols(data_for_model, resid = c(rep(NA, 0), residuals(model1, type = "deviance")))#for the model version when pollen isn't included
 resid_explor <- bind_cols(data_for_model, resid = c(rep(NA, max_lag), residuals(model1, type = "deviance")))# with pollen
resid_explor %>% #str(resid_explor)
  ungroup() %>%
  mutate(doy = yday(date),
         syear = year(date)) %>%
  #mutate(cuplag6 = cup_lag[,1]) %>%
  ##filter(NAB_station == "San Antonio A") %>%
  filter(date > ymd("2015-8-01") & date < ymd("2016-08-1")) %>%

  ggplot(aes(x = date, y = resid , col = v_pos_prop_corona_m)) +  theme_bw() +
  scale_x_date(breaks = pretty(resid_explor$date, n = 82)) + scale_color_viridis_c() +
  scale_y_continuous("residuals", sec.axis = sec_axis(~ ., name = "Cup pollen")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.y.right = element_text(color = "red")) +
  facet_wrap(~NAB_station)+
  geom_point(col = "black", alpha = 0.1) +
  geom_point(aes(x = date, y = cup_all_lm), col = "red", alpha = 0.1)+
  geom_line(aes(y=rollmean(resid , 21, na.pad=TRUE), size =2), alpha = 0.9) +
  # geom_line(aes(y=rollmean(resid , 14, na.pad=TRUE)), alpha = 0.9, lwd = 2) +
  #geom_line(aes(y=rollmean( pbir * 3, 7, na.pad=TRUE)), col = "black") +
  geom_line(aes(y=rollmean(cup_all_lm , 21, na.pad=TRUE)), alpha = 0.9, col = "red")
 
# geom_line(aes(y=rollmean( cuplag6 * .5, 1, na.pad=TRUE, align = "left")), col = "red") +
# geom_line(aes(y=rollmean( trees_lm * .5, 1, na.pad=TRUE, align = "left")), col = "green") +
# geom_line(aes(y=rollmean( pol_other_lm * .5, 1, na.pad=TRUE, align = "left")), col = "yellow") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_Rhinovirus_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "blue") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_corona_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "brown") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_RSV_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "purple") +
# geom_line(aes(y=rollmean( v_pos_rel_adj_flu_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "orange")


resid_explor %>%
  mutate(doy = yday(date),
         syear = year(date)) %>%
  ggplot(aes(x = doy, y = resid, col = as.factor(syear))) + theme_bw() + #geom_smooth(se = FALSE, color = "red") +
  geom_line(aes(y=rollmean(resid , 21, na.pad=TRUE)), alpha = 0.9) +
  facet_wrap(~NAB_station) + scale_color_viridis_d()
  # geom_line(aes(y=rollmean(cup_all_lm, 7, na.pad=TRUE, align = "left")), col = "red") +
  # geom_line(aes(y=rollmean(cup_all_lm, 14, na.pad=TRUE, align = "left")), col = "orange") +
  # geom_line(aes(y=rollmean(cup_all_lm, 21, na.pad=TRUE, align = "left")), col = "yellow")


# resid_explor %>%
#   mutate(doy = yday(date)) %>%
#   mutate(cup_all_lm_14d = rollmean(cup_all_lm, 14, na.pad=TRUE)) %>%
#   #filter(date < ymd("2016-07-15")) %>%
#   filter(doy < 70 | doy > 330) %>%
#   ggplot(aes(x = cup_all_lm_14d, y = resid)) + geom_point() + theme_bw() + facet_wrap(~NAB_station) +
#   geom_smooth(se = FALSE)

# #difference between years in PBIR
# data_for_model %>% 
#   mutate(years = year(date),
#          jday = yday(date)) %>% 
#   dplyr::select(years, jday, pbir, NAB_station) %>% 
#   ggplot(aes(x=jday, y = pbir, color = as.factor(years))) + theme_bw()+ facet_wrap(~NAB_station)+
#   geom_line(aes(y=rollmean(pbir, 7, na.pad=TRUE, align = "left"))) + #scale_y_carte(limits = c(0, 50))
#   coord_cartesian(ylim = c(0,60))



### Fig 2,3,4: visualize effects of pollen ###############################################################
#cup all  
pred1_cup <- crosspred(cup_lag,  model2, #at = 1,
                       at = seq(from = 0, to = max(data_for_model$cup_all_lm), by = 10), 
                       bylag = 1, cen = 0, cumul = TRUE) #str(pred1_cup)

child_RR_x_cup_25km <- 
  data.frame(pol_conc = (pred1_cup$predvar), 
             mean = pred1_cup$allRRfit,
             lower = pred1_cup$allRRlow,
             upper = pred1_cup$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() + 
  annotation_logticks(sides = "b")  
#ggtitle(paste0("ages ", age_low, "-", age_hi, "  n_cases = ", sum(data_for_model$n_cases))) 

#child_RR_x_cup_25km <- child_RR_x_cup_25km + geom_rug(data = data_for_model, aes(x = cup_all_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

child_lag_RR_x_cup_25km <-
  as.data.frame(exp(pred1_cup$cumfit)) %>% mutate(pol_conc = pred1_cup$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ #scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + #, begin = 0.3, end = 1)  #automatically bins and turns to factor
  annotation_logticks(sides = "b")  
#ggtitle(paste0("  n_pop = ", sum(pop_near_NAB_agegroup_x$agegroup_x_pop)))
cowplot::plot_grid(child_RR_x_cup_25km, child_lag_RR_x_cup_25km)

## trees #
pred1_trees <- crosspred(trees_lag,  model2, 
                         at = seq(from = 0, to = max(data_for_model$trees_m), by = 10),
                         bylag = 1, cen = 0, cumul = TRUE)

# plot(pred1_trees, "overall", ci = "lines", #ylim = c(0.95, 4), lwd = 2,
#      xlab = expression(paste("log10(tree pollen grains m"^"3",")")), 
#      ylab = "RR", main = "Overall effect of tree pollen")
child_RR_x_trees_25km <- 
  data.frame(pol_conc = (pred1_trees$predvar), 
             mean = pred1_trees$allRRfit,
             lower = pred1_trees$allRRlow,
             upper = pred1_trees$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()+ 
  annotation_logticks(sides = "b")  

#child_RR_x_trees_25km <-  child_RR_x_trees_25km +  geom_rug(data = data_for_model, aes(x = trees_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

# plot.crosspred(pred1_trees, "contour", cumul = TRUE,
#      plot.title = title(xlab = expression(paste("log10(tree pollen grains m"^"3",")")),
#                         ylab = "Lag", main = "Cumulative RR across lags for trees"), key.title = title("RR"))
child_lag_RR_x_trees_25km <-
  as.data.frame(exp(pred1_trees$cumfit)) %>% mutate(pol_conc = pred1_trees$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("tree pollen (pollen grains / m"^"3",")")))+ #scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + 
  annotation_logticks(sides = "b")  
child_lag_RR_x_trees_25km

## pol_other
pred1_pol_other <- crosspred(pol_other_lag,  model2, 
                             at = seq(from = 0, to = max(data_for_model$pol_other_m), by = 10),
                             bylag = 1, cen = 0, cumul = TRUE)
child_RR_x_pol_other_25km <-
  data.frame(pol_conc = (pred1_pol_other$predvar),
             mean = pred1_pol_other$allRRfit,
             lower = pred1_pol_other$allRRlow,
             upper = pred1_pol_other$allRRhigh) %>%
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() + 
  annotation_logticks(sides = "b")  

#child_RR_x_pol_other_25km <- child_RR_x_pol_other_25km +geom_rug(data = data_for_model, aes(x = pol_other_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)


child_lag_RR_x_pol_other_25km <-
  as.data.frame(exp(pred1_pol_other$cumfit)) %>% mutate(pol_conc = pred1_pol_other$predvar ) %>%
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>%
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = pol_conc) %>%
  ggplot(aes(x = pol_conc_exp + 1, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ #scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + 
  annotation_logticks(sides = "b")  
child_lag_RR_x_pol_other_25km

#saving the figs 
fig234 <-
  cowplot::plot_grid(child_RR_x_cup_25km, child_lag_RR_x_cup_25km, child_RR_x_trees_25km, child_lag_RR_x_trees_25km,
                     child_RR_x_pol_other_25km, child_lag_RR_x_pol_other_25km,
                     ncol = 2, labels = c("  A) Cupressaceae pollen", 
                                          "B) Cupressaceae pollen by lag", 
                                          "  C) tree pollen", 
                                          "D) tree pollen by lag",
                                          "    E) other pollen",
                                          "F) other pollen by lag"),
                     rel_widths = c(0.8, 1, 0.8, 1),
                     label_size = 11, label_x = 0.14, label_y = 0.9, hjust = 0, vjust = 0)
fig234
fig_234_name <- paste0("Z:/THCIC/Katz/results/",
                       "fig234_pol_ages",age_low,"_",age_hi,"_dist_", NAB_min_dist_threshold, "_",Sys.Date(),".jpg")
ggsave(file = fig_234_name, plot = fig234,
       height = 25, width = 21, units = "cm", dpi = 300)


### visualize effects of viruses ###############################################################

## rhinovirus
pred1_rhino <- crosspred(rhino_lag,  model2, cen = 0, cumul = TRUE,
                             at = seq(from = 0, to = max(data_for_model$v_pos_prop_rhino_m21), by = 0.0005))
data.frame(rhino_conc = pred1_rhino$predvar,
             mean = pred1_rhino$allRRfit,
             lower = pred1_rhino$allRRlow,
             upper = pred1_rhino$allRRhigh) %>%
  ggplot(aes(x = rhino_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab("rhino")+ ylab('RR')+theme_few()

## corona
pred1_corona <- crosspred(corona_lag,  model2, cen = 0, cumul = TRUE,
                         at = seq(from = 0, to = max(data_for_model$v_pos_prop_corona_m21), by = 0.0005))
data.frame(corona_conc = pred1_corona$predvar,
           mean = pred1_corona$allRRfit,
           lower = pred1_corona$allRRlow,
           upper = pred1_corona$allRRhigh) %>%
  ggplot(aes(x = corona_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab("corona")+ ylab('RR')+theme_few()

## RSV
pred1_rsv <- crosspred(rsv_lag,  model2, cen = 0, cumul = TRUE,
                       at = seq(from = 0, to = max(data_for_model$v_pos_prop_RSV_m21), by = 0.0005))
data.frame(flu_conc = pred1_rsv$predvar,
           mean = pred1_rsv$allRRfit,
           lower = pred1_rsv$allRRlow,
           upper = pred1_rsv$allRRhigh) %>%
  ggplot(aes(x = flu_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab("rsv")+ ylab('RR')+theme_few()

## flu
pred1_flu <- crosspred(flu_lag,  model2, cen = 0, cumul = TRUE,
                          at = seq(from = 0, to = max(data_for_model$v_pos_prop_flu_m21), by = 0.0005))
data.frame(flu_conc = pred1_flu$predvar,
           mean = pred1_flu$allRRfit,
           lower = pred1_flu$allRRlow,
           upper = pred1_flu$allRRhigh) %>%
  ggplot(aes(x = flu_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab("flu")+ ylab('RR')+theme_few()

### table 2: attributable risk ##########################################
#https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata/blob/master/attrdl.R
#set up blank table
table2 <- data.frame(variable = c("Cupressaceae", "Trees", "Other_pollen", "Rhinovirus", "Corona", "RSV",
                                  "Influenza", "Total"), 
                     yc_n_cases_mean = rep(NA, 8), yc_n_cases_2.5 = rep(NA, 8), yc_n_cases_97.5 = rep(NA, 8),
                     yc_p_cases_mean = rep(NA, 8), yc_p_cases_2.5 = rep(NA, 8), yc_p_cases_97.5 = rep(NA, 8),
                     c_n_cases_mean = rep(NA, 8), c_n_cases_2.5 = rep(NA, 8), c_n_cases_97.5 = rep(NA, 8),
                     c_p_cases_mean = rep(NA, 8), c_p_cases_2.5 = rep(NA, 8), c_p_cases_97.5 = rep(NA, 8),
                     a_n_cases_mean = rep(NA, 8), a_n_cases_2.5 = rep(NA, 8), a_n_cases_97.5 = rep(NA, 8),
                     a_p_cases_mean = rep(NA, 8), a_p_cases_2.5 = rep(NA, 8), a_p_cases_97.5 = rep(NA, 8))

cup_attr <- attrdl(x = data_for_model$cup_all_m, basis = cup_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                   cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[1] <- sprintf("%.0f", mean(cup_attr))
table2$yc_n_cases_2.5[1] <- sprintf("%.0f", as.numeric(quantile(cup_attr, probs = 0.025)))
table2$yc_n_cases_97.5[1] <- sprintf("%.0f", as.numeric(quantile(cup_attr, probs = 0.975)))
table2$yc_p_cases_mean[1] <- sprintf("%.1f", 100*mean(cup_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[1] <- sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[1] <- sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.975))/sum(model2$fitted.values))


trees_attr <- attrdl(x = data_for_model$trees_m, basis = trees_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                     cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[2] <- sprintf("%.0f", mean(trees_attr))
table2$yc_n_cases_2.5[2] <- sprintf("%.0f", as.numeric(quantile(trees_attr, probs = 0.025)))
table2$yc_n_cases_97.5[2] <- sprintf("%.0f", as.numeric(quantile(trees_attr, probs = 0.975)))
table2$yc_p_cases_mean[2] <- sprintf("%.1f", 100* mean(trees_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[2] <- sprintf("%.1f", 100*as.numeric(quantile(trees_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[2] <- sprintf("%.1f", 100*as.numeric(quantile(trees_attr, probs = 0.975))/sum(model2$fitted.values))


other_pol_attr <- attrdl(x = data_for_model$pol_other_m, basis = pol_other_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                         cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[3] <- sprintf("%.0f", mean(other_pol_attr))
table2$yc_n_cases_2.5[3] <- sprintf("%.0f", as.numeric(quantile(other_pol_attr, probs = 0.025)))
table2$yc_n_cases_97.5[3] <- sprintf("%.0f", as.numeric(quantile(other_pol_attr, probs = 0.975)))
table2$yc_p_cases_mean[3] <- sprintf("%.1f", 100* mean(other_pol_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[3] <- sprintf("%.1f", 100*as.numeric(quantile(other_pol_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[3] <- sprintf("%.1f", 100*as.numeric(quantile(other_pol_attr, probs = 0.975))/sum(model2$fitted.values))

#rhinovirus
rhino_attr <- attrdl(x = data_for_model$v_pos_prop_rhino_m21, basis = rhino_lag, cases = data_for_model$n_cases, 
                     model = model2, dir = "back", sim = TRUE, cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[4] <- sprintf("%.0f", mean(rhino_attr))
table2$yc_n_cases_2.5[4] <- sprintf("%.0f", as.numeric(quantile(rhino_attr, probs = 0.025)))
table2$yc_n_cases_97.5[4] <- sprintf("%.0f", as.numeric(quantile(rhino_attr, probs = 0.975)))
table2$yc_p_cases_mean[4] <- sprintf("%.1f", 100*mean(rhino_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[4] <- sprintf("%.1f", 100*as.numeric(quantile(rhino_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[4] <- sprintf("%.1f", 100*as.numeric(quantile(rhino_attr, probs = 0.975))/sum(model2$fitted.values))

# rhino_inter_attr <- attrdl(x = data_for_model$v_tests_perc_pos_Rhinovirus_m *data_for_model$v_tests_pos_Rhinovirus_ms, 
#                      basis = rhino_interaction_lag, cases = data_for_model$n_cases, 
#                      model = model2, dir = "back", sim = TRUE, cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
# sprintf("%.1f", 100*mean(rhino_inter_attr) / sum(model2$fitted.values))

#corona
corona_attr <- attrdl(x = data_for_model$v_pos_prop_corona_m21, basis = corona_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                      cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[5] <- sprintf("%.0f", mean(corona_attr))
table2$yc_n_cases_2.5[5] <- sprintf("%.0f", as.numeric(quantile(corona_attr, probs = 0.025)))
table2$yc_n_cases_97.5[5] <- sprintf("%.0f", as.numeric(quantile(corona_attr, probs = 0.975)))
table2$yc_p_cases_mean[5] <- sprintf("%.1f", 100*mean(corona_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[5] <- sprintf("%.1f", 100*as.numeric(quantile(corona_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[5] <- sprintf("%.1f", 100*as.numeric(quantile(corona_attr, probs = 0.975))/sum(model2$fitted.values))

#rsv
rsv_attr <- attrdl(x = data_for_model$v_pos_prop_RSV_m21, basis = rsv_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                   cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[6] <- sprintf("%.0f", mean(rsv_attr))
table2$yc_n_cases_2.5[6] <- sprintf("%.0f", as.numeric(quantile(rsv_attr, probs = 0.025)))
table2$yc_n_cases_97.5[6] <- sprintf("%.0f", as.numeric(quantile(rsv_attr, probs = 0.975)))
table2$yc_p_cases_mean[6] <- sprintf("%.1f", 100*mean(rsv_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[6] <- sprintf("%.1f", 100*as.numeric(quantile(rsv_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[6] <- sprintf("%.1f", 100*as.numeric(quantile(rsv_attr, probs = 0.975))/sum(model2$fitted.values))


#flu
flu_attr <- attrdl(x = data_for_model$v_pos_prop_flu_m21, basis = flu_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                   cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[7] <- sprintf("%.0f", mean(flu_attr))
table2$yc_n_cases_2.5[7] <- sprintf("%.0f", as.numeric(quantile(flu_attr, probs = 0.025)))
table2$yc_n_cases_97.5[7] <- sprintf("%.0f", as.numeric(quantile(flu_attr, probs = 0.975)))
table2$yc_p_cases_mean[7] <- sprintf("%.1f", 100*mean(flu_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[7] <- sprintf("%.1f", 100*as.numeric(quantile(flu_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[7] <- sprintf("%.1f", 100*as.numeric(quantile(flu_attr, probs = 0.975))/sum(model2$fitted.values))


table2

#prepare Table 2 for pasting into excel/word
table2$yc_n_cases_mean[8] <- sum(as.numeric(table2$yc_n_cases_mean[1:7]))
table2$yc_p_cases_mean[8] <- sum(as.numeric(table2$yc_p_cases_mean[1:7]), na.rm = T)

table2_paste <- data.frame(col1 = paste0(table2$yc_n_cases_mean, " (", table2$yc_n_cases_2.5, " - ", table2$yc_n_cases_97.5, ")"),
                           col2 = paste0(table2$yc_p_cases_mean, " (", table2$yc_p_cases_2.5, " - ", table2$yc_p_cases_97.5, ")"))
write.table(table2_paste, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)
table2_paste
# #back of the envelope calculations to see whether these AR estimates are in the right ballpark
# #predicted RR for a 1 unit increase (i.e., 10x since there's a log10 transformation on pollen): ~1.17
# sum((data_for_model$n_cases[22: nrow(data_for_model)])) * mean(data_for_model$trees_lm[22: nrow(data_for_model)]) * 0.17
# sum(data_for_model$n_cases[22: nrow(data_for_model)] * data_for_model$trees_lm[22: nrow(data_for_model)] * 0.17)
# (1.17 - 1)/1.17


### attributable risk over time figure ################################################
attr_t_cup <- attrdl(x = data_for_model$cup_all_lm, basis = cup_lag, cases = data_for_model$n_cases, 
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_trees <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases, 
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_other_pol <- attrdl(x = data_for_model$pol_other_lm, basis = pol_other_lag, cases = data_for_model$n_cases,
                           model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_rhino <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases, 
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_corona <- attrdl(x = data_for_model$v_pos_rel_adj_corona_m21, basis = corona_lag, cases = data_for_model$n_cases, 
                        model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_flu <- attrdl(x = data_for_model$v_pos_rel_adj_flu_m21, basis = flu_lag, cases = data_for_model$n_cases, 
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_rsv <- attrdl(x = data_for_model$v_pos_rel_adj_RSV_m21, basis = rsv_lag, cases = data_for_model$n_cases, 
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

#predicted_n_cases_t <- c(rep(NA, max_lag + 0), model1$fitted.values) #hist(model2$fitted.values)
#predicted_n_cases_t <- c(rep(NA, max_lag + 1), model2$fitted.values) #hist(model2$fitted.values)

# fit <- lm(data_for_model$n_cases ~ predicted_n_cases_t)
# summary(fit)
# plot(jitter(data_for_model$n_cases), predicted_n_cases_t)

attr_df <- data.frame(attr_t_cup, attr_t_trees, attr_t_other_pol, attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_rsv) #attr_t_other_pol, , predicted_n_cases_t
attr_df[attr_df < 0] <- 0 #removing net protective effects of all variables
#data_for_model$n_cases
# attr_df_p <- attr_df/predicted_n_cases_t
# summary(attr_df_p)
attr_full_df <- bind_cols(data_for_model, attr_df) %>% 
  # mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu - attr_t_other_pol) %>% 
  dplyr::select(date, NAB_station, agegroup_x_pop, 
                #attr_t_unexplained, 
                attr_t_cup, attr_t_trees, attr_t_other_pol, 
                attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_rsv) %>%   #attr_t_other_pol,
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
  mutate(week = week(date)) %>% 
  group_by(NAB_station, agegroup_x_pop, week, var) %>% 
  summarize(risk_cases = mean(risk_cases)) %>% 
  mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
                                                     Rhinovirus = "attr_t_rhino", 
                                                     Corona = "attr_t_corona",
                                                     RSV = "attr_t_rsv",
                                                     Influenza = "attr_t_flu",
                                                     Cupressaceae = "attr_t_cup", 
                                                     trees = "attr_t_trees", 
                                                     'other pollen' = "attr_t_other_pol"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "RSV","Influenza",
                                                                       "Cupressaceae", "trees", "other pollen")), #"other_pollen"  "unexplained"
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))

observed_ncases_t <- data_for_model %>% 
  dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
  mutate(week = week(date),
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 )) %>% 
  group_by(NAB_station, agegroup_x_pop, week, date2) %>% 
  summarize(mean_cases = mean(n_cases)) %>% 
  mutate(attr_risk_var = "Rhinovirus") 


fig567_ymax <-  ifelse(age_low == 0,   20, #setting figure ymax manually
                       (ifelse(age_low == 5,  55, 
                               (ifelse(age_low == 18, 10, 
                                       print("error in age_low"))))))
fig567 <- ggplot(attr_full_df, aes(x = date2, y = (risk_cases / agegroup_x_pop) * 1000000, fill = attr_risk_var)) + 
  facet_wrap(~NAB_station) + geom_area() + theme_bw() + scale_fill_viridis_d(name = "attributable risk") + 
  ylab("asthma-related ED visits (per 1,000,000 people per day)") + xlab("date") +
  scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(0, fig567_ymax)) + #c(-0.01, .15) #c(-0.02, .6)
  geom_step(data = observed_ncases_t, aes( x = date2, y =(mean_cases / agegroup_x_pop) * 1000000, 
                                           color = "observed cases")) + scale_color_discrete(name = "") 

#fig567
fig_567_name <- paste0("C:/Users/dsk856/Desktop/thcic_analysis/results/",
                       "fig567_AR_ages",age_low,"_",age_hi,"_dist_", NAB_min_dist_threshold, "_",Sys.Date(),".jpg")
ggsave(file = fig_567_name, plot = fig567, height = 25, width = 21, units = "cm", dpi = 300)






### attributable risk fraction over time ################################################
attr_tf_cup <- attrdl(x = data_for_model$cup_all_lm, basis = cup_lag, cases = data_for_model$n_cases,
                      model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_trees <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases,
                        model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_other_pol <- attrdl(x = data_for_model$pol_other_lm, basis = pol_other_lag, cases = data_for_model$n_cases,
                            model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_rhino <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases,
                        model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_corona <- attrdl(x = data_for_model$v_pos_rel_adj_corona_m21, basis = corona_lag, cases = data_for_model$n_cases,
                         model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_flu <- attrdl(x = data_for_model$v_pos_rel_adj_flu_m21, basis = flu_lag, cases = data_for_model$n_cases,
                      model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_rsv <- attrdl(x = data_for_model$v_pos_rel_adj_RSV_m21, basis = rsv_lag, cases = data_for_model$n_cases,
                      model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_f_df <- data.frame(attr_tf_cup, attr_tf_trees, attr_tf_other_pol, attr_tf_rhino, attr_tf_corona, attr_tf_flu, attr_tf_rsv) 
attr_f_df[attr_f_df < 0] <- 0 #removing net protective effects of all variables

### SI table: the percent of AR for each pollen type by each city across the main pollen season ##################
attr_f_cup_jan <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_cup) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  filter(date_month == 1) %>%
  group_by(NAB_station) %>%
  summarize(cup_prop_cases_jan = mean(attr_tf_cup))
attr_f_cup_allyr <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_cup) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  group_by(NAB_station, date_month) %>%
  summarize(cup_prop_cases_all_mo_r = mean(attr_tf_cup, na.rm = TRUE)) %>% #so the extra fall months aren't given more weight
  group_by(NAB_station) %>% 
  summarize(cup_prop_cases_all_mo = mean(cup_prop_cases_all_mo_r))


attr_f_trees_mar <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_trees) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  filter(date_month == 3) %>%
  group_by(NAB_station) %>%
  summarize(trees_prop_cases_mar = mean(attr_tf_trees))
attr_f_trees_allyr <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_trees) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  group_by(NAB_station, date_month) %>%
  summarize(trees_prop_cases_all_mo_r = mean(attr_tf_trees, na.rm = TRUE)) %>% #so the extra fall months aren't given more weight
  group_by(NAB_station) %>% 
  summarize(trees_prop_cases_all_mo = mean(trees_prop_cases_all_mo_r))


#prepare for pasting into excel/word
cup_by_city_jan_wholeyear <- sprintf("%.1f", c(100*attr_f_cup_jan$cup_prop_cases_jan, 100*attr_f_cup_allyr$cup_prop_cases_all_mo))
write.table(cup_by_city_jan_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

trees_by_city_mar_wholeyear <- sprintf("%.1f", c(100*attr_f_trees_mar$trees_prop_cases_mar, 
                                                 100*attr_f_trees_allyr$trees_prop_cases_all_mo))
write.table(trees_by_city_mar_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)



### model/Table 2 SI version without viruses ########################################################

model1 <- glm(n_cases ~  #number of cases at a station on an observed day
                NAB_station + #effect of station
                offset(log(agegroup_x_pop)) +  #offset for the population of a study area
                cup_lag +  trees_lag  + pol_other_lag + #dlnm crossbasis for each pollen type
                #rhino_lag + corona_lag  + rsv_lag +  flu_lag +
                week_day, #day of week term
              family = quasipoisson, #quasipoisson
              data = data_for_model)  

# include the 1-day lagged residual in the model
resid_model1 <- c(rep(NA, max_lag), residuals(model1, type = "deviance"))
model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1))  #length(resid_model1) #length(residuals(model1, type = "deviance"))

table2 <- data.frame(variable = c("Cupressaceae", "Trees", "Other_pollen", "Rhinovirus", "Corona", "RSV",
                                  "Influenza"), 
                     yc_n_cases_mean = rep(NA, 7), yc_n_cases_2.5 = rep(NA, 7), yc_n_cases_97.5 = rep(NA, 7),
                     yc_p_cases_mean = rep(NA, 7), yc_p_cases_2.5 = rep(NA, 7), yc_p_cases_97.5 = rep(NA, 7),
                     c_n_cases_mean = rep(NA, 7), c_n_cases_2.5 = rep(NA, 7), c_n_cases_97.5 = rep(NA, 7),
                     c_p_cases_mean = rep(NA, 7), c_p_cases_2.5 = rep(NA, 7), c_p_cases_97.5 = rep(NA, 7),
                     a_n_cases_mean = rep(NA, 7), a_n_cases_2.5 = rep(NA, 7), a_n_cases_97.5 = rep(NA, 7),
                     a_p_cases_mean = rep(NA, 7), a_p_cases_2.5 = rep(NA, 7), a_p_cases_97.5 = rep(NA, 7))

cup_attr <- attrdl(x = data_for_model$cup_all_lm, basis = cup_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                   cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[1] <- sprintf("%.0f", mean(cup_attr))
table2$yc_n_cases_2.5[1] <- sprintf("%.0f", as.numeric(quantile(cup_attr, probs = 0.025)))
table2$yc_n_cases_97.5[1] <- sprintf("%.0f", as.numeric(quantile(cup_attr, probs = 0.975)))
table2$yc_p_cases_mean[1] <- sprintf("%.1f", 100*mean(cup_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[1] <- sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[1] <- sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.975))/sum(model2$fitted.values))

trees_attr <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                     cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[2] <- sprintf("%.0f", mean(trees_attr))
table2$yc_n_cases_2.5[2] <- sprintf("%.0f", as.numeric(quantile(trees_attr, probs = 0.025)))
table2$yc_n_cases_97.5[2] <- sprintf("%.0f", as.numeric(quantile(trees_attr, probs = 0.975)))
table2$yc_p_cases_mean[2] <- sprintf("%.1f", 100* mean(trees_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[2] <- sprintf("%.1f", 100*as.numeric(quantile(trees_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[2] <- sprintf("%.1f", 100*as.numeric(quantile(trees_attr, probs = 0.975))/sum(model2$fitted.values))

other_pol_attr <- attrdl(x = data_for_model$pol_other_lm, basis = pol_other_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                         cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[3] <- sprintf("%.0f", mean(other_pol_attr))
table2$yc_n_cases_2.5[3] <- sprintf("%.0f", as.numeric(quantile(other_pol_attr, probs = 0.025)))
table2$yc_n_cases_97.5[3] <- sprintf("%.0f", as.numeric(quantile(other_pol_attr, probs = 0.975)))
table2$yc_p_cases_mean[3] <- sprintf("%.1f", 100* mean(other_pol_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[3] <- sprintf("%.1f", 100*as.numeric(quantile(other_pol_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[3] <- sprintf("%.1f", 100*as.numeric(quantile(other_pol_attr, probs = 0.975))/sum(model2$fitted.values))



# version without pollen -------------------------------------------------------
model1 <- glm(n_cases ~  #number of cases at a station on an observed day
                NAB_station + #effect of station
                offset(log(agegroup_x_pop)) +  #offset for the population of a study area
                #cup_lag +  trees_lag  + pol_other_lag + #dlnm crossbasis for each pollen type
                rhino_lag + corona_lag  + rsv_lag +  flu_lag +
                week_day, #day of week term
              family = quasipoisson, #quasipoisson
              data = data_for_model)  

# include the 1-day lagged residual in the model
resid_model1 <- c(rep(NA, 0), residuals(model1, type = "deviance"))#for the model version when pollen isn't included
model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1))  #length(resid_model1) #length(residuals(model1, type = "deviance"))

#rhinovirus
rhino_attr <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases, 
                     model = model2, dir = "back", sim = TRUE, cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[4] <- sprintf("%.0f", mean(rhino_attr))
table2$yc_n_cases_2.5[4] <- sprintf("%.0f", as.numeric(quantile(rhino_attr, probs = 0.025)))
table2$yc_n_cases_97.5[4] <- sprintf("%.0f", as.numeric(quantile(rhino_attr, probs = 0.975)))
table2$yc_p_cases_mean[4] <- sprintf("%.1f", 100*mean(rhino_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[4] <- sprintf("%.1f", 100*as.numeric(quantile(rhino_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[4] <- sprintf("%.1f", 100*as.numeric(quantile(rhino_attr, probs = 0.975))/sum(model2$fitted.values))

#corona
corona_attr <- attrdl(x = data_for_model$v_pos_rel_adj_corona_m21, basis = corona_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                      cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[5] <- sprintf("%.0f", mean(corona_attr))
table2$yc_n_cases_2.5[5] <- sprintf("%.0f", as.numeric(quantile(corona_attr, probs = 0.025)))
table2$yc_n_cases_97.5[5] <- sprintf("%.0f", as.numeric(quantile(corona_attr, probs = 0.975)))
table2$yc_p_cases_mean[5] <- sprintf("%.1f", 100*mean(corona_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[5] <- sprintf("%.1f", 100*as.numeric(quantile(corona_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[5] <- sprintf("%.1f", 100*as.numeric(quantile(corona_attr, probs = 0.975))/sum(model2$fitted.values))

#rsv
rsv_attr <- attrdl(x = data_for_model$v_pos_rel_adj_RSV_m21, basis = rsv_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                   cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[6] <- sprintf("%.0f", mean(rsv_attr))
table2$yc_n_cases_2.5[6] <- sprintf("%.0f", as.numeric(quantile(rsv_attr, probs = 0.025)))
table2$yc_n_cases_97.5[6] <- sprintf("%.0f", as.numeric(quantile(rsv_attr, probs = 0.975)))
table2$yc_p_cases_mean[6] <- sprintf("%.1f", 100*mean(rsv_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[6] <- sprintf("%.1f", 100*as.numeric(quantile(rsv_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[6] <- sprintf("%.1f", 100*as.numeric(quantile(rsv_attr, probs = 0.975))/sum(model2$fitted.values))


#flu
flu_attr <- attrdl(x = data_for_model$v_pos_rel_adj_flu_m21, basis = flu_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
                   cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[7] <- sprintf("%.0f", mean(flu_attr))
table2$yc_n_cases_2.5[7] <- sprintf("%.0f", as.numeric(quantile(flu_attr, probs = 0.025)))
table2$yc_n_cases_97.5[7] <- sprintf("%.0f", as.numeric(quantile(flu_attr, probs = 0.975)))
table2$yc_p_cases_mean[7] <- sprintf("%.1f", 100*mean(flu_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[7] <- sprintf("%.1f", 100*as.numeric(quantile(flu_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[7] <- sprintf("%.1f", 100*as.numeric(quantile(flu_attr, probs = 0.975))/sum(model2$fitted.values))


#prepare Table 2 for pasting into excel/word
table2_SI_paste <- data.frame(col1 = paste0(table2$yc_n_cases_mean, " (", table2$yc_n_cases_2.5, " - ", table2$yc_n_cases_97.5, ")"),
                              col2 = paste0(table2$yc_p_cases_mean, " (", table2$yc_p_cases_2.5, " - ", table2$yc_p_cases_97.5, ")"))
write.table(table2_SI_paste, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)
table2_SI_paste




### another version of everything that needs to be pasted into the figures in word ####################☻

#table 2
write.table(table2_paste, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

#table 2 SI (only pollen or only viruses)
write.table(table2_SI_paste, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

#effects of pollen during highest month per city
cup_by_city_jan_wholeyear <- sprintf("%.1f", c(100*attr_f_cup_jan$cup_prop_cases_jan, 100*attr_f_cup_allyr$cup_prop_cases_all_mo))
write.table(cup_by_city_jan_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

trees_by_city_mar_wholeyear <- sprintf("%.1f", c(100*attr_f_trees_mar$trees_prop_cases_mar, 
                                                 100*attr_f_trees_allyr$trees_prop_cases_all_mo))
write.table(trees_by_city_mar_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)






# attr_full_df <- bind_cols(data_for_model, attr_df) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 attr_t_cup, attr_t_trees, attr_t_other_pol, 
#                 attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_rsv) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(week = week(date)) %>% 
#   group_by(NAB_station, agegroup_x_pop, week, var) %>% 
#   summarize(risk_cases = mean(risk_cases)) %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Rhinovirus = "attr_t_rhino", 
#                                                      Corona = "attr_t_corona",
#                                                      RSV = "attr_t_rsv",
#                                                      Influenza = "attr_t_flu",
#                                                      Cupressaceae = "attr_t_cup", 
#                                                      trees = "attr_t_trees", 
#                                                      'other pollen' = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "RSV","Influenza",
#                                                                        "Cupressaceae", "trees", "other pollen")), #"other_pollen"  "unexplained"
#          date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))
# ggplot(attr_full_df, aes(x = date2, y = (risk_cases), fill = attr_risk_var)) + 
#   facet_wrap(~NAB_station) + geom_area() + theme_bw() + scale_fill_viridis_d(name = "attributable risk") + 
#   ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
#   scale_x_date(labels = date_format("%b")) 



# ## a table giving the percent of AR for each pollen type by each city across the study period
# bind_cols(data_for_model, attr_f_df) %>% 
#   group_by(NAB_station) %>% 
#   summarize(attr_tf_cup_mean = mean(attr_tf_cup, na.rm = TRUE), 
#             attr_tf_trees_mean = mean(attr_tf_trees, na.rm = TRUE),
#             attr_tf_other_pol_mean = mean(attr_tf_other_pol, na.rm = TRUE))
# 
# ## table: the percent of AR for each pollen type by each city across the main pollen season
# bind_cols(data_for_model, attr_f_df) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_cup) %>%   #attr_t_other_pol,
#   mutate(date_month = month(date)) %>% 
#   filter(date_month == 1) %>% 
#   group_by(NAB_station) %>% 
#   summarize(cup_prop_cases_jan = mean(attr_tf_cup))
# 
# bind_cols(data_for_model, attr_f_df) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop, attr_tf_trees) %>%   #attr_t_other_pol,
#   mutate(date_month = month(date)) %>% 
#   filter(date_month == 3) %>% 
#   group_by(NAB_station) %>% 
#   summarize(trees_prop_cases_march = mean(attr_tf_trees))


# #testing why cup AR isn't higher for adults (dose response curve is sig, table 2)
# hist(attrdl(x = data_for_model$cup_all_lm[data_for_model$cup_all_lm > 2.5], basis = cup_lag, 
#                cases = data_for_model$n_cases[data_for_model$cup_all_lm > 2.5], model = model2,
#                    dir = "back", sim = TRUE,
#                    cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 1000), breaks = 50)
# hist(data_for_model$cup_all_lm, breaks = 50)
# 
# cup_attr <- attrdl(x = data_for_model$cup_all_lm[data_for_model$cup_all_lm > 2.5], basis = cup_lag, 
#                    cases = data_for_model$n_cases[data_for_model$cup_all_lm > 2.5], model = model2,
#                    dir = "back", sim = TRUE,
#                    cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 1000)
# sprintf("%.1f", 100*mean(cup_attr) / sum(model2$fitted.values))
# sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.025))/sum(model2$fitted.values))
# sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.975))/sum(model2$fitted.values))
# 
# plot(data_for_model$n_cases, c(rep(NA, 8,), model2$fitted.values))
# length(data_for_model$n_cases); length(model2$fitted.values)
# sum(data_for_model$n_cases); sum(model2$fitted.values)
# 
# attr_t_cup <- attrdl(x = data_for_model$cup_all_lm , basis = cup_lag, cases = data_for_model$n_cases *2, 
#                      model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)
# #hist(attr_t_cup, breaks = 50)
# plot(attr_t_cup)
# mean(attr_t_cup, na.rm = TRUE)
# 
# ### a figure of just one city for the entire time period  
# NAB_station_filter <- "San Antonio A"
# attr_full_ex_df <- bind_cols(data_for_model, attr_df) %>% 
#   filter(NAB_station == NAB_station_filter) %>% 
#   # mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu - attr_t_other_pol) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 #attr_t_unexplained, 
#                 attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_other_pol) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Rhinovirus = "attr_t_rhino", Corona = "attr_t_corona", Influenza = "attr_t_flu",
#                                                      Cupressaceae = "attr_t_cup", trees = "attr_t_trees", other_pollen = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza",
#                                                  "Cupressaceae", "trees", "other_pollen"))) #"other_pollen"  "unexplained"
# 
# 
# observed_ncases_ex_t <- data_for_model %>% 
#   filter(NAB_station == NAB_station_filter) %>% 
#   dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
#   mutate(attr_risk_var = "Rhinovirus")
# 
# 
# test_data <- data_for_model 
# test_model_pred <- predict.glm(object = model2, newdata = test_data, type = "response")  #str(model_pred)
# 
# predicted_ncases_ex_t <-  data_for_model %>% ungroup() %>% 
#   mutate(predicted_ncases_vector_model1 = c(rep(NA, max_lag + 0), model1$fitted.values),
#          predicted_ncases_resid_model1 =  c(rep(NA, max_lag + 0),  model1$residuals),   
#          predicted_ncases_vector_model2 = c(rep(NA, max_lag + 1), model2$fitted.values),
#          predicted_ncases_resid_model2 =  c(rep(NA, max_lag + 1),  model2$residuals),   
#          predicted_ncases_glmpred = test_model_pred) %>% 
#   filter(NAB_station == NAB_station_filter) %>%                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#   mutate(attr_risk_var = "Rhinovirus")
# 
# ggplot(attr_full_ex_df, aes(x = date, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) + 
#   facet_wrap(~NAB_station) +  theme_bw() + scale_fill_viridis_d(name = "attributable risk") + # geom_area() +
#   ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
#   #coord_cartesian(ylim = c(-0.02, .9)) + #c(-0.01, .15) #c(-0.02, .6)
#   geom_line(data = observed_ncases_ex_t, aes( x = date, y =(n_cases / agegroup_x_pop) * 10000, 
#                                            color = "observed cases"), color = "black") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y =(predicted_ncases_vector_model1 / agegroup_x_pop) * 10000, 
#                                               color = "predicted cases"), color = "red") +
#   # geom_line(data = predicted_ncases_ex_t, aes( x = date, y =(predicted_ncases_resid_model1 / agegroup_x_pop) * 10000, 
#   #                                              color = "predicted cases"), color = "green") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y = cup_all_lm/3.7, 
#                                                color = "predicted cases"), color = "blue") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y = trees_lm/3.7, 
#                                                color = "predicted cases"), color = "green") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y = v_tests_perc_pos_Rhinovirus_m/100, 
#                                                color = "predicted cases"), color = "pink") +
#   
#   #scale_color_discrete(name = "") +
#   scale_x_date(limits = c(mdy("1/10/2016"), mdy("12/1/2016")))# labels = date_format("%b")) 
# 
# max(predicted_ncases_ex_t$cup_all_lm)
# 
# hist(model2$effects)
# str(model2)
# 
# model2$residuals[1100:1110] 
# (model2$fitted.values[1100:1110] )
# model2$y[1100:1110]
# 
# 
# ### figure of all time series as a kind of check
# bind_cols(data_for_model, attr_df) %>% 
#   ggplot(aes(x = date, y = pbir)) + geom_point(color = "gray") + theme_bw() + facet_wrap(~NAB_station) +
#   #geom_smooth(se = F, color = "black")+
#   geom_line(aes(y=zoo::rollmean(pbir, 7, na.pad=TRUE)), color = "black") +
#   
#   geom_line( aes( x = date, y = (cup_all_lm)/5,  color = "predicted cases"), color = "blue") +
#   geom_line( aes( x = date, y = (trees_lm)/5,  color = "predicted cases"), color = "green") +
#   
#   scale_y_continuous(limits = c(0, 1)) +
#   scale_x_date(limits = c(mdy("1/10/2016"), mdy("12/1/2016")))# labels = date_format("%b")) 
# 
# 
# 
# 
# ### for the animation
# attr_full_df_example <- bind_cols(data_for_model, attr_df) %>% 
#   #mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>%  
#   #- attr_t_other_pol) %>% #attr_t_other_pol
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(week = week(date)) %>% 
#   group_by(NAB_station, agegroup_x_pop, week, var) %>% 
#   summarize(risk_cases = mean(risk_cases)) %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Corona = "attr_t_corona", Influenza = "attr_t_flu",
#                                                      Rhinovirus = "attr_t_rhino", Cupressaceae = "attr_t_cup", trees = "attr_t_trees"),  #other_pollen = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Corona", "Influenza","Rhinovirus", 
#                                                                         "trees", "Cupressaceae")), #"other_pollen"
#          date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))
# 
# observed_ncases_t_example <-filter(observed_ncases_t, NAB_station == "San Antonio B") 
# 
# filter(attr_full_df_example, NAB_station == "San Antonio B") %>% 
# ggplot( aes(x = date2, y = (risk_cases / agegroup_x_pop) * 100000, fill = attr_risk_var)) + 
#  geom_area() + theme_bw() + 
#   scale_fill_manual(name = "attributable risk", values = c(rgb(68, 1, 84, alpha = 0, names = NULL, maxColorValue = 255), #corona
#                                                            rgb(253, 231, 37, alpha = 1, names = NULL, maxColorValue = 255), #cupr
#                                                            rgb(72, 40, 120, alpha = 0, names = NULL, maxColorValue = 255), #flu
#                                                            rgb(49, 104, 142, alpha = 0, names = NULL, maxColorValue = 255),  #rhino
#                                                            rgb(109, 205, 89, alpha = 0, names = NULL, maxColorValue = 255))) + #trees
#   ylab("asthma-related ED visits (per 100,000 people per day)") + xlab("date") +
#   scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(-0.02, 8)) + #c(-0.01, .15) #c(-0.02, .6)
#   geom_step(data = observed_ncases_t_example, aes( x = date2, y =(mean_cases / agegroup_x_pop) * 100000,
#                                                    color = "observed cases", fill = NA), 
#             color = rgb(1,0,0, alpha = 1, maxColorValue = 1)) +
#   theme(text = element_text(size=18))
# 
# 
# 
# 
# bind_cols(data_for_model, attr_df) %>% 
#   mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>%  
#   #- attr_t_other_pol) %>% #attr_t_other_pol
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 attr_t_unexplained, attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(week = week(date)) %>% 
#   group_by(NAB_station, agegroup_x_pop, week, var) %>% 
#   summarize(risk_cases = mean(risk_cases)) %>% 
#   filter(attr_full_df, NAB_station == "San Antonio B") %>%
#   filter(var != "attr_t_unexplained")  %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Rhinovirus = "attr_t_rhino", Corona = "attr_t_corona", Influenza = "attr_t_flu",
#                                                      Cupressaceae = "attr_t_cup", trees = "attr_t_trees"),  #other_pollen = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza",
#                                                                        "Cupressaceae", "trees"))) %>% 
# ggplot(aes(x = date2, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) +
#   geom_area() +
#   theme_bw() + scale_fill_viridis_d(name = "attributable risk") +
#   ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
#   scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(-0.02, .7)) +
# 
# 
# 
# 

# # raw version
# attr_full_df <- bind_cols(data_for_model, attr_df) %>% 
#   mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 attr_t_unexplained, attr_t_cup, attr_t_trees, attr_t_rhino, attr_t_corona, attr_t_flu) %>% 
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") 
# observed_ncases_t <- data_for_model %>% filter(NAB_station == "San Antonio A") %>% 
#   dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
#   mutate(var = "unexplained")
# attr_full_df %>% filter(NAB_station == "San Antonio A") %>% 
# ggplot(aes(x = date, y = (risk_cases / agegroup_x_pop) * 10000, fill = var)) + 
#   facet_wrap(~NAB_station) + geom_area() + theme_bw() + #scale_fill_viridis_d(name = "risk variable") + 
#   ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
#   #scale_x_date(labels = date_format("%b")) + 
#   geom_point(data = observed_ncases_t, aes( x = date, y =(n_cases / agegroup_x_pop) * 10000), color = "red") +
#   coord_cartesian(ylim = c(0.02, 0.2)) 







