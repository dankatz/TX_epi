

#rm(list = ls())
### this script is for assembling the NREVSS data that was requested from DHHS
# this was originally in the epi_preliminary_analysis_220805.R script but has been moved here

v_orig <- read_csv("Z:/THCIC/Katz/data_viral/TX_NREVSS_2003_2022.csv") 
#note: there are some rows that have the same city and date but different test info. I am summing them.
# unique(v$City)
# names(v)

#add nearby cities together for a more robust metro-region area and then filter to align with NAB data
v <- v_orig %>% 
  mutate( dates = mdy(WeekEndingDate)) %>% 
  mutate( 
    NAB_region = case_when(City == "Dallas" ~ "Dallas/FlowerMound", 
                           City == "Mesquite" ~ "Dallas/FlowerMound", 
                           City == "Fort Worth" ~ "Dallas/FlowerMound", 
                           City == "Plano" ~ "Dallas/FlowerMound", 
                           City == "Denton" ~ "Dallas/FlowerMound", 
                           City == "Richardson" ~ "Dallas/FlowerMound", 
                           City == "Allen" ~ "Dallas/FlowerMound", 
                           City == "Arlington" ~ "Dallas/FlowerMound", 
                           City == "Frisco" ~ "Dallas/FlowerMound", 
                           City == "Irvins" ~ "Dallas/FlowerMound", 
                           City == "Garland" ~ "Dallas/FlowerMound", 
                           City == "Mexia" ~ "Waco", 
                           City == "Temple" ~ "Waco", 
                           City == "Groesbeck" ~ "Waco", 
                           City == "Gatesville" ~ "Waco", 
                           City == "Huntsville" ~ "College Station", 
                           City == "Caldwell" ~ "College Station", 
                           City == "Bryan" ~ "College Station", 
                           City == "San Antonio" ~ "San Antonio", 
                           City == "Austin" ~ "Austin", 
                           City == "Houston" ~ "Houston",
                           City == "Galveston" ~ "Houston",
                           City == "Texas City" ~ "Houston",
                           TRUE ~ "other city")) 

#sum different test types and virus types calculate the percent of positive tests
v <- v %>% 
  rowwise() %>%  
  mutate(   
    RSV_pos_t = sum(RSVAPos, RSVBPos, RSVUnkPos, na.rm = TRUE),
    RSV_tests_t = RSVtest,
    
    CoV_pos_t = sum(CoVHKU1Pos, CovNL63Pos, CovOC43Pos, CoV229EPos, CoVUnkPos, na.rm = TRUE),
    CoV_tests_t = CoVTest,
    
    Rhino_pos_t = sum(Rhinopos),
    Rhino_tests_t = Rhinotest,
    
    Flu_pos_t = sum(FluPanAH1N1pos + FluAH1N1pos + FluAH3N2pos + FluAunkpos + FluBpos, na.rm = TRUE),
    Flu_tests_t = FluTest
  )  %>% 
  ungroup() %>% 
  group_by(NAB_region, dates) %>% 
  summarize(
    RSV_pos = sum(RSV_pos_t, na.rm = TRUE),
    RSV_tests = sum(RSV_tests_t, na.rm = TRUE),
    
    CoV_pos = sum(CoV_pos_t, na.rm = TRUE),
    CoV_tests = sum(CoV_tests_t, na.rm = TRUE),
    
    Rhino_pos = sum(Rhino_pos_t, na.rm = TRUE),
    Rhino_tests = sum(Rhino_tests_t, na.rm = TRUE),
    
    Flu_pos = sum(Flu_pos_t, na.rm = TRUE, na.rm = TRUE),
    Flu_tests = sum(Flu_tests_t, na.rm = TRUE, na.rm = TRUE)) %>% 
  
  mutate(
    RSV_pos_prop = RSV_pos/RSV_tests,
    CoV_pos_prop = CoV_pos/CoV_tests,
    Rhino_pos_prop = Rhino_pos/Rhino_tests,
    Flu_pos_prop = Flu_pos/Flu_tests)



#look for holes in data
unique(v$NAB_region)
v %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = RSV_pos_prop)) + geom_point() + theme_bw() + facet_wrap(~NAB_region)

v %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = CoV_pos_prop)) + geom_line() + theme_bw() + facet_wrap(~NAB_region)

v %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = Rhino_pos_prop)) + geom_line() + theme_bw() + facet_wrap(~NAB_region)

v %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = Flu_pos_prop)) + geom_line() + theme_bw() + facet_wrap(~NAB_region)


# filter(v, dates == ymd("2015-08-02"))
# test <- v %>%  ungroup() %>% dplyr::select(dates) %>% distinct() %>% filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021"))

#create a state-wide average
v_statewide <- v %>% 
  ungroup() %>% 
  group_by(dates) %>% 
  summarize(RSV_pos_prop_tx = mean(RSV_pos_prop, na.rm = TRUE),
            CoV_pos_prop_tx = mean(CoV_pos_prop, na.rm = TRUE),
            Rhino_pos_prop_tx = mean(Rhino_pos_prop, na.rm = TRUE),
            Flu_pos_prop_tx = mean(Flu_pos_prop, na.rm = TRUE))



#expanding the grid to deal with NA values (i.e., city x date for which there should be rows)
#create day x station grid
date_station_grid_weekly <- 
  expand_grid( sort(unique(v$dates)), unique(v$NAB_region)) %>% 
  `colnames<-`(c("dates", "NAB_region")) %>%
  filter(!is.na(NAB_region)) %>% 
  filter(NAB_region != "other city") %>% 
  ungroup()

v_f <- left_join(date_station_grid_weekly, v) %>% 
  arrange(NAB_region, dates) %>% 
  group_by(NAB_region)


#use the statewide average for data gaps
v_f <- 
  left_join(v_f, v_statewide) %>% 
  mutate(RSV_pos_prop_m = case_when(RSV_tests < 1 ~ RSV_pos_prop_tx,
                                    is.na(RSV_tests) ~ RSV_pos_prop_tx,
                                    #is.nan(RSV_pos_prop) ~ RSV_pos_prop_tx, 
                                    TRUE ~ RSV_pos_prop),
         CoV_pos_prop_m = case_when(CoV_tests < 1 ~ CoV_pos_prop_tx, 
                                    is.na(CoV_tests) ~ CoV_pos_prop_tx, 
                                    TRUE ~ CoV_pos_prop),
         Rhino_pos_prop_m = case_when(Rhino_tests < 1 ~ Rhino_pos_prop_tx, 
                                      is.na(Rhino_pos_prop) ~ Rhino_pos_prop_tx, 
                                      TRUE ~ Rhino_pos_prop),
         Flu_pos_prop_m = case_when(Flu_tests < 1 ~ Flu_pos_prop_tx, 
                                    is.na(Flu_pos_prop) ~ Flu_pos_prop_tx, 
                                    TRUE ~ Flu_pos_prop))

#visually test the statewide average
v_f %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = RSV_pos_prop_m, col = "red")) + geom_step() + theme_bw() + facet_wrap(~NAB_region) +
  geom_line(aes(x = dates, y = RSV_pos_prop), col = "black")

v_f %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = CoV_pos_prop_m, col = "red")) + geom_step() + theme_bw() + facet_wrap(~NAB_region) +
  geom_line(aes(x = dates, y = CoV_pos_prop), col = "black") 

v_f %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = Rhino_pos_prop_m, col = "red")) + geom_step() + theme_bw() + facet_wrap(~NAB_region) +
  geom_line(aes(x = dates, y = Rhino_pos_prop), col = "black")

v_f %>%  filter(NAB_region != "other city") %>% 
  filter(dates > mdy("9/1/2015") & dates < mdy("1/1/2021")) %>% 
  ggplot(aes(x=dates, y = Flu_pos_prop_m, col = "red")) + geom_step() + theme_bw() + facet_wrap(~NAB_region) +
  geom_line(aes(x = dates, y = Flu_pos_prop), col = "black")



#### fill in the individual days
#create day x station grid
date_station_grid <- expand_grid(seq(ymd("2015-07-01"),ymd("2021-03-01"), by = '1 day'), 
                                 unique(v_f$NAB_region)) %>% 
  `colnames<-`(c("dates", "NAB_region")) %>%
  filter(!is.na(NAB_region)) %>% 
  filter(NAB_region != "other city") %>% 
  ungroup()

v_f2 <- left_join(date_station_grid, v_f) %>% 
  arrange(NAB_region, dates) %>% 
  group_by(NAB_region)

v_f2 <- v_f2 %>% mutate( 
  v_pos_prop_RSV_m = round(rollapply(RSV_pos_prop_m, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
  v_pos_prop_corona_m = round(rollapply(CoV_pos_prop_m, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
  v_pos_prop_rhino_m = round(rollapply(Rhino_pos_prop_m, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
  v_pos_prop_flu_m = round(rollapply(Flu_pos_prop_m, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4))



test <- v_f2 %>% filter(is.na(v_pos_prop_RSV_m))


### creating moving averages

v_f3 <- v_f2 %>% 
  mutate(v_pos_prop_rhino_m14 = rollmean(v_pos_prop_rhino_m, 15, na.pad=TRUE, align = "center"),
         v_pos_prop_RSV_m14 = rollmean(v_pos_prop_RSV_m, 15, na.pad=TRUE, align = "center"),
         v_pos_prop_corona_m14 = rollmean(v_pos_prop_corona_m, 15, na.pad=TRUE, align = "center"),
         v_pos_prop_flu_m14 = rollmean(v_pos_prop_flu_m, 15, na.pad=TRUE, align = "center"),
         
         v_pos_prop_rhino_m14r = rollmean(v_pos_prop_rhino_m, 15, na.pad=TRUE, align = "right"),
         v_pos_prop_RSV_m14r = rollmean(v_pos_prop_RSV_m, 15, na.pad=TRUE, align = "right"),
         v_pos_prop_corona_m14r = rollmean(v_pos_prop_corona_m, 15, na.pad=TRUE, align = "right"),
         v_pos_prop_flu_m14r = rollmean(v_pos_prop_flu_m, 15, na.pad=TRUE, align = "right"),
         
         v_pos_prop_rhino_m14l = rollmean(v_pos_prop_rhino_m, 15, na.pad=TRUE, align = "left"),
         v_pos_prop_RSV_m14l = rollmean(v_pos_prop_RSV_m, 15, na.pad=TRUE, align = "left"),
         v_pos_prop_corona_m14l = rollmean(v_pos_prop_corona_m, 15, na.pad=TRUE, align = "left"),
         v_pos_prop_flu_m14l = rollmean(v_pos_prop_flu_m, 15, na.pad=TRUE, align = "left"),
         
         v_pos_prop_rhino_m21 = rollmean(v_pos_prop_rhino_m, 22, na.pad=TRUE, align = "center"),
         v_pos_prop_RSV_m21 = rollmean(v_pos_prop_RSV_m, 22, na.pad=TRUE, align = "center"),
         v_pos_prop_corona_m21 = rollmean(v_pos_prop_corona_m, 22, na.pad=TRUE, align = "center"),
         v_pos_prop_flu_m21 = rollmean(v_pos_prop_flu_m, 22, na.pad=TRUE, align = "center"),
         
         v_pos_prop_rhino_m28 = rollmean(v_pos_prop_rhino_m, 29, na.pad=TRUE, align = "center"),
         v_pos_prop_RSV_m28 = rollmean(v_pos_prop_RSV_m, 29, na.pad=TRUE, align = "center"),
         v_pos_prop_corona_m28 = rollmean(v_pos_prop_corona_m, 29, na.pad=TRUE, align = "center"),
         v_pos_prop_flu_m28 = rollmean(v_pos_prop_flu_m, 29, na.pad=TRUE, align = "center") )


### visual checks on the data
v_f3 %>%  
  ggplot(aes(x=dates, y = v_pos_prop_RSV_m, col = "black")) + geom_line() + theme_bw() + facet_wrap(~NAB_region) +
  geom_point(aes(x = dates, y = RSV_pos_prop), col = "red") +
  geom_line(aes(x = dates, y = v_pos_prop_RSV_m14, col = "green")) +
  geom_line(aes(x = dates, y = v_pos_prop_RSV_m21, col = "blue")) +
  geom_line(aes(x = dates, y = v_pos_prop_RSV_m28, col = "yellow")) 

v_f3 %>%  
  ggplot(aes(x=dates, y = v_pos_prop_rhino_m, col = "black")) + geom_line() + theme_bw() + facet_wrap(~NAB_region) +
  geom_point(aes(x = dates, y = Rhino_pos_prop), col = "red") +
  geom_line(aes(x = dates, y = v_pos_prop_rhino_m14, col = "green")) +
  geom_line(aes(x = dates, y = v_pos_prop_rhino_m21, col = "blue")) +
  geom_line(aes(x = dates, y = v_pos_prop_rhino_m28, col = "yellow")) 

v_f3 %>%  
  ggplot(aes(x=dates, y = v_pos_prop_corona_m, col = "black")) + geom_line() + theme_bw() + facet_wrap(~NAB_region) +
  geom_point(aes(x = dates, y = CoV_pos_prop), col = "red") +
  geom_line(aes(x = dates, y = v_pos_prop_corona_m14, col = "green")) +
  geom_line(aes(x = dates, y = v_pos_prop_corona_m21, col = "blue")) +
  geom_line(aes(x = dates, y = v_pos_prop_corona_m28, col = "yellow")) 

v_f3 %>%  
  ggplot(aes(x=dates, y = v_pos_prop_flu_m, col = "black")) + geom_line() + theme_bw() + facet_wrap(~NAB_region) +
  geom_point(aes(x = dates, y = Flu_pos_prop), col = "red") +
  geom_line(aes(x = dates, y = v_pos_prop_flu_m14), col = "green") +
  geom_line(aes(x = dates, y = v_pos_prop_flu_m21), col = "blue") +
  geom_line(aes(x = dates, y = v_pos_prop_flu_m28), col = "yellow") 



#making dataset compatible with the rest of the analysis and only selecting the final variables
v_f4 <- v_f3 %>% ungroup() %>% 
  mutate(viral_metro_area = NAB_region,
         date = dates) %>% 
  dplyr::select(date, viral_metro_area, 
                v_pos_prop_RSV_m, v_pos_prop_RSV_m14, v_pos_prop_RSV_m21, v_pos_prop_RSV_m28,
                v_pos_prop_rhino_m, v_pos_prop_rhino_m14, v_pos_prop_rhino_m21, v_pos_prop_rhino_m28,
                v_pos_prop_corona_m, v_pos_prop_corona_m14, v_pos_prop_corona_m21, v_pos_prop_corona_m28,
                v_pos_prop_flu_m, v_pos_prop_flu_m14, v_pos_prop_flu_m21, v_pos_prop_flu_m28,)

#ungroup() %>%
v_f4 %>% mutate(RSV_d_perc_pos = v_pos_prop_RSV_m * 100,
       corona_d_perc_pos = v_pos_prop_corona_m * 100,
       rhino_d_perc_pos = v_pos_prop_rhino_m * 100,
       flu_d_perc_pos = v_pos_prop_flu_m * 100) %>%
  
  # dplyr::select(date, RSV_d_perc_pos, corona_d_perc_pos, rhino_d_perc_pos, flu_d_perc_pos ) %>%
  pivot_longer(cols = c(RSV_d_perc_pos, corona_d_perc_pos , rhino_d_perc_pos, flu_d_perc_pos),
               names_to = "virus_type", values_to = "positive_tests") %>%
  # distinct() %>%
  #filter(date > mdy("10 - 31 - 2015")) %>%
  #arrange(virus_type, date) %>%
  ggplot(aes(x = date, y = positive_tests, color = virus_type)) + theme_few() +
  geom_step() + #geom_point() +
  ylab(expression(atop("positive tests", "(%)"))) +
  scale_color_viridis_d(breaks = c("flu_d_perc_pos", "corona_d_perc_pos", "rhino_d_perc_pos", "RSV_d_perc_pos"), 
                        option = "viridis",
                        labels = c("Influenza" ,"Seasonal coronavirus", "Rhinovirus", "RSV"), name = "virus type") +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position= "none")#c(0.75, 0.75))


### save csv with viral data
write_csv(v_f4, "Z:/THCIC/Katz/data_viral/TX_NREVSS_processed_220810.csv")
