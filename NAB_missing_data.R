#modeling the missing data from each NAB station
library(tidyr) 
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
#library(Amelia)

rm(list = ls())



### load in NAB data from 2009-2019 ##################################################################
NAB_locations <- read_csv("C:/Users/dsk856/Box/texas/NAB/EPHT_Pollen Station Inventory_062019_dk200331.csv")
NAB <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB2009_2019_pollen_200508.csv", guess_max = 50000)

names(NAB)
summary(NAB)
  
  
### create the categories of pollen and prepare data ################################################
NAB_tx <- left_join(NAB, NAB_locations) %>%
  filter(State == "TX") %>%
  mutate(date = ymd(Date),
         mo = month(date),
         doy = yday(date),
         year = year(date)
         # ja = case_when( doy < 60   ~ Cupressaceae,
         #                 doy > 345 ~ Cupressaceae,
         #                 TRUE ~ 0),
         # cup_other = case_when(ja == 0 ~ Cupressaceae,
         #                       ja > 0 ~ 0)
         ) %>%
  rowwise()%>%
  # mutate(trees = sum(Acer, Alnus,Betula, Carpinus.Ostrya, Corylus, Fraxinus,Juglans, Liquidambar, Other.Tree.Pollen,                    
  #                           Pinaceae, Populus, Pseudotsuga, Quercus, Salix, Arecaceae, Carya, Cyperaceae, Fagus, Ligustrum,
  #                           Morus,  Olea, Platanus, Tilia ,Celtis,  Prosopis, Myrica, Ulmus, Tsuga, na.rm = TRUE),
  #        pol_other = Total.Pollen.Count - ja - cup_other - trees)%>%  
  mutate(Fagales = sum(Alnus,Betula, Carpinus.Ostrya, Corylus, Quercus, Fagus, Myrica, na.rm = TRUE),
         other_trees = sum(Acer,Fraxinus, Juglans, Liquidambar, Other.Tree.Pollen, Pinaceae, Populus, Pseudotsuga, Salix, 
                           Carya, Ligustrum, Olea, Platanus, Tilia, Celtis, Tsuga, Prosopis, na.rm = TRUE),
         other_pollen = sum(Artemisia, Asteraceae..Excluding.Ambrosia.and.Artemisia., Chenopodiaceae.Amaranthaceae,
                                 Other.Weed.Pollen, Plantago, Rumex, Unidentified.Pollen, Arecaceae, Cyperaceae, Typha,
                            Eupatorium, Urticaceae, na.rm = TRUE ),
         grass = sum(Gramineae...Poaceae, Other.Grass.Pollen , na.rm = TRUE)) %>% 
  dplyr::select(date, mo, doy, year, City, FIPS, county_name, MSA, Lat, Long, NAB_station,
                #ja, cup_other, trees, pol_other, 
                Cupressaceae, Ambrosia, Morus, Ulmus, grass, Fagales, other_trees, other_pollen,
                tot_pol = Total.Pollen.Count) 



date_station_grid <- expand_grid(seq(min(NAB$date),max(NAB$date), by = '1 day'), 
                                 unique(NAB_tx$NAB_station)) %>% 
                    `colnames<-`(c("date", "NAB_station")) %>%
                    filter(!is.na(NAB_station))

pd <- left_join(date_station_grid, NAB_tx) %>%
          arrange(NAB_station, date)

#QA/QC
pd <- pd %>% 
  filter(NAB_station != "Waco B") %>% #Removing Waco B;
# There appear to be substantial issues with non-measured days being recorded as 0s and with Ulmus not being distinguished from
# unidentified pollen in 2009 and 2016.
# I'm also deeply skeptical of how clean the data are - there is so little variablility compared to all other stations
#   mutate( #ja = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ ja),
#           #cup_other = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ cup_other),
#          Cupressaceae = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ Cupressaceae),

#Quality control for Waco A, Cupressaceae seems to have been mistakenly entered as Ambrosia for a season
  mutate( Cupressaceae = case_when(NAB_station == "Waco A" & Cupressaceae == 0 &
                                     date > mdy("12/11/2009") & date < mdy("2/20/2010") ~ Ambrosia, TRUE ~ Cupressaceae),
           Ambrosia = case_when(NAB_station == "Waco A"  &
                                     date > mdy("12/11/2009") & date < mdy("2/20/2010") ~ 0, TRUE ~ Ambrosia)) %>% 
  rowwise()%>%
  mutate(Cupressaceae = Cupressaceae,
         trees = sum(Morus, Ulmus, Fagales, other_trees), 
         pol_other = sum(grass, Ambrosia, other_pollen)) %>% 
  mutate(cup_log10 = log10(Cupressaceae + 1),
         trees_log10 = log10(trees + 1),
         pol_other_log10 = log10(other_pollen + 1)) %>% 
  ungroup()


#Cupressaceae, trees, pol_other
ggplot(pd, aes(x = doy, y = cup_log10)) + geom_point() + facet_grid(year~NAB_station) + theme_bw() 

filter(pd, NAB_station == "Waco A") %>%
ggplot(aes(x = doy, y = Ambrosia+ 1)) + geom_point() + facet_grid(NAB_station~year) + theme_bw() + scale_y_log10() +
  geom_point(aes(x = doy, y = Cupressaceae + 1), col = "red")

### trying out a simple linear interpolation ###############################################################
#using a random forest is slightly better but far more complicated and I don't think it's worth it because it is just so much more complicated to explain
#the marginal benefit is probably an R2 improvement of ~0.02 
# so, as of 10/2/20, I'm going to switch over to just a linear interpolation:
library(imputeTS)

#linear interpolation of virus data
pd_m <- pd %>% 
  group_by(NAB_station) %>% 
  mutate(
              mo = month(date),
             doy = yday(date),
             year = year(date),
             year_f = paste0("y_", year(date)),
  
                        cup_lm = na_interpolation(cup_log10),
                        trees_lm = na_interpolation(trees_log10),
                        pol_other_lm = na_interpolation(pol_other_log10)
                        #Ambrosia_m = na_interpolation(Ambrosia)
                        # Morus_m = na_interpolation(Morus),
                        # Ulmus_m = na_interpolation(Ulmus),
                        # grass_m = na_interpolation(grass),
                        # Fagales_m = na_interpolation(Fagales),
                        # other_trees_m = na_interpolation(other_trees),
                        # other_pollen_m = na_interpolation(other_pollen)
             ) 

#check on data
  pd_m %>% 
  #filter(NAB_station == "College Station") %>% 
filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
ggplot(aes(x = date, y = cup_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
    geom_point(aes(x = date, y = cup_log10), color = "black")
  
  pd_m %>% 
    #filter(NAB_station == "College Station") %>% 
    filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
    ggplot(aes(x = date, y = trees_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
    geom_point(aes(x = date, y = trees_log10), color = "black")
  
  pd_m %>% 
    #filter(NAB_station == "College Station") %>% 
    filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
    ggplot(aes(x = date, y = pol_other_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
    geom_point(aes(x = date, y = pol_other_log10), color = "black")
  
  
write_csv(pd_m, "C:/Users/dsk856/Box/texas/NAB/NAB_pollen_modeled_linear_interp_210311.csv")


### assess how good of a job the linear interpolation does ######################
#select a portion of the data to withhold 
pd_withheld <- pd %>%  #str(pd)
  ungroup() %>%  #get rid of rowwise
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
  sample_frac(0.1) %>% #withold 10% of data
  dplyr::select(date, NAB_station) %>% 
  mutate(withheld = 1)
pd_mt <- left_join(pd, pd_withheld) %>%  #str(pd_mt)
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
  mutate(#withheld2 = case_when(withheld == 1 ~ withheld, is.na(withheld) ~ 0),
         cup_withheld = case_when(is.na(withheld) ~ cup_log10), #by not specifying a value, it defaults to NA
         trees_withheld = case_when(is.na(withheld) ~ trees_log10),
         pol_other_withheld = case_when(is.na(withheld) ~ pol_other_log10)
         )
  
#linear interpolation of time series that had extra data removed
pd_mt <- pd_mt %>% 
  group_by(NAB_station) %>% 
  mutate(
    cup_withheld_lm = na_interpolation(cup_withheld),
    trees_withheld_lm = na_interpolation(trees_withheld),
    pol_other_withheld_lm = na_interpolation(pol_other_withheld))

pd_mtc <- filter(pd_mt, withheld == 1) 

#compare withheld data with linear interp estimates
cup_mtc_fit <- lm(pd_mtc$cup_log10 ~ pd_mtc$cup_withheld_lm )
sqrt(mean(cup_mtc_fit$residuals^2)); summary(cup_mtc_fit) #RMSE and R2
cup_panel <- ggplot(pd_mtc, aes(x= 10^(cup_withheld_lm) - 1, y = 10^(cup_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000)) 

trees_mtc_fit <- lm(pd_mtc$trees_log10 ~ pd_mtc$trees_withheld_lm)
sqrt(mean(trees_mtc_fit$residuals^2)); summary(trees_mtc_fit) #RMSE and R2
tree_panel <- ggplot(pd_mtc, aes(x= 10^(trees_withheld_lm) - 1, y = 10^(trees_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000)) 

pol_other_mtc_fit <- lm(pd_mtc$pol_other_log10 ~ pd_mtc$pol_other_withheld_lm)
sqrt(mean(pol_other_mtc_fit$residuals^2)); summary(pol_other_mtc_fit) #RMSE and R2
other_panel <- ggplot(pd_mtc, aes(x= 10^(pol_other_withheld_lm) - 1, y = 10^(pol_other_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
  scale_x_log10(limits = c(1, 500)) + scale_y_log10(limits = c(1, 500)) 

cowplot::plot_grid(cup_panel, tree_panel, other_panel, labels = c("A", "B", "C"))
         



### older and more complicated approaches to estimating missing data ########################################
### download and extract met data from Daymet ###############################################################
library(daymetr)
library(sf)
library(magrittr)
library(tidyr)
library(purrr)

# NAB_tx_coords <- NAB_tx %>% dplyr::select(site = NAB_station, lat = Lat, long = Long) %>% distinct() %>% filter(!is.na(site))
# write_csv(NAB_tx_coords, "C:/Users/dsk856/Box/texas/NAB/TX_NAB_station_coords_daymetr200608.csv")
# #test <- download_daymet(lon = NAB_tx_coords$Long[1], lat = NAB_tx_coords$Lat[1], start = 2015, end = 2016, simplify = TRUE)
# weather_at_stations <- download_daymet_batch(file_location = "C:/Users/dsk856/Box/texas/NAB/TX_NAB_station_coords_daymetr200608.csv", start =2009, end = 2019, simplify = TRUE)
# #write_csv(weather_at_stations, "C:/Users/dsk856/Box/texas/NAB/weather_at_NAB_stations200608.csv")
# #unique(weather_at_stations$measurement)
weather_at_stations <- read_csv("C:/Users/dsk856/Box/texas/NAB/weather_at_NAB_stations200608.csv")%>% 
  mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
  mutate(measurement = gsub(pattern = ".", replacement = "", x = measurement, fixed = TRUE)) %>%
  dplyr::select(NAB_station = site, date, measurement, value) %>%
  pivot_wider(id_cols = c(NAB_station, date), names_from = measurement, values_from = value, names_prefix = "met_")
  #head(weather_at_stations)

pd2 <- left_join(pd, weather_at_stations)


### download met data from NOAA stations #######################################################
library("rnoaa")

#https://docs.ropensci.org/rnoaa/

# station_data <- ghcnd_stations(noaa_key = noaa_api_key) # Takes a while to run
# NAB_tx_coords_rnoaa <- NAB_tx_coords %>% rename(id = site, latitude = lat, longitude = long)
# 
# nearby_stations <- meteo_nearby_stations(lat_lon_df = NAB_tx_coords_rnoaa, radius = 150, limit = 1, 
#                                          year_min = 2009, year_max = 2019, var = "AWND")
# 
# station_names <- as.data.frame(nearby_stations) %>% select(contains("id")) %>% unlist()
# all_monitors_clean <- meteo_pull_monitors(monitors = station_names, keep_flags = FALSE, 
#                                           date_min = "2008-01-01", var = "all") #takes a few minutes to run
# #wasn't able to download all 8 at once, did it manually in batches
# unique(all_monitors_clean3$id)
# station_names2 <- station_names[1:4]
# all_monitors_clean2 <- meteo_pull_monitors(monitors = station_names[1], keep_flags = FALSE, 
#                                           date_min = "2008-01-01", var = "all") #takes a few minutes to run
# all_monitors_clean3 <- meteo_pull_monitors(monitors = station_names[2:4], keep_flags = FALSE, 
#                                            date_min = "2008-01-01", var = "all") #takes a few minutes to run
# all_monitors_clean4 <- bind_rows(all_monitors_clean, all_monitors_clean2, all_monitors_clean3)
# station_names_lookup <- data.frame(id = station_names, NAB_station = names(station_names)) %>%
#   mutate(NAB_station = gsub(pattern = ".", replacement = " ", x = NAB_station, fixed = TRUE),
#          NAB_station = gsub(pattern = "id", replacement = "", x = NAB_station, fixed = TRUE), 
#          NAB_station = stringr::str_trim(NAB_station))
# 
# #names(all_monitors_clean4 )
# all_monitors_clean4 <- all_monitors_clean4 %>% rename_at(vars(c(-id, -date, - NAB_station)), ~ paste0("ghcn_", .))
# #unique(all_monitors_clean4$NAB_station)

#write_csv(all_monitors_clean4, "C:/Users/dsk856/Box/texas/NAB/weatherNOAA_at_NAB_stations200611.csv")
all_monitors_clean4 <- read_csv("C:/Users/dsk856/Box/texas/NAB/weatherNOAA_at_NAB_stations200611.csv", guess_max = 40000)
#names(all_monitors_clean4)
pd3 <- left_join(pd2, all_monitors_clean4)
#write_csv(pd3, "C:/Users/dsk856/Box/texas/NAB/NAB_weather201002.csv")

### summarizing missing pollen data: stats for manuscript #########################################################################
#pd3 <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_weather201002.csv", guess_max = 40000)
pd3 <- pd3 %>% mutate(mo = month(date),
                      doy = as.numeric(day(date)),
                      year = year(date),
                      year_f = paste0("y_", year(date))) 

#proportion of data missing by site
#Note that Waco B appears to have some observations where NA has been miscoded as 0
pd3 %>% 
  filter(date > mdy("10 - 31 - 2015") & date < mdy("1 - 1 - 2018")) %>% 
  select(tot_pol, NAB_station) %>% 
  group_by(NAB_station) %>% 
  summarize(
            n_NA = sum(is.na(tot_pol)),
            n_obs = n(),
            prop_NA = n_NA/n_obs)

#proportion of data missing by day of week
pd3 %>% 
  filter(date > mdy("10 - 31 - 2015") & date < mdy("1 - 1 - 2018")) %>% 
  select(tot_pol, date, NAB_station) %>% 
  mutate(
    week_day = weekdays(date),
    week_day = forcats::fct_relevel(week_day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>% 
  group_by(NAB_station, week_day) %>% 
  summarize(
    n_NA = sum(is.na(tot_pol)),
    n_obs = n(),
    prop_NA = n_NA/n_obs) %>% 
  ggplot(aes(x = week_day, y = prop_NA)) + geom_bar(stat = "identity") + facet_wrap(~NAB_station) +
  xlab("day of week") + ylab("observations missing (proportion)") + theme_bw()


#figure of missing data by time (for total pollen)
pd3 %>% 
  mutate(NA_flag = case_when(is.na(tot_pol) ~ 0.1,
                             !is.na(tot_pol) ~ NaN)) %>% 
  filter(date > mdy("10 - 31 - 2015") & date < mdy("1 - 1 - 2018")) %>% 
ggplot(aes(x = date, y = tot_pol + 1)) + geom_point() + scale_y_log10() + facet_wrap(~NAB_station) +
  geom_jitter(aes(x = date, y = NA_flag), col = "red", size = 0.2, width = 0.01, height = 0.1) + theme_bw() + ylab(total~pollen~(grains/m^3))



     
### modeling with RF: create derived variables and subset training and testing datasets ###########################################
library(magrittr)
library("slider")
library(purrr)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

#names(which(sapply(dp2, anyNA)))
#set.seed(100)
pd3 <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_weather201002.csv", guess_max = 40000)
pd3 <- pd3 %>% mutate(mo = month(date),
                  doy = as.numeric(day(date)),
                  year = year(date),
                  year_f = paste0("y_", year(date))) 
        #filter(NAB_station != "College Station") 
        #filter(NAB_station == "Flower Mound")
        #filter(NAB_station != "Waco B") 
pd4 <- pd3 %>% dplyr::select(c(#train0_test1, 
                                    NAB_station, ja, cup_other, Cupressaceae, trees, pol_other, tot_pol,
                                    mo, doy, year, year_f, date,
                            starts_with("met_"), 
                            starts_with("ghcn_")))%>% 
                      select(-ghcn_psun, -ghcn_tsun, -ghcn_snow, -ghcn_snwd, - ghcn_tavg, -ghcn_wesd,-ghcn_wesf,
                             -starts_with("ghcn_wt")) %>% 
              mutate(
                   ja_l1 = lag(ja, 1),
                   ja_l2 = lag(ja, 2),
                   ja_lead1 = lead(ja, 1),
                   ja_lead2 = lead(ja, 2),
                   ja_ma3 = slide_vec(ja, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   ja_ma5 = slide_vec(ja, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   ja_ma7 = slide_vec(ja, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   ja_ma9 = slide_vec(ja, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4),
                   cup_other_l1 = lag(cup_other, 1),
                   cup_other_l2 = lag(cup_other, 2),
                   cup_other_lead1 = lead(cup_other, 1),
                   cup_other_lead2 = lead(cup_other, 2),
                   cup_other_ma3 = slide_vec(cup_other, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   cup_other_ma5 = slide_vec(cup_other, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   cup_other_ma7 = slide_vec(cup_other, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   cup_other_ma9 = slide_vec(cup_other, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4),
                   cup_all = Cupressaceae,
                   cup_all_l1 = lag(cup_all, 1),
                   cup_all_l2 = lag(cup_all, 2),
                   cup_all_lead1 = lead(cup_all, 1),
                   cup_all_lead2 = lead(cup_all, 2),
                   cup_all_ma3 = slide_vec(cup_all, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   cup_all_ma5 = slide_vec(cup_all, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   cup_all_ma7 = slide_vec(cup_all, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   cup_all_ma9 = slide_vec(cup_all, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4),
                   trees_l1 = lag(trees, 1),
                   trees_l2 = lag(trees, 2),
                   trees_lead1 = lead(trees, 1),
                   trees_lead2 = lead(trees, 2),
                   trees_ma3 = slide_vec(trees, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   trees_ma5 = slide_vec(trees, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   trees_ma7 = slide_vec(trees, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   trees_ma9 = slide_vec(trees, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4),
                   pol_other_l1 = lag(pol_other, 1),
                   pol_other_l2 = lag(pol_other, 2),
                   pol_other_lead1 = lead(pol_other, 1),
                   pol_other_lead2 = lead(pol_other, 2),
                   pol_other_ma3 = slide_vec(pol_other, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   pol_other_ma5 = slide_vec(pol_other, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   pol_other_ma7 = slide_vec(pol_other, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   pol_other_ma9 = slide_vec(pol_other, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4)
              ) 
        #select(-cup_other, -trees, -pol_other, -tot_pol) #%T>%
        # {filter(., train0_test1 == 0) ->> TrainSet} %T>%  
        # {filter(., train0_test1 == 1) ->> TestSet} 

#convert NaN to NA
is.nan.data.frame <- function(x) {do.call(cbind, lapply(x, is.nan))}
pd4[is.nan(pd4)] <- NA

#records with missing pollen data 
pol_missing_data <- pd4 %>% filter(is.na(tot_pol)) %>% #when one pollen type is NA all other types are also NA
                    mutate(focal_pollen = NA)

# setting up to compare different imputation methods (in the tsImpute section of script)
# records with missing pollen data on the days where Georgetown had missing data
# pol_missing_data_georgetown <- pd4 %>%
#   filter(NAB_station == "Georgetown" ) %>%
#   #filter(date > mdy("11 - 1 - 2015") & date < mdy("1 - 1- 2018")) %>%
#   select(trees) %>%
#   transmute(missing_george = is.na(trees))
# 
# pol_missing_data_sim_fm <- pd4 %>%
#   filter(NAB_station == "Flower Mound" ) %>%
#   #filter(date > mdy("11 - 1 - 2015") & date < mdy("1 - 1- 2018")) %>%
#   bind_cols(., pol_missing_data_georgetown) %>%
#   rowwise() %>%
#   mutate(focal_pollen = ifelse(isTRUE(missing_george), NA, trees))



### modeling missing pollen using tidymodels and RFIntervals ################################################
library(tidymodels)
library(rfinterval)
# RFInterval allows the extraction of CI's around each prediction 
# https://github.com/haozhestat/RFIntervals
# https://haozhestat.github.io/files/manuscript_RFIntervals_FinalVersion.pdf
# I tried quantregforest first, but got a large number of NA values and switched to RFIntervals instead

#Note: I'm not making this in to a full function because I want to check each step along the way
#I have, however, made it easy to interchange the pollen type

### Set focal pollen type here:
focal_pollen_var <- "trees" #ja, cup_other, trees, pol_other, tot_pol
###

pd5 <- pd4 %>% #filter(NAB_station == "San Antonio A") %>%  #names(pd5)
  #filter(NAB_station != "Waco B") %>% 
  #filter(NAB_station == "Flower Mound") %>% 
  filter(!is.na(ja)) %>% #remove the missing observations
  select_if(~sum(!is.na(.)) > 0) %>%  #remove columns that are all NAs
  mutate(focal_pollen = .data[[focal_pollen_var]]) #making the focal pollen type a column; see programming with dplyr vignette
  
pd5_split <- initial_split(pd5, prop = 0.9)
pd5_recipe <- training(pd5_split) %>%
  recipe(focal_pollen ~.) %>%
  step_rm(ja, cup_other, cup_all, Cupressaceae, trees, pol_other, tot_pol) %>%  #remove the pollen measurements to prevent data leakage #, NAB_station
  step_log(all_outcomes(), contains("_ma"), contains("_lead"), contains("_l1"), contains("_l2"), offset = 1) %>% 
  step_center(all_numeric(), -all_outcomes(), -contains("_ma"), -contains("_lead"), -contains("_l1"), -contains("_l2"), -date) %>%
  step_scale(all_numeric(), -all_outcomes(), -contains("_ma"), -contains("_lead"), -contains("_l1"), -contains("_l2"), -date) %>%
  #step_nzv(all_predictors()) %>% 
  #step_meanimpute(all_numeric(), -contains("_ma"), -contains("_lead"), - contains("_l1"), -contains("_l2")) %>%
  #rollimpute worked better than switching NA values to a flag
  step_rollimpute(contains("_ma"), contains("_lead"), contains("_l1"), contains("_l2"), window = 3, statistic = median) %>%
  step_rollimpute(contains("_ma"), contains("_lead"), contains("_l1"), contains("_l2"), window = 5, statistic = median) %>%
  step_rollimpute(contains("_ma"), contains("_lead"), contains("_l1"), contains("_l2"), window = 7, statistic = median) %>%
  #step_mutate_at(contains("_ma"), contains("_lead"), contains("_l1"), contains("_l2"), fn = ~replace_na(., replace = -999)) %>% 
  step_meanimpute(all_numeric()) %>%
  step_naomit(all_predictors()) %>%
  step_date(date, features = c("doy", "month", "year")) %>% 
  #step_num2factor(date_year, transform = as.integer, levels = paste0("y", 2008:2019)) %>% 
  step_dummy(all_nominal()) %>% 
  prep()

pd5_training <- pd5_recipe %>% bake(training(pd5_split))
pd5_testing  <- pd5_recipe %>% bake(testing(pd5_split)) 
pd5_testing_nab <- testing(pd5_split)$NAB_station
focal_pollen_missing_data_baked <- pd5_recipe %>% bake(new_data = pol_missing_data) 
#focal_pollen_missing_data_baked <- pd5_recipe %>% bake(new_data = pol_missing_data_sim_fm) #for the georgetown/flower mound methods check

#running a separate model to check the importance of each variable
pd5_ranger <- rand_forest(trees = 1000, mode = "regression", mtry = 5) %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(focal_pollen ~ ., data = pd5_training)
pd5_ranger
sort(pd5_ranger$fit$variable.importance)

#checking model on testing dataset
rfint_mod <- rfinterval(focal_pollen ~ ., 
                        train_data = as.data.frame(pd5_training),
                        test_data = as.data.frame(pd5_testing),
                        #params_ranger(importance = "impurity_corrected"), #not sure why this isnt' working
                        method =c("oob"), #, "split-conformal", "quantreg"
                        symmetry = TRUE, alpha = 0.05) 

#str(rfint_mod)

pd5_testing %>% mutate(NAB_station = pd5_testing_nab,
                       focal_pollen = pd5_testing$focal_pollen,
                       focal_pollen_hat_rfint_mean = unlist(rfint_mod$testPred)) %>% 
  ggplot(aes(x = focal_pollen, y = focal_pollen_hat_rfint_mean, 
             color = NAB_station)) + 
  geom_point() + geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0) + theme_bw()
summary(lm(pd5_testing$focal_pollen ~ unlist(rfint_mod$testPred)))


#predict the missing pollen data
rfint_mod <- rfinterval(focal_pollen ~ ., train_data = as.data.frame(pd5_training),
                        test_data = as.data.frame(focal_pollen_missing_data_baked),
                        method =c("oob"), #, "split-conformal", "quantreg"
                        symmetry = TRUE, alpha = 0.05) 

focal_pollen_missing_data_rfint_mod_join <- focal_pollen_missing_data_baked %>% #get pre-baked versions of the variables
  mutate(focal_pollen_rfint_log_mean = unlist(rfint_mod$testPred)) %>% 
  select(focal_pollen_rfint_log_mean) %>% 
  rename_with(~ gsub("focal_pollen", eval(focal_pollen_var), .x)) %>% 
  mutate(NAB_station = pol_missing_data$NAB_station, 
         year = pol_missing_data$year,
         mo = pol_missing_data$mo,
         doy = pol_missing_data$doy) 

#to save the output switch this around between pollen types
#pd7 <- left_join(pd4, focal_pollen_missing_data_rfint_mod_join)
 pd7 <- left_join(pd7, focal_pollen_missing_data_rfint_mod_join)

#do predictions match?
#pd7 %>% ggplot(aes(x = focal_pollen_hat_log, y = focal_pollen_hat_rfint_log_mean)) + geom_point() + facet_wrap(~NAB_station) + theme_bw()

pd7_save <- pd7 %>% select(NAB_station, date, contains("rfint_"))
#write_csv(pd7_save, "C:/Users/dsk856/Box/texas/NAB/NAB_pollen_modeled200810.csv")
#write_csv(pd7, "C:/Users/dsk856/Box/texas/NAB/pd7_NAB_pollen_modeled200810.csv")
#pd7_save <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_pollen_modeled200618.csv")

#some graphs to check up on it
pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(ja + 1 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_point(aes(x = date,y = ja_rfint_log_mean, color = "red")) 

pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(trees + 1 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_point(aes(x = date,  y = trees_rfint_log_mean), color = "red")

pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(pol_other + 1 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_point(aes(x = date, y = pol_other_rfint_log_mean,  color = "red")) + scale_color_discrete(name = "modeled data")

pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(pol_other + 1 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_point(aes(x = date, y = pol_other_rfint_log_mean, color = "red")) 












### trying out Amelia for imputation ############################################################################
pd3 <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_weather200611.csv", guess_max = 40000)
pd3 <- pd3 %>% mutate(mo = month(date),
                      doy = as.numeric(day(date)),
                      year = year(date),
                      year_f = paste0("y_", year(date))) %>% 
  filter(NAB_station != "College Station") 
pd4 <- pd3 %>% dplyr::select(c(#train0_test1, 
  NAB_station, ja, cup_other, trees, pol_other, tot_pol,
  mo, doy, year, year_f, date,
  starts_with("met_"), 
  starts_with("ghcn_")))%>% 
  select(-ghcn_psun, -ghcn_tsun, -ghcn_snow, -ghcn_snwd, - ghcn_tavg, -ghcn_wesd,-ghcn_wesf,
         -starts_with("ghcn_wt"))

data_sub <- filter(pd4, NAB_station == "Georgetown") %>% select(-cup_other, - ja, -pol_other, -tot_pol, -year_f) %>% 
  select(-ghcn_fmtm, -ghcn_pgtm, -ghcn_wdf2, -ghcn_wdf5, -ghcn_wsf2, -ghcn_wsf5,  -ghcn_awnd) %>% 
  mutate(trees_l = log10(trees + 1)) %>% select(-trees) %>% 
  select(trees_l, met_tmaxdegc, date)
names(data_sub)
summary(data_sub)

data_sub <- as.data.frame(data_sub)
trees_matrix <- matrix(data = c(17, 0, 4), nrow = 1)
data_sub_out <- amelia(x = data_sub,  ts = "date", #polytime = 2, 
                       cs = "NAB_station", 
                       lags = "trees_l", leads = "trees_l", 
                       bounds = trees_matrix)
summary(data_sub_out)
compare.density(data_sub_out, var = "trees_l")

#tscsPlot(data_sub_out, cs = 1, var = "trees_l")
data_sub_out_imp1 <- data_sub_out$imputations$imp1
ggplot(data_sub_out_imp1, aes(x = date, y = trees_l)) + geom_point()
ggplot(data_sub, aes(x = date, y = trees_l)) + geom_point()

data_sub_out_imp1 %>% 
  mutate(trees_l_orig = data_sub$trees_l) %>% 
  filter(date > mdy("11 - 1- 2015") & date <  mdy("11 - 1- 2016")) %>% 
  ggplot( aes(x = date, y = trees_l)) + 
  geom_point(color = "red", size = 2) +
  geom_point(aes(x= date, y = trees_l_orig), size = 1) 


# ?amelia
# ggplot(data_sub, aes(x = date, y = trees_l)) + geom_point() + scale_y_log10()
# names(pd4)
# 
# str(africa)
# str(data_sub)
# 
# data(africa)
# a.out <- amelia(x = africa, cs = "country", ts = "year", logs = "gdp_pc")
# summary(a.out)
# plot(a.out)


### trying out mtsdi ###############################################################################################
library(mtsdi)
f <- ~trees_l + met_tmaxdegc + date 
test <- mnimput(formula = f, data = data_sub, eps=1e-3, ts=TRUE, method="arima", sp.control=list(df=c(7,7,7,7,7)))
str(test)
data_sub_out_imp1 <- predict(test) %>% 
  transmute(trees_l_imp = trees_l) %>% 
  bind_cols(., data_sub)

head(data_sub_out_imp1)

data_sub_out_imp1%>% 
  filter(date > mdy("11 - 1- 2015") & date <  mdy("11 - 1- 2016")) %>% 
  ggplot( aes(x = date, y = trees_l_imp)) + 
  geom_point(color = "red", size = 2) +
  geom_point(aes(x= date, y = trees_l), size = 1) 


data(miss)
f <- ~c31+c32+c33+c34+c35
## one-window covariance
i <- mnimput(f,miss,eps=1e-3,ts=TRUE, method="spline",sp.control=list(df=c(7,7,7,7,7)))
summary(i)


### trying out tsImpute ##########################################################################
library(imputeTS)

pd3 <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_weather200810.csv", guess_max = 40000)
pd3 <- pd3 %>% mutate(mo = month(date),
                      doy = as.numeric(day(date)),
                      year = year(date),
                      year_f = paste0("y_", year(date))) 
      #filter(NAB_station != "College Station") 
pd4 <- pd3 %>% dplyr::select(c(#train0_test1, 
  NAB_station, date, ja, cup_other, trees, pol_other, tot_pol))

pd4 %>% 
  filter(NAB_station == "San Antonio A") %>% 
  filter(date > mdy("11 -1 - 2015") & date < mdy("1 - 1- 2018")) %>% 
  select(tot_pol) %>% 
ggplot_na_distribution(x = .) + scale_y_log10()

pd4 %>% 
#  filter(NAB_station != "College Station" & NAB_station != "Waco B") %>% 
  filter(date > mdy("11 - 1 - 2015") & date < mdy("1 - 1- 2018")) %>% 
  select(tot_pol) %>% 
ggplot_na_gapsize(.)


#linear interpolation
pd4 %>% 
  filter(NAB_station == "Dallas" ) %>% 
  #filter(NAB_station != "College Station" & NAB_station != "Waco B") %>% 
  filter(date > mdy("11 - 1 - 2015") & date < mdy("1 - 1- 2018")) %>% 
  select(trees) %>% 
  mutate(trees = log10(trees + 1)) %>% 
  mutate(trees_int = unlist(na_interpolation(., option = "linear"))) %>% 
  ggplot_na_imputations(x_with_na = .$trees, x_with_imputations = .$trees_int)  

#kalman filtering using either an ARIMA or structural time series approach
pd4 %>% 
  filter(NAB_station == "Dallas" ) %>% 
  #filter(NAB_station != "College Station" & NAB_station != "Waco B") %>% 
  filter(date > mdy("11 - 1 - 2015") & date < mdy("1 - 1- 2018")) %>% 
  select(trees) %>% 
  mutate(trees = log10(trees + 1)) %>% 
  mutate(trees_int = unlist(na_kalman(., model = "auto.arima"))) %>% # model = "auto.arima",  model = "StructTS"
  ggplot_na_imputations(x_with_na = .$trees, x_with_imputations = .$trees_int)  


#comparing the fit of a few different methods by altering a known time series
#using the Flower Mound as a test case since there are only 2 NA values
ts_fm <- pd4 %>%  
  filter(NAB_station == "Flower Mound" ) %>% 
  filter(date > mdy("11 - 1 - 2015") & date < mdy("1 - 1- 2018")) %>% 
  select(trees) 
summary(ts_fm)

#using the NA positions from Georgetown to create NAs in the Flower Mound time series
georgetown_NAs <- pd4 %>%  
  filter(NAB_station == "Georgetown" ) %>% 
  filter(date > mdy("11 - 1 - 2015") & date < mdy("1 - 1- 2018")) %>% 
  select(trees) %>% 
  is.na() 
ts_fm_sim_na <- ts_fm
ts_fm_sim_na[georgetown_NAs == TRUE] <- NA

#comparing a few different options for the efficacy of different methods
#linear interpolation
ts_fm_sm_na_int_lin <- na_interpolation(ts_fm_sim_na, option = "linear")

plot( log(ts_fm[georgetown_NAs == TRUE] + 1), 
      log(ts_fm_sm_na_int_lin[georgetown_NAs == TRUE] + 1))
summary(lm(log(ts_fm[georgetown_NAs == TRUE] + 1) ~ 
             log(ts_fm_sm_na_int_lin[georgetown_NAs == TRUE] + 1)))

#Kalman smoothing
ts_fm_sm_na_int_kal_arima <- na_kalman(ts_fm_sim_na, model = "auto.arima")

plot( log(ts_fm[georgetown_NAs == TRUE] + 1), 
      log(ts_fm_sm_na_int_kal_arima[georgetown_NAs == TRUE] + 1))
summary(lm(log(ts_fm[georgetown_NAs == TRUE] + 1) ~ 
             log(ts_fm_sm_na_int_kal_arima[georgetown_NAs == TRUE] + 1)))


#comparing to the RF model I created (earlier in script)
#this was custom made by plugging in a particular dataframe into the RF predictions, the regular version shouldn't go here
focal_pollen_missing_data_rfint_fm_sim <- focal_pollen_missing_data_rfint_mod_join %>% 
  filter(date > mdy("10 - 31 - 2015") & date < mdy("1 - 1 - 2018")) %>% 
  select(focal_pollen_rfint_log_mean) %>% 
  unlist

plot( log(ts_fm[georgetown_NAs == TRUE] + 1), 
      focal_pollen_missing_data_rfint_fm_sim[georgetown_NAs == TRUE] + 1)
fit <- lm(  log(ts_fm[georgetown_NAs == TRUE] + 1) ~ 
             focal_pollen_missing_data_rfint_fm_sim[georgetown_NAs == TRUE] )
summary(fit)

# Function that returns Root Mean Squared Error
rmse <- function(error){sqrt(mean(error^2))}
rmse(fit$residuals)

# Function that returns Mean Absolute Error
mae <- function(error){mean(abs(error))}
mae(fit$residuals)


#R2 for this method for trees in Flower Mound when simulating missing data (taking the Georgetown station): is 0.92
  #whereas when using rf for just Flower Mound with existing data split into testing/training data, the R2 is 0.87 



### playing around with the workflow package ########################################
#http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
#https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
#https://www.benjaminsorensen.me/post/modeling-with-parsnip-and-tidymodels/

set.seed(123)
pd4_split <- initial_split(pd4, prop = 3/4)
pd4_train_data <- training(pd4_split)
pd4_test_data <- testing(pd4_split)
pd4_cv <- vfold_cv(pd4_train)

names(pd4_train)

pd4_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(ja ~ ., 
         data = pd4_train) %>%
  # and some pre-processing steps
  step_log(all_outcomes(), -all_predictors(), offset = 0.1) %>% 
  #step_naomit(all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_meanimpute(all_numeric(), -all_outcomes()) %>% 
  prep()
  #step_nzv(all_numeric())
  #step_knnimpute(all_predictors())
  #step_unknown() #%>% 

#prepped <- pd4_recipe %>% prep(retain = TRUE)

pd4_training <- pd4_recipe %>%
  bake(training(pd4_split)) 

pd4_testing <- pd4_recipe %>%
  bake(testing(pd4_split)) 

# #str(pd4_train)
# pd4_train_preprocessed <- pd4_recipe %>% 
#   prep(pd4_train) %>% 
#   juice()
  
rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune()) %>%
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("regression") 

rf_model2 <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("regression") 



rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(pd4_recipe) %>%
  # add the model
  add_model(rf_model)

# specify which values eant to try
rf_grid <- expand.grid(mtry = c(3))

# extract results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = pd4_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(rmse, rsq) # metrics we care about
  )
rf_tune_results

param_final <- rf_tune_results %>%
  select_best(metric = "rsq")

param_final

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(pd4_split)
rf_fit

test_performance <- rf_fit %>% collect_metrics()
test_performance

# generate predictions from the test set
test_predictions <- rf_fit %>% collect_predictions()
test_predictions

final_model <- fit(rf_workflow, pd4)
final_model
