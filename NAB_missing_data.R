#modeling the missing data from each NAB station
library(tidyr) 
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)

rm(list = ls())



### load in NAB data from 2009-2019 ##################################################################
NAB_locations <- read_csv("C:/Users/dsk856/Box/texas/NAB/EPHT_Pollen Station Inventory_062019_dk200331.csv")
NAB <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB2009_2019_pollen_200508.csv", guess_max = 50000)

### create the categories of pollen and prepare data ################################################
NAB_tx <- left_join(NAB, NAB_locations) %>%
  filter(State == "TX") %>%
  mutate(date = ymd(Date),
         mo = month(date),
         doy = yday(date),
         year = year(date),
         ja = case_when( mo < 3   ~ Cupressaceae,
                         mo > 11 ~ Cupressaceae,
                         TRUE ~ 0),
         cup_other = case_when(ja == 0 ~ Cupressaceae,
                               ja > 0 ~ 0)) %>%
rowwise()%>%
  mutate(trees = sum(Acer, Alnus,Betula, Carpinus.Ostrya, Corylus, Fraxinus,Juglans, Liquidambar, Other.Tree.Pollen,                    
                            Pinaceae, Populus, Pseudotsuga, Quercus, Salix, Arecaceae, Carya,Cyperaceae, Fagus, Ligustrum,
                            Morus,  Olea, Platanus, Tilia ,Celtis,  Prosopis, Myrica, Ulmus, Tsuga, na.rm = TRUE),
         pol_other = Total.Pollen.Count - ja - cup_other - trees)%>%  
  dplyr::select(date, mo, doy, year, City, FIPS, county_name, MSA, Lat, Long, NAB_station,
                ja, cup_other, trees, pol_other,
                tot_pol = Total.Pollen.Count) 

date_station_grid <- expand_grid(seq(min(NAB$date),max(NAB$date), by = '1 day'), 
                                 unique(NAB_tx$NAB_station)) %>% 
                    `colnames<-`(c("date", "NAB_station")) %>%
                    filter(!is.na(NAB_station))

pd <- left_join(date_station_grid, NAB_tx) %>%
          arrange(NAB_station, date)


### download and extract met data from Daymet ###############################################################
library(daymetr)
library(sf)
library(magrittr)
library(tidyr)
library(purrr)

NAB_tx_coords <- NAB_tx %>% dplyr::select(site = NAB_station, lat = Lat, long = Long) %>% distinct() %>% filter(!is.na(site))
write_csv(NAB_tx_coords, "C:/Users/dsk856/Box/texas/NAB/TX_NAB_station_coords_daymetr200608.csv")
#test <- download_daymet(lon = NAB_tx_coords$Long[1], lat = NAB_tx_coords$Lat[1], start = 2015, end = 2016, simplify = TRUE)
weather_at_stations <- download_daymet_batch(file_location = "C:/Users/dsk856/Box/texas/NAB/TX_NAB_station_coords_daymetr200608.csv", start =2009, end = 2019, simplify = TRUE)
#write_csv(weather_at_stations, "C:/Users/dsk856/Box/texas/NAB/weather_at_NAB_stations200608.csv")
#unique(weather_at_stations$measurement)
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

station_data <- ghcnd_stations(noaa_key = noaa_api_key) # Takes a while to run
NAB_tx_coords_rnoaa <- NAB_tx_coords %>% rename(id = site, latitude = lat, longitude = long)

nearby_stations <- meteo_nearby_stations(lat_lon_df = NAB_tx_coords_rnoaa, radius = 150, limit = 1, 
                                         year_min = 2009, year_max = 2019, var = "AWND")

station_names <- as.data.frame(nearby_stations) %>% select(contains("id")) %>% unlist()
all_monitors_clean <- meteo_pull_monitors(monitors = station_names, keep_flags = FALSE, 
                                          date_min = "2008-01-01", var = "all") #takes a few minutes to run
#wasn't able to download all 8 at once, did it manually in batches
unique(all_monitors_clean3$id)
station_names2 <- station_names[1:4]
all_monitors_clean2 <- meteo_pull_monitors(monitors = station_names[1], keep_flags = FALSE, 
                                          date_min = "2008-01-01", var = "all") #takes a few minutes to run
all_monitors_clean3 <- meteo_pull_monitors(monitors = station_names[2:4], keep_flags = FALSE, 
                                           date_min = "2008-01-01", var = "all") #takes a few minutes to run
all_monitors_clean4 <- bind_rows(all_monitors_clean, all_monitors_clean2, all_monitors_clean3)
station_names_lookup <- data.frame(id = station_names, NAB_station = names(station_names)) %>%
  mutate(NAB_station = gsub(pattern = ".", replacement = " ", x = NAB_station, fixed = TRUE),
         NAB_station = gsub(pattern = "id", replacement = "", x = NAB_station, fixed = TRUE), 
         NAB_station = stringr::str_trim(NAB_station))

#names(all_monitors_clean4 )
all_monitors_clean4 <- all_monitors_clean4 %>% rename_at(vars(c(-id, -date, - NAB_station)), ~ paste0("ghcn_", .))
#unique(all_monitors_clean4$NAB_station)

#write_csv(all_monitors_clean4, "C:/Users/dsk856/Box/texas/NAB/weatherNOAA_at_NAB_stations200611.csv")
all_monitors_clean4 <- read_csv("C:/Users/dsk856/Box/texas/NAB/weatherNOAA_at_NAB_stations200611.csv", guess_max = 40000)
#names(all_monitors_clean4)
pd3 <- left_join(pd2, all_monitors_clean4)
#write_csv(pd3, "C:/Users/dsk856/Box/texas/NAB/NAB_weather200611.csv")

### create derived variables and subset training and testing datasets ###########################################
library(magrittr)
library("slider")
library(purrr)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

#names(which(sapply(dp2, anyNA)))
#set.seed(100)
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
pol_missing_data <- pd4 %>% filter(is.na(ja)) %>% #when one pollen type is NA all other types are also NA
                    mutate(focal_pollen = NA)


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
focal_pollen_var <- "pol_other" #ja, cup_other, trees, pol_other, tot_pol
###

pd5 <- pd4 %>% #filter(NAB_station == "San Antonio A") %>% 
  filter(!is.na(ja)) %>% #remove the missing observations
  select_if(~sum(!is.na(.)) > 0) %>%  #remove columns that are all NAs
  mutate(focal_pollen = .data[[focal_pollen_var]]) #making the focal pollen type a column; see programming with dplyr vignette
  
pd5_split <- initial_split(pd5, prop = 0.8)
pd5_recipe <- training(pd5_split) %>%
  recipe(focal_pollen ~.) %>%
  step_rm(ja, cup_other, trees, pol_other, tot_pol) %>%  #remove the pollen measurements to prevent data leakage
  step_log(all_outcomes(), contains("_ma"), contains("_lead"), contains("_l1"), contains("_l2"), offset = 0.9) %>% 
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

pd5_testing %>% mutate(NAB_station = pd5_testing_nab,
                       focal_pollen = pd5_testing$focal_pollen,
                       focal_pollen_hat_rfint_mean = unlist(rfint_mod$testPred),
                       focal_pollen_hat_rfint_lo = unlist(rfint_mod$oob_interval$lo),
                       focal_pollen_hat_rfint_up = unlist(rfint_mod$oob_interval$up)) %>% 
  ggplot(aes(x = focal_pollen, y = focal_pollen_hat_rfint_mean, 
             ymin = focal_pollen_hat_rfint_lo,
             ymax = focal_pollen_hat_rfint_up,
             color = NAB_station)) + 
  geom_pointrange() + geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0) + theme_bw()


#predict the missing pollen data
rfint_mod <- rfinterval(focal_pollen ~ ., train_data = as.data.frame(pd5_training),
                        test_data = as.data.frame(focal_pollen_missing_data_baked),
                        method =c("oob"), #, "split-conformal", "quantreg"
                        symmetry = TRUE, alpha = 0.05) 

focal_pollen_missing_data_rfint_mod_join <- focal_pollen_missing_data_baked %>% #get pre-baked versions of the variables
  mutate(focal_pollen_rfint_log_mean = unlist(rfint_mod$testPred),
          focal_pollen_rfint_log_lo = unlist(rfint_mod$oob_interval$lo),
          focal_pollen_rfint_log_up = unlist(rfint_mod$oob_interval$up),
          focal_pollen_rfint_log_sd = (focal_pollen_rfint_log_up - focal_pollen_rfint_log_lo) /(1.96 * 2)) %>% 
  select(focal_pollen_rfint_log_mean, focal_pollen_rfint_log_sd, 
         focal_pollen_rfint_log_lo, focal_pollen_rfint_log_up) %>% 
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
#write_csv(pd7_save, "C:/Users/dsk856/Box/texas/NAB/NAB_pollen_modeled200618.csv")

#some graphs to check up on it
pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(ja + 0.9 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_pointrange(aes(x = date,
                      y = ja_rfint_log_mean,
                      ymin = ja_rfint_log_mean - ja_rfint_log_sd,
                      ymax = ja_rfint_log_mean + ja_rfint_log_sd, # + focal_pollen_rfint_log_sd),
                      color = "red")) 

pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(trees + 0.9 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_pointrange(aes(x = date,
                      y = trees_rfint_log_mean,
                      ymin = trees_rfint_log_mean - trees_rfint_log_sd,
                      ymax = trees_rfint_log_mean + trees_rfint_log_sd, # + focal_pollen_rfint_log_sd),
                      color = "red")) 

pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(pol_other + 0.9 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_pointrange(aes(x = date,
                      y = pol_other_rfint_log_mean,
                      ymin = pol_other_rfint_log_mean - pol_other_rfint_log_sd,
                      ymax = pol_other_rfint_log_mean + pol_other_rfint_log_sd, # + focal_pollen_rfint_log_sd),
                      color = "red")) 

pd7 %>% filter(year == 2017) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(pol_other + 0.9 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_pointrange(aes(x = date,
                      y = pol_other_rfint_log_mean,
                      ymin = pol_other_rfint_log_mean - pol_other_rfint_log_sd,
                      ymax = pol_other_rfint_log_mean + pol_other_rfint_log_sd, # + focal_pollen_rfint_log_sd),
                      color = "red")) 





















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
