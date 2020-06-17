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
write_csv(weather_at_stations, "C:/Users/dsk856/Box/texas/NAB/weather_at_NAB_stations200608.csv")
unique(weather_at_stations$measurement)
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
#wasn't able to download all 8 at once, trying it out for
                  
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
names(all_monitors_clean4)
pd3 <- left_join(pd2, all_monitors_clean4)


### create derived variables and subset training and testing datasets ###########################################
library(magrittr)
library("slider")
library(purrr)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

#names(which(sapply(dp2, anyNA)))
#write_csv(pd3, "C:/Users/dsk856/Box/texas/NAB/NAB_weather200611.csv")
set.seed(100)
pd3 <- read_csv("C:/Users/dsk856/Box/texas/NAB/NAB_weather200611.csv", guess_max = 40000)
pd3 <- pd3 %>% mutate(mo = month(date),
                  doy = as.numeric(day(date)),
                  year = year(date),
                  year_f = paste0("y_", year(date))) %>% 
        filter(NAB_station != "College Station",
               NAB_station != "Waco A",
               NAB_station != "Waco B") 
pd4 <- pd3 %>% dplyr::select(c(#train0_test1, 
                                    NAB_station, ja, cup_other, trees, pol_other, tot_pol,
                                    mo, doy, year, year_f, date,
                            starts_with("met_"), 
                            starts_with("ghcn_")))%>% 
                      select(-ghcn_psun, -ghcn_tsun, -ghcn_snow, -ghcn_snwd, - ghcn_tavg, -ghcn_wesd,-ghcn_wesf,
                             -starts_with("ghcn_wt")
                             ) %>% 
              mutate(
                   ja_l1 = lag(ja, 1),
                   ja_l2 = lag(ja, 2),
                   ja_lead1 = lead(ja, 1),
                   ja_lead2 = lead(ja, 2),
                   ja_ma3 = slide_vec(ja, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   ja_ma5 = slide_vec(ja, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   ja_ma7 = slide_vec(ja, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   ja_ma9 = slide_vec(ja, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4),
                   cup_other_ma3 = slide_vec(cup_other, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   cup_other_ma5 = slide_vec(cup_other, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   cup_other_ma7 = slide_vec(cup_other, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   cup_other_ma9 = slide_vec(cup_other, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4),
                   trees_ma3 = slide_vec(trees, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   trees_ma5 = slide_vec(trees, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   trees_ma7 = slide_vec(trees, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   trees_ma9 = slide_vec(trees, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4),
                   pol_other_ma3 = slide_vec(pol_other, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
                   pol_other_ma5 = slide_vec(pol_other, ~mean(.x[c(1:2,4:5)], na.rm = TRUE), .before = 2, .after = 2),
                   pol_other_ma7 = slide_vec(pol_other, ~mean(.x[c(1:3,5:7)], na.rm = TRUE), .before = 3, .after = 3),
                   pol_other_ma9 = slide_vec(pol_other, ~mean(.x[c(1:4,6:9)], na.rm = TRUE), .before = 4, .after = 4)
              ) %>% 
        select(-cup_other, -trees, -pol_other, -tot_pol) #%T>%
        # {filter(., train0_test1 == 0) ->> TrainSet} %T>%  
        # {filter(., train0_test1 == 1) ->> TestSet} 

is.nan.data.frame <- function(x) {do.call(cbind, lapply(x, is.nan))}
#is.nan.data.frame(pd4)
pd4[is.nan(pd4)] <- NA
ja_missing_data <- pd4 %>% filter(is.na(ja))
pd4 <- filter(pd4, !is.na(ja))

str(pd4)

### using a partial tidymodel workflow with ranger ############################################

pd5 <- pd4 %>% #filter(NAB_station == "San Antonio A") %>% 
  select_if(~sum(!is.na(.)) > 0)

pd5_split <- initial_split(pd5, prop = 0.8)
pd5_recipe <- training(pd5_split) %>%
  recipe(ja ~.) %>%
  step_log(all_outcomes(), contains("_ma"), offset = 0.1) %>% 
  #step_nzv(all_predictors()) %>% 
  step_meanimpute(all_numeric()) %>%
  step_naomit(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes(), -contains("_ma"), -date) %>%
  step_scale(all_numeric(), -all_outcomes(), -contains("_ma"), -date) %>%
  step_date(date, features = c("doy", "month", "year")) %>% 
  #step_num2factor(date_year, transform = as.integer, levels = paste0("y", 2008:2019)) %>% 
  step_dummy(all_nominal()) %>% 
  prep()

#unique(pd5$year)
#?step_num2factor
pd5_training <- pd5_recipe %>% bake(training(pd5_split))
pd5_testing  <- pd5_recipe %>% bake(testing(pd5_split)) 
pd5_testing_nab <- testing(pd5_split)$NAB_station
pd5_ranger <- rand_forest(trees = 1000, mode = "regression", mtry = 5) %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(ja ~ ., data = pd5_training)
pd5_ranger
ranger_obj <- pd5_ranger$fit
sort(ranger_obj$variable.importance)
ranger_pred <- predict(pd5_ranger, pd5_testing, type = "numeric")


pd5_testing %>% mutate(NAB_station = pd5_testing_nab,
                         ja = pd5_testing$ja,
                         ja_hat_ranger = unlist(ranger_pred)) %>% 
  ggplot(aes(x = ja + 0.1, y = ja_hat_ranger + 0.1, color = NAB_station)) + 
  geom_point() + geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0) + theme_bw()

ja_missing_data_baked <- pd5_recipe %>% bake(new_data = ja_missing_data)
ja_missing_data_pred <- predict(pd5_ranger, ja_missing_data_baked)
#hist(unlist(ja_missing_data_pred))

ja_missing_data_pred_join <- ja_missing_data_baked %>% 
  mutate(NAB_station = ja_missing_data$NAB_station, #get pre-baked versions of the variables
         year = ja_missing_data$year,
         mo = ja_missing_data$mo,
         doy = ja_missing_data$doy,
        ja_hat_log = unlist(ja_missing_data_pred),
        ja_hat = exp(ja_hat_log ) - 0.1) %>% 
                              select(NAB_station, year, mo, doy, ja_hat, ja_hat_log)
pd6 <- left_join(pd3, ja_missing_data_pred_join)


pd6 %>% filter(year == 2015) %>% # & NAB_station == "Georgetown") %>% 
ggplot(aes(x = date, y = ja + 0.1)) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_point(aes(x = date, y = ja_hat + 0.1), color = "red") + scale_y_log10()


### adding in CIs with the RFinterval ########################################
# https://github.com/haozhestat/RFIntervals
# https://haozhestat.github.io/files/manuscript_RFIntervals_FinalVersion.pdf
library(rfinterval)

rfint_mod <- rfinterval(ja ~ ., train_data = as.data.frame(pd5_training),
                                    test_data = as.data.frame(pd5_testing),
                                    method =c("oob"), #, "split-conformal", "quantreg"
                                    symmetry = TRUE, alpha = 0.05) 
y <- pd5_testing$ja
mean(rfint_mod$oob_interval$lo < y & rfint_mod$oob_interval$up > y)
rfint_mod$testPred

pd5_testing %>% mutate(NAB_station = pd5_testing_nab,
                       ja = pd5_testing$ja,
                       ja_hat_rfint_mean = unlist(rfint_mod$testPred),
                       ja_hat_rfint_lo = unlist(rfint_mod$oob_interval$lo),
                       ja_hat_rfint_up = unlist(rfint_mod$oob_interval$up)) %>% 
  ggplot(aes(x = ja, y = ja_hat_rfint_mean, 
             ymin = ja_hat_rfint_lo,
             ymax = ja_hat_rfint_up,
               color = NAB_station)) + 
  geom_pointrange() + geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0) + theme_bw()

#create the predictions 
ja_missing_data_baked <- pd5_recipe %>% bake(new_data = ja_missing_data)
rfint_mod <- rfinterval(ja ~ ., train_data = as.data.frame(pd5_training),
                        test_data = as.data.frame(ja_missing_data_baked),
                        method =c("oob"), #, "split-conformal", "quantreg"
                        symmetry = TRUE, alpha = 0.05) 

ja_missing_data_rfint_mod_join <- ja_missing_data_baked %>% 
  mutate(NAB_station = ja_missing_data$NAB_station, #get pre-baked versions of the variables
         year = ja_missing_data$year,
         mo = ja_missing_data$mo,
         doy = ja_missing_data$doy,
         ja_hat_rfint_log_mean = unlist(rfint_mod$testPred),
         ja_hat_rfint_log_lo = unlist(rfint_mod$oob_interval$lo),
         ja_hat_rfint_log_up = unlist(rfint_mod$oob_interval$up),
         ja_hat_rfint_log_sd = (ja_hat_rfint_log_up - ja_hat_rfint_log_lo) /(1.96 * 2)) %>% 
  select(NAB_station, year, mo, doy, ja_hat_rfint_log_mean, ja_hat_rfint_log_sd, ja_hat_rfint_log_lo, ja_hat_rfint_log_up)
pd7 <- left_join(pd6, ja_missing_data_rfint_mod_join)

#do predictions match?
pd7 %>% ggplot(aes(x = ja_hat_log, y = ja_hat_rfint_log_mean)) + geom_point() + facet_wrap(~NAB_station) + theme_bw()

pd7 %>% filter(year == 2015) %>% # & NAB_station == "Georgetown") %>% 
  ggplot(aes(x = date, y = log(ja + 0.1 ))) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
  geom_pointrange(aes(x = date,
                     y = ja_hat_rfint_log_mean,
                     ymin = ja_hat_rfint_log_mean - ja_hat_rfint_log_sd,
                     ymax = ja_hat_rfint_log_mean + ja_hat_rfint_log_sd, # + ja_hat_rfint_log_sd),
                     color = "red")) 
  # geom_pointrange(aes(x = date + 2, 
  #                     y = ja_hat_rfint_log_mean, 
  #                     ymin = ja_hat_rfint_log_lo,
  #                     ymax = ja_hat_rfint_log_up
  #                     ), 
  #                 color = "green") 

pd7$ja_hat_rfint_log_mean - pd7$ja_hat_rfint_log_lo
pd7$ja_hat_rfint_log_sd


















### previous attempts at applying the workflow package to my dataset ########################################
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

ja_missing_data$ja <- NA
ja_missing_data_prepped <- pd4_recipe %>% bake(new_data = ja_missing_data)
ja_missing_data_pred <- predict(final_model, pd4_train_data)

names(pd4_train)
names(pd4)

?predict
names(ja_missing_data)
names(pd4)
ranger_obj <- pull_workflow_fit(final_model)$fit
ranger_obj
ranger_obj$variable.importance


pd4_testing <- pd4_recipe %>%
  bake(testing(pd4_split)) 
predict(final_model, pd4_testing)
  
  


#my data
pd4_toy <- pd4 %>% #filter(NAB_station == "San Antonio A") %>% 
  select_if(~sum(!is.na(.)) > 0)
#summary(pd4_toy$met_swekgm2)
#str(pd4_toy)
iris_split <- initial_split(pd4_toy, prop = 0.7)
iris_recipe <- training(iris_split) %>%
  recipe(ja ~.) %>%
  step_log(all_outcomes(), offset = 0.1) %>% 
  step_nzv(all_predictors()) %>% 
  step_meanimpute(all_numeric()) %>%
  step_naomit(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal()) %>% 
  prep()

iris_training <- iris_recipe %>% bake(training(iris_split))
iris_testing  <- iris_recipe %>% bake(testing(iris_split)) 
iris_testing_nab <- testing(iris_split)$NAB_station
iris_ranger <- rand_forest(trees = 500, mode = "regression") %>%
  set_engine("ranger") %>%
  fit(ja ~ ., data = iris_training)
#iris_ranger <- boost_tree(mtry = 3, trees = 500, mode = "regression") %>% 
# iris_ranger <- svm_poly(mode = "regression") %>% 
#   fit(ja ~ ., data = iris_training)
iris_ranger
ranger_pred <- predict(iris_ranger, iris_testing, type = "numeric")

library("quantregForest")
iris_training2 <- select(iris_training, -ja)# %>% sample_n(500)
iris_testing2 <- select(iris_testing, -ja) #%>% sample_n(333)

iris_qrf <- quantregForest(x = iris_training2, y = iris_training$ja, keep.inbag = TRUE, ntree = 1000, mtry = 4)
summary(iris_qrf)
conditionalMean <- predict(iris_qrf, iris_testing2, what = mean) #breaks silently if you put in "new data = "!
conditionalSD <- predict(iris_qrf, iris_testing2, what = sd)

library(magrittr)
iris_testing2 %>% mutate(NAB_station = iris_testing_nab,
                        ja = iris_testing$ja,
                        ja_hat_ranger = unlist(ranger_pred),
                        ja_hat = conditionalMean,
                        ja_hat_sd = conditionalSD) %>% 
ggplot(aes(x = ja + 0.1, y = ja_hat_ranger + 0.1, color = NAB_station)) + 
  geom_point() + geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0) + theme_bw()
   #+ scale_x_log10() + scale_y_log10()



iris_testing2 %>% mutate(ja = iris_testing$ja,
                         ja_hat = conditionalMean,
                         ja_hat_ranger = unlist(ranger_pred),
                         ja_hat_sd = conditionalSD) %>% 
  ggplot(aes(x = ja + 0.1, y = ja_hat + 0.1, ymax = ja_hat + ja_hat_sd + 0.1 , ymin = ja_hat - ja_hat_sd + 0.1)) + 
  ggplot(aes(x = ja_hat_ranger + 0.1, y = ja_hat + 0.1, ymax = ja_hat + ja_hat_sd + 0.1 , ymin = ja_hat - ja_hat_sd + 0.1)) + 
  geom_point() + geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0) + theme_bw()+
  geom_linerange() + scale_x_log10() + scale_y_log10()


ja_missing_data$ja <- NA
ja_missing_data_baked <- iris_recipe %>% bake(new_data = ja_missing_data)
ja_missing_data_pred <- predict(final_model, ja_missing_data_baked)


str(ja_missing_data)
ja_missing_data <- ja_missing_data %>% filter(NAB_station == unique(pd4_toy$NAB_station)) 
ja_missing_data$ja <- as.numeric(ja_missing_data$ja)
ja_missing_data_prepped <- iris_recipe %>% bake(ja_missing_data) 
predict(iris_ranger, new_data = ja_missing_data_prepped)



#original
iris_split <- initial_split(iris, prop = 0.6)
iris_split
iris_recipe <- training(iris_split) %>%
  recipe(Species ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
iris_testing <- iris_recipe %>%
  bake(testing(iris_split)) 
iris_training <- juice(iris_recipe)
iris_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Species ~ ., data = iris_training)
predict(iris_ranger, iris_testing)
iris_testing$Species[1] <- NA
iris_testing$Sepal.Length[1] <- NA
