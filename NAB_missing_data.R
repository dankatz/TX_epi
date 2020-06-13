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
pd4 <- pd3 %>% mutate(mo = month(date),
              doy = as.numeric(day(date)),
              year = year(date)
              #train0_test1 = rbinom(n = nrow(pd3) , size = 1, prob = 0.2)
              ) %>%
                    dplyr::select(c(#train0_test1, 
                                    NAB_station, mo, doy, year, ja, cup_other, trees, pol_other, tot_pol,
                            starts_with("met_"), 
                            starts_with("ghcn_")))%>% 
                      select(-ghcn_psun, -ghcn_tsun, -ghcn_snow, -ghcn_snwd, - ghcn_tavg, -ghcn_wesd,-ghcn_wesf,
                             -starts_with("ghcn_wt"),
                             ) %>% 
                      # mutate(
                      #        ja_orig = ja,
                      #        ja = ifelse(train0_test1 == 0, ja_orig, NA),
                      #        cup_other_orig = cup_other,
                      #        cup_other = ifelse(train0_test1 == 0, cup_other_orig, NA),
                      #        trees_orig = trees,
                      #        trees = ifelse(train0_test1 == 0, trees_orig, NA),
                      #        pol_other_orig = pol_other,
                      #        pol_other = ifelse(train0_test1 == 0, pol_other_orig, NA),) %>%
              mutate(ja_ma3 = slide_vec(ja, ~mean(.x[c(1,3)], na.rm = TRUE), .before = 1, .after = 1),
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
pd4[is.nan(pd4)] <- NA
ja_missing_data <- pd4 %>% filter(is.na(ja))
pd4 <- filter(pd4, !is.na(ja))

str(pd4)

### predict missing values ###################################################################
library(randomForest)

focal_station <- "Georgetown" #unique(pd4$NAB_station)

TrainSet2 <- TrainSet %>% select(-ja_orig, -cup_other_orig, - trees_orig, -pol_other_orig, -tot_pol)
  
#sapply(TrainSet2, function(y) sum(length(which(is.na(y))))) #count number of NAs in each column
TrainSet2 <- TrainSet2 %>% 
              filter(NAB_station == focal_station) %>% 
              filter(!is.na(ja)) %>% 
              replace(., is.na(.), 0)

#start_time <- Sys.time() 
model1 <- randomForest(ja ~ ., data = TrainSet2, importance = TRUE)



#print(Sys.time() - start_time)
model1
importance(model1)

#plot(model1)

# model2 <- randomForest(taxa ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
# model2

# Predicting on train set
#predTrain <- predict(model1, TrainSet, type = "class")
#str(predTrain)
# Checking classification accuracy
#table(predTrain, TrainSet$taxa)  



### model assessment ###############################################################
#library(ggplot2)
# Predicting on Validation set
#ValidSet$taxa2 <- as.character(ValidSet$taxa) #ValidSet$taxa <- NULL #ValidSet$taxa <- ValidSet$taxa2
#TrainSet2 <- TrainSet %>% select(-ja_orig, -cup_other_orig, - trees_orig, -pol_other_orig)

#sapply(TestSet, function(y) sum(length(which(is.na(y))))) #count number of NAs in each column
TestSet2 <- TestSet %>% 
  filter(NAB_station == focal_station) %>% 
  select(-ja) %>% 
  replace(., is.na(.), 0) 

predValid <- predict(object = model1, newdata = TestSet2)
TestSet2 <- mutate(TestSet2, predValid = predValid)

ggplot(TestSet2, aes(x= predValid, y = ja_orig)) + geom_point() + theme_bw() + geom_smooth(method = "lm") + 
  geom_abline(slope = 1, intercept = 0) #+ scale_x_log10() + scale_y_log10()
fit <- lm(ja_orig ~ predValid, data = TestSet2)
summary(fit)

model_importance_df <- as.data.frame(importance(model1))        
View(model_importance_df) #names(model_importance_df)
varImpPlot(model1) 

#hist(model_importance_df$MeanDecreaseGini)
#str(model_importance_df)

#save(model1, file = "C:/Users/dsk856/Box/MIpostdoc/trees/tree_identificaiton/d_rf200503.RData")



### trying the tidymodel workflow #################################################
install.packages("tidymodels")
install.packages("dials")
library(tidymodels)

set.seed(1243)

dia_split <- initial_split(diamonds, prop = .1, strata = price)

dia_train <- training(dia_split)
dia_test  <- testing(dia_split)

dim(dia_train)
#> [1] 5395   10
dim(dia_test)
#> [1] 48545    10

dia_vfold <- vfold_cv(dia_train, v = 3, repeats = 1, strata = price)
dia_vfold %>% 
  mutate(df_ana = map(splits, analysis),
         df_ass = map(splits, assessment))

qplot(carat, price, data = dia_train) +
  scale_y_continuous(trans = log_trans(), labels = function(x) round(x, -2)) +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 4)") +
  labs(title = "Nonlinear relationship between price and carat of diamonds",
       subtitle = "The degree of the polynomial is a potential tuning parameter")

dia_rec <-
  recipe(price ~ ., data = dia_train) %>%
  step_log(all_outcomes()) %>%
  step_normalize(all_predictors(), -all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_poly(carat, degree = 2)

prep(dia_rec)

# Note the linear and quadratic term for carat and the dummies for e.g. color
dia_juiced <- juice(prep(dia_rec))
dim(dia_juiced)

lm_model <-
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

rand_forest(mtry = 3, trees = 500, min_n = 5) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "impurity_corrected")

lm_fit1 <- fit(lm_model, price ~ ., dia_juiced)
lm_fit1

glance(lm_fit1$fit)
tidy(lm_fit1) %>% 
  arrange(desc(abs(statistic)))

lm_predicted <- augment(lm_fit1$fit, data = dia_juiced) %>% 
  rowid_to_column()
select(lm_predicted, rowid, price, .fitted:.std.resid)

ggplot(lm_predicted, aes(.fitted, price)) +
  geom_point(alpha = .2) +
  # ggrepel::geom_label_repel(aes(label = rowid), 
  #                           data = filter(lm_predicted, abs(.resid) > 2)) +
  labs(title = "Actual vs. Predicted Price of Diamonds")



# How does dia_vfold look?
dia_vfold

# Extract analysis/training and assessment/testing data
lm_fit2 <- mutate(dia_vfold,
                  df_ana = map (splits,  analysis),
                  df_ass = map (splits,  assessment))
lm_fit2

lm_fit2 <- 
  lm_fit2 %>% 
  # prep, juice, bake
  mutate(
    recipe = map (df_ana, ~prep(dia_rec, training = .x)),
    df_ana = map (recipe,  juice),
    df_ass = map2(recipe, 
                  df_ass, ~bake(.x, new_data = .y))) %>% 
  # fit
  mutate(
    model_fit  = map(df_ana, ~fit(lm_model, price ~ ., data = .x))) %>% 
  # predict
  mutate(
    model_pred = map2(model_fit, df_ass, ~predict(.x, new_data = .y)))

select(lm_fit2, id, recipe:model_pred)

lm_preds <- 
  lm_fit2 %>% 
  mutate(res = map2(df_ass, model_pred, ~data.frame(price = .x$price,
                                                    .pred = .y$.pred))) %>% 
  select(id, res) %>% 
  tidyr::unnest(res) %>% 
  group_by(id)
lm_preds
metrics(lm_preds, truth = price, estimate = .pred)

rf_model <- 
  rand_forest(mtry = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")

parameters(rf_model)

rf_model %>% 
  parameters() %>% 
  # Here, the maximum of mtry equals the number of predictors, i.e., 24.
  finalize(x = select(juice(prep(dia_rec)), -price)) %>% 
  pull("object")

dia_rec2 <-
  recipe(price ~ ., data = dia_train) %>%
  step_log(all_outcomes()) %>%
  step_normalize(all_predictors(), -all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_poly(carat, degree = tune())

dia_rec2 %>% 
  parameters() %>% 
  pull("object")

rf_wflow <-
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(dia_rec2)
rf_wflow

rf_param <-
  rf_wflow %>%
  parameters() %>%
  update(mtry = mtry(range = c(3L, 5L)),
         degree = degree_int(range = c(2L, 4L)))
rf_param$object

rf_grid <- grid_regular(rf_param, levels = 3)
rf_grid


library("doFuture")
all_cores <- parallel::detectCores(logical = FALSE) - 1

registerDoFuture()
cl <- makeCluster(all_cores)
plan(future::cluster, workers = cl)

rf_search <- tune_grid(rf_wflow, grid = rf_grid, resamples = dia_vfold,
                       param_info = rf_param)

autoplot(rf_search, metric = "rmse") +
  labs(title = "Results of Grid Search for Two Tuning Parameters of a Random Forest")

show_best(rf_search, "rmse", n = 9)
select_best(rf_search, metric = "rmse")
select_by_one_std_err(rf_search, mtry, degree, metric = "rmse")

rf_param_final <- select_by_one_std_err(rf_search, mtry, degree,
                                        metric = "rmse")

rf_wflow_final <- finalize_workflow(rf_wflow, rf_param_final)

rf_wflow_final_fit <- fit(rf_wflow_final, data = dia_train)

dia_rec3     <- pull_workflow_prepped_recipe(rf_wflow_final_fit)
rf_final_fit <- pull_workflow_fit(rf_wflow_final_fit)

dia_test$.pred <- predict(rf_final_fit, 
                          new_data = bake(dia_rec3, dia_test))$.pred
dia_test$logprice <- log(dia_test$price)

metrics(dia_test, truth = logprice, estimate = .pred)


### trying out the tidymodels with my dataset ########################################
set.seed(123)
pd4_split <- initial_split(pd4, prop = 3/4)
pd4_train <- training(pd4_split)
pd4_test <- testing(pd4_split)
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

#str(pd4_train)
pd4_train_preprocessed <- pd4_recipe %>% 
  prep(pd4_train) %>% 
  juice()
  
rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune()) %>%
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

ja_missing_data$ja <- NULL
ja_missing_data_pred <- predict(final_model, pd4_train_preprocessed)

?predict
names(ja_missing_data)
names(pd4)
ranger_obj <- pull_workflow_fit(final_model)$fit
ranger_obj
ranger_obj$variable.importance


pd4_testing <- pd4_recipe %>%
  bake(testing(pd4_split)) 
predict(final_model, pd4_testing)
  
  
iris_split <- initial_split(iris, prop = 0.6)
iris_split

#my data
pd4_toy <- select(pd4, ja, doy, met_tmaxdegc)
iris_split <- initial_split(pd4_toy, prop = 0.6)
iris_recipe <- training(iris_split) %>%
  recipe(ja ~.) %>%
  #step_corr(all_predictors()) %>%
  step_naomit(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep()
iris_testing <- iris_recipe %>%
  bake(testing(iris_split)) 
iris_training <- juice(iris_recipe)
iris_ranger <- rand_forest(trees = 100, mode = "regression") %>%
  set_engine("ranger") %>%
  fit(ja ~ ., data = iris_training)
predict(iris_ranger, iris_testing)
iris_testing$ja[1] <- NA


#original
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
