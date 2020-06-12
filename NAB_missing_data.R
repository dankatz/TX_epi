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
              doy = day(date),
              year = year(date),
              train0_test1 = rbinom(n = nrow(pd3) , size = 1, prob = 0.2)) %>%
                    dplyr::select(c(train0_test1, NAB_station, mo, doy, year, ja, cup_other, trees, pol_other, tot_pol,
                            starts_with("met_"), 
                            starts_with("ghcn_")))%>% 
                      mutate(
                             ja_orig = ja,
                             ja = ifelse(train0_test1 == 0, ja_orig, NA),
                             cup_other_orig = cup_other,
                             cup_other = ifelse(train0_test1 == 0, cup_other_orig, NA),
                             trees_orig = trees,
                             trees = ifelse(train0_test1 == 0, trees_orig, NA),
                             pol_other_orig = pol_other,
                             pol_other = ifelse(train0_test1 == 0, pol_other_orig, NA),) %>%
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
              ) %T>%
        {filter(., train0_test1 == 0) ->> TrainSet} %T>%  
        {filter(., train0_test1 == 1) ->> TestSet} 


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





