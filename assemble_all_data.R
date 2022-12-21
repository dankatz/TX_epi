#data assembly for preliminary epidemiological analysis. This script was originally part of the epi_preliminary_analysis 
#install.packages("noncensus")
library(tidyr) #install.packages("tidyr") 
library(dplyr) #install.packages("cli")
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(stringr)
library(zipcode) #downloaded from the archives, no longer on CRAN
library(sf)
#library(noncensus) #not available for R 4.0.0 yet
library(zoo)
library(tidycensus) 
library(tigris)
options(tigris_use_cache = TRUE)
library(lwgeom)
library(imputeTS)
library(daymetr)


#rm(list = ls())

#removing Waco B because of data quality issues

### start loop to go through all the distance thresholds ##############################
dist_threshold_list <- rep(c(10, 25, 50), 3)
age_low_list <- c(0,0,0, 5,5,5, 18,18,18)
age_hi_list <- c(4,4,4, 17,17,17, 99,99,99)

for(dist_age in 1:9){ #dist_age <- 1
  print(paste("distance:", dist_threshold_list[dist_age]))
  print(paste("age low:", age_low_list[dist_age]))
  print(paste("age hi:", age_hi_list[dist_age]))


### options for different data subsets
# distance cutoff from NAB station
NAB_min_dist_threshold <- dist_threshold_list[dist_age] 

# define target age range here 
age_low <- age_low_list[dist_age] 
age_hi <-  age_hi_list[dist_age] 

# NAB_min_dist_threshold <- 25
# age_low <-5  # >= #young kids = 0, school-aged kids = 5, adults = 18
# age_hi <- 17  # <= #young kids = 4, school-aged kids = 17, adults = 99

### load in NAB data  #####################################################
# the pollen data are now assembled and processed in 'NAB_data_assembly.R' on github
# there is also a 1-week interpolation of missing data 
# files still need to manually pasted over to the Z drive
NAB_tx <- read_csv("Z:/THCIC/Katz/data_pollen/NAB2009_2021_tx_epi_pollen_220831.csv", guess_max = 92013)
NAB_tx <- filter(NAB_tx, NAB_station!= "Waco B")

### THCIC outpatient data on asthma-related ED visits #####################################################
# this dataset was created in the 'THCIC_assembly.R' script on github: 
# https://github.com/dankatz/TX_epi/blob/master/THCIC_assembly.R

opa_raw <- read_csv("Z:/THCIC/Katz/op_ip_asthma_2015q4_2020.csv")
#opa_raw$PAT_COUNTY <- sprintf("%03s",opa_raw$PAT_COUNTY) %>% sub(" ", "0",.) %>% sub(" ", "0",.)
opa_raw <- mutate(opa_raw, PAT_COUNTY = sprintf("%03s", PAT_COUNTY), 
                  PAT_COUNTY = sub(" ", "0", PAT_COUNTY),
                  PAT_COUNTY = sub(" ", "0", PAT_COUNTY),
                  PAT_ADDR_CENSUS_BLOCK_GROUP_c = gsub(".", "", PAT_ADDR_CENSUS_BLOCK_GROUP, fixed = TRUE), #remove the extra periods from this column
                  GEOID10 = paste0(PAT_ADDR_CENSUS_BLOCK_GROUP_c, PAT_ADDR_CENSUS_BLOCK), #to link up with coordinates downloaded from the census
                  GEOID = PAT_ADDR_CENSUS_BLOCK_GROUP_c, 
                  #GEOID_n = nchar(GEOID), #for trouble shooting
                  zip_pat = substr(PAT_ZIP, 1, 5))

#opa_raw %>% select(PAT_ADDR_CENSUS_BLOCK_GROUP_c, PAT_ADDR_CENSUS_BLOCK_GROUP)

#head(opa_raw)
#names(opa_raw)
#get the coordinates for each case
#load in the coordinates for all census blocks in Texas (generated with this script: C:\Users\dsk856\Box\texas\preliminary_epi\census_data\census_block_centroid_TX_200330.R)
# block_coord <- read_csv("C:/Users/dsk856/Desktop/misc_data/TX_block_centroids.csv", 
#                         col_types = cols("GEOID10" =col_character(), 
#                                  "lat" = col_double(), 
#                                  "lon" = col_double()))

block_group_coord <- read_csv("Z:/THCIC/Katz/TX_block_group_centroids.csv",  
                              col_types = cols("GEOID" =col_character(),
                                               "lat" = col_double(),
                                               "lon" = col_double()))

opa_raw <- left_join(opa_raw, block_group_coord) #names(opa_raw) #names(block_group_coord)#summary(opa_raw$lat) #
#head(opa_raw)
#test <- slice_sample(opa_raw, n = 100)

## Using census tract centroids when the block group isn't available but the census tract is (23967 records)
census_tract_coord <- read_csv("Z:/THCIC/Katz/TX_census_tract_centroids.csv",  
                               col_types = cols("GEOID11" =col_character(),
                                                "lat_tract" = col_double(),
                                                "lon_tract" = col_double())) %>% 
  rename(PAT_ADDR_CENSUS_BLOCK_GROUP_c = GEOID11)
opa_raw <- left_join(opa_raw, census_tract_coord) #names(opa_raw) #names(block_group_coord)
#test <- filter(opa_raw, is.na(lon) & !is.na(lon_tract))

# get coordinates for each patient's census block
# census_block_unique <- mutate(opa_raw, block = paste(PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_ADDR_CENSUS_BLOCK, sep = " "),
#                                        state = substr(PAT_ADDR_CENSUS_BLOCK_GROUP, 1, 2)) %>%
#                         filter(state == 48) %>%
#                         dplyr::select(PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_COUNTY) %>%
#                         distinct()
# test <- get_acs(state="TX",geography="block group", year = 2016, variables= "B01001_003", geometry=TRUE)

## Using zip code centroids when neither census blocks nor tracts work but a zip code is available (4662 records out of 277232 records)
data("zipcode") #head(zipcode)
zipcode2 <- dplyr::select(zipcode, 
                          zip_pat = zip,
                          lat_zip = latitude, 
                          lon_zip = longitude)
opa_raw <- left_join(opa_raw, zipcode2) %>% 
  mutate(lon_imp = case_when(!is.na(lon) ~ lon,
                             is.na(lon) & !is.na(lon_tract) ~ lon_tract, #use census tract centroid when block not available
                             is.na(lon) & is.na(lon_tract) ~ lon_zip), #use the zip code centroid if the census info is messed up 
         lat_imp = case_when(!is.na(lat) ~ lat,
                             is.na(lat) & !is.na(lat_tract) ~ lat_tract,
                             is.na(lat) & is.na(lat_tract) ~ lat_zip)) %>% 
  mutate(lon_imp = case_when(lon_imp < 0 ~ lon_imp,
                             lon_imp > 0 ~ lon_imp * -1)) #correct for incorrectly entered coordinates
# opa_raw$lat_imp[is.na(opa_raw$lat_imp)] <- opa_raw$lat_zip[is.na(opa_raw$lat_imp)] #including coordinates that are imputed from zip code
# opa_raw$lon_imp[is.na(opa_raw$lon_imp)] <- opa_raw$lon_zip[is.na(opa_raw$lon_imp)]
# opa_raw$lon_imp[opa_raw$lon_imp > 0 & !is.na(opa_raw$lon_imp)] <- opa_raw$lon_imp[opa_raw$lon_imp > 0 & !is.na(opa_raw$lon_imp)] * -1
# test <- opa_raw %>% mutate(nchar_geoid = nchar(GEOID10)) %>% filter(nchar_geoid < 14)
# hist(nchar(opa_raw$GEOID10))

#how many records didn't have the block group but did have the census tract
length(opa_raw$RECORD_ID[is.na(opa_raw$lon) & !is.na(opa_raw$lon_tract)]) / nrow(opa_raw)

#how many records didn't have the census tract but did have zip
length(opa_raw$RECORD_ID[is.na(opa_raw$lon) & is.na(opa_raw$lon_tract) & !is.na(opa_raw$lat_zip)]) / nrow(opa_raw)


#how many records in each group
# test <- filter(opa_raw, is.na(lon) ) #51340/586828   
# test <- filter(opa_raw, is.na(lon) & !is.na(lon_tract) ) #23967/277232
# test <- filter(opa_raw, is.na(lon) & is.na(lon_tract) & !is.na(lon_zip)) #4662/277232 #188/277232 #27011/

# some graphical checks
# opa_raw %>% sample_n(10000) %>%
# ggplot(aes(x = lon_imp, y = lat_imp)) + geom_point()
#   opa_raw %>% filter(PROVIDER_NAME == "Childrens Medical Center-Dallas") %>%
#     filter(lat_imp > 32 & lat_imp < 33) %>%
#     filter(lat_zip > 32 & lat_zip < 33) %>%
#   ggplot(aes(x = lat_imp, y = lat_zip)) + geom_point(alpha = 0.1) + theme_bw() #do a quick visual check to see how much accuracy is lost using zip codes


### calculate distance from the nearest NAB station to each case ###############################################
opa_raw_sf <- opa_raw %>%
  filter(!is.na(lon_imp) & !is.na(lat_imp)) %>%         #sample_n(10000) %>%
  st_as_sf(coords = c( "lon_imp", "lat_imp"), crs = 4326) %>% 
  st_transform(crs = 26914)   #UTM 14 N

NAB_tx_sf <- NAB_tx %>% 
  dplyr::select(Lat, Long, NAB_station) %>%
  distinct() %>%
  #filter(NAB_station != "") %>% #not sure how this made its way in, maybe a floating decimal?
  filter(!is.na(Lat)) %>% #not sure how this made its way in
  as.data.frame() %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)  %>%
  st_transform(crs = 26914)  #UTM 14 N

distances <- st_distance(opa_raw_sf, NAB_tx_sf, by_element = FALSE) /1000 #calculate distances and convert to km
distances_df <- as.data.frame(distances) 
distances_min <- apply(distances_df, 1, FUN = min) #minimum distance to a NAB station
which_station_closest <- apply(distances_df, 1, function(x) which(x == min(x, na.rm = TRUE))) #which station is closest
NAB_station_lookup <- data.frame(NAB_station = NAB_tx_sf$NAB_station, n_lookup = 1:length(unique(NAB_tx_sf$NAB_station)))
station_looked_up <- left_join(data.frame(n_lookup = which_station_closest), NAB_station_lookup)
opa_raw_sf <- mutate(opa_raw_sf, NAB_min_dist = distances_min, NAB_station = station_looked_up$NAB_station)
NAB_dist_opa_join <- opa_raw_sf 
NAB_dist_opa_join$geometry <- NULL

opa_raw <- left_join(opa_raw, NAB_dist_opa_join) 

### filter asthma ED visits where residence was within X km of an NAB station ###############################################
#NAB_min_dist_threshold <- 25 #moved to top of script
opa <- opa_raw %>%
  filter(NAB_min_dist < NAB_min_dist_threshold) %>% #restrict cases to patients whose residence was within 25 km of a station
  #filter(opa_raw, PAT_COUNTY %in% NAB_counties)  %>% #Travis county number is 453
  mutate(PAT_AGE_YEARS = as.numeric(PAT_AGE_YEARS), 
         PAT_AGE_DAYS = as.numeric(PAT_AGE_DAYS),
         date = ymd(STMT_PERIOD_FROM)) %>%
  dplyr::select(SEX_CODE, PAT_ZIP, PAT_AGE_YEARS, PAT_AGE_DAYS, RACE, ETHNICITY, PRINC_DIAG_CODE, PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_ADDR_CENSUS_BLOCK,
                PAT_COUNTY, PAT_AGE_GROUP, date, NAB_min_dist, NAB_station, lon_imp, lat_imp) %>% 
  mutate(school_metro_area = case_when(NAB_station == "San Antonio A" ~ "San Antonio", #this part needs to be before school section
                                                              NAB_station == "San Antonio B" ~ "San Antonio",
                                                              NAB_station == "Waco A" ~ "Waco",
                                                              TRUE ~ NAB_station))
# opa %>% #sample_frac(0.1) %>%
# ggplot(aes(x= lon_imp, y = lat_imp, color = NAB_station)) + geom_point(alpha = 0.03) + theme_bw() + xlab("longitude") + ylab("latitude") +
#   guides(color = guide_legend(override.aes = list(alpha =1)))


# opa_msa <- filter(opa_raw, PAT_COUNTY %in% NAB_MSA_counties)  %>% #Travis county number is 453
#   dplyr::select(SEX_CODE, PAT_ZIP, PAT_AGE_YEARS, RACE, ETHNICITY, PRINC_DIAG_CODE, PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_ADDR_CENSUS_BLOCK,
#                 PAT_COUNTY, PAT_AGE_GROUP, date) %>%
#             mutate(FIPS = PAT_COUNTY)
# opa_msa <- left_join(opa_msa, msa)


# make sure that dates with no cases have zeros instead of missing from list
day_list <- seq(mdy("9/1/2015"), mdy("12/31/2020"), by = "1 day") #as.data.frame( seq(mdy("9/1/2015"), mdy("12/31/2017"), by = "1 day"))
NAB_list <- unique(opa$NAB_station)
day_NAB_list <- expand.grid(day_list, NAB_list)
names(day_NAB_list) <- c("date", "NAB_station")
day_NAB_list <- mutate(day_NAB_list, n_cases = 0, doy = yday(date))



### get the population of each census block group that's near an NAB station ###############################################
#population for each of those counties #the following code is from RAZ
# v17 <- load_variables(2017, "acs5", cache = TRUE)
# v10 <- load_variables(2010, "acs5", cache = TRUE)
# 
# Clean.Census.Data <- function(x) {
#   x <- separate(x, `variable`, into = c("race", "age_grp"), sep = "_" )
#   x$age_grp <- as.numeric(x$age_grp) 
#   output <- x %>% mutate(
#     sex = ifelse(age_grp == 3:25, "M", "F"),
#     age_group = ifelse(age_grp %in% 3:6 | age_grp %in% 31:49, "1", "2") #DK: https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15&table=B01001
#   ) 
#   result <- output %>%
#     group_by(Year, GEOID) %>%
#     summarise(
#       children_pop = sum(estimate[age_group=="1"]), 
#       adults_pop = sum(estimate[age_group=="2"]),
#       total_pop = children_pop + adults_pop
#     )
#   return(result)
# }
# 
# AllM <- v17$name[5:27]
# AllF <- v17$name[29:51]
# All_vars <- c(AllM, AllF)

# ### define target age range here #moved to top of script
# age_low <- 0 # >=
# age_hi <- 4 # <=

#Variables that I want: ages 5-17  #B01001_003
c_vars_youngkids <- c(paste0("B0100", 1003), #males 0 - 4: 1003; 5 - 17: 1004:1006, 18 +: 1007:1025
                      paste0("B0100", 1027)) %>%  #females 0 - 4: 1027; 5 - 17: 1028:1030, 18 +: 1031:1049
  gsub(pattern = "B01001", replacement ="B01001_", x = .) #adding the underscore back in
c_vars_schoolkids <- c("B01001_004", "B01001_005", "B01001_006", #males from 5-17 years old
                       "B01001_028", "B01001_029", "B01001_030") #females from 5-17 years old
c_vars_adults <- c(paste0("B0100", 1007:1025), #males
                   paste0("B0100", 1031:1049)) %>%  #females
  gsub(pattern = "B01001", replacement ="B01001_", x = .) #adding the underscore back in

#census_All_2017 <- get_acs(state="TX", geography="block group", year = 2017, variables=All_vars, geometry=FALSE) #takes 5 min
#write_csv(census_All_2017, "Z:/THCIC/Katz/ACS_pop_by_age_2017_200331.csv")
census_All_2017 <- read_csv("Z:/THCIC/Katz/ACS_pop_by_age_2017_200331.csv",
                            col_types = cols("GEOID" =col_character()))
census_All_2017 <- left_join(census_All_2017, block_group_coord) %>% #add in coordinates for each block group
  mutate(lon = lon * -1)
census_All_2017_sf <- census_All_2017 %>% st_as_sf(coords = c( "lon", "lat"), crs = 4326) %>% #takes a min to run
  st_transform(crs = 26914)   #UTM 14 N

#select all block groups that are within threshold distance from a NAB station
NAB_min_dist_threshold #was defined earlier for selecting cases within that distance from an NAB station

distances_bg <- st_distance(census_All_2017_sf, NAB_tx_sf, by_element = FALSE) /1000 #calculate distances and convert to km
distances_bg_df <- as.data.frame(distances_bg) 
distances_bg_min <- apply(distances_bg_df, 1, FUN = min) #minimum distance to a NAB station
which_station_closest_bg <- apply(distances_bg_df, 1, function(x) which(x == min(x, na.rm = TRUE))) #which station is closest
station_looked_up_bg <- left_join(data.frame(n_lookup = which_station_closest_bg), NAB_station_lookup) #NAB_station_lookup was defined earlier

census_All_2017_sf <- mutate(census_All_2017_sf, NAB_min_dist_bg = distances_bg_min, NAB_station = station_looked_up_bg$NAB_station)
census_All_2017_sf$geometry <- NULL

pop_near_NAB_young_kids <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  filter(variable %in% c_vars_youngkids) %>%  #only select variables that are population of children between 5 and 17
  group_by(NAB_station) %>%
  summarize(young_kids_pop = sum(estimate)) #names(pop_near_NAB)

pop_near_NAB_schoolkids <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  filter(variable %in% c_vars_schoolkids) %>%  #only select variables that are population of children between 5 and 17
  group_by(NAB_station) %>%
  summarize(schoolkids_pop = sum(estimate)) #names(pop_near_NAB)

pop_near_NAB_adult <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  filter(variable %in% c_vars_adults) %>%  #only select variables that are population of children between 5 and 17
  group_by(NAB_station) %>%
  summarize(adult_pop = sum(estimate)) #names(pop_near_NAB)


# #block groups IDs that are near NAB stations
#  block_groups_near_NAB <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
#   select(GEOID, NAB_station) %>%
#   distinct()

# #a check to compare my numbers with RAZ's 
# census_All_2017_sf %>%
#   filter(grepl("Travis County, Texas", NAME)) %>% 
#   filter(variable %in% c_vars_agegroup_x) %>%  
#   summarize(adult_pop = sum(estimate)) #names(pop_near_NAB)


### download and extract met data ###############################################################
#start with the pixels of each NAB station
NAB_tx_sf_coords <- st_transform(NAB_tx_sf, crs = 4326) %>% mutate(site = NAB_station,
                                                                   lat = st_coordinates(.)[,2], long = st_coordinates(.)[,1],
                                                                   NAB_station = NULL)
#geometry = NULL)
#test <- download_daymet(lon = NAB_tx_sf_coords$long[1], lat = NAB_tx_sf_coords$lat[1], start =2015, end = 2017, simplify = TRUE)
NAB_tx_sf_coords$geometry <- NULL
setwd("Z:/THCIC/Katz/met_data")
# write_csv(NAB_tx_sf_coords, "NAB_tx_coords.csv")
# weather_at_stations <- download_daymet_batch(file_location = "NAB_tx_coords.csv", start =2015, end = 2020, simplify = TRUE)
# write_csv(weather_at_stations, "weather_at_NAB_stations220809.csv")
# unique(weather_at_stations$measurement)
weather_at_stations <- read_csv("Z:/THCIC/Katz/met_data/weather_at_NAB_stations220809.csv")%>% 
  mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
  mutate(measurement = gsub(pattern = ".", replacement = "", x = measurement, fixed = TRUE)) %>%
  dplyr::select(NAB_station = site, date, measurement, value) %>%
  pivot_wider(id_cols = c(NAB_station, date), names_from = measurement, values_from = value, names_prefix = "met_") %>% 
  filter(NAB_station != "Waco B")
#head(weather_at_stations)



### Virus monitoring data from DHHS #############################################################
#viral data is now processed in a separate script:
#viral_data_assembly.R on github
nrevss_data4 <- read_csv("Z:/THCIC/Katz/data_viral/TX_NREVSS_processed_220810.csv")
unique(nrevss_data4$viral_metro_area)



### adding in school calendars; will need to eventually move this over to data assembly script ###########################
school <- read_csv("Z:/THCIC/Katz/data_viral/tx_school_calendars_2015_2020_long.csv") %>% 
  mutate_at(., vars(contains("break_")), mdy)

# #some data vis for QA/QC using the original wide version
# school %>% 
#   ggplot(aes(x = summer_break_start, xmin = summer_break_start, xmax = summer_break_end, y = viral_metro_area, color = year )) + geom_pointrange() +
#   facet_wrap(~year, scales = "free")

#all school_metro_areas x date grid
#unique(school$school_metro_area)
study_date_list <- seq(mdy("9-1-2015"),mdy("1-1-2021"), by = "days")
school_metro_date_grid <- expand.grid(date = study_date_list, school_metro_area = unique(school$school_metro_area))

#having a hard time doing this within groups, so I'm running it separately for each metro area
school_metro_areas <- unique(school$school_metro_area)

for(i in 1:length(school_metro_areas)){
  focal_metro <- school_metro_areas[i]  #focal_metro <- school_metro_areas[1]
  
  school_breaks <- school %>% 
    filter(school_metro_area == focal_metro) %>% 
    transmute(sbreaks = interval(break_start, break_end))  %>% 
    as.list() 
  
  date_df <- school_metro_date_grid %>% 
    filter(school_metro_area == focal_metro)
  
  output_metro <- date_df %>% 
    rowwise() %>% 
    mutate(on_break_raw = any(date %within% school_breaks),
           on_break = case_when(on_break_raw == TRUE ~ 1,
                                on_break_raw == FALSE ~ 0)) #any(date_df$date[70] %within% school_breaks)
  
  if(i == 1){all_metros <- output_metro}else{all_metros <- bind_rows(all_metros, output_metro)}
}

# #visual check
# all_metros %>% ungroup() %>% 
#   filter(date < mdy("10/1/16")) %>% 
# ggplot(aes(x = date, y = on_break )) + geom_line() + facet_wrap(~viral_metro_area)

#creating a few derived variables including days since summer and winter breaks (possible effect times are roughly based on calendars and Eggo's paper)
school_breaks_join <- 
  all_metros %>% ungroup() %>% 
  dplyr::select(-on_break_raw) %>% 
  mutate(last_date_on_holiday = case_when(date == mdy("10/1/2015") ~ mdy("8/25/2015"),
                                          on_break == 1 ~ date)) %>% 
  fill(last_date_on_holiday) %>% 
  mutate(days_since_holiday = as.numeric(date - last_date_on_holiday),
         doy = yday(date),
         days_since_win_break = case_when(doy < 28 ~ days_since_holiday,
                                          doy >= 28 ~ 28),
         days_since_win_break = case_when(days_since_win_break == 0 ~ 29, TRUE ~ days_since_win_break),
         days_since_sum_break = case_when(doy > 213  & doy < 274 ~ days_since_holiday,
                                          TRUE ~ 28),
         days_since_sum_break = case_when(days_since_sum_break == 0 ~ 29,
                                          days_since_sum_break > 28 ~ 28, TRUE ~ days_since_sum_break)) %>% 
  dplyr::select(-doy, -last_date_on_holiday) %>% 
  distinct() #getting rid of duplicates caused by multiple cities being in the same viral metro area


#test2 <- school_breaks_join  %>% dplyr::select(school_metro_area, date) %>% count( school_metro_area, date)




### combine the various datasets ######################################################################
opa_day <- opa %>% group_by(date, NAB_station) %>% #names(opa) opa$PAT_AGE_YEARS
  filter(between(PAT_AGE_YEARS, age_low, age_hi)) %>% #for adults
  summarize(n_cases = n()) %>% 
  mutate(doy = yday(date)) 

opa_day <- bind_rows(day_NAB_list, opa_day)
opa_day <- opa_day %>% group_by(date, NAB_station, doy) %>%
  summarize(n_cases = sum(n_cases)) #add up n_cases from each day

opa_day <- left_join(opa_day, NAB_tx)
opa_day <- left_join(opa_day, pop_near_NAB_young_kids)
opa_day <- left_join(opa_day, pop_near_NAB_schoolkids)
opa_day <- left_join(opa_day, pop_near_NAB_adult)

opa_day <- opa_day %>%  #unique(opa_day$City)
  mutate(viral_metro_area = case_when(NAB_station == "Dallas" ~ "Dallas/FlowerMound",
                                      NAB_station == "Flower Mound" ~ "Dallas/FlowerMound",
                                      NAB_station == "San Antonio A" ~ "San Antonio",
                                      NAB_station == "San Antonio B" ~ "San Antonio",
                                      NAB_station == "Waco A" ~ "Waco",
                                      #NAB_station == "Waco B" ~ "Waco",
                                      TRUE ~ NAB_station)) %>% 
  left_join(., nrevss_data4) #str(nrevss_data4) 

#opa_day <- left_join(opa_day, flu) #str(flu)
#opa_day <- left_join(opa_day, virus) ##head(virus)
opa_day <- left_join(opa_day, weather_at_stations)

# holiday_df <- data.frame(date = ymd(unlist(timeDate::holidayNYSE(2015:2017))), holiday = 1) #install.packages("timeDate")
# opa_day <- left_join(opa_day, holiday_df)
# opa_day$holiday[is.na(opa_day$holiday)] <- 0
opa_day <- opa_day %>% ungroup() %>% group_by(NAB_station) %>% arrange(NAB_station, date) %>%
  mutate(week_day = weekdays(date),
         week_day = forcats::fct_relevel(week_day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                         "Saturday", "Sunday"))  %>% filter(date > mdy('9-30-15')) 


opa_day <- opa_day  %>% 
  mutate(school_metro_area = case_when(NAB_station == "San Antonio A" ~ "San Antonio", #this part needs to be before school section
                                       NAB_station == "San Antonio B" ~ "San Antonio",
                                       NAB_station == "Waco A" ~ "Waco",
                                       TRUE ~ NAB_station)) %>% 
  left_join(., school_breaks_join) 


# #for children
# opa_day <- mutate(opa_day, pbir = ((n_cases/children_pop) * 10000), #PIBR per 10,000 for children
#                            pbir_py = (pbir / ((children_pop))) * 100000)
# write_csv(opa_day, "C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_50km_200918.csv")
# 
# #for adults
# opa_day_adult <- mutate(opa_day, pbir = ((n_cases/adult_pop) * 10000), #PIBR per 10,000 for children
#                   pbir_py = (pbir / ((adult_pop))) * 100000)
# write_csv(opa_day_adult, "C:/Users/dsk856/Desktop/thcic_analysis/opa_day_adult_50km_200918.csv")


#making sure the population is set for the correct age group. 
#WARNING: BE CAUTIOUS IF NOT USING THE EXACT AGEGROUPS
if(age_hi < 6){opa_day_agegroup_x <- mutate(opa_day, agegroup_x_pop = young_kids_pop)} #PIBR per 1,000,000 
if(age_hi == 17){opa_day_agegroup_x <- mutate(opa_day, agegroup_x_pop = schoolkids_pop)} #PIBR per 1,000,000 
if(age_hi > 90){opa_day_agegroup_x <- mutate(opa_day, agegroup_x_pop = adult_pop)} #PIBR per 1,000,000 

opa_day_agegroup_x <- opa_day_agegroup_x %>% mutate(pbir =  (n_cases/agegroup_x_pop) * 1000000)


csv_file_name <- paste0("Z:/THCIC/Katz/",
                        "opa_day_ages_",age_low,"_",age_hi,"_dist_", NAB_min_dist_threshold, "_",Sys.Date(),".csv")
write_csv(opa_day_agegroup_x, csv_file_name)

#summary(opa_day_agegroup_x)

}### end the distance and agegroup loop
unique(opa_day_agegroup_x$NAB_station)
