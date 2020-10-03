#data assembly for preliminary epidemiological analysis
#install.packages("noncensus")
library(tidyr) #install.packages("tidyr") 
library(dplyr) #install.packages("dplyr")
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(stringr)
library(zipcode) #downloaded from the archives, no longer on CRAN
library(sf)
library(noncensus) #not available for R 4.0.0 yet
library(zoo)
library(tidycensus) 
library(tigris)
options(tigris_use_cache = TRUE)
library(lwgeom)
#install.packages("tidycensus")
#library(rjags)

rm(list = ls())



### load in NAB data from 2009-2019 ##################################################################
NAB_locations <- read_csv("C:/Users/dsk856/Desktop/misc_data/EPHT_Pollen Station Inventory_062019_dk200331.csv")
NAB <- read_csv("C:/Users/dsk856/Desktop/misc_data/NAB2009_2019_pollen_191230.csv", guess_max = 92013)
# NAB_modeled_orig <- read_csv("C:/Users/dsk856/Desktop/misc_data/NAB_pollen_modeled200618.csv", guess_max = 31912)
NAB_modeled <- read_csv("C:/Users/dsk856/Desktop/misc_data/NAB_pollen_modeled_linear_interp_201002.csv", guess_max = 31912)

# msa <- read.csv("C:/Users/dsk856/Desktop/misc_data/county_to_MSA_clean.csv", stringsAsFactors = FALSE)
# msa$FIPS <- sprintf("%03s",msa$FIPS) %>% sub(" ", "0",.) %>% sub(" ", "0",.)

NAB_tx <- left_join(NAB, NAB_locations) %>%
  filter(State == "TX") %>%
  mutate(date = ymd(Date),
         doy = yday(date)) %>%  #, FIPS = as.character(FIPS
  rowwise()%>%
  # mutate(trees = sum(Acer, Alnus, Betula, Carpinus.Ostrya, Corylus, Fraxinus, Juglans, Liquidambar, Other.Tree.Pollen,                    
  #                    Populus, Pseudotsuga, Quercus, Salix, Arecaceae, Carya, Cyperaceae, Fagus, Ligustrum,
  #                    Morus, Olea, Platanus, Tilia, Celtis, Prosopis, Myrica, Ulmus, Tsuga, na.rm = TRUE), #not including Pinaceae
  #        pol_other = Total.Pollen.Count - ja - cup_other - trees,
  #        tot_pol = Total.Pollen.Count)%>%  
  dplyr::select(date, doy, file, #City, FIPS, county_name, MSA, Lat, Long, NABStation_ID, file, 
                #ja, cup_other, trees, pol_other, tot_pol,  #Total.Pollen.Count, Cupressaceae,grass = Gramineae...Poaceae,Ambrosia, Quercus, Ulmus, Acer, Platanus,    
                NAB_station)


NAB_tx <- left_join(NAB_modeled, NAB_tx) %>% 
  dplyr::select(-file) #names(NAB_tx)

NAB_tx <- left_join(NAB_tx, NAB_locations) %>%  #have to do a second join with location data due to some unfortunate formatting
  dplyr::select(date, doy, file, City, FIPS, county_name, MSA, Lat, Long, NABStation_ID, 
                #ja, cup_other, trees, pol_other, tot_pol, ja_rfint_log_mean, cup_other_rfint_log_mean, trees_rfint_log_mean,  
                #pol_other_rfint_log_mean, 
                cup = Cupressaceae, Ambrosia, Morus, Ulmus, grass, Fagales, other_trees, other_pollen,
                cup_m = Cupressaceae_m, Ambrosia_m, Morus_m, Ulmus_m, grass_m, Fagales_m, other_trees_m, other_pollen_m,
                NAB_station) %>%   #Total.Pollen.Count, Cupressaceae,grass = Gramineae...Poaceae,Ambrosia, Quercus, Ulmus, Acer, Platanus,    
  mutate(FIPS = sprintf("%03s",FIPS), #NAB_tx$FIPS
                         FIPS = sub(" ", "0",FIPS),
                         FIPS = sub(" ", "0",FIPS),
        doy = yday(date),
        year = year(date),
        mo = month(date)) 
  # #Quality control for Waco B, which appears to have coded many NA values as zeros
  # mutate(ja = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ ja), 
  #        cup_other = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ cup_other),
  #        cup_all = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ ja + cup_other),
  #        trees = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ trees),
  #        pol_other = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ pol_other),
  #        tot_pol = case_when(tot_pol == 0 & NAB_station == "Waco B" ~ NA_real_, TRUE ~ tot_pol)) %>% 
  # mutate(ja_m =  case_when(is.na(ja) ~ exp(ja_rfint_log_mean) -1 , !is.na(ja) ~ ja),
  #        cup_all_m =  case_when(is.na(ja) ~ exp(cup_other_rfint_log_mean) - 1 + exp(ja_rfint_log_mean) -1 , !is.na(ja) ~ ja + cup_other),
  #        trees_m = case_when(is.na(trees) ~ exp(trees_rfint_log_mean) -1, !is.na(trees) ~ trees),
  #        pol_other_m = case_when(is.na(pol_other) ~ exp(pol_other_rfint_log_mean) - 1, !is.na(pol_other) ~ pol_other),
  #        ja_lm = log10(ja_m + 1),
  #        cup_all_lm = log10(cup_all_m + 1),
  #        trees_lm = log10(trees_m + 1),
  #        pol_other_lm = log10(pol_other_m + 1))

#comparing modeled to observed pollen concentrations
# filter(NAB_tx, date > ymd("2015-10-1") & date < ymd("2018-01-01")) %>% #summarise(n = n())
# ggplot(aes( x = date, y = cup_m )) + geom_point(color = "red", size = 0.5) + facet_wrap(~NAB_station) +
#   geom_point(aes(x = date, y = cup),  color = "gray") +  theme_bw() +  coord_cartesian(ylim = c(0, 15)) #scale_y_log10() +
# 
# filter(NAB_tx, date > ymd("2015-10-1") & date < ymd("2018-01-01")) %>% #summarise(n = n())
#   ggplot(aes( x = date, y = trees_m )) + geom_point(color = "red", size = 0.5) + facet_wrap(~NAB_station) +
#   geom_point(aes(x = date, y = trees), color = "gray") +  theme_bw() +  coord_cartesian(ylim = c(0, 15)) #scale_y_log10() +


### THCIC outpatient data on asthma-related ED visits #####################################################
opa_raw <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/op_asthma.csv")
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

block_group_coord <- read_csv("C:/Users/dsk856/Desktop/misc_data/TX_block_group_centroids.csv",  
                        col_types = cols("GEOID" =col_character(),
                                         "lat" = col_double(),
                                         "lon" = col_double()))
                        # rename(lat_bg = lat,
                        #        lon_bg = lon)

opa_raw <- left_join(opa_raw, block_group_coord) #names(opa_raw) #names(block_group_coord)
#summary(opa_raw$lat) #

# get coordinates for each patient's census block
# census_block_unique <- mutate(opa_raw, block = paste(PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_ADDR_CENSUS_BLOCK, sep = " "),
#                                        state = substr(PAT_ADDR_CENSUS_BLOCK_GROUP, 1, 2)) %>%
#                         filter(state == 48) %>%
#                         dplyr::select(PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_COUNTY) %>%
#                         distinct()
# test <- get_acs(state="TX",geography="block group", year = 2016, variables= "B01001_003", geometry=TRUE)

## Using zip code centroids when there is a problem with the input variable (28817 records out of 277232 records)
data("zipcode") #head(zipcode)
zipcode2 <- dplyr::select(zipcode, 
                        zip_pat = zip,
                        lat_zip = latitude, 
                       lon_zip = longitude)
opa_raw <- left_join(opa_raw, zipcode2) %>% 
          # mutate(lat_imp = lat,
          #        lon_imp = lon),
          mutate(lon_imp = case_when(!is.na(lon) & lon < 0 ~ lon,
                                     !is.na(lon) & lon > 0 ~ lon * -1, #correct for incorrectly entered coordinates
                                     is.na(lon) ~ lon_zip), #use the zip code centroid if the census info is messed up 
                 lat_imp = case_when(!is.na(lat) ~ lat,
                                      is.na(lat) ~ lat_zip))
# opa_raw$lat_imp[is.na(opa_raw$lat_imp)] <- opa_raw$lat_zip[is.na(opa_raw$lat_imp)] #including coordinates that are imputed from zip code
# opa_raw$lon_imp[is.na(opa_raw$lon_imp)] <- opa_raw$lon_zip[is.na(opa_raw$lon_imp)]
# opa_raw$lon_imp[opa_raw$lon_imp > 0 & !is.na(opa_raw$lon_imp)] <- opa_raw$lon_imp[opa_raw$lon_imp > 0 & !is.na(opa_raw$lon_imp)] * -1

# opa_raw %>% sample_n(10000) %>%
# ggplot(aes(x = lon_imp, y = lat_imp)) + geom_point()
#   opa_raw %>% filter(PROVIDER_NAME == "Childrens Medical Center-Dallas") %>%
#     filter(lat_bg > 32 & lat_bg < 33) %>%
#     filter(lat_zip > 32 & lat_zip < 33) %>%
#   ggplot(aes(x = lat_bg, y = lat_zip)) + geom_point(alpha = 0.1) + theme_bw() #do a quick visual check to see how much accuracy is lost using zip codes


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
              st_as_sf(coords = c("Long", "Lat"), crs = 4326)  %>%
              st_transform(crs = 26914)  #UTM 14 N

distances <- st_distance(opa_raw_sf, NAB_tx_sf, by_element = FALSE) /1000 #calculate distances and convert to km
distances_df <- as.data.frame(distances) 
distances_min <- apply(distances_df, 1, FUN = min) #minimum distance to a NAB station
which_station_closest <- apply(distances_df, 1, function(x) which(x == min(x, na.rm = TRUE))) #which station is closest
NAB_station_lookup <- data.frame(NAB_station = NAB_tx_sf$NAB_station, n_lookup = 1:8)
station_looked_up <- left_join(data.frame(n_lookup = which_station_closest), NAB_station_lookup)
opa_raw_sf <- mutate(opa_raw_sf, NAB_min_dist = distances_min, NAB_station = station_looked_up$NAB_station)
NAB_dist_opa_join <- opa_raw_sf 
NAB_dist_opa_join$geometry <- NULL

opa_raw <- left_join(opa_raw, NAB_dist_opa_join) 

### filter asthma ED visits where residence was within 25 km of an NAB station ###############################################
NAB_min_dist_threshold <- 25
opa <- opa_raw %>%
        filter(NAB_min_dist < NAB_min_dist_threshold) %>% #restrict cases to patients whose residence was within 25 km of a station
        #filter(opa_raw, PAT_COUNTY %in% NAB_counties)  %>% #Travis county number is 453
        mutate(PAT_AGE_YEARS = as.numeric(PAT_AGE_YEARS)) %>%
        dplyr::select(SEX_CODE, PAT_ZIP, PAT_AGE_YEARS, RACE, ETHNICITY, PRINC_DIAG_CODE, PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_ADDR_CENSUS_BLOCK,
                      PAT_COUNTY, PAT_AGE_GROUP, date, NAB_min_dist, NAB_station, lon_imp, lat_imp) 
# opa %>% #sample_frac(0.1) %>%
# ggplot(aes(x= lon_imp, y = lat_imp, color = NAB_station)) + geom_point(alpha = 0.03) + theme_bw() + xlab("longitude") + ylab("latitude") +
#   guides(color = guide_legend(override.aes = list(alpha =1)))


# opa_msa <- filter(opa_raw, PAT_COUNTY %in% NAB_MSA_counties)  %>% #Travis county number is 453
#   dplyr::select(SEX_CODE, PAT_ZIP, PAT_AGE_YEARS, RACE, ETHNICITY, PRINC_DIAG_CODE, PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_ADDR_CENSUS_BLOCK,
#                 PAT_COUNTY, PAT_AGE_GROUP, date) %>%
#             mutate(FIPS = PAT_COUNTY)
# opa_msa <- left_join(opa_msa, msa)


# make sure that dates with no cases have zeros instead of missing from list
day_list <- seq(mdy("9/1/2015"), mdy("12/31/2017"), by = "1 day") #as.data.frame( seq(mdy("9/1/2015"), mdy("12/31/2017"), by = "1 day"))
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

### define target age range here 
age_low <- 5 # >=
age_hi <- 17 # <=

#Variables that I want: ages 5-17  #B01001_003
c_vars <- c("B01001_004", "B01001_005", "B01001_006", #males from 5-17 years old
            "B01001_028", "B01001_029", "B01001_030") #females from 5-17 years old
c_vars_adult <- c(paste0("B0100", 1007:1025), #males
                  paste0("B0100", 1031:1049)) %>%  #females
                  gsub(pattern = "B01001", replacement ="B01001_", x = .) #adding the underscore back in
c_vars_agegroup_x <- c(paste0("B0100", 1004:1006), #males
                    paste0("B0100", 1028:30)) %>%  #females
  gsub(pattern = "B01001", replacement ="B01001_", x = .) #adding the underscore back in

#census_All_2017 <- get_acs(state="TX", geography="block group", year = 2017, variables=All_vars, geometry=FALSE) #takes 5 min
#write_csv(census_All_2017, "C:/Users/dsk856/Desktop/misc_data/ACS_pop_by_age_2017_200331.csv")
census_All_2017 <- read_csv("C:/Users/dsk856/Desktop/misc_data/ACS_pop_by_age_2017_200331.csv",
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
pop_near_NAB <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
    filter(variable %in% c_vars) %>%  #only select variables that are population of children between 5 and 17
    group_by(NAB_station) %>%
    summarize(children_pop = sum(estimate)) #names(pop_near_NAB)

pop_near_NAB_adult <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  filter(variable %in% c_vars_adult) %>%  #only select variables that are population of children between 5 and 17
  group_by(NAB_station) %>%
  summarize(adult_pop = sum(estimate)) #names(pop_near_NAB)

pop_near_NAB_agegroup_x <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  filter(variable %in% c_vars_agegroup_x) %>%  #only select variables that are population of children between 5 and 17
  group_by(NAB_station) %>%
  summarize(agegroup_x_pop = sum(estimate)) #names(pop_near_NAB)

# #block groups IDs that are near NAB stations
#  block_groups_near_NAB <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
#   select(GEOID, NAB_station) %>%
#   distinct()

### download and extract met data ###############################################################
# library(daymetr)
# #start with the pixels of each NAB station
# NAB_tx_sf_coords <- st_transform(NAB_tx_sf, crs = 4326) %>% mutate(site = NAB_station,
#                                                                    lat = st_coordinates(.)[,2], long = st_coordinates(.)[,1],
#                                                                    NAB_station = NULL)
#                                                                    #geometry = NULL)
# #test <- download_daymet(lon = NAB_tx_sf_coords$long[1], lat = NAB_tx_sf_coords$lat[1], start =2015, end = 2017, simplify = TRUE)
# NAB_tx_sf_coords$geometry <- NULL
# setwd("C:/Users/dsk856/Desktop/misc_data")
# write_csv(NAB_tx_sf_coords, "NAB_tx_coords.csv")
# weather_at_stations <- download_daymet_batch(file_location = "NAB_tx_coords.csv", start =2015, end = 2017, simplify = TRUE)
# write_csv(weather_at_stations, "weather_at_NAB_stations200401.csv")
# unique(weather_at_stations$measurement)
weather_at_stations <- read_csv("C:/Users/dsk856/Desktop/misc_data/weather_at_NAB_stations200401.csv")%>% 
  mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) %>%
  mutate(measurement = gsub(pattern = ".", replacement = "", x = measurement, fixed = TRUE)) %>%
  dplyr::select(NAB_station = site, date, measurement, value) %>%
  pivot_wider(id_cols = c(NAB_station, date), names_from = measurement, values_from = value, names_prefix = "met_")
#head(weather_at_stations)



### Virus monitoring data from DHHS #############################################################
library(imputeTS)

virus <- read_csv("C:/Users/dsk856/Desktop/misc_data/virus_2015_2017_daily_adj.csv")

#linear interpolation of virus data
virus <- virus %>% 
  mutate(v_tests_pos_Rhinovirus_ms = as.numeric(scale(na_interpolation(v_tests_pos_Rhinovirus))),
         v_tests_pos_RSV_ms = as.numeric(scale(na_interpolation(v_tests_pos_RSV))),
         v_tests_pos_Corona_ms = as.numeric(scale(na_interpolation(v_tests_pos_Corona))),
         v_tests_perc_pos_Rhinovirus_ms = as.numeric(scale(na_interpolation(v_tests_perc_pos_Rhinovirus))),
         v_tests_perc_pos_RSV_ms = as.numeric(scale(na_interpolation(v_tests_perc_pos_RSV))),
         v_tests_perc_pos_Corona_ms = as.numeric(scale(na_interpolation(v_tests_perc_pos_Corona))),
         v_tests_perc_pos_Rhinovirus_m = na_interpolation(v_tests_perc_pos_Rhinovirus),
         v_tests_perc_pos_RSV_m = na_interpolation(v_tests_perc_pos_RSV),
         v_tests_perc_pos_Corona_m = na_interpolation(v_tests_perc_pos_Corona),
         v_tests_adj_pos_Rhinovirus_m = na_interpolation(adj_pos_Rhinovirus),
         v_tests_adj_pos_RSV_m = na_interpolation(adj_pos_RSV),
         v_tests_adj_pos_Corona_m = na_interpolation(adj_pos_Corona))
#ggplot(virus, aes(x = date, y = v_tests_adj_pos_Rhinovirus_m)) + geom_step() 


### load in flu data ###############################################################################
#downloaded via CDCfluview package, script is here: https://github.com/dankatz/TX_epi/blob/master/cdc_flu_data_acquisition191111.R
flu <- read.csv("C:/Users/dsk856/Desktop/misc_data/flu_daily_200916.csv") %>% 
  mutate(date = ymd(date)) %>% 
  dplyr::select(-proportion_positive, -total_positive)

#it appears a few days at the start didn't have data, taking data from the next week which looks like a pretty reasonable assumption
flu$flu_d_prop_pos[flu$date == "2015-10-01" | flu$date == "2015-10-02" | flu$date == "2015-10-03" ] <- 
  flu$flu_d_prop_pos[flu$date == "2015-10-04" ] 

flu$flu_d_total_pos[flu$date == "2015-10-01" | flu$date == "2015-10-02" | flu$date == "2015-10-03" ] <- 
  flu$flu_d_total_pos[flu$date == "2015-10-04" ] 


#summary(flu)
# flu %>% filter(date > mdy("10-31-2015") & date < mdy("01-01-2018")) %>%
# ggplot(aes(x = date, y = flu_d_prop_pos)) + geom_step() + theme_bw()

### combine the various datasets ######################################################################
opa_day <- opa %>% group_by(date, NAB_station) %>% #names(opa) opa$PAT_AGE_YEARS
  #filter(between(PAT_AGE_YEARS, 5, 17)) %>% #for children
  #filter(between(PAT_AGE_YEARS, 18, 99)) %>% #for adults
  filter(between(PAT_AGE_YEARS, age_low, age_hi)) %>% #for adults
  summarize(n_cases = n()) %>% 
  mutate(doy = yday(date)) 

opa_day <- bind_rows(day_NAB_list, opa_day)
opa_day <- opa_day %>% group_by(date, NAB_station, doy) %>%
  summarize(n_cases = sum(n_cases)) #add up n_cases from each day

opa_day <- left_join(opa_day, NAB_tx)
opa_day <- left_join(opa_day, pop_near_NAB)
opa_day <- left_join(opa_day, pop_near_NAB_adult)
opa_day <- left_join(opa_day, pop_near_NAB_agegroup_x)
opa_day <- left_join(opa_day, flu) #str(flu)
opa_day <- left_join(opa_day, virus) ##head(virus)
opa_day <- left_join(opa_day, weather_at_stations)

# holiday_df <- data.frame(date = ymd(unlist(timeDate::holidayNYSE(2015:2017))), holiday = 1) #install.packages("timeDate")
# opa_day <- left_join(opa_day, holiday_df)
# opa_day$holiday[is.na(opa_day$holiday)] <- 0
opa_day <- opa_day %>% ungroup() %>% group_by(NAB_station) %>% arrange(NAB_station, date) %>%
                               mutate(week_day = weekdays(date),
                              week_day = forcats::fct_relevel(week_day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                                     "Saturday", "Sunday"))  %>% filter(date > mdy('9-30-15')) 

# #for children
# opa_day <- mutate(opa_day, pbir = ((n_cases/children_pop) * 10000), #PIBR per 10,000 for children
#                            pbir_py = (pbir / ((children_pop))) * 100000)
# write_csv(opa_day, "C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_50km_200918.csv")
# 
# #for adults
# opa_day_adult <- mutate(opa_day, pbir = ((n_cases/adult_pop) * 10000), #PIBR per 10,000 for children
#                   pbir_py = (pbir / ((adult_pop))) * 100000)
# write_csv(opa_day_adult, "C:/Users/dsk856/Desktop/thcic_analysis/opa_day_adult_50km_200918.csv")
#for agegroup_x
opa_day_agegroup_x <- mutate(opa_day, pbir = ((n_cases/agegroup_x_pop) * 10000), #PIBR per 10,000 for children
                  pbir_py = (pbir / ((agegroup_x_pop))) * 100000)
# write_csv(opa_day_agegroup_x, paste0("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_",age_low,"_",age_hi, "_25km_201002.csv"))

#summary(opa_day_agegroup_x)

# ### exploring data ###################################################
# opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_25km_200916.csv", guess_max = 8260)
# opa_day_adult <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_adult_25km_200916.csv", guess_max = 8260)
# 
# 
# ### fig 2: time series of each var ############################################################
# names(opa_day)
# 
# #time series for ED visits: children
# pbir_global_mean <- opa_day %>% #the average across all the study areas
#   group_by(date) %>% 
#   summarize(pbir_global_mean = mean(pbir)) 
# panel_ed <-  
#   opa_day %>% 
#   ggplot(aes(x = date, y = pbir, col = NAB_station)) + theme_few() + 
#   geom_line(aes(x = date, y=rollmean((pbir ), 7, na.pad=TRUE)), alpha = 0.3) +
#   geom_line(data = pbir_global_mean, aes(x = date, y = rollmean(pbir_global_mean, 7, na.pad=TRUE)), col = "black") +
#   coord_cartesian(ylim = c(0, 0.55)) +  scale_color_grey() +
#   ylab("Asthma ED \n visits \n (per 10,000)") +
#   theme(strip.text.x = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         legend.position= "none",axis.title.x=element_blank(), axis.text.x=element_blank())
# 
# #time series for ED visits: adults
# pbir_global_mean_adult <- opa_day_adult %>% #the average across all the study areas
#   group_by(date) %>% 
#   summarize(pbir_global_mean = mean(pbir)) 
# 
# panel_ed_adult <-  
#   opa_day_adult %>% 
#   ggplot(aes(x = date, y = pbir, col = NAB_station)) + theme_few() + 
#   geom_line(aes(x = date, y=rollmean((pbir ), 7, na.pad=TRUE)), alpha = 0.3) +
#   geom_line(data = pbir_global_mean_adult, aes(x = date, y = rollmean(pbir_global_mean, 7, na.pad=TRUE)), col = "black") +
#   coord_cartesian(ylim = c(0, 0.12)) +  scale_color_grey(name = "NAB station") +
#   ylab("Asthma ED \n visits \n (per 10,000)") +
#   theme(strip.text.x = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         legend.position= "none", 
#         axis.title.x=element_blank(), axis.text.x=element_blank())
# 
# ## time series for pollen
# pol_global_mean <- opa_day %>% group_by(date) %>% 
#   summarize(cup_all_m_global = mean(cup_all_m),
#             trees_m_global = mean(trees_m),
#             pol_other_m_global = mean(pol_other_m))
# 
# panel_pol_cup <-  opa_day %>% 
#   ggplot(aes(x = date, y = cup_all_m + 1, col = NAB_station, group = NAB_station)) + theme_few() + scale_y_log10() + 
#     geom_line(aes(x = date, y=rollmean((cup_all_m + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station), alpha = 0.3) + 
#   ylab(expression(atop(pollen, (grains/m^3)))) +  scale_color_grey(name = "NAB station")+ #scale_color_discrete(name = "NAB station") + 
#   geom_line(data = pol_global_mean, aes(x = date, y = rollmean(cup_all_m_global + 1, 7, na.pad=TRUE), col = NA, group = NA), col = "black") +
#     theme(legend.position= "none" ) + theme(axis.title.x=element_blank(), axis.text.x=element_blank())
# 
# panel_pol_trees <-  opa_day %>% 
#   ggplot(aes(x = date, y = trees_m + 1, col = NAB_station, group = NAB_station)) + theme_few() + scale_y_log10() + 
#   geom_line(aes(x = date, y=rollmean((trees_m + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station), alpha = 0.3) + 
#   ylab(expression(atop(pollen, (grains/m^3)))) + scale_color_grey(name = "NAB station")+ #scale_color_discrete(name = "NAB station") + 
#   geom_line(data = pol_global_mean, aes(x = date, y = rollmean(trees_m_global + 1, 7, na.pad=TRUE), col = NA, group = NA), col = "black") +
#   theme(legend.position= "none" ) + theme(axis.title.x=element_blank(), axis.text.x=element_blank())
# 
# panel_pol_other <-  opa_day %>% 
#   ggplot(aes(x = date, y = pol_other_m + 1, col = NAB_station, group = NAB_station)) + theme_few() + scale_y_log10() + 
#   geom_line(aes(x = date, y=rollmean((pol_other_m + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station), alpha = 0.3) + 
#   ylab(expression(atop(pollen, (grains/m^3)))) + scale_color_grey(name = "NAB station")+ #scale_color_discrete(name = "NAB station") + 
#   geom_line(data = pol_global_mean, aes(x = date, y = rollmean(pol_other_m_global + 1, 7, na.pad=TRUE), col = NA, group = NA), col = "black") +
#   theme(legend.position= "none" ) + theme(axis.title.x=element_blank(), axis.text.x=element_blank())
# 
# 
# #time series for viruses
# panel_vir <-
#   opa_day %>% ungroup() %>% 
#    mutate(flu_d_perc_pos = flu_d_prop_pos * 100) %>% 
#   #        v_tests_adj_pos_Corona_ms = v_tests_adj_pos_Corona_m/130.6684,
#   #        v_tests_adj_pos_Rhinovirus_ms = v_tests_adj_pos_Rhinovirus_m/130.6684 ,
#   #        v_tests_adj_pos_RSV_ms = v_tests_adj_pos_RSV_m/130.6684 ) %>% 
#     dplyr::select(date, v_tests_perc_pos_Corona_m, v_tests_perc_pos_Rhinovirus_m, v_tests_perc_pos_RSV_m, flu_d_perc_pos) %>% 
#     pivot_longer(cols = c(v_tests_perc_pos_Corona_m, v_tests_perc_pos_Rhinovirus_m, v_tests_perc_pos_RSV_m, flu_d_perc_pos), 
#                  names_to = "virus_type", values_to = "positive_tests") %>% 
#     distinct() %>% 
#     #filter(date > mdy("10 - 31 - 2015")) %>% 
#     arrange(virus_type, date) %>% 
#     ggplot(aes(x = date, y = positive_tests, color = virus_type)) + theme_few() + 
#     geom_step() + #geom_point() +
#     ylab(expression(atop("positive tests", "(%)"))) +
#     scale_color_viridis_d(breaks = c("flu_d_perc_pos", "v_tests_perc_pos_Corona_m", "v_tests_perc_pos_Rhinovirus_m",
#                                     "v_tests_perc_pos_RSV_m"), option = "viridis",
#                        labels = c("Influenza" ,"Seasonal coronavirus", "Rhinovirus","RSV"), name = "virus type") +
#     theme(strip.text.x = element_blank(),
#           strip.background = element_rect(colour="white", fill="white"),
#           legend.position= "none")#c(0.75, 0.75))
# 
# #putting together the time series into one plot  
# ts_panels <- cowplot::plot_grid(panel_ed, panel_ed_adult, 
#                                 panel_pol_cup, panel_pol_trees, panel_pol_other, panel_vir, 
#                                 align = "v", ncol = 1, rel_heights = c(1, 1, 1, 1, 1, 1),
#                                 labels = c("A) Pediatric ARED", "B) Adult ARED", "C) Cupressaceae pollen", "D) tree pollen",
#                                            "E) other pollen","F) Viruses"),
#                                 label_size = 11,
#                                 label_x = 0.17, label_y = 0.8,
#                                 hjust = 0, vjust = 0)
# ggsave(file = "C:/Users/dsk856/Desktop/thcic_analysis/time_series_fig_200916.jpg", plot = ts_panels, 
#        height =20, width = 17, units = "cm", dpi = 300)  
# 
# 
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




#### Children: analysis with a distributed lags model using dlnm package #######################################
library(dlnm)
library(splines)
library(MASS)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)


#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_25km_200918.csv", guess_max = 8260) #opa_day_child_200910.csv
#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_10km_200918.csv", guess_max = 8260) 
#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_50km_200918.csv", guess_max = 8260) 
opa_day <- opa_day_agegroup_x

opa_day %>% #group_by(NAB_station) %>% 
  summarize(total_cases = sum(n_cases), PBIR_mean = mean(pbir))
ggplot(opa_day, aes( x = date, y = cup + 1))  + facet_wrap(~NAB_station) + theme_bw()+ scale_y_log10() + 
  geom_point(color = "black") +
  geom_point(aes(x = date, y = cup_m + 1), color = "red", size = 0.5) 

#str(data_for_model)
#names(opa_day)
data_for_model <- opa_day %>%
  filter(date > ymd("2015 - 10 - 01") & date < ymd("2018 - 01 - 01")) %>% 
  #filter(NAB_station == "San Antonio A") %>% #unique(opa_day$NAB_station)
  #filter(NAB_station != "College Station" & NAB_station != "Waco A" & NAB_station != "Waco B") %>% 
  mutate(
    log_child_pop = log(children_pop),
    child_pop = children_pop,
    log_adult_pop = log(adult_pop),
    log_agegroup_x_pop = log(agegroup_x_pop),
    NAB_station_n = as.numeric(as.factor(NAB_station)),
    met_prcp_flag = ifelse(met_prcpmmday > 0, 1, 0),
    met_prcpmmday_l = log(met_prcpmmday + 1),
    met_prcpmmday_ls = scale(met_prcpmmday_l),
    met_sradWm2_s = scale(met_sradWm2),
    met_tmaxdegc_s = scale(met_tmaxdegc),
    met_tmindegc_s = scale(met_tmindegc),
    met_tavg_s = scale(met_tmaxdegc + met_tmindegc),
    met_vpPa_s = scale(met_vpPa),
    cup_lm = log10(cup_m +1),
    Fagales_lm = log10(Fagales_m + 1),
    Morus_lm = log10(Morus_m + 1),
    Ulmus_lm = log10(Ulmus_m + 1),
    Ambrosia_lm = log10(Ambrosia_m + 1),
    grass_lm = log10(grass_m + 1),
    other_trees_lm = log10(other_trees_m + 1),
    other_pollen_lm = log10(other_pollen_m + 1)
  ) %>%
  dplyr::select(NAB_station, date, n_cases, pbir, child_pop, log_child_pop, adult_pop, log_adult_pop, agegroup_x_pop, log_agegroup_x_pop,
                week_day,
                cup_m, Fagales_m, Morus_m, Ulmus_m, Ambrosia_m, grass_m, other_trees_m, other_pollen_m,
                cup_lm, Fagales_lm, Morus_lm, Ulmus_lm, Ambrosia_lm, grass_lm, other_trees_lm, other_pollen_lm,
                # ja_lm, #ja_rfint_log_mean, ja_l,
                # #cup_other_lm, #cup_other_lms, 
                # cup_all_lm, cup_all_m,
                # trees_lm, trees_m,
                # pol_other_lm, pol_other_m,
                v_tests_pos_Rhinovirus_ms,
                v_tests_pos_RSV_ms,
                v_tests_pos_Corona_ms,
                v_tests_perc_pos_Rhinovirus_m,
                v_tests_perc_pos_RSV_m,
                v_tests_perc_pos_Corona_m,
                v_tests_adj_pos_Rhinovirus_m,
                v_tests_adj_pos_RSV_m,
                v_tests_adj_pos_Corona_m,
                flu_d_prop_pos, 
                flu_d_total_pos #,
                # met_prcpmmday, met_sradWm2, met_tmaxdegc, met_tmindegc, met_vpPa, #hist(data_for_model$met_prcpmmday_ls)
                # met_prcpmmday_l, met_prcpmmday_ls, met_sradWm2_s, met_tmaxdegc_s, met_tmindegc_s, met_vpPa_s, met_tavg_s, met_prcp_flag
  ) %>%
  arrange(NAB_station, date) %>%
  ungroup() %>%  #to avoid an error with filtering the complete cases on the next line: https://stackoverflow.com/questions/59603972/result-must-have-length-error-with-complete-cases 
  filter(complete.cases(.)) %>% 
  #filter(   NAB_station != "Waco B") %>% # NAB_station != "Waco A",   # NAB_station != "College Station") %>% 
  group_by(NAB_station) %>% 
  mutate(time = row_number())

# names(data_for_model) 
# summary(data_for_model)
# ggplot(data_for_model, aes( x = date, y = log10(trees +1))) + geom_point() + facet_wrap(~NAB_station) +
#   geom_point(aes(x = date, y = trees_lm), color = "red", size = 0.5) + scale_y_log10()

#polynomial distributed lag
# ja_lag <- crossbasis(data_for_model$ja_lm, lag = 21,
#                           argvar=list("bs",degree=2,df=3), arglag = list(fun = "poly", degree = 3)) #hist(data_for_model$ja_lm)

#cup_m, Fagales_m, Morus_m, Ulmus_m, Ambrosia_m, grass_m, other_trees_m, other_pollen_m,

max_lag <-14
cup_lag <- crossbasis(data_for_model$cup_lm, lag = max_lag,
                     argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)
Fagales_lag <- crossbasis(data_for_model$Fagales_lm, lag = max_lag,
                          argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)
Morus_lag <- crossbasis(data_for_model$Morus_lm, lag = max_lag,
                        argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)
Ulmus_lag <- crossbasis(data_for_model$Ulmus_lm, lag = max_lag,
                        argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)
Ambrosia_lag <- crossbasis(data_for_model$Ambrosia_lm, lag = max_lag,
                           argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)
grass_lag <- crossbasis(data_for_model$grass_lm, lag = max_lag,
                        argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)
other_trees_lag <- crossbasis(data_for_model$other_trees_lm, lag = max_lag,
                              argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)
other_pollen_lag <- crossbasis(data_for_model$other_pollen_lm, lag = max_lag,
                               argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 2)) #hist(data_for_model$ja_lm) #str(ja_lag)

# 
# trees_lag <- crossbasis(data_for_model$trees_lm, lag = max_lag, 
#                         argvar=list(fun = "poly", degree = 3), arglag = list(fun = "poly", degree = 3))
# 
# pol_other_lag <- crossbasis(data_for_model$pol_other_lm, lag = max_lag,
#                             argvar=list(fun = "poly", degree = 3), arglag = list(fun = "poly", degree = 3))

#glm.nb can be substituted in, but doesn't seem to change much
model1 <- glm.nb(n_cases ~ NAB_station + 
                    offset(log(agegroup_x_pop)) +  #offset(log(child_pop)) + #offset(log(adult_pop))
                    cup_lag +  Fagales_lag + other_pollen_lag +Ulmus_lag + other_trees_lag + grass_lag + Ambrosia_lag + Morus_lag +
                    v_tests_perc_pos_Rhinovirus_m+
                    v_tests_perc_pos_RSV_m+
                    v_tests_perc_pos_Corona_m+
                    flu_d_prop_pos + #flu_d_total_pos + flu_d_prop_pos
                    week_day + 
                    #met_vpPa +
                     # met_prcpmmday_ls +
                     # met_tavg_s +  #met_tmindegc_s + ###met_vpPa +  humidity matters and overwrites temperature and precip
                    #met_tmaxdegc_s +
                    #met_tmindegc_s +
                    #met_sradWm2_s + #
                    #met_prcp_flag +
                    ns(time, 12),
             #family = quasipoisson, 
              data = data_for_model) #quasipoisson #data_for_model$agegroup_x_pop
summary(model1)


# INCLUDE THE 1-DAY LAGGED RESIDUAL IN THE MODEL
resid_model1 <- c(rep(NA, max_lag), residuals(model1, type = "deviance"))
model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1)) 

# hist(model1$fitted.values, n = 100)
# hist(model2$fitted.values, n = 200)
# hist(data_for_model$n_cases, n = 100)

### model selection #############################################
# library("MuMIn")
# 
# #reuses AIC from poisson family estimation
# x.quasipoisson <- function(...){
#   res <- quasipoisson(...)
#   res$aic <- poisson(...)$aic
#   res
# }
# 
# QAIC(update(model2, family = x.quasipoisson), chat = chat)

### model diagnotistic plots
#deviance residuals over time
data_for_model %>%
  ungroup() %>%
  mutate(resid = c(rep(NA, 15), residuals(model2, type = "deviance"))) %>%
  ggplot(aes(x = date, y = resid)) + theme_bw() +
  geom_point() + facet_wrap(~NAB_station) + ylab("Deviance residuals")

#partial autocorrelation plots of the deviance residuals
pacf(residuals(model1, type = "deviance"), na.action=na.omit,main="From original model")
pacf(residuals(model2, type = "deviance"), na.action=na.omit,main="From model adjusted for residual autocorrelation")

summary(model1)
summary(model2)

#cup_m, Fagales_m, Morus_m, Ulmus_m, Ambrosia_m, grass_m, other_trees_m, other_pollen_m,
### new version of fig ####################################################
lag_graph_fun <- function(lag_object){ #lag_object <- cup_lag
  max_pol_conc <- attributes(lag_object)$argvar$scale  #maximum pollen concentration
  pred_1_focal_pol <- crosspred(cup_lag, model2, at = seq(from = 0, to = max_pol_conc, by = 0.10),
                                bylag = 0.2, cen = 0, cumul = TRUE)
  test <- deparse(quote(lag_object))
  # child_lag_RR_x_cup_25km <-
  #   as.data.frame(exp(pred_1_focal_pol$cumfit)) %>% mutate(pol_conc = pred_1_focal_pol$predvar) %>% 
  #   pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  #   mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
  #          pol_conc_exp = 10^pol_conc) %>% 
  #   ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  #   xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ scale_x_log10() +
  #   scale_fill_viridis_d(option = "magma", direction = -1, name = "RR") + #, begin = 0.3, end = 1)  #automatically bins and turns to factor
  #   #scale_fill_brewer(palette = "RdYlBu")
  #   ggtitle(paste0("  n_pop = ", sum(pop_near_NAB_agegroup_x$agegroup_x_pop)))
  return(test)
}

lag_graph_fun(cup_lag)

cup_lag$intercept

str(lag_object)
pred1_cup <- crosspred(cup_lag, model2, at = seq(from = 0, to = max(data_for_model$cup_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 
pred1_cup <- crosspred(Fagales_lag, model2, at = seq(from = 0, to = max(data_for_model$Fagales_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 
pred1_cup <- crosspred(Morus_lag, model2, at = seq(from = 0, to = max(data_for_model$Morus_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 
pred1_cup <- crosspred(Ulmus_lag, model2, at = seq(from = 0, to = max(data_for_model$Ulmus_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 
pred1_cup <- crosspred(Ambrosia_lag, model2, at = seq(from = 0, to = max(data_for_model$Ambrosia_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 
pred1_cup <- crosspred(grass_lag, model2, at = seq(from = 0, to = max(data_for_model$grass_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 
pred1_cup <- crosspred(other_trees_lag, model2, at = seq(from = 0, to = max(data_for_model$other_trees_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 
pred1_cup <- crosspred(other_pollen_lag, model2, at = seq(from = 0, to = max(data_for_model$other_pollen_lm), by = 0.10), 
                       bylag = 0.2, cen = 0, cumul = TRUE) 

as.data.frame(exp(pred1_cup$cumfit)) %>% mutate(pol_conc = pred1_cup$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "magma", direction = -1, name = "RR") + #, begin = 0.3, end = 1)  #automatically bins and turns to factor
  #scale_fill_brewer(palette = "RdYlBu")
  ggtitle(paste0("  n_pop = ", sum(pop_near_NAB_agegroup_x$agegroup_x_pop)))

data.frame(pol_conc = 10^(pred1_cup$predvar), 
           mean = pred1_cup$allRRfit,
           lower = pred1_cup$allRRlow,
           upper = pred1_cup$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() +
  ggtitle(paste0("ages ", age_low, "-", age_hi, "  n_cases = ", sum(data_for_model$n_cases)))



child_lag_RR_x_cup_25km <-
  as.data.frame(exp(pred1_cup$cumfit)) %>% mutate(pol_conc = pred1_cup$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "magma", direction = -1, name = "RR") + #, begin = 0.3, end = 1)  #automatically bins and turns to factor
  #scale_fill_brewer(palette = "RdYlBu")
  ggtitle(paste0("  n_pop = ", sum(pop_near_NAB_agegroup_x$agegroup_x_pop)))


#cup all  
pred1_cup <- crosspred(cup_lag,  model2, #at = 1,
                      at = seq(from = 0, to = max(data_for_model$cup_lm), by = 0.10), 
                      bylag = 0.2, cen = 0, cumul = TRUE) #str(pred1_ja)

# plot(pred1_ja, "overall", ci = "lines", #ylim = c(0.95, 10), lwd = 2,
#      xlab = expression(paste("log10(Cupressaceae pollen grains m"^"3",")")),
#      ylab = "RR", main = "Overall effect of Cupressaceae pollen")
#child_RR_x_cup_25km <- 
  data.frame(pol_conc = 10^(pred1_cup$predvar), 
                     mean = pred1_cup$allRRfit,
                     lower = pred1_cup$allRRlow,
                     upper = pred1_cup$allRRhigh) %>% 
ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() +
  ggtitle(paste0("ages ", age_low, "-", age_hi, "  n_cases = ", sum(data_for_model$n_cases)))
  

# plot.crosspred(pred1_ja, "contour", cumul = TRUE, #trace(plot.crosspred, edit = TRUE)
#      plot.title = title(xlab = expression(paste("log10(Cupressaceae pollen grains m"^"3",")")), 
#                         ylab = "Lag", main = "Cumulative RR across lags for Cupressaceae"), key.title = title("RR"))
child_lag_RR_x_cup_25km <-
  as.data.frame(exp(pred1_cup$cumfit)) %>% mutate(pol_conc = pred1_cup$predvar) %>% 
         pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
         mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
                pol_conc_exp = 10^pol_conc) %>% 
ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "magma", direction = -1, name = "RR") + #, begin = 0.3, end = 1)  #automatically bins and turns to factor
  #scale_fill_brewer(palette = "RdYlBu")
  ggtitle(paste0("  n_pop = ", sum(pop_near_NAB_agegroup_x$agegroup_x_pop)))

## trees #
pred1_trees <- crosspred(trees_lag,  model2, cen = 0, cumul = TRUE,
                         at = seq(from = 0, to = max(data_for_model$trees_lm), by = .10))

# plot(pred1_trees, "overall", ci = "lines", #ylim = c(0.95, 4), lwd = 2,
#      xlab = expression(paste("log10(tree pollen grains m"^"3",")")), 
#      ylab = "RR", main = "Overall effect of tree pollen")
child_RR_x_trees_25km <- 
  data.frame(pol_conc = 10^(pred1_trees$predvar), 
             mean = pred1_trees$allRRfit,
             lower = pred1_trees$allRRlow,
             upper = pred1_trees$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()


# plot.crosspred(pred1_trees, "contour", cumul = TRUE,
#      plot.title = title(xlab = expression(paste("log10(tree pollen grains m"^"3",")")),
#                         ylab = "Lag", main = "Cumulative RR across lags for trees"), key.title = title("RR"))
child_lag_RR_x_trees_25km <-
  as.data.frame(exp(pred1_trees$cumfit)) %>% mutate(pol_conc = pred1_trees$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  xlab(expression(paste("tree pollen (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "viridis", direction = -1, name = "RR") 


# ## pol_other
# pred1_pol_other <- crosspred(pol_other_lag,  model2, cen = 0, cumul = TRUE,
#                          at = seq(from = 0, to = max(data_for_model$pol_other_lm), by = .10))
# child_RR_x_pol_other_25km <- 
#   data.frame(pol_conc = 10^(pred1_pol_other$predvar), 
#              mean = pred1_pol_other$allRRfit,
#              lower = pred1_pol_other$allRRlow,
#              upper = pred1_pol_other$allRRhigh) %>% 
#   ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
#   geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
#   xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()
# 
# child_lag_RR_x_pol_other_25km <-
#   as.data.frame(exp(pred1_pol_other$cumfit)) %>% mutate(pol_conc = pred1_pol_other$predvar) %>% 
#   pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
#   mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
#          pol_conc_exp = 10^pol_conc) %>% 
#   ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
#   xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ scale_x_log10() +
#   scale_fill_viridis_d(option = "viridis", direction = -1, name = "RR") 


#saving the figs 
#child_pol_25km <-
  cowplot::plot_grid(child_RR_x_cup_25km, child_lag_RR_x_cup_25km, child_RR_x_trees_25km, child_lag_RR_x_trees_25km,
                     #child_RR_x_pol_other_25km, child_lag_RR_x_pol_other_25km,
                   ncol = 2, labels = c("A) Cupressaceae pollen", 
                                        "B) Cupressaceae pollen by lag", 
                                        "C) tree pollen", 
                                        "D) tree pollen by lag"),
                                        # "E) other pollen", 
                                        # "F) other pollen by lag"),
                   rel_widths = c(0.8, 1, 0.8, 1),
                   label_size = 11, label_x = 0.14, label_y = 0.9, hjust = 0, vjust = 0)

# ggsave(file = "C:/Users/dsk856/Desktop/thcic_analysis/child_pol_25km_200918.jpg", plot = ts_panels, 
#        height =20, width = 17, units = "cm", dpi = 300)  





### attributable risk ##########################################
#https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata/blob/master/attrdl.R
cup_attr <- attrdl(x = data_for_model$cup_all_lm, basis = cup_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
       cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
  summary(cup_attr)
  quantile(cup_attr, probs = c(0.025, 0.95))
  mean(cup_attr) / sum(model2$fitted.values)
  quantile(cup_attr, probs = c(0.025, 0.95))/sum(model2$fitted.values)

  
trees_attr <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = TRUE,
               cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
summary(trees_attr)
quantile(trees_attr, probs = c(0.025, 0.95))
mean(trees_attr) / sum(model2$fitted.values)
quantile(trees_attr, probs = c(0.025, 0.95))/sum(model2$fitted.values)
mean(trees_attr) / sum(model2$fitted.values)


#no viruses
#No rhinovirus #hist(data_for_model$v_tests_perc_pos_Rhinovirus_ms)
newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_Rhinovirus_ms #str(data_for_model$v_tests_pos_Rhinoviruss)
  #mutate(v_tests_perc_pos_Rhinovirus_m = 0) 
  mutate(v_tests_perc_pos_Rhinovirus_m = min(data_for_model$v_tests_perc_pos_Rhinovirus_m) )
model_pred_no_rhino <- predict.glm(object = model2, newdata = newdata, type = "response")  #str(model_pred)
sum(model2$fitted.values)
sum(model_pred_no_rhino, na.rm = TRUE)
(sum(model2$fitted.values) - sum(model_pred_no_rhino, na.rm = TRUE) ) / sum(model2$fitted.values) 

# #No RSV
# newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_RSV_ms #str(data_for_model$v_tests_pos_RSV)
#   mutate(v_tests_perc_pos_RSV_ms = 0) 
# model_pred <- predict.glm(object = model2, newdata = newdata, type = "response") 
# (sum(model2$fitted.values) - sum(model_pred, na.rm = TRUE) ) / sum(model2$fitted.values) 

#No corona
newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_RSV_ms #str(data_for_model$v_tests_pos_RSV)
  mutate(v_tests_perc_pos_Corona_m = min(data_for_model$v_tests_perc_pos_Corona_m)) 
model_pred_no_corona <- predict.glm(object = model2, newdata = newdata, type = "response") 
sum(model2$fitted.values) 
sum(model_pred_no_corona, na.rm = TRUE)
(sum(model2$fitted.values) - sum(model_pred_no_corona, na.rm = TRUE) ) / sum(model2$fitted.values) 

#No flu
newdata <- data_for_model %>%  #data_for_model$flu_ds #str(data_for_model$v_tests_pos_RSV)
  mutate(flu_d_prop_pos = min(data_for_model$flu_d_prop_pos)) 
model_pred_no_flu <- predict.glm(object = model2, newdata = newdata, type = "response") 
sum(model2$fitted.values) 
sum(model_pred_no_flu, na.rm = TRUE)
(sum(model2$fitted.values) - sum(model_pred_no_flu, na.rm = TRUE) ) / sum(model2$fitted.values) 




# #back of the envelope calculations to see whether these AR estimates are in the right ballpark
# #predicted RR for a 1 unit increase (i.e., 10x since there's a log10 transformation on pollen): ~1.17
# sum((data_for_model$n_cases[22: nrow(data_for_model)])) * mean(data_for_model$trees_lm[22: nrow(data_for_model)]) * 0.17
# sum(data_for_model$n_cases[22: nrow(data_for_model)] * data_for_model$trees_lm[22: nrow(data_for_model)] * 0.17)
# (1.17 - 1)/1.17


### attributable risk over time figure ################################################
attr_t_cup <- attrdl(x = data_for_model$cup_all_lm, basis = cup_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = FALSE,
                   cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_trees <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases, model = model2, dir = "back", sim = FALSE,
                     cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_rhino <- (c(rep(NA, max_lag + 1), model2$fitted.values) - model_pred_no_rhino) #hist(attr_t_cup)
attr_t_corona <- (c(rep(NA, max_lag + 1), model2$fitted.values) - model_pred_no_corona)
attr_t_flu <- (c(rep(NA, max_lag + 1), model2$fitted.values) - model_pred_no_flu)

attr_t_baseline <- c(rep(NA, max_lag + 1), model2$fitted.values) #hist(model2$fitted.values)
attr_df <- data.frame(attr_t_cup, attr_t_trees, attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_baseline)
attr_df[attr_df < 0] <- 0 #removing net protective effects of all variables

# attr_df_p <- attr_df/attr_t_baseline
# summary(attr_df_p)
attr_full_df <- bind_cols(data_for_model, attr_df) %>% 
  mutate(attr_t_unexplained = attr_t_baseline - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>% 
  dplyr::select(date, NAB_station, agegroup_x_pop, 
                attr_t_unexplained, attr_t_cup, attr_t_trees, attr_t_rhino, attr_t_corona, attr_t_flu) %>% 
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
  mutate(week = week(date)) %>% 
  group_by(NAB_station, agegroup_x_pop, week, var) %>% 
  summarize(risk_cases = mean(risk_cases)) %>% 
  mutate(attr_risk_var_unorder = forcats::fct_recode(var, unexplained = "attr_t_unexplained",
                                                     Rhinovirus = "attr_t_rhino", Corona = "attr_t_corona", Influenza = "attr_t_flu",
                                              Cupressaceae = "attr_t_cup", trees = "attr_t_trees"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza", "Cupressaceae", "trees",
                                                                       "unexplained")),
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))
         
observed_ncases_t <- data_for_model %>% 
  dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
  mutate(week = week(date),
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 )) %>% 
  group_by(NAB_station, agegroup_x_pop, week, date2) %>% 
  summarize(mean_cases = mean(n_cases)) %>% 
  mutate(attr_risk_var = "unexplained")

ggplot(attr_full_df, aes(x = date2, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) + 
  facet_wrap(~NAB_station) + geom_area() + theme_bw() + scale_fill_viridis_d(name = "attributable risk") + 
  ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
  scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(0.02, 0.2)) +
  geom_step(data = observed_ncases_t, aes( x = date2, y =(mean_cases / agegroup_x_pop) * 10000, 
                                           color = "observed cases")) + scale_color_discrete(name = "")  
  
  


# # raw version
# attr_full_df <- bind_cols(data_for_model, attr_df) %>% 
#   mutate(attr_t_unexplained = attr_t_baseline - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>% 
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






# names(data_for_model)
# ggplot(data_for_model, aes(x = date, y = cup_all_m + 1)) + geom_point() + facet_wrap(~NAB_station) + theme_bw() +
#   scale_y_log10()








#### ADULTS: analysis with a distributed lags model using dlnm package #######################################
#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_adult_25km_200918.csv", guess_max = 8260)
#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_adult_10km_200918.csv", guess_max = 8260) 
opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_adult_50km_200918.csv", guess_max = 8260) 

opa_day %>% group_by(NAB_station) %>% summarize(total_cases = sum(n_cases), PBIR_mean = mean(pbir))
# ggplot(opa_day, aes( x = date, y = log(trees + 1))) + geom_point() + facet_wrap(~NAB_station) +
#   geom_point(aes(x = date, y = trees_rfint_log_mean), color = "red")

#str(data_for_model)
#names(opa_day)
data_for_model <- opa_day %>%
  filter(date > ymd("2015 - 10 - 01") & date < ymd("2018 - 01 - 01")) %>% 
  #filter(NAB_station == "San Antonio A") %>% #unique(opa_day$NAB_station)
  #filter(NAB_station != "College Station" & NAB_station != "Waco A" & NAB_station != "Waco B") %>% 
  mutate(
    log_child_pop = log(children_pop),
    child_pop = children_pop,
    log_adult_pop = log(adult_pop),
    NAB_station_n = as.numeric(as.factor(NAB_station)),
    met_prcp_flag = ifelse(met_prcpmmday > 0, 1, 0),
    met_prcpmmday_l = log(met_prcpmmday + 1),
    met_prcpmmday_ls = scale(met_prcpmmday_l),
    met_sradWm2_s = scale(met_sradWm2),
    met_tmaxdegc_s = scale(met_tmaxdegc),
    met_tmindegc_s = scale(met_tmindegc),
    met_tavg_s = scale(met_tmaxdegc + met_tmindegc),
    met_vpPa_s = scale(met_vpPa)
  ) %>%
  dplyr::select(NAB_station, date, n_cases, pbir, child_pop, log_child_pop, adult_pop, log_adult_pop,
                week_day,
                ja_lm, #ja_rfint_log_mean, ja_l,
                #cup_other_lm, #cup_other_lms, 
                cup_all_lm, cup_all_m,
                trees_lm, trees_m,
                pol_other_lm, pol_other_m,
                v_tests_pos_Rhinovirus_ms,
                v_tests_pos_RSV_ms,
                v_tests_pos_Corona_ms,
                v_tests_perc_pos_Rhinovirus_m,
                v_tests_perc_pos_RSV_m,
                v_tests_perc_pos_Corona_m,
                v_tests_adj_pos_Rhinovirus_m,
                v_tests_adj_pos_RSV_m,
                v_tests_adj_pos_Corona_m,
                flu_d_prop_pos, 
                flu_d_total_pos,
                met_prcpmmday, met_sradWm2, met_tmaxdegc, met_tmindegc, met_vpPa, #hist(data_for_model$met_prcpmmday_ls)
                met_prcpmmday_l, met_prcpmmday_ls, met_sradWm2_s, met_tmaxdegc_s, met_tmindegc_s, met_vpPa_s, met_tavg_s, met_prcp_flag
  ) %>%
  arrange(NAB_station, date) %>%
  filter(complete.cases(.)) %>% 
  #filter(   NAB_station != "Waco B") %>% # NAB_station != "Waco A",   # NAB_station != "College Station") %>% 
  group_by(NAB_station) %>% 
  mutate(time = row_number())

adult_cup_lag <- crossbasis(data_for_model$cup_all_lm, lag = 14,
                     argvar=list(fun = "poly", degree = 3), arglag = list(fun = "poly", degree = 3)) #hist(data_for_model$ja_lm) #str(ja_lag)

adult_trees_lag <- crossbasis(data_for_model$trees_lm, lag = 14, 
                        argvar=list(fun = "poly", degree = 3), arglag = list(fun = "poly", degree = 3))

adult_pol_other_lag <- crossbasis(data_for_model$pol_other_lm, lag = 14,
                            argvar=list(fun = "poly", degree = 3), arglag = list(fun = "poly", degree = 3))

#glm.nb can be substituted in, but doesn't seem to change much
adult_model1 <- glm.nb(n_cases ~ NAB_station + 
                offset(log(adult_pop)) + #offset(log(adult_pop))
                adult_cup_lag +  adult_trees_lag + adult_pol_other_lag + 
                # v_tests_pos_Rhinovirus_ms+
                # v_tests_pos_RSV_ms+
                # v_tests_pos_Corona_ms+
                # v_tests_adj_pos_Rhinovirus_m+
                # v_tests_adj_pos_RSV_m+
                # v_tests_adj_pos_Corona_m+
                v_tests_perc_pos_Rhinovirus_m+
                v_tests_perc_pos_RSV_m+
                v_tests_perc_pos_Corona_m+
                flu_d_prop_pos +
                week_day + 
                met_vpPa +  
                # met_prcpmmday_ls + 
                # met_tavg_s +  
                # met_tmindegc_s + ###met_vpPa +  humidity matters and overwrites temperature and precip
                # met_tmaxdegc_s + 
                met_sradWm2_s + #
                met_prcp_flag +
                ns(time, 12),
              #family = quasipoisson, 
              data = data_for_model) #quasipoisson
summary(adult_model1)


# INCLUDE THE 1-DAY LAGGED RESIDUAL IN THE MODEL
resid_adult_model1 <- c(rep(NA, 14), residuals(adult_model1, type = "deviance"))
adult_model2 <- update(adult_model1, .~. + tsModel::Lag(resid_adult_model1, 1)) 

# hist(model1$fitted.values, n = 100)
# hist(model2$fitted.values, n = 200)
# hist(data_for_model$n_cases, n = 100)


### model diagnotistic plots
#deviance residuals over time
data_for_model %>% 
  ungroup() %>% 
  mutate(resid = c(rep(NA, 15), residuals(adult_model2, type = "deviance"))) %>% 
  ggplot(aes(x = date, y = resid)) + theme_bw() +
  geom_point() + facet_wrap(~NAB_station) + ylab("Deviance residuals")


#partial autocorrelation plots of the deviance residuals
pacf(residuals(adult_model1, type = "deviance"), na.action=na.omit,main="From original model")
pacf(residuals(adult_model2, type = "deviance"), na.action=na.omit,main="From model adjusted for residual autocorrelation")

summary(adult_model1)
summary(adult_model2)

#Cup
pred1_adult_cup <- crosspred(adult_cup_lag,  adult_model2, #at = 1,
                      at = seq(from = 0, to = max(data_for_model$cup_all_lm), by = 0.10), 
                      bylag = 0.2, cen = 0, cumul = TRUE) #str(pred1_ja)
# plot(pred1_adult_cup, "overall", ci = "lines", #ylim = c(0.95, 10), lwd = 2,
#      xlab = expression(paste("log10(Cupressaceae pollen grains m"^"3",")")),
#      ylab = "RR", main = "Overall effect of Cupressaceae pollen")
# 
# plot.crosspred(pred1_adult_cup, "contour", cumul = TRUE,
#                plot.title = title(xlab = expression(paste("log10(Cupressaceae pollen grains m"^"3",")")), 
#                                   ylab = "Lag", main = "Cumulative RR across lags for Cupressaceae"), key.title = title("RR"))
# 


adult_RR_x_cup_25km <- 
  data.frame(pol_conc = 10^(pred1_adult_cup$predvar), 
             mean = pred1_adult_cup$allRRfit,
             lower = pred1_adult_cup$allRRlow,
             upper = pred1_adult_cup$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()

adult_lag_RR_x_cup_25km <-
  as.data.frame(exp(pred1_adult_cup$cumfit)) %>% mutate(pol_conc = pred1_adult_cup$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "magma", direction = -1, name = "RR") #, begin = 0.3, end = 1) #automatically bins and turns to factor
#scale_fill_brewer(palette = "RdYlBu")




#trees
pred1_adult_trees <- crosspred(adult_trees_lag,  adult_model2, 
                         at = seq(from = 0, to = max(data_for_model$trees_lm), by = .10), 
                         cen = 0, cumul = TRUE)

# plot(pred1_adult_trees, "overall", ci = "lines", #ylim = c(0.95, 4), lwd = 2,
#      xlab = expression(paste("log10(tree pollen grains m"^"3",")")), 
#      ylab = "RR", main = "Overall effect of tree pollen")
# plot.crosspred(pred1_adult_trees, "contour", cumul = TRUE,
#                plot.title = title(xlab = expression(paste("log10(tree pollen grains m"^"3",")")),
#                                   ylab = "Lag", main = "Cumulative RR across lags for trees"), key.title = title("RR"))

adult_RR_x_trees_25km <- 
  data.frame(pol_conc = 10^(pred1_adult_trees$predvar), 
             mean = pred1_adult_trees$allRRfit,
             lower = pred1_adult_trees$allRRlow,
             upper = pred1_adult_trees$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()

adult_lag_RR_x_trees_25km <-
  as.data.frame(exp(pred1_adult_trees$cumfit)) %>% mutate(pol_conc = pred1_adult_trees$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "magma", direction = -1, name = "RR") #, begin = 0.3, end = 1) #automatically bins and turns to factor
#scale_fill_brewer(palette = "RdYlBu")




#other pollen
pred1_adult_pol_other <- crosspred(adult_pol_other_lag,  adult_model2, #at = 1,
                            at = seq(from = 0, to = max(data_for_model$pol_other_lm), by = 0.10), 
                            bylag = 0.2, cen = 0, cumul = TRUE) #str(pred1_ja)

plot(pred1_adult_pol_other, "overall", ci = "lines", #ylim = c(0.95, 10), lwd = 2,
     xlab = expression(paste("log10(other pollen grains m"^"3",")")),
     ylab = "RR", main = "Overall effect of other pollen")

plot.crosspred(pred1_adult_pol_other, "contour", cumul = TRUE,
               plot.title = title(xlab = expression(paste("log10(other pollen grains m"^"3",")")), 
                                  ylab = "Lag", main = "Cumulative RR across lags for other pollen"), key.title = title("RR"))
adult_RR_x_pol_other_25km <- 
  data.frame(pol_conc = 10^(pred1_adult_pol_other$predvar), 
             mean = pred1_adult_pol_other$allRRfit,
             lower = pred1_adult_pol_other$allRRlow,
             upper = pred1_adult_pol_other$allRRhigh) %>% 
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()

adult_lag_RR_x_pol_other_25km <-
  as.data.frame(exp(pred1_adult_pol_other$cumfit)) %>% mutate(pol_conc = pred1_adult_pol_other$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 15) + theme_few() +
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "magma", direction = -1, name = "RR") #, begin = 0.3, end = 1) #automatically bins and turns to factor
#scale_fill_brewer(palette = "RdYlBu")

#saving the figs 
cowplot::plot_grid(adult_RR_x_cup_25km, adult_lag_RR_x_cup_25km, adult_RR_x_trees_25km, adult_lag_RR_x_trees_25km,
                   adult_RR_x_pol_other_25km, adult_lag_RR_x_pol_other_25km,
                   ncol = 2, labels = c("A) Cupressaceae pollen", 
                                        "B) Cupressaceae pollen by lag", 
                                        "C) tree pollen", 
                                        "D) tree pollen by lag",
                                        "E) other pollen",
                                        "F) other pollen by lag)"),
                   rel_widths = c(0.8, 1, 0.8, 1, 0.8, 1),
                   label_size = 11, label_x = 0.14, label_y = 0.9, hjust = 0, vjust = 0)

# ggsave(file = "C:/Users/dsk856/Desktop/thcic_analysis/child_pol_25km_200918.jpg", plot = ts_panels, 
#        height =20, width = 17, units = "cm", dpi = 300)  



### attributable risk ##########################################
#https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata/blob/master/attrdl.R
#cup
adult_cup_attr <- attrdl(x = data_for_model$cup_all_lm, basis = adult_cup_lag, cases = data_for_model$n_cases, model = adult_model2, dir = "back", 
                         sim = TRUE, cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
summary(adult_cup_attr)
quantile(adult_cup_attr, probs = c(0.025, 0.95))
mean(adult_cup_attr) / sum(adult_model2$fitted.values)
quantile(adult_cup_attr, probs = c(0.025, 0.95))/sum(adult_model2$fitted.values)

#trees
adult_trees_attr <- attrdl(x = data_for_model$trees_lm, basis = adult_trees_lag, cases = data_for_model$n_cases, model = adult_model2, dir = "back", sim = TRUE,
                     cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
summary(adult_trees_attr)
quantile(adult_trees_attr, probs = c(0.025, 0.95))
mean(adult_trees_attr) / sum(adult_model2$fitted.values)
quantile(adult_trees_attr, probs = c(0.025, 0.95))/sum(adult_model2$fitted.values)
mean(adult_trees_attr) / sum(adult_model2$fitted.values)

#pol_other
adult_pol_other_attr <- attrdl(x = data_for_model$pol_other_lm, basis = adult_pol_other_lag, cases = data_for_model$n_cases, 
                               model = adult_model2, dir = "back", sim = TRUE,cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
summary(adult_pol_other_attr)
quantile(adult_pol_other_attr, probs = c(0.025, 0.95))
mean(adult_pol_other_attr) / sum(adult_model2$fitted.values)
quantile(adult_pol_other_attr, probs = c(0.025, 0.95))/sum(adult_model2$fitted.values)
mean(adult_pol_other_attr) / sum(adult_model2$fitted.values)


#no viruses
#No rhinovirus #hist(data_for_model$v_tests_perc_pos_Rhinovirus_ms)
newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_Rhinovirus_ms #str(data_for_model$v_tests_pos_Rhinoviruss)
  #mutate(v_tests_perc_pos_Rhinovirus_m = 0) 
  mutate(v_tests_perc_pos_Rhinovirus_m = min(data_for_model$v_tests_perc_pos_Rhinovirus_m) )
adult_model_pred_no_rhino <- predict.glm(object = adult_model2, newdata = newdata, type = "response")  #str(model_pred)
sum(adult_model2$fitted.values)
sum(adult_model_pred_no_rhino, na.rm = TRUE)
(sum(adult_model2$fitted.values) - sum(adult_model_pred_no_rhino, na.rm = TRUE) ) / sum(adult_model2$fitted.values) 

#No RSV
newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_Rhinovirus_ms #str(data_for_model$v_tests_pos_Rhinoviruss)
  mutate(v_tests_perc_pos_RSV_m = min(data_for_model$v_tests_perc_pos_RSV_m) )
adult_model_pred_no_RSV <- predict.glm(object = adult_model2, newdata = newdata, type = "response")  #str(model_pred)
sum(adult_model2$fitted.values)
sum(adult_model_pred_no_RSV, na.rm = TRUE)
(sum(adult_model2$fitted.values) - sum(adult_model_pred_no_RSV, na.rm = TRUE) ) / sum(adult_model2$fitted.values) 

#No corona
newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_RSV_ms #str(data_for_model$v_tests_pos_RSV)
  mutate(v_tests_perc_pos_Corona_m = min(data_for_model$v_tests_perc_pos_Corona_m)) 
adult_model_pred_no_corona <- predict.glm(object = adult_model2, newdata = newdata, type = "response") 
sum(adult_model2$fitted.values) 
sum(adult_model_pred_no_corona, na.rm = TRUE)
(sum(adult_model2$fitted.values) - sum(adult_model_pred_no_corona, na.rm = TRUE) ) / sum(adult_model2$fitted.values) 

#No flu
newdata <- data_for_model %>%  #data_for_model$flu_ds #str(data_for_model$v_tests_pos_RSV)
  mutate(flu_d_prop_pos = min(data_for_model$flu_d_prop_pos)) 
adult_model_pred_no_flu <- predict.glm(object = adult_model2, newdata = newdata, type = "response") 
sum(adult_model2$fitted.values) 
sum(adult_model_pred_no_flu, na.rm = TRUE)
(sum(adult_model2$fitted.values) - sum(adult_model_pred_no_flu, na.rm = TRUE) ) / sum(adult_model2$fitted.values) 





### Adult attributable risk over time figure ################################################
adult_attr_t_cup <- attrdl(x = data_for_model$cup_all_lm, basis = adult_cup_lag, cases = data_for_model$n_cases, model = adult_model2, 
                           dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

adult_attr_t_trees <- attrdl(x = data_for_model$trees_lm, basis = adult_trees_lag, cases = data_for_model$n_cases, model = adult_model2, 
                             dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

adult_attr_t_rhino <- (c(rep(NA, 15), adult_model2$fitted.values) - adult_model_pred_no_rhino)
adult_attr_t_corona <- (c(rep(NA, 15), adult_model2$fitted.values) - adult_model_pred_no_corona)
adult_attr_t_flu <- (c(rep(NA, 15), adult_model2$fitted.values) - adult_model_pred_no_flu)

adult_attr_t_baseline <- c(rep(NA, 15), adult_model2$fitted.values)
adult_attr_df <- data.frame(adult_attr_t_cup, adult_attr_t_trees, adult_attr_t_rhino, adult_attr_t_corona, adult_attr_t_flu, adult_attr_t_baseline)

adult_attr_df_p <- adult_attr_df/adult_attr_t_baseline
summary(adult_attr_df_p)
adult_attr_full_df <- bind_cols(data_for_model, adult_attr_df) %>% 
  dplyr::select(date, NAB_station, adult_pop, adult_attr_t_cup, adult_attr_t_trees, adult_attr_t_rhino, 
                adult_attr_t_corona, adult_attr_t_flu) %>% 
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "prop_risk") %>% 
  mutate(week = week(date)) %>% 
  group_by(NAB_station, adult_pop, week, var) %>% 
  summarize(prop_risk = mean(prop_risk)) %>% 
  mutate(attr_risk_var_unorder = forcats::fct_recode(var, Rhinovirus = "adult_attr_t_rhino", Corona = "adult_attr_t_corona", 
                                                     Influenza = "adult_attr_t_flu",
                                                     Cupressaceae = "adult_attr_t_cup", trees = "adult_attr_t_trees"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza", "Cupressaceae", "trees")),
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))

### LEAVING OFF HERE
#not including the minor protective effects of low pollen
bind_cols(data_for_model, adult_attr_df) %>% 
  dplyr::select(date, NAB_station, adult_pop, adult_attr_t_cup, adult_attr_t_trees, adult_attr_t_rhino, 
                adult_attr_t_corona, adult_attr_t_flu) %>% 
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "prop_risk") %>% 
  mutate(week = week(date)) %>% 
  group_by(NAB_station, adult_pop, week, var) %>% 
  #filter(prop_risk > 0) %>% 
  ungroup() %>% 
  summarize(prop_risk = mean(prop_risk)) %>% 
  mutate(attr_risk_var_unorder = forcats::fct_recode(var, Rhinovirus = "adult_attr_t_rhino", Corona = "adult_attr_t_corona", 
                                                     Influenza = "adult_attr_t_flu",
                                                     Cupressaceae = "adult_attr_t_cup", trees = "adult_attr_t_trees"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza", "Cupressaceae", "trees")),
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 )) %>% filter(prop_risk > 0) %>%  group_by(attr_risk_var) %>% summarize(sum_risk = sum(prop_risk))
# names(attr_full_df)
# ggplot(attr_full_df, aes(x = week, y = (prop_risk / child_pop) * 10000, col = var)) + facet_wrap(~NAB_station) + geom_line() + theme_bw()

ggplot(adult_attr_full_df, aes(x = date2, y = (prop_risk /adult_pop) * 10000, fill = attr_risk_var)) + 
  facet_wrap(~NAB_station) + geom_area() + theme_bw() + scale_fill_viridis_d(name = "risk variable") + 
  ylab("asthma-related ED visits (per 10,000 adults per day)") + xlab("date") +
  scale_x_date(labels = date_format("%b")) #+ coord_cartesian(ylim = c(0.02, 0.5))












#######################################################################################################
########################old stuff ##############################
#######################################################################################################
str(model_pred)

trees_lag <- crossbasis(data_for_model$trees_lm, lag = 21, 
                        argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 3))


model1 <- glm(n_cases ~ NAB_station + 
                offset(log(child_pop)) + 
                cup_all_lag +  trees_lag + pol_other_lag + 
                v_tests_pos_Rhinoviruss + v_tests_pos_RSVs + v_tests_pos_Coronas + flu_ds + 
                week_day + 
                #met_vpPa_s + #humidity matters and overwrites temperature and precip
                ns(time, 12),
              family = quasipoisson, data = data_for_model)

##
# #what curves do different numbers of knots provide?
ggplot(data_for_model, aes(date, n_cases)) + #, col = cup_all_ls
  #scale_color_viridis_c()+
  geom_point(alpha = 0.2, size = 3) +
  geom_smooth(method = lm, formula = y ~ splines::ns(x, 30), se = FALSE, lwd = 1) + 
  #geom_line(aes(x = date, y=rollmean(cup_all_ls, 7, na.pad=TRUE)), color = "black") +
  theme_bw() + facet_wrap(~NAB_station, scales = "free")

data_for_model %>% 
  filter(date > ymd("2015 - 12 - 01") & date < ymd("2016 - 02 - 01")) %>%
  ggplot(aes(date, n_cases_s)) +
  scale_color_viridis_c()+
  geom_line(aes(x = date, y=rollmean(n_cases_s, 1, na.pad=TRUE)), color = "red") +
  geom_line(aes(x = date, y=rollmean(cup_all_ls, 1, na.pad=TRUE)), color = "black") +
  # geom_line(aes(x = date, y=rollmean(not_cup_ls, 7, na.pad=TRUE)), color = "blue") +
  theme_bw() + facet_wrap(~NAB_station, scales = "free") +
  ylab("standardized variable")

ccf_df <- data_for_model %>% filter(NAB_station == "San Antonio A") %>% 
  mutate(n_cases_s_ts = ts(n_cases_s),
         cup_all_ls_ts = ts(cup_all_ls))

ccf(ccf_df$n_cases_s_ts, ccf_df$cup_all_ls_ts, lag.max = 30)
?ccf

panel_a <- data_for_model %>%
  ggplot(aes(x = date, y = n_cases, color = n_cases)) + theme_few() +  
  geom_point(alpha = 0.8)  +  ylab("number of asthma ED visits") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + scale_color_viridis_c(name = "n cases") +
  geom_line(aes(x = date, y=rollmean(n_cases, 7, na.pad=TRUE)), color = "black")
panel_b <- data_for_model %>% 
  ggplot(aes(x = date, y = cup_all_l, color = n_cases)) + theme_few() +  #scale_x_log10() + 
  geom_point(alpha = 0.8, size = 2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + scale_color_viridis_c(name = "n cases") +
  geom_line(aes(x = date, y=rollmean(cup_all_l, 7, na.pad=TRUE)), color = "black")
panel_d <- data_for_model %>% 
  ggplot(aes(x = date, y = trees_lm, color = n_cases)) + theme_few() +  #scale_x_log10() + 
  geom_point(alpha = 0.8, size = 2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + scale_color_viridis_c(name = "n cases") +
  geom_line(aes(x = date, y=rollmean(trees_lm, 7, na.pad=TRUE)), color = "black")
panel_e <- data_for_model %>% 
  ggplot(aes(x = date, y = pol_other_lm, color = n_cases)) + theme_few() +  #scale_x_log10() + 
  geom_point(alpha = 0.8, size = 2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + scale_color_viridis_c(name = "n cases") +
  geom_line(aes(x = date, y=rollmean(pol_other_lm, 7, na.pad=TRUE)), color = "black")

cowplot::plot_grid(panel_a, panel_b, #panel_c, 
                   panel_d, nrow = 3,
                   
                   labels = c("ED visits", "all Cupressaceae", "trees"))#, "other pollen"))

cup_all_lag <- crossbasis(data_for_model$ja_lm, lag = 21,
                          argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 3))

data_for_model_pred <- data_for_model %>% mutate()
predict.glm(model1, )


# 
# ggplot(data_for_model, aes(date, n_cases, col = v_tests_pos_Coronas)) +
#   scale_color_viridis_c()+
#   geom_point(alpha = 0.9, size = 3) +
#   geom_smooth(method = lm, formula = y ~ splines::bs(x, 50), se = FALSE) + theme_bw() + facet_wrap(~NAB_station, scales = "free")
# 



# # #splines instead of a polynomial
# varknots <- equalknots(data_for_model$ja_lms, fun="bs",df=6,degree=3)
# lagknots <- logknots(c(0,lag_length), 3)
# ja_lag <- crossbasis(data_for_model$ja_lms, lag= lag_length, argvar=list(fun="bs",  knots=varknots), arglag=list(knots=lagknots))
#argvar = list(fun = "ns"), arglag = list())
#argvar=list(fun = "thr", side = "d"))


#### analysis of a single station with a Bayesian distributed lags model #######################################
#assemble Williamson County data
data_for_model <- opa_day %>%
  filter(NAB_station == "Georgetown") %>% # == "Harris") %>% !is.na(county_name)) 
  mutate(NAB_station_n = as.numeric(as.factor(NAB_station)),
         v_tests_pos_Rhinoviruss = scale(v_tests_pos_Rhinovirus),
         v_tests_pos_RSVs = scale(v_tests_pos_RSV),
         v_tests_pos_Coronas = scale(v_tests_pos_Corona),
         flu_ds = scale(flu_d),
         cups = scale(log10(cupr + 1)),
         tot_pols = scale(log10(tot_pol - cupr + 1))) %>%
  dplyr::select(date, n_cases, 
                week_day,
                tot_pol, 
                cupr, 
                v_tests_pos_Rhinovirus,
                v_tests_pos_RSV,
                v_tests_pos_Corona,
                flu_d, NAB_station,
                v_tests_pos_Rhinoviruss, v_tests_pos_RSVs, v_tests_pos_Coronas, flu_ds, cups, tot_pols) %>%
  arrange(date) %>%
  filter(complete.cases(.)) 



#hist(data_for_model$cupr)
sink("model_b.txt")
cat("  
  ### model #
    model{
    ## Likelihood
    for(i in 22:N){ #excluding the first few observations because they don't currently have pollen data
      y[i] ~ dpois(lambda[i])
      log(lambda[i]) <- beta0 + 
      lag_cup[i] +      #cumulative distributed lags for Cupressaceae
      #lag_tot_pol[i] + #cumulative distributed lags for Cupressaceae

      alpha[1] * v_tests_pos_Rhinovirus[i] + 
      alpha[2] * v_tests_pos_RSV[i] + 
      alpha[3] * v_tests_pos_Corona[i] +
      
      kappa[1] * flu_d[i]
      
      #distributed lags for Cupressaceae
       for(t in 1: NLag_Cup){
        lagS_cup[i,t] <- cup[i - (t - 1)] *
                            theta_temp[t]
      }

      lag_cup[i] <- sum(lagS_cup[i,])
    }     
      
    ## Priors 
    beta0 ~ dnorm(0, 0.0001) 
    # for(b in 1:4){beta[b] ~ dnorm(0, 0.0001)}
    for(a in 1:3){alpha[a] ~ dnorm(0, 0.0001)}
    kappa ~ dnorm(0, 0.0001)
    lag_test ~ dnorm(0, 0.0001)
    
    ## Priors for cup lag
        X2_temp ~ dnorm(0, 1)

      for(t in 1:NLag_Cup){
        X1_temp[t] ~ dnorm(0,1)
        l.dist_temp[t] <- max(t, pi_temp)
        l.weight_temp[t] <- exp(eta2_temp * l.dist_temp[t])
        l.var_temp[t] <- exp(eta1_temp * •[t] / 2) * sigma_temp[ind_temp]
        theta.prime_temp[t] <- l.weight_temp[t] * X1_temp[t] + (1 - l.weight_temp[t]) * X2_temp
        theta_temp[t] <- theta.prime_temp[t] * l.var_temp[t]
      }

      pi_temp ~ dunif(0, NLag_Cup)
      eta1_temp ~ dunif(-1,-0.00001)
      eta2_temp ~ dunif(-1, -0.00001)
      sigma_temp[2] ~ dunif(0.0001, 100)
      sigma_temp[1] <- 0
      ind_temp <- sel_temp + 1
      sel_temp ~ dbern(0.5)
   
}
    ",fill=TRUE)
sink() 

jags <- jags.model('model_b.txt', 
                   data = list(
                     #general stuff
                     y = as.numeric(data_for_model$n_cases),  # DV
                     N = nrow(data_for_model),  # sample size
                     NLag_Cup = 7,
                     
                     #NAB data 
                     cup = as.numeric(data_for_model$tot_pols),
                     # tot_pol_log = log(data_for_model$tot_pol + 1),  # predictors
                     # tot_pol_l1_log = log(data_for_model$tot_pol_l1 + 1),  
                     # tot_pol_l2_log = log(data_for_model$tot_pol_l2 + 1),  # predictors
                     # tot_pol_l3_log = log(data_for_model$tot_pol_l3 + 1),  # predictors
                     # tot_pol_m7_log = log(data_for_model$tot_pol_m7 + 1),  # predictors
                     # tot_pol_m14_log = log(data_for_model$tot_pol_m14 + 1),  # predictors
                     
                     #virus data
                     v_tests_pos_Rhinovirus = as.numeric(scale(data_for_model$v_tests_pos_Rhinovirus)),
                     v_tests_pos_RSV = as.numeric(scale(data_for_model$v_tests_pos_RSV)) ,
                     v_tests_pos_Corona = as.numeric(scale(data_for_model$v_tests_pos_Corona)) ,
                     flu_d = as.numeric(scale(data_for_model$flu_d))
                     
                   ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter= 100) #update(jags,n.iter=1000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("alpha", "theta_temp", "kappa"),  n.iter = 3000, thin = 3) #variables to monitor
mcmc_samples_params2 <- coda.samples(jags, variable.names=c("theta.prime_temp", "l.var_temp"),  n.iter = 100, thin = 10) #variables to monitor
plot(mcmc_samples_params2)
results_param <- summary(mcmc_samples_params)
results_params <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params$parameter<-row.names(results_params)
results_params$param<-substr(results_params$parameter,1,2)



# ### Google Trends, pollen seasons, and other unused data ###########################################################################
# ### daily google searches for "pollen" from Dallas, San Antonio, and Houston:
# g_pollen_Dallas <- read.csv("C:/Users/dsk856/Desktop/misc_data/google_trends_Dallas_FtWorth_pollen_daily_2004_2019.csv")  %>% mutate(MSA = "Dallas-Fort Worth-Arlington")
# g_pollen_Houston <- read.csv("C:/Users/dsk856/Desktop/misc_data/google_trends_Houston_pollen_daily_2004_2019.csv") %>% mutate(MSA = "Houston-The Woodlands-Sugar Land")
# g_pollen_SanAntonio <- read.csv("C:/Users/dsk856/Desktop/misc_data/google_trends_SanAntonio_pollen_daily_2004_2019.csv") %>% mutate(MSA = "San Antonio-New Braunfels")
# g_pollen_Austin <- read.csv("C:/Users/dsk856/Desktop/misc_data/google_trends_Austin_pollen_daily_2004_2019.csv") %>% mutate(MSA = "Austin-Round Rock")
# 
# g_pollen <- bind_rows(g_pollen_Dallas, g_pollen_Houston, g_pollen_SanAntonio, g_pollen_Austin)
# g_pollen_join <- mutate(g_pollen, date = ymd(date),
#                         hits_day_adj_pollen = hits_day_adj) %>% select(date, hits_day_adj_pollen, MSA)
# opa_day <-   left_join(opa_day, g_pollen_join, by =c("date", "MSA")) 
# 
# ## more google trends data for "colds"
# #unique(opa_day$MSA)
# cold_dallas <- read_csv("C:/Users/dsk856/Desktop/misc_data/google_trends_Dallas_cold_daily_2004_2019.csv")
# cold_dallas <- dplyr::select(cold_dallas, date, gtrend_cold = hits_day_adj) %>%
#                 mutate(MSA = "Dallas-Fort Worth-Arlington")
# 
# common_cold_dallas <- read_csv("C:/Users/dsk856/Desktop/misc_data/google_trends_Dallas_common cold_daily_2004_2019.csv")
# common_cold_dallas <- dplyr::select(common_cold_dallas, date, gtrend_common_cold = hits_day_adj) %>%
#   mutate(MSA = "Dallas-Fort Worth-Arlington")
# 
# cold_sanant <- read_csv("C:/Users/dsk856/Desktop/misc_data/google_trends_SanAntoniocold_daily_2004_2019.csv")
# cold_sanant <- dplyr::select(cold_sanant, date, gtrend_cold = hits_day_adj) %>%
#   mutate(MSA = "San Antonio-New Braunfels")
# 
# common_cold_sanant <- read_csv("C:/Users/dsk856/Desktop/misc_data/google_trends_SanAntoniocommon cold_daily_2004_2019.csv")
# common_cold_sanant <- dplyr::select(common_cold_sanant, date, gtrend_common_cold = hits_day_adj) %>%
#   mutate(MSA = "San Antonio-New Braunfels")
# 
# gtrends_cold <- bind_rows(cold_dallas, cold_sanant)
# gtrends_common_cold <- bind_rows(common_cold_dallas, common_cold_sanant)
# 
# 
# opa_day <- left_join(opa_day, gtrends_cold)
# opa_day <- left_join(opa_day, gtrends_common_cold)
#
# #load in daily rhinovirus data
# colds <- read.csv("C:/Users/dsk856/Desktop/misc_data/rhino_daily_from_Eggo_191111.csv")
# colds <- mutate(colds, date = ymd(date_vector)) %>% select(date, rhino_child, rhino_adult)
# opa_day <- left_join(opa_day, colds)
#   
# #define pollen seasons based on general airborne pollen trends from NAB; see graph produced from "TX_NAB_googletrends190917.R"
# opa_day$p_season <- "none"
# opa_day$p_season[opa_day$doy > 349 | opa_day$doy < 31] <- "J. ashei"
# opa_day$p_season[opa_day$doy > 85 & opa_day$doy < 115] <- "spring trees"
# opa_day$p_season[opa_day$doy > 250 & opa_day$doy < 292] <- "ragweed"
# 
# 
# #define pollen seasons based on google searches for 'pollen' 
# opa_day$g_season <- "none"
# #Juas
# opa_day$g_season[opa_day$date > ymd("2015-12-11") & opa_day$date < ymd("2016-02-05")] <- "J. ashei"
# opa_day$g_season[opa_day$date > ymd("2016-12-25") & opa_day$date < ymd("2017-01-25")] <- "J. ashei"
# #didn't catch it at the end of Dec 2017
# 
# #spring trees
# opa_day$g_season[opa_day$date > ymd("2016-03-01") & opa_day$date < ymd("2016-04-30")] <- "spring trees"
# opa_day$g_season[opa_day$date > ymd("2017-03-01") & opa_day$date < ymd("2017-04-30")] <- "spring trees"
# 
# #ragweed
# opa_day$g_season[opa_day$date > ymd("2015-09-10") & opa_day$date < ymd("2015-11-01")] <- "ragweed"
# opa_day$g_season[opa_day$date > ymd("2016-09-15") & opa_day$date < ymd("2016-10-25")] <- "ragweed"
# opa_day$g_season[opa_day$date > ymd("2017-09-10") & opa_day$date < ymd("2017-11-01")] <- "ragweed"
# 
# 
# #define flu season based on CDC data in Texas
# #install.packages("cdcdfluview")
# #manually snagging general flu season definitions from TX for right now.  2017 is way higher than 2016 for flu.  Not sure how well this represents Austin
# opa_day$flu <- "none"
# opa_day$flu[opa_day$date > ymd("2016-02-14") & opa_day$date < ymd("2016-04-24")] <- "flu"
# opa_day$flu[opa_day$date > ymd("2017-01-07") & opa_day$date < ymd("2017-04-01")] <- "flu"
# 
# #define cold season based on Ego et al. 2016 paper, fig. 2.  Cut off dates are just eye-balled
# #based on Eggo et al. 2016, just choosing semi-arbitrary cut off date based on Fig 2: Aug 25 - Sep 20
# opa_day$cold <- "none"
# opa_day$cold[opa_day$doy > 237 & opa_day$doy < 263] <- "cold"



### analysis of a single NAB station ##############################################################
#assemble Williamson County data
names(opa_day)
unique(opa_day$NAB_station)
data_for_model <- opa_day %>%
  filter(NAB_station == "San Antonio A") %>% # == "Harris") %>% !is.na(county_name)) 
  #filter(p_season == "J. ashei") %>%
  dplyr::select(date, n_cases, 
                week_day,
                tot_pol, tot_pol_l1, tot_pol_l2, tot_pol_l3, tot_pol_m7, tot_pol_m14,
                #cupr, cupr_l1, cupr_l2, cupr_l3, cupr_m7, cupr_m14, 
                v_tests_pos_Rhinovirus,
                v_tests_pos_RSV,
                v_tests_pos_Corona,
                flu_d, flu_d_m7, flu_d_m14, NAB_station) %>%
  arrange(date) %>%
  mutate(NAB_station_n = as.numeric(as.factor(NAB_station)))  %>%
  filter(complete.cases(.))
data_for_model <- data_for_model %>% mutate(obs_n = 1:nrow(data_for_model))


# ggplot(data_for_model, aes(x= obs_n, y = v_tests_pos_Rhinovirus)) + theme_bw()+  #log(cupr_l1 + 1)
#   geom_point() + geom_line() + facet_wrap(~NAB_station) + 
#   geom_point(aes(x=obs_n, y =n_cases), color = "red") +
#   geom_line(aes(x=obs_n, y =n_cases), color = "red")
# 
# ggplot(data_for_model, aes(x= log(cupr + 1), y = n_cases)) + theme_bw()+
#   geom_point() + geom_smooth(method = "lm")
# 

#hist(data_for_model$cupr)
sink("model_a.txt")
cat("  
  ### model #
    model{
    ## Likelihood
    for(i in 1:N){
      y[i] ~ dpois(lambda[i])
      log(lambda[i]) <- beta0 + 
      beta[1] * tot_pol_log[i] + 
      beta[2] * tot_pol_l1_log[i] + 
      beta[3] * tot_pol_m7_log[i] + 
      beta[4] * tot_pol_m14_log[i] +
      
      alpha[1] * v_tests_pos_Rhinovirus[i] + 
      alpha[2] * v_tests_pos_RSV[i] + 
      alpha[3] * v_tests_pos_Corona[i] +
      
      kappa[1] * flu_d[i]
    }     
      
    ## Priors 
    beta0 ~ dnorm(0, 0.0001) 
    for(b in 1:4){beta[b] ~ dnorm(0, 0.0001)}
    for(a in 1:3){alpha[a] ~ dnorm(0, 0.0001)}
    kappa ~ dnorm(0, 0.0001)
}
    ",fill=TRUE)
sink() 

jags <- jags.model('model_a.txt', 
                   data = list(
                     #general stuff
                     y = as.numeric(data_for_model$n_cases),  # DV
                     N = nrow(data_for_model),  # sample size
                     
                     #NAB data 
                     tot_pol_log = log(data_for_model$tot_pol + 1),  # predictors
                     tot_pol_l1_log = log(data_for_model$tot_pol_l1 + 1),  
                     tot_pol_l2_log = log(data_for_model$tot_pol_l2 + 1),  # predictors
                     tot_pol_l3_log = log(data_for_model$tot_pol_l3 + 1),  # predictors
                     tot_pol_m7_log = log(data_for_model$tot_pol_m7 + 1),  # predictors
                     tot_pol_m14_log = log(data_for_model$tot_pol_m14 + 1),  # predictors
                     
                     #virus data
                     v_tests_pos_Rhinovirus = as.numeric(data_for_model$v_tests_pos_Rhinovirus),
                     v_tests_pos_RSV = as.numeric(data_for_model$v_tests_pos_RSV) ,
                     v_tests_pos_Corona = as.numeric(data_for_model$v_tests_pos_Corona) ,
                     flu_d = as.numeric(data_for_model$flu_d)
                     
                   ),
                   n.chains = 3,
                   n.adapt = 1000)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter= 1000) #update(jags,n.iter=1000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("alpha", "beta", "kappa"),  n.iter = 1000, thin = 10) #variables to monitor
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params$parameter<-row.names(results_params)
results_params$param<-substr(results_params$parameter,1,2)


### analysis of all NAB stations ##############################################################
#assemble Williamson County data
names(opa_day)
unique(opa_day$NAB_station)
data_for_model <- opa_day %>%
  ungroup()%>%
  #filter(NAB_station == "San Antonio A") %>% # == "Harris") %>% !is.na(county_name)) 
  #filter(p_season == "J. ashei") %>%
  dplyr::select(date, n_cases, 
                children_pop,
                week_day,
                tot_pol, tot_pol_l1, tot_pol_l2, tot_pol_l3, tot_pol_m7, tot_pol_m14,
                #cupr, cupr_l1, cupr_l2, cupr_l3, cupr_m7, cupr_m14, 
                v_tests_pos_Rhinovirus,
                v_tests_pos_RSV,
                v_tests_pos_Corona,
                flu_d, flu_d_m7, flu_d_m14, NAB_station) %>%
  arrange(date) %>%
  mutate(NAB_station_n = as.numeric(as.factor(NAB_station)),
         day_week_n = as.numeric(as.factor(week_day)))  %>%
  filter(complete.cases(.))#na.omit() #
data_for_model <- data_for_model %>% mutate(obs_n = 1:nrow(data_for_model))


# ggplot(data_for_model, aes(x= obs_n, y = v_tests_pos_Rhinovirus)) + theme_bw()+  #log(cupr_l1 + 1)
#   geom_point() + geom_line() + facet_wrap(~NAB_station) + 
#   geom_point(aes(x=obs_n, y =n_cases), color = "red") +
#   geom_line(aes(x=obs_n, y =n_cases), color = "red")
# 
# ggplot(data_for_model, aes(x= log(cupr + 1), y = n_cases)) + theme_bw()+
#   geom_point() + geom_smooth(method = "lm")
# 

#hist(data_for_model$cupr)
sink("model_a.txt")
cat("  
  ### model ########### 
    model{
    ## Likelihood
    for(i in 1:N){
      y[i] ~ dpois(lambda2[i])
      lambda2[i] <- lambda1[i] * children_pop[i]
      
      log(lambda1[i]) <- 
      beta[1] * tot_pol_log[i] + 
      beta[2] * tot_pol_l1_log[i] + 
      beta[3] * tot_pol_m7_log[i] + 
      beta[4] * tot_pol_m14_log[i] +
      
      alpha[1] * v_tests_pos_Rhinovirus[i] + 
      alpha[2] * v_tests_pos_RSV[i] + 
      alpha[3] * v_tests_pos_Corona[i] +
      alpha[4] * flu_d[i] +
      
      #kappa[1] * flu_d[i] +
      
      dayweek_effect[dayweek[i]]+
      city_fe[station[i]]
    }     
      
    ## Priors 
    
    for(b in 1:4){beta[b] ~ dnorm(0, 0.0001)}
    for(a in 1:4){alpha[a] ~ dnorm(0, 0.0001)}
    for(d in 1:7){dayweek_effect[d] ~ dnorm(0, 0.0001)}
    kappa ~ dnorm(0, 0.0001)
    for(c in 1:nstations){city_fe[c] ~ dnorm(0, 0.0001)}
}
    ",fill=TRUE)
sink() 

jags <- jags.model('model_a.txt', 
                   data = list(
                     #general stuff
                     y = as.numeric(data_for_model$n_cases),  # DV
                     N = nrow(data_for_model),  # sample size
                     nstations = max(data_for_model$NAB_station_n),
                     station = data_for_model$NAB_station_n,
                     children_pop = data_for_model$children_pop,
                     dayweek = data_for_model$day_week_n,
                     
                     #NAB data 
                     tot_pol_log = log(data_for_model$tot_pol + 1),  # predictors
                     tot_pol_l1_log = log(data_for_model$tot_pol_l1 + 1),  
                     tot_pol_l2_log = log(data_for_model$tot_pol_l2 + 1),  # predictors
                     tot_pol_l3_log = log(data_for_model$tot_pol_l3 + 1),  # predictors
                     tot_pol_m7_log = log(data_for_model$tot_pol_m7 + 1),  # predictors
                     tot_pol_m14_log = log(data_for_model$tot_pol_m14 + 1),  # predictors
                     
                     #virus data
                     v_tests_pos_Rhinovirus = as.numeric(data_for_model$v_tests_pos_Rhinovirus),
                     v_tests_pos_RSV = as.numeric(data_for_model$v_tests_pos_RSV) ,
                     v_tests_pos_Corona = as.numeric(data_for_model$v_tests_pos_Corona) ,
                     flu_d = as.numeric(data_for_model$flu_d)
                     
                   ),
                   n.chains = 3,
                   n.adapt = 1000)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter= 1000) #update(jags,n.iter=1000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("alpha", "beta", "kappa", "city_fe"),  n.iter = 10000, thin = 10) #variables to monitor
plot(mcmc_samples_params)
Sys.time() #started at noon
results_param <- summary(mcmc_samples_params)
results_params <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params$parameter<-row.names(results_params)
results_params$param<-substr(results_params$parameter,1,2)

filter(results_params, param == "al" | param == "be") %>%
  ggplot(aes(x = parameter, y = Mean, ymax = X97.5., ymin = X2.5.)) + geom_pointrange() + theme_bw() +
  geom_abline(slope = 0, intercept = 0, lty = 2) +
  ylab("parameter estimate")


jags <- jags.model('model_a.txt',
                   data = list(
                     'nobs' = nrow(samples_pred),
                     'ragp' = as.numeric(samples_pred$ragp),
                     'year18'= as.numeric(samples_pred$year18),
                     
                     #pixel level variables
                     'class_29_4m_c' = as.numeric(samples_pred$class_29_4m_c),
                     'class_32_4m_c' = as.numeric(samples_pred$class_32_4m_c),
                     'class_44_4m_c' = as.numeric(samples_pred$class_44_4m_c),
                     'class_5_4m_c' = as.numeric(samples_pred$class_5_4m_c),
                     
                     'trees' = as.numeric(samples_pred$trees17_4m_de),
                     'buildings' = as.numeric(samples_pred$build_4m_de),
                     'ground' = as.numeric(samples_pred$ground_4m_de),
                     
                     'demo_16m' = as.numeric(samples_pred$lidardemo16m_4m_de), #as.numeric(samples_pred$lidardemo4m_de2),
                     'demo_64m' = as.numeric(samples_pred$lidardemo64m_4m_de), #as.numeric(samples_pred$lidardemo4m_de2),
                     'par_impval_0' = as.numeric(samples_pred$parcel_impval_0),       
                     'roads' = as.numeric(samples_pred$roads)
                   ),
                   n.chains = 3,
                   n.adapt = 200)

dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter= 5000) #update(jags,n.iter=1000) 
#mcmc_samples_params <- coda.samples(jags, variable.names=c("theta"),  n.iter = 100)
mcmc_samples_params <- coda.samples(jags, variable.names=c("delta", "beta", "alpha"),  n.iter = 1000, thin = 10) #variables to monitor
gelman.diag(mcmc_samples_params)
plot(mcmc_samples_params)  #plot(samples[,'a'])















###analysis of all counties with NAB data ##############################################################
library(forcats)
names(opa_day)
levels(opa_day$county_name)
str(opa_day$county_name)
as.numeric(ordered(opa_day$county_name))
data_for_model <- filter(opa_day, !is.na(n_cases)) %>% # == "Harris") %>% !is.na(county_name)) 
  #filter(p_season == "J. ashei") %>%
  #mutate(county_n = as.numeric(as.factor(county_name))) %>%
  dplyr::select(date, n_cases, county_name, PAT_COUNTY, children_pop, pbir_child,
                cupr, cupr_m7, cupr_l1, cupr_l2, cupr_l3, cupr_m7, cupr_m14, 
                tot_pol = Total.Pollen.Count,
                Ambrosia,
                Quercus,
                #rhino_child, rhino_child_m7, rhino_child_m14,
                flu_d, flu_d_m7 #flu_d_m14, 
  ) %>%
  arrange(date) %>%
  na.omit
data_for_model$county_n <- as.numeric(ordered(data_for_model$county_name)) #for some reason this wasn't working in piping
data_for_model$obs_n <- 1:nrow(data_for_model)

data_for_model$cupr

ggplot(data_for_model, aes(x= obs_n, y = n_cases)) + theme_bw()+  #log(cupr_l1 + 1)
  geom_point() + geom_line() + facet_wrap(~county_name) + 
  geom_point(aes(x=obs_n, y =n_cases), color = "red") +
  geom_line(aes(x=obs_n, y =n_cases), color = "red")


ggplot(data_for_model, aes(x= log(Quercus + 1), y = n_cases)) + theme_bw()+
  geom_point() + geom_smooth(method = "lm") +facet_wrap(~county_name)

ggplot(data_for_model, aes(x= rhino_child_m7, y = n_cases, color = (cupr_m14 + 1))) + theme_bw()+
  geom_point() + geom_smooth(method = "lm") + scale_color_viridis_c(trans = "log") + facet_wrap(~county_name)


#hist(data_for_model$cupr)
sink("model_a.txt")
cat("  
  ### model ########### 
    model{
    ## Likelihood
    for(i in 1:N){
      pbir[i] <- y[i]/pop_10k[i]
      y[i] ~ dpois(lambda[i])
      log(lambda[i]) <- 
      alpha[county_j[i]] + 
      beta_cupr[county_j[i]]/pop_10k[i] * cupr_log[i] + 
      beta_quer[county_j[i]]/pop_10k[i] * Quercus_log[i] + 
      beta_ambr[county_j[i]]/pop_10k[i] * Ambrosia_log[i] +
      beta_flu[county_j[i]]/pop_10k[i] * flu_d[i] 
      
          # beta[4] * cupr_l3_log[i] +
          # beta[5] * cupr_m7_log[i] + 
          # beta[6] * cupr_m14_log[i] +
          # 
          # alpha[1] * flu_d[i]  
          # # alpha[2] * rhino_child_m7[i] + 
          # # alpha[3] * rhino_child_m14[i] +
          # 
          # 
          # # kappa[1] * flu_d[i]

    }     
      
      #average effect
      beta_cupr_avg <- (beta_cupr[1] + beta_cupr[2] + beta_cupr[3]+ beta_cupr[4]+ beta_cupr[5]+ beta_cupr[6])/n_counties
      beta_quer_avg <- (beta_quer[1] + beta_quer[2] + beta_quer[3]+ beta_quer[4]+ beta_quer[5]+ beta_quer[6])/n_counties
      beta_ambr_avg <- (beta_ambr[1] + beta_ambr[2] + beta_ambr[3]+ beta_ambr[4]+ beta_ambr[5]+ beta_ambr[6])/n_counties
      beta_flu_avg  <- (beta_flu[1] + beta_flu[2] + beta_flu[3]+ beta_flu[4]+ beta_flu[5]+ beta_flu[6])/n_counties
      
      
      
    ## Priors 
    for(a in 1:n_counties){ alpha[a] ~ dnorm(0, 0.0001)}
    for(b in 1:n_counties){beta_cupr[b] ~ dnorm(0, 0.0001)}
    for(b in 1:n_counties){beta_quer[b] ~ dnorm(0, 0.0001)}
    for(b in 1:n_counties){beta_ambr[b] ~ dnorm(0, 0.0001)}
    for(b in 1:n_counties){beta_flu[b] ~ dnorm(0, 0.0001)}
    #for(b in 1:4){beta[b] ~ dnorm(0, 0.0001)}
    # for(a in 1:3){alpha[a] ~ dnorm(0, 0.0001)}
    # kappa ~ dnorm(0, 0.0001)
}
    ",fill=TRUE)
sink() 

jags <- jags.model('model_a.txt', 
                   data = list(
                     
                     y = as.numeric(data_for_model$n_cases),  # DV
                     N = nrow(data_for_model),
                     #county = data_for_model$county_n,
                     n_counties = length(unique(data_for_model$county_n)),
                     county_j = data_for_model$county_n,
                     pop_10k = (data_for_model$children_pop/10000),
                     
                     #NAB data for Cupr
                     cupr_log = log10(data_for_model$cupr + 1),  # predictors
                     #tot_pol =  log10(data_for_model$tot_pol + 1),  # predictors
                     Quercus_log =  log10(data_for_model$Quercus + 1),  # predictors
                     Ambrosia_log =  log10(data_for_model$Ambrosia + 1),  # predictors
                     # cupr_l1_log = log(data_for_model$cupr_l1 + 1),  
                     # cupr_l2_log = log(data_for_model$cupr_l2 + 1),  # predictors
                     # cupr_l3_log = log(data_for_model$cupr_l3 + 1),  # predictors
                     # cupr_m7_log = log(data_for_model$cupr_m7 + 1),  # predictors
                     # cupr_m14_log = log(data_for_model$cupr_m14 + 1),  # predictors
                     
                     #rhinovirus from Eggleston
                     # rhino_child = as.numeric(data_for_model$rhino_child),
                     # rhino_child_m7 = as.numeric(data_for_model$rhino_child_m7) ,
                     # rhino_child_m14 = as.numeric(data_for_model$rhino_child_m14) ,
                     flu_d = as.numeric(data_for_model$flu_d) # sample size
                     #mu.beta=rep(0,2),  # priors centered on 0
                     #tau.beta=diag(.0001,2))
                   ),
                   n.chains = 3,
                   n.adapt = 1000)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter= 1000) #update(jags,n.iter=1000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("alpha", "beta_cupr", "beta_quer","beta_ambr", "beta_flu",
                                                           "beta_cupr_avg", "beta_quer_avg", "beta_ambr_avg", "beta_flu_avg"),  
                                    n.iter = 1000, thin = 10) #variables to monitor
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params$parameter<-row.names(results_params)
results_params$param<-substr(results_params$parameter,1,2)



mcmc_samples_params_pbir <- coda.samples(jags, variable.names=c("pbir"),  
                                         n.iter = 1000, thin = 10) #variables to monitor
#plot(mcmc_samples_params)
results_param_pbir <- summary(mcmc_samples_params_pbir)
results_params_pbir <- data.frame(results_param_pbir$statistics, results_param_pbir$quantiles) #multi-var model
results_params_pbir$parameter<-row.names(results_params_pbir)
results_params_pbir$param<-substr(results_params_pbir$parameter,1,2)

results <- data_for_model
results$pbir_calc_mean <- results_params_pbir$Mean

ggplot(results, aes(x= county_name, y = pbir_calc_mean)) + geom_boxplot()

names(opa_day)
opa_day %>%
  group_by(county_name) %>%
  summarize(pbir_child_mean = mean(pbir_child)) # = sum(n_cases)/())


### a toy model for initial Poisson regression analysis ####################################################################
# #construct some toy data to make sure everything is working
# N <- 1000
# beta0 <- 1  # intercept
# beta1 <- 1  # slope
# x <- rnorm(n=N)  # standard Normal predictor
# mu <- beta0*1 + beta1*x  # linear predictor function
# lambda <- exp(mu)  # CEF
# y <- rpois(n=N, lambda=lambda)  # Poisson DV
# dat <- data.frame(x,y)  
# 
# 
# sink("model_a.txt")
# cat("  
#   ### model ##
#     model{
#     ## Likelihood
#     for(i in 1:N){
#       y[i] ~ dpois(lambda[i])
#       log(lambda[i]) <- beta0 + beta1 * x[i]
#     }     
#       
#     ## Priors 
#     beta0 ~dnorm(0, 0.0001) 
#     beta1 ~ dnorm(0, 0.0001)
# }
#     ",fill=TRUE)
# sink() 
# 
# jags <- jags.model('model_a.txt', 
#                    data = list(
#                     x= dat$x,  # predictors
#                     y= dat$y,  # DV
#                     N=N  # sample size
#                     #mu.beta=rep(0,2),  # priors centered on 0
#                     #tau.beta=diag(.0001,2))
#                     ),
#                 n.chains = 3,
#                 n.adapt = 1000)  # diffuse priors
# 
# dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
# #Sys.time()
# update(jags,n.iter= 500) #update(jags,n.iter=1000) 
# mcmc_samples_params <- coda.samples(jags, variable.names=c("beta1"),  n.iter = 1000, thin = 10) #variables to monitor
# plot(mcmc_samples_params)

# 
# ### some preliminary analysis #####################################################################
# head(opa_day) #unique(opa_day$county_name) #unique(opa_day$NAB_station)
# names(opa_day)
# test <- filter(opa_day, NAB_station == "San Antonio A" | NAB_station == "San Antonio B" ) #,  doy > 350 | doy < 50)
# #test <- opa_day
# fit <- glm(n_cases ~  
#              log10(tot_pol_m21 - cupr_m21 + 1) + 
#              #log10(cupr + 1) +
#              log10(cupr_m21 + 1)  +
#              v_tests_pos_RSV + v_tests_pos_Rhinovirus + v_tests_pos_Corona 
#            , data = test, family = "poisson")
# 
# summary(fit)
# 
# data_for_model <- filter(opa_day, !is.na(n_cases)) %>% 
#   dplyr::select(date, n_cases, county_name, PAT_COUNTY, children_pop, pbir_child,
#                 cupr, cupr_m7, cupr_l1, cupr_l2, cupr_l3, cupr_m7, cupr_m14, 
#                 tot_pol = Total.Pollen.Count,
#                 Ambrosia,
#                 Quercus,
#                 #rhino_child, rhino_child_m7, rhino_child_m14,
#                 flu_d, flu_d_m7 #flu_d_m14, 
#   ) %>%
#   arrange(date) %>%
#   na.omit
# data_for_model$county_n <- as.numeric(ordered(data_for_model$county_name)) #for some reason this wasn't working in piping
# data_for_model$obs_n <- 1:nrow(data_for_model)
# 
# 
# 
# names(data_for_model)
# fit <- glm((pbir_child + 0.001) ~  cupr + #* county_name +
#              Ambrosia + # *county_name + 
#              Quercus  +#* county_name + 
#              flu_d  +
#              county_name, #* county_name, 
#            data = data_for_model, family = Gamma(link = "log"))
# summary(fit)
# 
# fit <- glm(log(pbir_child + 0.001) ~  cupr +  #* county_name +
#              Ambrosia + # *county_name + 
#              Quercus  +#* county_name + 
#              flu_d  +
#              county_name, #* county_name, 
#            data = opa_day, family = "gaussian")
# summary(fit)
# hist((opa_day$pbir_child))
# 
# par(mfrow=c(1,1))
# hist(log(opa_day$pbir_child))
# t.test(opa_day$pbir_py[opa_day$p_season == "J. ashei"], opa_day$pbir_py[opa_day$flu == "flu"])
# t.test(opa_day$pbir_py[opa_day$p_season == "J. ashei"], opa_day$pbir_py[opa_day$cold == "cold"])
# t.test(opa_day$pbir_py[opa_day$p_season == "spring trees"], opa_day$pbir_py[opa_day$flu == "flu"])
# t.test(opa_day$pbir_py[opa_day$p_season == "spring trees"], opa_day$pbir_py[opa_day$cold == "cold"])
# t.test(opa_day$pbir_py[opa_day$p_season == "J. ashei"], opa_day$pbir_py[opa_day$p_season == "spring trees"])
# 
# opa_day %>% group_by(county_name) %>%
#   summarize(oak = mean(Quercus),
#             rag = mean(Ambrosia),
#             cupr = mean(cupr))
# t.test(opa_day$cupr[opa_day$county_name == "Bexar"], opa_day$cupr[opa_day$county_name == "Dallas"])



### assess temporal trends in ED visits for an unrelated cause (injury) ########################
# block_groups_near_NAB_nostation <- unlist(block_groups_near_NAB$GEOID)
# 
# opi <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/op_injuries.csv")
# opi <- opi %>% mutate(
#           PAT_ADDR_CENSUS_BLOCK_GROUP_c = gsub(".", "", PAT_ADDR_CENSUS_BLOCK_GROUP, fixed = TRUE), #remove the extra periods from this column
#           GEOID10 = paste0(PAT_ADDR_CENSUS_BLOCK_GROUP_c, PAT_ADDR_CENSUS_BLOCK), #to link up with coordinates downloaded from the census
#           GEOID = PAT_ADDR_CENSUS_BLOCK_GROUP_c) %>% 
#         filter(PAT_ADDR_CENSUS_BLOCK_GROUP_c %in% block_groups_near_NAB_nostation) 
# opi_day <- left_join(opi, block_groups_near_NAB) %>% 
#               filter(date > mdy("10 - 01 - 2015"),
#                      date < mdy("12 - 31 - 2017")) %>%
#               group_by(NAB_station, date) %>%
#               summarize(n_cases_injury = n()) %>%
#               mutate(week_day = weekdays(date))
# opi_day %>%
# ggplot( aes(x = date, y = n_cases_injury)) + geom_point() + facet_wrap(~NAB_station, scales = "free") + theme_bw() +  
#   geom_line(aes(x = date, y=rollmean(n_cases_injury, 14, na.pad=TRUE)), color = "red") +
#   scale_x_date(date_breaks = "2 month", limits = c(mdy("10 - 01 - 2015"), mdy("12 - 23 - 2017"))) +
#   theme(axis.text.x=element_text(angle=60, hjust=1)) +
#   ylab("ED visits for injury (daily)")
# 
# ggplot(opi_day, aes(x = week_day, y = n_cases_injury)) + geom_boxplot() + facet_wrap(~NAB_station, scales = "free") + theme_bw() 

# opa_day%>%
# ggplot(aes(x = as.factor(holiday), y = n_cases)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~NAB_station, scales = "free") + theme_bw() + 
#   xlab("holiday (holiday = 1)") + ylab("number of cases per day")

