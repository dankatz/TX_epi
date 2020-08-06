#data assembly for preliminary epidemiological analysis
#install.packages("noncensus")
library(tidyr) #install.packages("tidyr") 
library(dplyr) #install.packages("dplyr")
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(zipcode) #downloaded from the archives, no longer on CRAN
library(sf)
library(noncensus) #not available for R 4.0.0 yet
library(zoo)
library(ggthemes)
library(tidycensus) 
library(scales)
library(tigris)
options(tigris_use_cache = TRUE)
library(lwgeom)
#install.packages("tidycensus")
library(rjags)

rm(list = ls())
#test


### load in NAB data from 2009-2019 ##################################################################
NAB_locations <- read_csv("C:/Users/dsk856/Desktop/misc_data/EPHT_Pollen Station Inventory_062019_dk200331.csv")
NAB <- read_csv("C:/Users/dsk856/Desktop/misc_data/NAB2009_2019_pollen_191230.csv", guess_max = 92013)
NAB_modeled <- read_csv("C:/Users/dsk856/Desktop/misc_data/NAB_pollen_modeled200618.csv", guess_max = 31912)
# msa <- read.csv("C:/Users/dsk856/Desktop/misc_data/county_to_MSA_clean.csv", stringsAsFactors = FALSE)
# msa$FIPS <- sprintf("%03s",msa$FIPS) %>% sub(" ", "0",.) %>% sub(" ", "0",.)

NAB_tx <- left_join(NAB, NAB_locations) %>%
  filter(State == "TX") %>%
  mutate(date = ymd(Date),
         doy = yday(date),
         year = year(date),
         mo = month(date) ,
         ja = case_when( mo < 3  ~ Cupressaceae,
                         mo > 11 ~ Cupressaceae,
                         TRUE ~ 0) ,
         cup_other = case_when(ja == 0 ~ Cupressaceae,
                               ja > 0 ~ 0)) %>%  #, FIPS = as.character(FIPS
  rowwise()%>%
  mutate(trees = sum(Acer, Alnus, Betula, Carpinus.Ostrya, Corylus, Fraxinus, Juglans, Liquidambar, Other.Tree.Pollen,                    
                     Populus, Pseudotsuga, Quercus, Salix, Arecaceae, Carya, Cyperaceae, Fagus, Ligustrum,
                     Morus, Olea, Platanus, Tilia, Celtis, Prosopis, Myrica, Ulmus, Tsuga, na.rm = TRUE), #not including Pinaceae
         pol_other = Total.Pollen.Count - ja - cup_other - trees)%>%  
  dplyr::select(date, doy, year, City, FIPS, county_name, MSA, Lat, Long, NABStation_ID, file, 
                ja, cup_other, trees, pol_other,  #Total.Pollen.Count, Cupressaceae,grass = Gramineae...Poaceae,Ambrosia, Quercus, Ulmus, Acer, Platanus,    
                NAB_station) %>%
  mutate(FIPS = sprintf("%03s",FIPS), #NAB_tx$FIPS
                         FIPS = sub(" ", "0",FIPS),
                         FIPS = sub(" ", "0",FIPS))
NAB_tx <- full_join(NAB_tx, NAB_modeled) %>% 
  mutate(doy = yday(date))
# head(NAB_tx)
# head(NAB_modeled)
#str(NAB_tx)
#NAB_tx$PAT_COUNTY <- sprintf("%03s",NAB_tx$FIPS) %>% sub(" ", "0",.) %>% sub(" ", "0",.)
#NAB_tx <- left_join(NAB_tx, msa)
# filter(NAB_tx, date > ymd("2015-11-1") & date < ymd("2018-01-01")) %>% #summarise(n = n())
#   ggplot(aes(x = date, y = Total.Pollen.Count + 0.1, color = file)) + geom_point(alpha = 0.5) + facet_wrap(~NAB_station) + theme_bw() + scale_y_log10() + ylab("pollen grains/m3") +
#   theme(legend.position = "none")


#counties that I have pollen data for (from 2009-2019)
#NAB_counties <- dplyr::select(NAB_tx, FIPS) %>% distinct() %>% unlist()
#counties that are in an MSA that I have pollen data from
#msa_selected <- dplyr::select(NAB_tx, MSA) %>% distinct() %>% unlist()
#NAB_MSA_counties <- msa$FIPS[msa$MSA %in% msa_selected]

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
              select(Lat, Long, NAB_station) %>%
              distinct() %>%
              filter(NAB_station != "") %>% #not sure how this made its way in, maybe a floating decimal?
              st_as_sf(coords = c("Long", "Lat"), crs = 4326)  %>%
              st_transform(crs = 26914)  #UTM 14 N

distances <- st_distance(opa_raw_sf, NAB_tx_sf, by_element = FALSE) /1000 #calculate distances and convert to km
distances_df <- as.data.frame(distances) 
distances_min <- apply(distances_df, 1, FUN = min) #minimum distance to a NAB station
which_station_closest <- apply(distances_df, 1, function(x) which(x == min(x, na.rm = TRUE))) #which statin is closest
NAB_station_lookup <- data.frame(NAB_station = NAB_tx_sf$NAB_station, n_lookup = 1:9)
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


  #filter(PAT_COUNTY != "155") #very few cases from here, so I'm removing it from the analysis

# make sure that dates with no cases have zeros instead of missing from list
day_list <- seq(mdy("9/1/2015"), mdy("12/31/2017"), by = "1 day") #as.data.frame( seq(mdy("9/1/2015"), mdy("12/31/2017"), by = "1 day"))
NAB_list <- unique(opa$NAB_station)
day_NAB_list <- expand.grid(day_list, NAB_list)
names(day_NAB_list) <- c("date", "NAB_station")
day_NAB_list <- mutate(day_NAB_list, n_cases = 0, doy = yday(date))


# #opa_msa
# opa_msa_day <- opa_msa %>% group_by(date, MSA) %>% summarize(n_cases = n()) %>%
#   mutate(doy = yday(date)) %>%
#   filter(MSA != "--") #very few cases from here, so I'm removing it from the analysis
#ggplot(opa_day, aes(x = date, y = n_cases)) + geom_point() + facet_wrap(~PAT_COUNTY)

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

#Variables that I want: ages 5-17  #B01001_003
c_vars <- c("B01001_004", "B01001_005", "B01001_006", #males from 5-17 years old
            "B01001_028", "B01001_029", "B01001_030") #females from 5-17 years old

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

# closest_station_bg <- recode(as.character(which_station_closest_bg), 
#                           "1" = "Georgetown",
#                           "2" =  "Houston",
#                           "3" =   "Waco1",
#                           "4" = "Dallas",
#                           "5" = "Waco2",
#                           "6" =  "College Station",
#                           "7" = "Flower Mound",
#                           "8" = "San Antonio 1",
#                           "9" = "San Antonio 2")
census_All_2017_sf <- mutate(census_All_2017_sf, NAB_min_dist_bg = distances_bg_min, NAB_station = station_looked_up_bg$NAB_station)
census_All_2017_sf$geometry <- NULL
pop_near_NAB <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
    filter(variable %in% c_vars) %>%  #only select variables that are population of children between 5 and 17
    group_by(NAB_station) %>%
    summarize(children_pop = sum(estimate)) #names(pop_near_NAB)

#block groups IDs that are near NAB stations
block_groups_near_NAB <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  select(GEOID, NAB_station) %>%
  distinct()

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
head(weather_at_stations)



### Virus monitoring data from DHHS #############################################################
virus <- read_csv("C:/Users/dsk856/Desktop/misc_data/virus_2015_2017_daily.csv")
#virus$date


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

### load in flu data ###############################################################################
#downloaded via CDCfluview package, script is here: 
flu <- read.csv("C:/Users/dsk856/Desktop/misc_data/flu_total_positive191111.csv")
flu$date <- ymd(flu$date)


### prepare data for exploration and modeling ######################################################################

## combine the various datasets
#head(opa_day)
str(opa_day)
opa_day <- opa %>% group_by(date, NAB_station) %>% #names(opa) opa$PAT_AGE_YEARS
  filter(between(PAT_AGE_YEARS, 5, 18)) %>%
  summarize(n_cases = n()) %>% 
  mutate(doy = yday(date)) 

opa_day <- bind_rows(day_NAB_list, opa_day)
opa_day <- opa_day %>% group_by(date, NAB_station, doy) %>%
  summarize(n_cases = sum(n_cases)) #add up n_cases from each day

opa_day <- left_join(opa_day, pop_near_NAB)
opa_day <- mutate(opa_day, pbir = ((n_cases/children_pop) * 10000), #PIBR per 10,000 for children 
                  pbir_py = (pbir / ((children_pop))) * 100000)
opa_day <- left_join(opa_day, NAB_tx)
# summary(NAB_tx$doy)
# NAB_tx$doy
# opa_day$date

opa_day <- left_join(opa_day, flu)
opa_day <- left_join(opa_day, virus) ##head(virus)
opa_day <- left_join(opa_day, weather_at_stations)

#install.packages("timeDate")
holiday_df <- data.frame(date = ymd(unlist(timeDate::holidayNYSE(2015:2017))), holiday = 1)
opa_day <- left_join(opa_day, holiday_df)
opa_day$holiday[is.na(opa_day$holiday)] <- 0


#adding in some lags and leads
#names(opa_day)# head(opa_day) unique(opa_day$PAT_COUNTY)
#lag in pollen for last: 1 day
names(opa_day)


opa_day <- opa_day %>% ungroup() %>% group_by(NAB_station) %>% arrange(NAB_station, date) %>%
                               mutate(
                              # cupr = Cupressaceae,
                              # cupr_l1 = lag(Cupressaceae, n = 1),
                              # cupr_m7 = rollapply(Cupressaceae, 7, align='right', fill=NA,  function(x) mean(x, na.rm = TRUE)),
                              # tot_pol = Total.Pollen.Count,
                              # tot_pol_l1 = lag(Total.Pollen.Count,n = 1),
                              # tot_pol_m7 = rollapply(Total.Pollen.Count,7,align='right',fill=NA,  function(x) mean(x, na.rm = TRUE)),
                              # flu_d_m7 = rollapply(flu_d, 7, align='right',fill=NA,  function(x) mean(x, na.rm = TRUE)),
                              # flu_d_m14 = rollapply(flu_d, 14, align='right',fill=NA,  function(x) mean(x, na.rm = TRUE)),
                              week_day = weekdays(date),
                              week_day = forcats::fct_relevel(week_day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                                                     "Saturday", "Sunday"))  %>%
                      filter(date > mdy('9-30-15')) 

write_csv(opa_day, "C:/Users/dsk856/Desktop/thcic_analysis/opa_day200707.csv")
opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day200707.csv", guess_max = 7434)

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

### exploring data ###################################################


### fig 2: time series of each var ############################################################

names(opa_day)

#time series for major pollen types
panel_pol <-  opa_day %>% 
  mutate(Cupressaceae = ja + cup_other,
         other = pol_other) %>% 
  select(NAB_station, date, Cupressaceae, trees, other) %>% 
  pivot_longer(cols = c(Cupressaceae, trees, other), names_to = "pollen_type", values_to = "pollen") %>% 
  arrange(NAB_station, pollen_type, date) %>% 
  filter(!is.na(pollen)) %>% 
  #filter(NAB_station == "San Antonio A" | NAB_station == "San Antonio B") %>% 
  # filter(date > mdy ("11 - 1- 2017") ) %>% 
  ggplot(aes(x = date, y = pollen + 1, col = NAB_station, group = NAB_station)) + theme_few() + scale_y_log10() + 
    geom_line(aes(x = date, y=rollmean((pollen + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station)) + 
  facet_wrap(~pollen_type, nrow = 4) + ylab(pollen~(grains/m^3)) + scale_color_discrete(name = "NAB station") +
    theme(legend.position= "none" )

#time series for viruses
panel_vir <-
  opa_day %>% 
    select(NAB_station, date, v_tests_pos_Corona, v_tests_pos_Rhinovirus, v_tests_pos_RSV) %>% 
    pivot_longer(cols = c(v_tests_pos_Corona, v_tests_pos_Rhinovirus, v_tests_pos_RSV), 
                 names_to = "virus_type", values_to = "positive_tests") %>% 
    arrange(NAB_station, virus_type, date) %>% 
    #filter(!is.na(pollen)) %>% 
    #filter(NAB_station == "San Antonio A" | NAB_station == "San Antonio B") %>% 
    # filter(date > mdy ("11 - 1- 2017") ) %>% 
    ggplot(aes(x = date, y = positive_tests, color = virus_type)) + theme_few() + 
    geom_line() + #geom_point() + 
    ylab("positive tests (n)") +
    scale_color_discrete(breaks = c("v_tests_pos_Corona", "v_tests_pos_Rhinovirus","v_tests_pos_RSV"),
                       labels = c("Seasonal coronavirus", "Rhinovirus","RSV"), name = "virus type") +
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour="white", fill="white"),
          legend.position= "top")
  
#time series for ED visits
panel_ed <- 
  opa_day %>% 
    #select(NAB_station, date, v_tests_pos_Corona, v_tests_pos_Rhinovirus, v_tests_pos_RSV) %>% 
    # pivot_longer(cols = c(v_tests_pos_Corona, v_tests_pos_Rhinovirus, v_tests_pos_RSV), 
    #              names_to = "virus_type", values_to = "positive_tests") %>% 
    # arrange(NAB_station, virus_type, date) %>% 
    # #filter(!is.na(pollen)) %>% 
    #filter(NAB_station == "San Antonio A" | NAB_station == "San Antonio B") %>% 
    # filter(date > mdy ("11 - 1- 2017") ) %>% 
    ggplot(aes(x = date, y = pbir, color = NAB_station)) + theme_few() + 
    geom_line(aes(x = date, y=rollmean((pbir + 1), 7, na.pad=TRUE), col = NAB_station, group = NAB_station)) + 
    #geom_line() + #geom_point() + 
    ylab("Asthma ED visits (per 10,000 residents)") +
  
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour="white", fill="white"),
          legend.position= "none")
  
#putting together the time series into one plot  
ts_panels <- cowplot::plot_grid(panel_ed, panel_pol, panel_vir, align = "v", ncol = 1, rel_heights = c(1,3,1))
ggsave(file = "C:/Users/dsk856/Desktop/thcic_analysis/time_series_fig_poster200806.jpg", plot = ts_panels, 
       height =20, width = 17, units = "cm", dpi = 300)  
?ggsave


#comparing pollen to ED visits for asthma over time
names(opa_day)
panel_a <- opa_day %>%
filter(date > ymd("2015 - 11 - 01")) %>%
filter(NAB_station == "San Antonio A") %>%
  ggplot(aes(x = date, y = n_cases)) + theme_few() +  
  geom_point(alpha = 0.2)  +  ylab("number of asthma ED visits") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
  geom_line(aes(x = date, y=rollmean(n_cases, 7, na.pad=TRUE)), color = "black")
panel_b <- opa_day %>% 
  filter(date > ymd("2015 - 11 - 01")) %>%
  filter(NAB_station == "San Antonio A") %>%
  ggplot(aes(x = date, y = log(ja + 1))) + theme_few() +  #scale_x_log10() + 
  geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
  geom_line(aes(x = date, y=rollmean(log(ja + 1), 7, na.pad=TRUE)), color = "black")
panel_c <- opa_day %>% 
  filter(date > ymd("2015 - 11 - 01")) %>%
  filter(NAB_station == "San Antonio A") %>%
  ggplot(aes(x = date, y = log(cup_other_rfint_log_mean + 1))) + theme_few() +  #scale_x_log10() + 
  geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
  geom_line(aes(x = date, y=rollmean(log(cup_other_rfint_log_mean + 1), 7, na.pad=TRUE)), color = "black")
panel_d <- opa_day %>% 
  filter(date > ymd("2015 - 11 - 01")) %>%
  filter(NAB_station == "San Antonio A") %>%
  ggplot(aes(x = date, y = log(trees + 1))) + theme_few() +  #scale_x_log10() + 
  geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
  geom_line(aes(x = date, y=rollmean(log(trees + 1), 7, na.pad=TRUE)), color = "black")
panel_e <- opa_day %>% 
  filter(date > ymd("2015 - 11 - 01")) %>%
  filter(NAB_station == "San Antonio A") %>%
  ggplot(aes(x = date, y = log(pol_other + 1))) + theme_few() +  #scale_x_log10() + 
  geom_point(alpha = 0.2)  +  ylab("log(pollen grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
  xlab("") + #scale_color_viridis_c(name = "pollen grains per m3") +
  geom_line(aes(x = date, y=rollmean(log(pol_other + 1), 7, na.pad=TRUE)), color = "black")

cowplot::plot_grid(panel_a, panel_b, panel_c, panel_d, panel_e, nrow = 5, 
                   labels = c("ED visits", "J. ashei", "other Cupressaceae", "trees", "other pollen"))

  
  -opa_day %>%
    filter(date > ymd("2015 - 11 - 01")) %>%
    filter(NAB_station == "Houston" | NAB_station == "Dallas" | NAB_station == "San Antonio A") %>%
    ggplot(aes(x = date, y = n_cases, color = v_tests_pos_RSV )) + theme_few() +  facet_wrap(~NAB_station, scale = "free_y", ncol = 1) + #scale_x_log10() + 
    geom_point()  +  ylab("number of asthma ED visits") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
    xlab("") + scale_color_viridis_c() +
    geom_line(aes(x = date, y=rollmean(n_cases, 7, na.pad=TRUE)), color = "black")
  
  
  
  opa_day %>%
    filter(date > ymd("2015 - 11 - 01")) %>%
    filter(NAB_station == "Georgetown" ) %>%
    ggplot(aes(x = date, y = tot_pol +1 )) + theme_bw() +  #facet_wrap(~NAB_station, scale = "free_y", ncol = 1) + #scale_x_log10() + 
    geom_point()  +  ylab("airborne pollen (grains/m3)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
    xlab("") + #+ scale_color_viridis_c(name = "max temp (C)") +
    #geom_line(aes(x = date, y= tot_pol_m7 + 1)) +
    scale_x_date(breaks = pretty_breaks(15)) +
    scale_y_log10()
  
  
  
  opa_day %>%
    filter(date > ymd("2015 - 11 - 01")) %>%
    filter(NAB_station == "Houston" | NAB_station == "Dallas" | NAB_station == "San Antonio A") %>%
    ggplot(aes(x = date, y = met_tmaxdegc )) + theme_few() +  facet_wrap(~NAB_station, scale = "free_y", ncol = 1) + #scale_x_log10() + 
    #geom_point()  +  
    ylab("temperature (C)") + # ylab("PBIR (asthma ED visits per 10,000 residents)") + #+ geom_smooth(method = "lm", se = FALSE, color = "gray")
    #xlab("") + 
    geom_line(aes(x = date, y=rollmean(n_cases, 3, na.pad=TRUE), col = log10(tot_pol + 1)), lwd = 2) +
    geom_line(aes(x = date, y=rollmean(met_tmaxdegc, 7, na.pad=TRUE)), color = "red") +
     scale_y_continuous("ED visits", sec.axis = sec_axis(~ . , name = "temperature")) +
    theme(      axis.title.y.right = element_text(color = "red")) +
    scale_color_viridis_c()
  
  
opa_day %>%
  ggplot(aes(y = tot_pol, x = met_tmaxdegc, col = doy)) + theme_few() +  facet_wrap(~NAB_station) + scale_y_log10() +  #facet_wrap(~year, ncol = 1) + #
  geom_point() + xlab("maximum temp (C)") + ylab("pollen grains per m3") + #geom_smooth(method = "lm", se = FALSE)  + 
  scale_color_viridis_c(name = "day of year")#ylab("PBIR (asthma ED visits per 10,000 residents)")  
  # ylab("positive flu tests (from CDC)") + scale_x_log10() + 
  # xlab("Cupressaceae (pollen grains per m3)") 
#scale_color_discrete(name = "pollen season")
#geom_hex() #geom_jitter() +#

names(opa_day)

opa_day %>% ungroup() %>%
  #convert time series so that they are all between 0 and 100
  # mutate(tot_pol_stan = log10(tot_pol + 1) * 20,
  #        pbir_child_stan = pbir_child * 300) %>% 
#  filter(MSA == "San Antonio-New Braunfels") %>% 
  filter(NAB_station == "San Antonio A" | NAB_station == "Dallas" | NAB_station == "Houston") %>%
  filter(date > ymd("2015-11-01")) %>%
  filter(year == 2016) %>%
  ggplot(aes(x = date, y = tot_pol_stan )) + theme_bw() +  facet_wrap(~NAB_station, ncol = 1) +#+ facet_wrap(~year, ncol = 1) + #+ scale_y_log10() + 
  #geom_point(aes(x = date, y =tot_pol_stan)) + #scale_y_log10() + #geom_smooth(method = "lm", se = FALSE)  + 
    #scale_color_viridis_c()+
  #geom_point(aes(x = date, y = pbir_child_stan)) +
  geom_line(aes(x = date , y=rollmean(log10(tot_pol + 1) * 25, 7, na.pad=TRUE)), color = "blue") +
  geom_line(aes(x = date , y=rollmean(v_tests_pos_Rhinovirus * 0.3, 7, na.pad=TRUE)), color = "green") +
  #geom_line(aes(x = date, y=rollmean(log10(tot_pol_m14 + 1) * 20, 7, na.pad=TRUE)), color = "blue") +
  #geom_line(aes(x = date, y= log10(tot_pol_m7 + 1)*20), color = "blue") +
  #geom_line(aes(x = date, y=rollmean(hits_day_adj_pollen * 2, 7, na.pad=TRUE)), color = "green") +
  geom_line(aes(x = date, y=rollmean(pbir * 175, 7, na.pad=TRUE)), color = "red") +
  #geom_line(aes(x = date, y=rollmean(met_tmaxdegc * 2, 1, na.pad=TRUE)), color = "black", lty = 2) +
  #geom_line(aes(x = date - 12, y=rollmean(gtrend_common_cold * 10, 7, na.pad=TRUE)), color = "black") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_date(breaks = pretty_breaks(30)) +
  ylab("variable scaled to 100")
#ylab("PBIR (asthma ED visits per 10,000 residents)")  

test <- filter(opa_day, MSA == "San Antonio-New Braunfels")
ccf(test$hits_day_adj_pollen, test$tot_pol)
ccf(test$hits_day_adj_pollen, test$pbir_child)

opa_day %>% ungroup() %>% #unique(opa_day$NAB_station)
  filter(date > ymd("2015-11-01")) %>%
  #filter(year == 2017) %>%
  #filter(NAB_station == "San Antonio B" | NAB_station == "San Antonio A" ) %>%
  #filter(NAB_station == "Waco B" | NAB_station == "Waco A" ) %>%
  filter(NAB_station == "Dallas" | NAB_station == "Flower Mound" ) %>%
  #filter(NAB_station == "Houston" | NAB_station == "Dallas" | NAB_station == "San Antonio A" | NAB_station == "Georgetown") %>%
  #filter(NAB_station == "Flower Mound" | NAB_station == "San Antonio B" | NAB_station == "San Antonio A" | NAB_station == "Georgetown") %>%
  ggplot(aes(x = date, y = pbir * 500 , col = log10(tot_pol+ 1) )) + theme_bw() +  facet_wrap(~NAB_station, ncol = 1) +
  scale_color_viridis_c() +
  #geom_line( lwd = 2) + 
  geom_line(aes(x = date - 10, y=rollmean(pbir * 1000, 7, na.pad=TRUE)), color = "red", lwd = 2) + 
  geom_point(aes(x = date, y = 50 * log(tot_pol + 1)))+
  geom_line(aes(x = date, y = v_tests_pos_Adenovirus), color = "pink")+
  geom_line(aes(x = date, y = v_tests_pos_HMPV), color = "pink")+
  geom_line(aes(x = date, y = v_tests_pos_Corona), color = "green")+
  geom_line(aes(x = date, y = v_tests_pos_Rhinovirus), color = "blue")+
  geom_line(aes(x = date, y = v_tests_pos_RSV), color = "goldenrod")+
  geom_line(aes(x = date, y = v_tests_pos_Parainfluenza), color = "pink") 
  

opa_day %>%
  filter(date > ymd("2015-11-01")) %>%
  filter(year == 2017) %>%
  ggplot(aes(x = date, y = tot_pol_stan )) + theme_bw() +  facet_wrap(~NAB_station, ncol = 1) +#+ facet_wrap(~year, ncol = 1) + #+ scale_y_log10() + 
  geom_line(aes(x = date, y=rollmean(log10(tot_pol + 1) * 20, 7, na.pad=TRUE)), color = "blue") +
  geom_line(aes(x = date, y=rollmean(hits_day_adj_pollen * 2, 7, na.pad=TRUE)), color = "green") +
  geom_line(aes(x = date -10, y=rollmean(pbir * 100, 7, na.pad=TRUE)), color = "red") +
  geom_line(aes(x = date, y=rollmean(met_tmaxdegc * 2, 1, na.pad=TRUE)), color = "black", lty = 2) +
  #geom_line(aes(x = date - 12, y=rollmean(gtrend_common_cold * 10, 7, na.pad=TRUE)), color = "black") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_date(breaks = pretty_breaks(30))
#ylab("PBIR (asthma ED visits per 10,000 residents)")  


opa_day %>%
filter(NAB_station == "Dallas") %>%
#filter(year == 2016) %>%
ggplot(aes(x = date, y = pbir, col = (tot_pol + 1))) + geom_point(size = 2) + theme_bw() + 
  scale_color_viridis_c(trans = "log10", name = "total pollen (g/m3)") +
  scale_x_date(breaks = pretty_breaks(20)) + ylab("PBIR (Asthma ED visits per 10,000 residents)") +#scale_y_log10()  + 
  geom_line(aes(x = date, y=rollmean(pbir, 14, na.pad=TRUE)), color = "red") 
  #geom_line(aes(x = date, y=rollmean(log10(tot_pol + 1)/10, 3, na.pad=TRUE)), color = "blue") 


names(opa_day)


# #overlap between San Antonio datasets?
# filter(NAB_tx, county_name == "Bexar") %>%
#   ggplot(aes(x = date, y = Cupressaceae, color = file)) + geom_point() + facet_wrap(~file, ncol = 1)
# 
# filter(NAB_tx, county_name == "Bexar") %>% 
#   dplyr::select(date, file, Cupressaceae) %>%
#   pivot_wider(names_from = file, values_from = Cupressaceae) %>%
#   setNames(., c("date", "San_198", "San_219")) %>%
#   ggplot(aes(x= San_198 + 1, y = San_219 + 1)) + geom_point() + theme_few() + scale_x_log10() + scale_y_log10() + geom_abline(slope = 1, lty = 2) + 
#   xlab("Station 198 (pollen grains/m3)") + ylab("Station 219 (pollen grains/m3)") #+ geom_smooth(method = "lm")
# 
# filter(NAB_tx, county_name == "McLennan") %>% 
#   dplyr::select(date, file, Cupressaceae) %>%
#   pivot_wider(names_from = file, values_from = Cupressaceae) %>%
#   setNames(., c("date", "Waco_105", "Waco_106")) %>%
#   mutate(doy = yday(date)) %>%
#   ggplot(aes(x= Waco_105 + 1, y = Waco_106 + 1, color = doy)) + geom_point() + theme_few() + scale_x_log10() + scale_y_log10() + geom_abline(slope = 1, lty = 2) + 
#   xlab("Station 105 (pollen grains/m3)") + ylab("Station 106 (pollen grains/m3)") #+ geom_smooth(method = "lm")
# 
# 
# NAB_tx %>% mutate(doy = yday(date),
#                   year = year(date)) %>%
#   ggplot(aes(x= doy, y = Cupressaceae + 1, color = year)) + geom_point(alpha = 0.5) + geom_line(aes(y=rollmean((Cupressaceae + 1), 14, na.pad=TRUE))) +
#   facet_wrap(~file) + scale_y_log10() + 
#   scale_color_viridis_c() + theme_few() 

# 
# #MSA data explor
# #comparing pollen to ED visits for asthma
# names(opa_msa_day2)
# #opa_day$p_season
# opa_msa_day2 %>%
#   mutate(doy = yday(date)) %>%
#   filter(doy < 30,
#        file != "219- San Antonio (3)_resaved.xls") %>% 
#   # file != "198- San Antonio (2)_resaved.xls") %>% 
#   ggplot(aes(x = Cupressaceae, y = n_cases)) + theme_few() +  facet_wrap(~MSA, scales = "free") + scale_x_log10() + 
#   geom_point() + geom_smooth(method = "lm", se = FALSE, color = "gray") + ylab("ED visits/d") + 
#   xlab("Cupressaceae (pollen grains per m3)") 
# #scale_color_discrete(name = "pollen season")
# #geom_hex() #geom_jitter() +#


#make some time series
names(NAB_tx)
NAB_tx %>% filter(county_name == "Dallas") %>%
  filter(date > mdy("11/1/2015"),
         date < mdy("12/31/2017")) %>%
# ggplot(aes(x = date, y = Cupressaceae + 0.1)) + geom_point() + scale_y_log10() + theme_few() +
#   geom_line(aes(x = date, y=rollmean(Cupressaceae + 0.1, 14, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)  #geom_smooth(method = "loess") +
ggplot(aes(x = doy, y = gtrend_commo + 1, color = as.factor(year))) + geom_point(alpha = 0.4, size = 2) + scale_y_log10() + theme_few() + 
  facet_wrap(~as.factor(year), ncol = 1) 
  #geom_line(aes(x = doy, y=rollmean(Cupressaceae + 0.1, 14, na.pad=TRUE, color = year)))  #
 #geom_smooth(method = "loess") 


filter(opa_day,   
      # !is.na(county_name),
       county_name == "Bexar" | county_name == "Dallas") %>% 
  # filter(date > ymd("15/11/1"),
  #        date < mdy("12/31/2017")) %>%
  ggplot(aes(x = date, y = gtrend_common_cold)) + theme_few() +  facet_wrap(~county_name, nrow = 2) + # scale_x_log10() + 
  geom_point() + #geom_smooth(method = "lm", se = FALSE, color = "gray") + #ylab("PBIR (asthma ED visits per 10,000 residents)") + 
  #ylab("pbir") + scale_color_viridis_c(trans = "log") + #name = "Cupressaceae (pollen grains per m3)", 
  geom_line(aes(x = date, y=rollmean(gtrend_common_cold, 14, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)  #geom_smooth(method = "loess") +


filter(opa_day,   
       # !is.na(county_name),
       county_name == "Bexar" | county_name == "Dallas") %>% 
  # filter(date > ymd("15/11/1"),
  #        date < mdy("12/31/2017")) %>%
  ggplot(aes(x = date, y = gtrend_common_cold)) + theme_few() +  facet_wrap(~county_name, nrow = 2) + # scale_x_log10() + 
  geom_point(alpha = 0.5) + #geom_smooth(method = "lm", se = FALSE, color = "gray") + #ylab("PBIR (asthma ED visits per 10,000 residents)") + 
  #ylab("pbir") + scale_color_viridis_c(trans = "log") + #name = "Cupressaceae (pollen grains per m3)", 
  geom_line(aes(x = date, y=rollmean(gtrend_common_cold, 14, na.pad=TRUE)), inherit.aes = FALSE, color = "blue", lwd = 2)  #geom_smooth(method = "loess") +




#summary of cases per day in Travis county
#as a function of pollen season based on Google
# ggplot(opa_day, aes(x = date, y = n_cases, color = g_season)) + geom_point(alpha = 0.5) + theme_few() + 
#   ylab("number of asthma-related ED visits in Travis County") + scale_color_viridis_d(name = "Google based pollen season")
# ggplot(opa_day, aes(x = doy, y = n_cases, color = g_season)) + geom_point(alpha = 0.5) + theme_few() + 
#   ylab("number of asthma-related ED visits in Travis County")+ scale_color_viridis_d(name = "Google based pollen season") + xlab("day of year")

# #as a function of pollen season based on NAB
# ggplot(opa_day, aes(x = date, y = n_cases, color = g_season)) + geom_point(alpha = 0.5) + theme_few() + 
#   ylab("number of asthma-related ED visits in Travis County") + scale_color_viridis_d(name = "Google based pollen season") +
#   scale_x_date(breaks = scales::pretty_breaks(n = 20)) + 
#   geom_line(aes(x = date, y=rollmean(n_cases, 14, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)  #geom_smooth(method = "loess") +
# 
# ggplot(opa_day, aes(x = g_season, y = pibr, color = g_season)) + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3, width = 0.2) + theme_few() + 
#   ylab("asthma-related ED visits in Travis County (per 10,000 people)") + xlab("") +
#   scale_color_discrete(name = "pollen season (Google search based)")
# 
# ggplot(opa_day, aes(x = p_season, y = pibr, color = p_season)) + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3, width = 0.2) + theme_few() + 
#   ylab("asthma-related ED visits in Travis County (per 10,000 people)") + xlab("") +
#   scale_color_discrete(name = "pollen season (NAB based)")
# 
# #as a function of cold season
# ggplot(opa_day, aes(x = date, y = n_cases, color = cold)) + geom_point(alpha = 0.5) + theme_few() + 
#   ylab("number of asthma-related ED visits in Travis County") + scale_color_viridis_d(name = "cold season") +
#   scale_x_date(breaks = scales::pretty_breaks(n = 20)) + 
#   geom_line(aes(x = date, y=rollmean(n_cases, 14, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)  #geom_smooth(method = "loess") +
# 
# ggplot(opa_day, aes(x = cold, y = pibr, color = cold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3, width = 0.2) + theme_few() + 
#   ylab("asthma-related ED visits in Travis County (per 10,000 people)") + xlab("") +
#   scale_color_discrete(name = "cold season")
# 
# 
# #as a function of flu season, based off of manually looking at CDC flu reports for TX from these time periods
# ggplot(opa_day, aes(x = date, y = n_cases, color = flu)) + geom_point(alpha = 0.5) + theme_few() + 
#   ylab("number of asthma-related ED visits in Travis County") + scale_color_viridis_d(name = "CDC based flu season") +
#   scale_x_date(breaks = scales::pretty_breaks(n = 20))  
  
  
# ### the figure that EM wanted:
# opa_day$nothing <- "none"
# opa_day$nothing[opa_day$p_season == "none" & opa_day$flu == "none" & opa_day$cold == "none"] <- "low seasonal triggers"
# d0 <- opa_day %>% mutate(var = "all dates") %>% group_by(var) %>% summarize(pibr_mean = mean(pibr), pibr_sd = sd(pibr))  
# d1 <- opa_day %>% mutate(var = p_season) %>% group_by(var) %>% summarize(pibr_mean = mean(pibr), pibr_sd = sd(pibr))  
# d2 <- opa_day %>% mutate(var = g_season) %>% group_by(var) %>% summarize(pibr_mean = mean(pibr), pibr_sd = sd(pibr))  
# d3 <- opa_day %>% mutate(var = flu) %>% group_by(var) %>% summarize(pibr_mean = mean(pibr), pibr_sd = sd(pibr))  
# d4 <- opa_day %>% mutate(var = cold) %>% group_by(var) %>% summarize(pibr_mean = mean(pibr), pibr_sd = sd(pibr))  
# d5 <- opa_day %>% mutate(var = nothing) %>% group_by(var) %>% summarize(pibr_mean = mean(pibr), pibr_sd = sd(pibr))  
# df <- bind_rows(d0, d1, d3, d4, d5) %>% filter(var != "none")  
# 
# #with visits per 100,000 person years
# opa_day$nothing <- "none"
# opa_day$nothing[opa_day$p_season == "none" & opa_day$flu == "none" & opa_day$cold == "none"] <- "low seasonal triggers"
# d0 <- opa_day %>% mutate(var = "all dates") %>% group_by(var) %>% summarize(pbir_py_mean = mean(pbir_py), pbir_py_sd = sd(pbir_py))  
# d1 <- opa_day %>% mutate(var = p_season) %>% group_by(var) %>% summarize(pbir_py_mean = mean(pbir_py), pbir_py_sd = sd(pbir_py))  
# d2 <- opa_day %>% mutate(var = g_season) %>% group_by(var) %>% summarize(pbir_py_mean = mean(pbir_py), pbir_py_sd = sd(pbir_py))  
# d3 <- opa_day %>% mutate(var = flu) %>% group_by(var) %>% summarize(pbir_py_mean = mean(pbir_py), pbir_py_sd = sd(pbir_py))  
# d4 <- opa_day %>% mutate(var = cold) %>% group_by(var) %>% summarize(pbir_py_mean = mean(pbir_py), pbir_py_sd = sd(pbir_py))  
# d5 <- opa_day %>% mutate(var = nothing) %>% group_by(var) %>% summarize(pbir_py_mean = mean(pbir_py), pbir_py_sd = sd(pbir_py))  
# df <- bind_rows(d0, d1, d3, d4, d5) %>% filter(var != "none")  
# 
# ggplot(df, aes(x = var, y = pibr_mean, ymax = pibr_mean + pibr_sd, ymin = pibr_mean - pibr_sd)) + geom_errorbar(width = 0.1) + geom_point() + theme_few()+
#   xlab("seasonal asthma triggers") + ylab("asthma-related ED visits in Travis County (per 10,000; mean + SD)") + coord_cartesian(ylim = c(0, 0.18))

opa_day$pbir_py 

opa_day %>% group_by(p_season) %>%
  summarize(pbir_child_mean = mean(pbir_child))

test <- opa_day %>% group_by(county_name) %>% 
                    mutate(rhino_child_ranking = percent_rank(rhino_child),
                           rhino_child_high = "low")
test$rhino_child_high[test$rhino_child_ranking > 0.90] <- "hi" 
test %>% group_by(rhino_child_high) %>%
  summarize(pbir_child_mean = mean(pbir_child))

test <- opa_day %>% mutate(flu_d_ranking = percent_rank(flu_d),
                           flu_d_high = "low")
test$flu_d_high[test$flu_d_ranking > 0.90] <- "hi" 
test %>% group_by(flu_d_high) %>%
  summarize(pbir_child_mean = mean(pbir_child))

test <- opa_day %>% group_by(county_name) %>%
  mutate(cupr_ranking = percent_rank(cupr),
                           cupr_high = "low")
test$cupr_high[test$cupr_ranking > 0.90] <- "hi" 
test %>% group_by(cupr_high) %>%
  summarize(pbir_child_mean = mean(pbir_child))


### more detailed exploration ###########################################

ggplot(opa_day, aes(x = date, y = pbir_py, color = hits_day_adj)) + geom_point(alpha = 0.9, size = 2) + theme_few() + 
  ylab("asthma-related ED visits per 100,000 person years in Travis County") + 
  scale_color_viridis_c(name = "Google searches for 'pollen'", limits = c(0, 30)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 20), limits = as.Date(c('2016-02-14','2016-05-01'))) + 
  geom_line(aes(x = date, y=rollmean(pbir_py, 7, na.pad=TRUE, color = "gray")), inherit.aes = FALSE) +
  annotate("segment", x = ymd("2016-02-14"), xend = ymd("2016-04-24"), y = 8, yend = 8, color = "red") + 
  annotate("segment", x = ymd("2016-03-26"), xend = ymd("2016-04-25"), y = 7.8, yend = 7.8, color = "blue")

ggplot(opa_day, aes(x = date, y = pbir_py, color = hits_day_adj)) + geom_point(alpha = 0.9, size = 2) + theme_few() + 
  ylab("asthma-related ED visits per 100,000 person years in Travis County") + 
  scale_color_viridis_c(name = "Google searches for 'pollen'", limits = c(0, 20)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 20), limits = as.Date(c('2016-03-25','2016-04-25'))) + 
  geom_line(aes(x = date, y=rollmean(pbir_py, 7, na.pad=TRUE, color = "gray")), inherit.aes = FALSE) +
  annotate("segment", x = ymd("2016-02-14"), xend = ymd("2016-04-24"), y = 8, yend = 8, color = "red") + 
  annotate("segment", x = ymd("2016-03-26"), xend = ymd("2016-04-25"), y = 7.8, yend = 7.8, color = "blue")


names(opa_day)
filter(opa_day, county_name == "Dallas") %>% #, 
     #  doy > 350 | doy < 50) %>%
ggplot(aes(x = date, y = pbir_child, color = gtrend_common_cold)) + geom_point(alpha = 0.9, size = 2) + theme_few() +
  #ylab("asthma-related ED visits per 100,000 person years in Travis County") +
  ylab("asthma-related ED visits per 10,000 people per day") + 
  #ylab("rhino virus modeled incidence") + 
  scale_color_viridis_c() + #limits = c(0, 150) name = "var"
  scale_x_date(breaks = scales::pretty_breaks(n = 20)) + #, limits = as.Date(c('2016-02-14','2016-05-01'))) +
  geom_line(aes(x = date, y=rollmean(pbir_child, 14, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)

# ggplot(aes(x = cupr + 0.3, y = pbir_child, color = rhino_child)) + geom_jitter() + theme_few() + geom_smooth(method = "lm", se = FALSE) + scale_x_log10() +
#   ylab("asthma-related ED visits per 10,000 people per day") + xlab("Cupressaceae pollen grains/m3") + scale_color_viridis_c()

ggplot(opa_day, aes(x = date, y = pbir_py, color = log(flu_d))) + geom_point(alpha = 0.9, size = 2) + theme_few() + 
  ylab("asthma-related ED visits per 100,000 person years in Travis County") + 
  scale_color_viridis_c() + #limits = c(0, 150) name = "var"
  scale_x_date(breaks = scales::pretty_breaks(n = 20)) + #, limits = as.Date(c('2016-02-14','2016-05-01'))) + 
  geom_line(aes(x = date, y=rollmean(pbir_py, 7, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)



ggplot(opa_day, aes(x = hits_day_adj, y = n_cases, color = doy)) + geom_point() + theme_bw() + scale_color_viridis_c() + geom_smooth()
ggplot(opa_day, aes(x = flu_d, y = n_cases, color = doy)) + geom_point() + theme_bw() + scale_color_viridis_c() + geom_smooth()
ggplot(opa_day, aes(x = tot_pol, y = pbir_child, color = doy)) + geom_point() + theme_bw() + scale_color_viridis_c() + geom_smooth() + scale_x_log10() +
  facet_wrap(~county_name)

# names(opa_day)
# ggplot(opa_day, aes(x = date, y = n_cases, color = log(hits_day_adj + 1))) + geom_point(alpha = 0.9, size = 2) + theme_few() + 
#   ylab("asthma-related ED visits per 100,000 person years in Travis County") + 
#   scale_color_viridis_c() + #limits = c(0, 150) name = "var"
#   scale_x_date(breaks = scales::pretty_breaks(n = 20)) + #, limits = as.Date(c('2016-02-14','2016-05-01'))) + 
#   geom_line(aes(x = date, y=rollmean(n_cases, 7, na.pad=TRUE, color = "gray")), inherit.aes = FALSE)
# 


 

### some preliminary analysis #####################################################################
head(opa_day) #unique(opa_day$county_name) #unique(opa_day$NAB_station)
names(opa_day)
test <- filter(opa_day, NAB_station == "San Antonio A" | NAB_station == "San Antonio B" ) #,  doy > 350 | doy < 50)
#test <- opa_day
fit <- glm(n_cases ~  
             log10(tot_pol_m21 - cupr_m21 + 1) + 
             #log10(cupr + 1) +
             log10(cupr_m21 + 1)  +
             #log10(cupr_m14 + 1)  +
             #log10(cupr_m14 + 1) +
             #log10(cupr_m21 + 1) +
             #log10(cupr_m28 + 1) +
             #log10(cupr_m35 + 1) +
             # log10(tot_pol - cupr + 1) +   
             # log10(cupr + 1) +
             # log10(cupr_l1 + 1) +
             # log10(cupr_l2 + 1) +
             # log10(cupr_l3 + 1) +
             # log10(cupr_l4 + 1) +
             # log10(cupr_l5 + 1) +
             # log10(cupr_l6 + 1) +
             # log10(cupr_l7 + 1) +
             # log10(cupr_l8 + 1) +
             # log10(cupr_l9 + 1) +
             # log10(cupr_l10+ 1) +
             # log10(cupr_l11+ 1) +
             # log10(cupr_l12+ 1) +
             # log10(cupr_l13+ 1) +
             # log10(cupr_l14+ 1) +
             # log10(cupr_l15+ 1) +
             # log10(cupr_l16+ 1) +
             # log10(cupr_l17+ 1) +
             # log10(cupr_l18+ 1) +
             # log10(cupr_l19+ 1) +
             # log10(cupr_l20+ 1) +
             # log10(cupr_l21+ 1) +
             v_tests_pos_RSV + v_tests_pos_Rhinovirus + v_tests_pos_Corona 
             , data = test, family = "poisson")

summary(fit)

data_for_model <- filter(opa_day, !is.na(n_cases)) %>% 
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



names(data_for_model)
fit <- glm((pbir_child + 0.001) ~  cupr + #* county_name +
                                  Ambrosia + # *county_name + 
                                  Quercus  +#* county_name + 
                                  flu_d  +
                                  county_name, #* county_name, 
            data = data_for_model, family = Gamma(link = "log"))
summary(fit)

fit <- glm(log(pbir_child + 0.001) ~  cupr +  #* county_name +
             Ambrosia + # *county_name + 
             Quercus  +#* county_name + 
             flu_d  +
             county_name, #* county_name, 
           data = opa_day, family = "gaussian")
summary(fit)
hist((opa_day$pbir_child))

par(mfrow=c(1,1))
hist(log(opa_day$pbir_child))
t.test(opa_day$pbir_py[opa_day$p_season == "J. ashei"], opa_day$pbir_py[opa_day$flu == "flu"])
t.test(opa_day$pbir_py[opa_day$p_season == "J. ashei"], opa_day$pbir_py[opa_day$cold == "cold"])

t.test(opa_day$pbir_py[opa_day$p_season == "spring trees"], opa_day$pbir_py[opa_day$flu == "flu"])
t.test(opa_day$pbir_py[opa_day$p_season == "spring trees"], opa_day$pbir_py[opa_day$cold == "cold"])

t.test(opa_day$pbir_py[opa_day$p_season == "J. ashei"], opa_day$pbir_py[opa_day$p_season == "spring trees"])


names(opa_day)
opa_day %>% group_by(county_name) %>%
  summarize(oak = mean(Quercus),
            rag = mean(Ambrosia),
            cupr = mean(cupr))
t.test(opa_day$cupr[opa_day$county_name == "Bexar"], opa_day$cupr[opa_day$county_name == "Dallas"])


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




#### analysis of a single station with a distributed lags model using dlm package #######################################
library(dlnm)
library(splines)

#str(data_for_model)
data_for_model <- opa_day %>%
  filter(date > ymd("2015 - 11 - 01")) %>% # & date < ymd("2016 - 05 - 01")) %>%
  #filter(NAB_station == "San Antonio A") %>% #unique(opa_day$NAB_station)
    mutate(
         n_cases_s = scale(n_cases),
         log_child_pop = log(children_pop),
         child_pop = children_pop,
         NAB_station_n = as.numeric(as.factor(NAB_station)),
         v_tests_pos_Rhinoviruss = scale(v_tests_pos_Rhinovirus),
         v_tests_pos_RSVs = scale(v_tests_pos_RSV),
         v_tests_pos_Coronas = scale(v_tests_pos_Corona),
         flu_ds = scale(flu_d),
         ja_l = log10(ja + 0.9),
         ja_lm = case_when(is.na(ja_l) ~ ja_rfint_log_mean,
                           !is.na(ja_l) ~ ja_l), #hist(data_for_model$ja_lm)
         ja_lms = scale(ja_lm),
         cup_other_l = log10(cup_other + 0.9),
         cup_other_lm = case_when(is.na(cup_other_l) ~ cup_other_rfint_log_mean,
                           !is.na(cup_other_l) ~ cup_other_l),
         cup_other_lms = scale(cup_other_lm),
         cup_all_l = log10(ja + cup_other + 0.9),
         #cup_all_lm = ja_lm + cup_other_lm,
         cup_all_ls = scale(cup_all_l),
         pol_tot_ls = scale(log10(ja + cup_other + trees + pol_other + 0.9)),
         trees_l = log10(trees + 0.9),
         trees_lm = case_when(is.na(trees_l) ~ trees_rfint_log_mean,
                           !is.na(trees_l) ~ trees_l),
         trees_lms = scale(trees_lm),
         not_cup_ls = scale(log10(pol_other + trees + 0.9)),
         cup_ls = scale(log10(ja + cup_other + 0.9)),
         pol_other_l = log10(pol_other + 0.9),
         pol_other_lm = case_when(is.na(pol_other_l) ~ pol_other_rfint_log_mean,
                           !is.na(pol_other_l) ~ pol_other_l),
         pol_other_lms = scale(pol_other_lm)) %>%
    dplyr::select(NAB_station, date, n_cases, n_cases_s, pbir, child_pop, log_child_pop,
                week_day,
                ja_lms, ja_lm, 
                cup_other_lms, cup_other_lm,
                cup_all_l, cup_all_ls,
                trees_lms, trees_lm,
                pol_other_lms, pol_other_lm,
                not_cup_ls, cup_ls, pol_tot_ls,
                v_tests_pos_Rhinovirus,
                v_tests_pos_RSV,
                v_tests_pos_Corona,
                flu_d, 
                v_tests_pos_Rhinoviruss, v_tests_pos_RSVs, v_tests_pos_Coronas, flu_ds) %>%
  arrange(NAB_station, date) %>%
  filter(complete.cases(.)) %>% 
  filter(NAB_station != "Waco A",
         NAB_station != "Waco B",
         NAB_station != "College Station") %>% 
  group_by(NAB_station) %>% 
  mutate(time = row_number())

ja_min <- min(data_for_model$ja_lms)
# hist(data_for_model$ja_lms)
# 

lag_length  <- 21
poly_degree <- 5

#polynomial distributed lag
cup_all_lag <- crossbasis(data_for_model$ja_lm, lag = 21,
                    argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 3))
trees_lag <- crossbasis(data_for_model$trees_lm, lag = 21, 
                            argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 4))
pol_other_lag <- crossbasis(data_for_model$pol_other_lm, lag = 10, 
                            argvar=list(fun = "lin"), arglag = list(fun = "poly", degree = 3))

model1 <- glm(n_cases ~ NAB_station + 
                offset(log(child_pop)) + 
                cup_all_lag +  trees_lag + pol_other_lag + 
                v_tests_pos_Rhinoviruss + v_tests_pos_RSVs + v_tests_pos_Coronas + flu_ds + 
                week_day + 
                ns(time, 30),
                family = quasipoisson, data = data_for_model)
summary(model1)


#cup all hist(data_for_model$ja_lm)
pred1.pm <- crosspred(cup_all_lag,  model1, at = 1:4, bylag = 0.2, cen = 0, cumul = TRUE)
plot(pred1.pm, "slices", var=1,  main="Association with a 10x increase in independent variable", ylab = "RR")
plot(pred1.pm, "slices", var=1, cumul = TRUE,  main="Association with a 10x increase in independent variable", ylab = "cumulative RR")

#trees
pred1.pm <- crosspred(trees_lag,  model1, at = 1, bylag = 0.2, cen = 0, cumul = TRUE)
plot(pred1.pm, "slices", var=1,  main="Association with a 10x increase in independent variable", ylab = "RR")
plot(pred1.pm, "slices", var=1, cumul = TRUE,  main="Association with a 10x increase in independent variable", ylab = "cumulative RR")

#pol_other
pred1.pm <- crosspred(pol_other_lag,  model1, at = 1, bylag = 0.2, cen = 0, cumul = TRUE)
plot(pred1.pm, "slices", var=1,  main="Association with a 10x increase in independent variable", ylab = "RR")
plot(pred1.pm, "slices", var=1, cumul = TRUE,  main="Association with a 10x increase in independent variable", ylab = "cumulative RR")


### model diagnotistic plots


#deviance residuals over time
data_for_model %>% 
  ungroup() %>% 
  mutate(resid = c(rep(NA, 30), residuals(model1, type = "deviance"))) %>% 
  ggplot(aes(x = date, y = resid)) + theme_bw() +
  geom_point() + facet_wrap(~NAB_station) + ylab("Deviance residuals")
  

#partial autocorrelation plots of the deviance residuals
pacf(residuals(model1, type = "deviance"), na.action=na.omit,main="From original model")

# INCLUDE THE 1-DAY LAGGED RESIDUAL IN THE MODEL
resid_model1 <- c(rep(NA, 30), residuals(model1, type = "deviance"))
model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1) + + tsModel::Lag(resid_model1, 2))
pacf(residuals(model2, type = "deviance"), na.action=na.omit,main="From model adjusted for residual autocorrelation")

summary(model1)
summary(model2)

#partial autocorrelation plots of the deviance residuals
pacf(res7,na.action=na.omit,main="From original model")

# INCLUDE THE 1-DAY LAGGED RESIDUAL IN THE MODEL
model9 <- update(model7,.~.+Lag(res7,1))

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

