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
library(imputeTS)


rm(list = ls())

### options for different data subsets

# distance cutoff from NAB station
NAB_min_dist_threshold <- 25

# define target age range here 
age_low <- 18 # >=
age_hi <- 99 # <=

### load in NAB data and do linear interpolation #####################################################
#before 3/15/21 this was a separate script here: ~/TX_epi/NAB_missing_data.R
# I originally used random forest instead of linear imputation, but the results were only a little better
# and it was much more difficult to explain, so I switched to linear imputation
NAB_locations <- read_csv("C:/Users/dsk856/Desktop/misc_data/EPHT_Pollen Station Inventory_062019_dk200331.csv") %>% 
  filter(Station_name != "Family Allergy & Asthma Care") #prevent duplication of Flower Mound
NAB <- read_csv("C:/Users/dsk856/Desktop/misc_data/NAB2009_2019_pollen_200508.csv", guess_max = 92013)

#QA/QC
#NAB <- NAB %>% 
  #filter(file != "106- Waco (2)_resaved.xls") #Removing Waco B 
  # There appear to be substantial issues with non-measured days being recorded as 0s and with Ulmus not being distinguished from
  # unidentified pollen in 2009 and 2016.
  # I'm also deeply skeptical of how clean the data are - there is so little variablility compared to all other stations
  
  # #Quality control for Waco A, Cupressaceae seems to have been mistakenly entered as Ambrosia for a season
  # mutate( Cupressaceae = case_when(NAB_station == "Waco A" & Cupressaceae == 0 &
  #                                    date > mdy("12/11/2009") & date < mdy("2/20/2010") ~ Ambrosia, TRUE ~ Cupressaceae),
  #         Ambrosia = case_when(NAB_station == "Waco A"  &
  #                                date > mdy("12/11/2009") & date < mdy("2/20/2010") ~ 0, TRUE ~ Ambrosia)) 

NAB_locations_first_join <- NAB_locations %>% dplyr::select(NAB_station, file, State)

NAB_tx_pol <- left_join(NAB, NAB_locations_first_join) %>%
  filter(State == "TX") %>%
  filter(!is.na(NAB_station)) %>% 
  mutate(date = ymd(Date)) %>%
  rowwise()%>%
  mutate(Fagales = sum(Alnus, Betula, Carpinus.Ostrya, Corylus, Quercus, Fagus, Myrica, na.rm = TRUE),
         other_trees = sum(Acer, Fraxinus, Juglans, Liquidambar, Other.Tree.Pollen, Pinaceae, Populus, Pseudotsuga, Salix, 
                           Carya, Ligustrum, Olea, Platanus, Tilia, Celtis, Tsuga, Prosopis, na.rm = TRUE),
         trees = sum(Morus, Ulmus, Fagales, other_trees, na.rm = TRUE), 
         grass = sum(Gramineae...Poaceae, Other.Grass.Pollen , na.rm = TRUE),
         herbaceous = sum( Artemisia, Asteraceae..Excluding.Ambrosia.and.Artemisia., Chenopodiaceae.Amaranthaceae,
                            Other.Weed.Pollen, Plantago, Rumex,  Arecaceae, Cyperaceae, Typha,
                            Eupatorium, Urticaceae, na.rm = TRUE ),
         pol_other = sum(grass, Ambrosia, Unidentified.Pollen, herbaceous, na.rm = TRUE),
         
         #log10 transform final selected categories
         cup_log10 = log10(Cupressaceae + 1),
         trees_log10 = log10(trees + 1),
         pol_other_log10 = log10(pol_other + 1)
         
         #a quick check to make sure that I'm not losing any pollen columns in this section
         #test_pol = Total.Pollen.Count - Cupressaceae - trees - pol_other #this check shows my math is fine, but there are ~10 rows
         #...  where the Total.Pollen.Count column appears to have incorrect sums (based on manual inspection of original .xls files)
         ) %>% 
  ungroup() %>% 
  #arrange(desc(test_pol)) #for manual check, described in lines just above
  dplyr::select(NAB_station, date, #mo, doy, year, #City, FIPS, county_name, MSA, Lat, Long, NAB_station,
                Cupressaceae, trees, pol_other,
                cup_log10, trees_log10, pol_other_log10) 

#expand to include missing dates
date_station_grid <- expand_grid(seq(min(NAB$date),max(NAB$date), by = '1 day'), 
                                 unique(NAB_tx_pol$NAB_station)) %>% 
  `colnames<-`(c("date", "NAB_station")) %>%
  filter(!is.na(NAB_station)) %>% 
  ungroup()

NAB_tx_pol_full <- left_join(date_station_grid, NAB_tx_pol) %>%
  arrange(NAB_station, date)

NAB_locations_second_join <- NAB_locations %>% dplyr::select(NAB_station, Lat, Long, FIPS, MSA, county_name, ZIP, City)
NAB_tx_pol_full <- left_join(NAB_tx_pol_full, NAB_locations_second_join) %>% 
  mutate(mo = month(date),
         doy = yday(date),
         year = year(date),
        FIPS = sprintf("%03s",FIPS), #NAB_tx_pol$FIPS
        FIPS = sub(" ", "0",FIPS),
        FIPS = sub(" ", "0",FIPS))


### simple linear interpolation 
# using a random forest is slightly better but far more complicated and I don't think it's worth it because it is just so much more complicated to explain
# the marginal benefit is probably an R2 improvement of ~0.02 
# so, as of 10/2/20, I'm going to switch over to just a linear interpolation:


#linear interpolation of pollen data
NAB_tx <- NAB_tx_pol_full %>% 
  group_by(NAB_station) %>% 
  mutate(
    mo = month(date),
    doy = yday(date),
    year = year(date),
    year_f = paste0("y_", year(date)),
    
    cup_all_m = na_interpolation(Cupressaceae),
    cup_all_lm = na_interpolation(cup_log10),
    trees_m = na_interpolation(trees),
    trees_lm = na_interpolation(trees_log10),
    pol_other_m = na_interpolation(pol_other),
    pol_other_lm = na_interpolation(pol_other_log10)
  ) 

#check on data
NAB_tx %>% 
  #filter(NAB_station == "College Station") %>% 
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
  ggplot(aes(x = date, y = cup_all_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
  geom_point(aes(x = date, y = cup_log10), color = "black")

NAB_tx %>% 
  #filter(NAB_station == "College Station") %>% 
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
  ggplot(aes(x = date, y = trees_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
  geom_point(aes(x = date, y = trees_log10), color = "black")

NAB_tx %>% 
  #filter(NAB_station == "College Station") %>% 
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
  ggplot(aes(x = date, y = pol_other_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
  geom_point(aes(x = date, y = pol_other_log10), color = "black")


### assess how good of a job the linear interpolation does ######################
#select a portion of the data to withhold 
NAB_tx_withheld <- NAB_tx %>%  #str(NAB_tx)
  ungroup() %>%  #get rid of rowwise
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
  sample_frac(0.1) %>% #withold 10% of data
  dplyr::select(date, NAB_station) %>% 
  mutate(withheld = 1)
NAB_tx_mt <- left_join(NAB_tx, NAB_tx_withheld) %>%  #str(NAB_tx_mt)
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
  mutate(#withheld2 = case_when(withheld == 1 ~ withheld, is.na(withheld) ~ 0),
    cup_withheld = case_when(is.na(withheld) ~ cup_log10), #by not specifying a value, it defaults to NA
    trees_withheld = case_when(is.na(withheld) ~ trees_log10),
    pol_other_withheld = case_when(is.na(withheld) ~ pol_other_log10)
  )

#linear interpolation of time series that had extra data removed
NAB_tx_mt <- NAB_tx_mt %>% 
  group_by(NAB_station) %>% 
  mutate(
    cup_withheld_lm = na_interpolation(cup_withheld),
    trees_withheld_lm = na_interpolation(trees_withheld),
    pol_other_withheld_lm = na_interpolation(pol_other_withheld))

NAB_tx_mtc <- filter(NAB_tx_mt, withheld == 1) 

#compare withheld data with linear interp estimates
cup_mtc_fit <- lm(NAB_tx_mtc$cup_log10 ~ NAB_tx_mtc$cup_withheld_lm )
sqrt(mean(cup_mtc_fit$residuals^2)); summary(cup_mtc_fit) #RMSE and R2
cup_panel <- ggplot(NAB_tx_mtc, aes(x= 10^(cup_withheld_lm) - 1, y = 10^(cup_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000)) 

trees_mtc_fit <- lm(NAB_tx_mtc$trees_log10 ~ NAB_tx_mtc$trees_withheld_lm)
sqrt(mean(trees_mtc_fit$residuals^2)); summary(trees_mtc_fit) #RMSE and R2
tree_panel <- ggplot(NAB_tx_mtc, aes(x= 10^(trees_withheld_lm) - 1, y = 10^(trees_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000)) 

pol_other_mtc_fit <- lm(NAB_tx_mtc$pol_other_log10 ~ NAB_tx_mtc$pol_other_withheld_lm)
sqrt(mean(pol_other_mtc_fit$residuals^2)); summary(pol_other_mtc_fit) #RMSE and R2
other_panel <- ggplot(NAB_tx_mtc, aes(x= 10^(pol_other_withheld_lm) - 1, y = 10^(pol_other_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
  scale_x_log10(limits = c(1, 500)) + scale_y_log10(limits = c(1, 500)) 

cowplot::plot_grid(cup_panel, tree_panel, other_panel, labels = c("A", "B", "C"))




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
NAB_station_lookup <- data.frame(NAB_station = NAB_tx_sf$NAB_station, n_lookup = 1:length(unique(NAB_tx_sf$NAB_station)))
station_looked_up <- left_join(data.frame(n_lookup = which_station_closest), NAB_station_lookup)
opa_raw_sf <- mutate(opa_raw_sf, NAB_min_dist = distances_min, NAB_station = station_looked_up$NAB_station)
NAB_dist_opa_join <- opa_raw_sf 
NAB_dist_opa_join$geometry <- NULL

opa_raw <- left_join(opa_raw, NAB_dist_opa_join) 

### filter asthma ED visits where residence was within 25 km of an NAB station ###############################################
#NAB_min_dist_threshold <- 25 moved to top of script
opa <- opa_raw %>%
        filter(NAB_min_dist < NAB_min_dist_threshold) %>% #restrict cases to patients whose residence was within 25 km of a station
        #filter(opa_raw, PAT_COUNTY %in% NAB_counties)  %>% #Travis county number is 453
        mutate(PAT_AGE_YEARS = as.numeric(PAT_AGE_YEARS), 
               PAT_AGE_DAYS = as.numeric(PAT_AGE_DAYS)) %>%
        dplyr::select(SEX_CODE, PAT_ZIP, PAT_AGE_YEARS, PAT_AGE_DAYS, RACE, ETHNICITY, PRINC_DIAG_CODE, PAT_ADDR_CENSUS_BLOCK_GROUP, PAT_ADDR_CENSUS_BLOCK,
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
# AllF <- v17$name[29:51]9
# All_vars <- c(AllM, AllF)

# ### define target age range here #moved to top of script
# age_low <- 0 # >=
# age_hi <- 4 # <=

#Variables that I want: ages 5-17  #B01001_003
c_vars <- c("B01001_004", "B01001_005", "B01001_006", #males from 5-17 years old
            "B01001_028", "B01001_029", "B01001_030") #females from 5-17 years old
c_vars_adult <- c(paste0("B0100", 1007:1025), #males
                  paste0("B0100", 1031:1049)) %>%  #females
                  gsub(pattern = "B01001", replacement ="B01001_", x = .) #adding the underscore back in
c_vars_agegroup_x <- c(paste0("B0100", 1004:1006), #males 0 - 4: 1003; 5 - 17: 1004:1006, 18 +: 1007:1025
                    paste0("B0100", 1028:1030)) %>%  #females 0 - 4: 1027; 5 - 17: 1028:1030, 18 +: 1031:1049
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

# #a check to compare my numbers with RAZ's 
# census_All_2017_sf %>%
#   filter(grepl("Travis County, Texas", NAME)) %>% 
#   filter(variable %in% c_vars_agegroup_x) %>%  
#   summarize(adult_pop = sum(estimate)) #names(pop_near_NAB)


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

# virus <- read_csv("C:/Users/dsk856/Desktop/misc_data/virus_2015_2017_daily_adj.csv")
# 
# #linear interpolation of virus data
# virus <- virus %>% 
#   mutate(v_tests_pos_Rhinovirus_ms = as.numeric(scale(na_interpolation(v_tests_pos_Rhinovirus))),
#          v_tests_pos_RSV_ms = as.numeric(scale(na_interpolation(v_tests_pos_RSV))),
#          v_tests_pos_Corona_ms = as.numeric(scale(na_interpolation(v_tests_pos_Corona))),
#          v_tests_perc_pos_Rhinovirus_ms = as.numeric(scale(na_interpolation(v_tests_perc_pos_Rhinovirus))),
#          v_tests_perc_pos_RSV_ms = as.numeric(scale(na_interpolation(v_tests_perc_pos_RSV))),
#          v_tests_perc_pos_Corona_ms = as.numeric(scale(na_interpolation(v_tests_perc_pos_Corona))),
#          v_tests_perc_pos_Rhinovirus_m = na_interpolation(v_tests_perc_pos_Rhinovirus),
#          v_tests_perc_pos_RSV_m = na_interpolation(v_tests_perc_pos_RSV),
#          v_tests_perc_pos_Corona_m = na_interpolation(v_tests_perc_pos_Corona),
#          v_tests_adj_pos_Rhinovirus_m = na_interpolation(adj_pos_Rhinovirus),
#          v_tests_adj_pos_RSV_m = na_interpolation(adj_pos_RSV),
#          v_tests_adj_pos_Corona_m = na_interpolation(adj_pos_Corona))
# #ggplot(virus, aes(x = date, y = v_tests_adj_pos_Rhinovirus_m)) + geom_step() 

v <- read_csv("C:/Users/dsk856/Desktop/misc_data/TX NREVSS1516-1718.csv")
vs <- v %>% 
  mutate(dates = mdy(WeekEnding)) %>% 
  group_by(HSR_REGION, dates) %>% 
  summarize(RSV_pos = sum(RSVA_POS, RSVB_POS, RSVUNK_POS),
            RSV_tests = sum(RSV_TEST),
            RSV_pos_prop = RSV_pos/RSV_tests,
            CoV_pos = sum(CovNL63Pos, CovOC43Pos, CoV229EPos, CoVUnkPos, na.rm = TRUE),
            CoV_tests = sum(CoVTest),
            CoV_pos_prop = CoV_pos/CoV_tests,
            Rhino_pos = sum(RhinoPos),
            Rhino_tests = sum(RhinoTest),
            Rhino_pos_prop = Rhino_pos/Rhino_tests) %>% 
  filter(HSR_REGION == "PHR 2/3" | HSR_REGION == "PHR 7" |HSR_REGION == "PHR 8" |HSR_REGION == "PHR 6/5S")

NAB_stations_join <- data.frame(NAB_station = c("San Antonio A", "San Antonio B", "Georgetown", "Waco A", "Waco B",
                                                "College Station", "Houston", "Dallas","Flower Mound"), 
                                HSR_REGION = c("PHR 8", "PHR 8", "PHR 7", "PHR 7","PHR 7",
                                               "PHR 7", "PHR 6/5S", "PHR 2/3", "PHR 2/3"))
vs2 <- left_join(NAB_stations_join, vs)

#what about Houston corona virus across all years? 
#Using the statewide numbers for Houston since it seems they didn't do any testing in Houston.
#Another approach might be to take average of cities within a certain distance of Houston
vs2 <- v %>% 
  mutate(dates = mdy(WeekEnding)) %>% 
  group_by(dates) %>% 
  summarize(CoV_pos_tx = sum(CovNL63Pos, CovOC43Pos, CoV229EPos, CoVUnkPos, na.rm = TRUE),
            CoV_tests_tx = sum(CoVTest),
            CoV_pos_prop_tx = CoV_pos_tx/CoV_tests_tx) %>% 
  mutate(NAB_station = "Houston") %>% 
  left_join(vs2, .) %>% 
  mutate(CoV_pos = case_when(NAB_station == "Houston" ~ CoV_pos_tx,
                             NAB_station != "Houston" ~ CoV_pos),
         CoV_tests = case_when(NAB_station == "Houston" ~ CoV_tests_tx,
                             NAB_station != "Houston" ~ CoV_tests),
         CoV_pos_prop = case_when(NAB_station == "Houston" ~ CoV_pos_prop_tx,
                             NAB_station != "Houston" ~ CoV_pos_prop)) %>% 
  dplyr::select(-c(CoV_pos_tx, CoV_tests_tx, CoV_pos_prop_tx))

# vs2 %>%
#   ggplot(aes(x = dates, y = CoV_tests, color = NAB_station)) + geom_line() + theme_bw() + ylab("RSV tests (proportion positive)")
 #facet_wrap(~NAB_station)

flu_raw <- read_csv("C:/Users/dsk856/Desktop/misc_data/TX NREVSS_Flu_1516-1718.csv") %>% 
  mutate(dates = mdy(WeekEnding)) %>% 
  mutate(NAB_region = case_when(CITY == "Dallas" ~ "Dallas/FlowerMound", 
                                CITY == "Mesquite" ~ "Dallas/FlowerMound", 
                                CITY == "Fort Worth" ~ "Dallas/FlowerMound", 
                                CITY == "Mexia" ~ "Waco", 
                                CITY == "Temple" ~ "Waco", 
                                CITY == "Huntsville" ~ "College Station", 
                                CITY == "Bryan" ~ "College Station", 
                                CITY == "San Antonio" ~ "San Antonio", 
                                CITY == "Austin" ~ "Georgetown", 
                                CITY == "Houston" ~ "Houston",
                                TRUE ~ "other city")) 
#unique(flu_raw$CITY)
flu <-  flu_raw %>% group_by(NAB_region, dates) %>% arrange(NAB_region, dates) %>% 
  summarize(flu_tests = sum(FLU_TEST),
            flu_pos = sum(FluPanAH1N1Pos, AH3N2POS, AUNK_POS, FLUB_POS, na.rm = TRUE),
            flu_pos_prop = sum(FluPanAH1N1Pos, AH3N2POS, AUNK_POS, FLUB_POS, na.rm = TRUE)/flu_tests) 
# flu %>%   
# group_by(NAB_region) %>% summarize(flu_sum2 = sum(flu_sum)) %>% print(n = 23)
#ggplot(aes(x = dates, y = flu_sum, color = NAB_region)) + geom_line() + theme_bw() + ylab("RSV tests (proportion positive)") 

flu_city <- data.frame(NAB_station = c("San Antonio A", "San Antonio B", "Georgetown", "Waco A", "Waco B",
                                       "College Station", "Houston", "Dallas","Flower Mound"), 
                       NAB_region = c("San Antonio", "San Antonio", "Georgetown", "Waco","Waco",
                                      "College Station", "Houston", "Dallas/FlowerMound", "Dallas/FlowerMound"))

flus <-  left_join(flu_city, flu)
nrevss_data <- left_join(vs2, flus)
nrevss_data %>% 
  ggplot(aes(x = dates, y = flu_pos, color = NAB_station)) + geom_line() + theme_bw() + 
  facet_wrap(~NAB_station) #ylab("RSV tests (proportion positive)") + 

nrevss_sum <- nrevss_data %>% group_by(NAB_station) %>% 
            #total number of positive tests at each site
  summarize(NAB_station_flu_all_pos = sum(flu_pos, na.rm = TRUE),
            NAB_station_rsv_all_pos = sum(RSV_pos, na.rm = TRUE),
            NAB_station_CoV_all_pos = sum(CoV_pos, na.rm = TRUE),
            NAB_station_rhino_all_pos = sum(Rhino_pos, na.rm = TRUE),
            
            #mean proportionf of tests that were positive for each site
            NAB_station_flu_prop_pos = mean(flu_pos_prop, na.rm = TRUE),
            NAB_station_rsv_prop_pos = mean(RSV_pos_prop, na.rm = TRUE),
            NAB_station_CoV_prop_pos = mean(CoV_pos_prop, na.rm = TRUE),
            NAB_station_rhino_prop_pos = mean(Rhino_pos_prop, na.rm = TRUE)
            )

nrevss_data2 <- left_join(nrevss_data, nrevss_sum) %>% #str(nrevss_data2) 
            mutate(v_pos_rel_Rhinovirus = Rhino_pos/NAB_station_rhino_all_pos,
                   v_pos_rel_RSV = RSV_pos/NAB_station_rsv_all_pos,
                   v_pos_rel_corona = CoV_pos/NAB_station_CoV_all_pos,
                   v_pos_rel_flu = flu_pos/NAB_station_flu_all_pos,
                   
                   v_pos_rel_adj_Rhinovirus = v_pos_rel_Rhinovirus * NAB_station_rhino_prop_pos,
                   v_pos_rel_adj_RSV = v_pos_rel_RSV * NAB_station_rsv_prop_pos,
                   v_pos_rel_adj_corona = v_pos_rel_corona * NAB_station_CoV_prop_pos,
                   v_pos_rel_adj_flu = v_pos_rel_flu * NAB_station_flu_prop_pos
                   )

#Austin flu in 2016 summer/fall was not reported; it was close to 0, so setting it to 0 manually
nrevss_data3 <- nrevss_data2 %>% 
  mutate(flu_tests = case_when(NAB_station == "Georgetown" & dates < ymd("2016-09-25") & dates > ymd("2016-06-03") ~ 0,
                            TRUE ~ flu_tests),
         flu_pos = case_when(NAB_station == "Georgetown" & dates < ymd("2016-09-25") & dates > ymd("2016-06-03") ~ 0,
                            flu_tests == 0 ~ 0,
                            TRUE ~ flu_pos_prop),
         flu_pos_prop = case_when(NAB_station == "Georgetown" & dates > ymd("2016-06-03") & dates < ymd("2016-09-25") ~ 0,
                            flu_tests == 0 ~ 0,
                            TRUE ~ flu_pos_prop),
         v_pos_rel_flu = case_when(NAB_station == "Georgetown" & dates > ymd("2016-06-03") & dates < ymd("2016-09-25") ~ 0,
                            TRUE ~ v_pos_rel_flu),
         v_pos_rel_adj_flu = case_when(flu_tests == 0 ~ 0,
                            TRUE ~ v_pos_rel_adj_flu)) %>% 
  mutate(CoV_pos_prop = case_when(CoV_tests == 0 ~ 0,
                            TRUE ~ CoV_pos_prop),
         Rhino_pos_prop = case_when(Rhino_tests == 0 ~ 0,
                            TRUE ~ Rhino_pos_prop)) %>% 
  rename(date = dates)
#names(test)
#test <- nrevss_data3 %>% filter(!complete.cases(.))
# str(nrevss_data3)

date_station_grid <- expand_grid(seq(ymd("2015-08-01"),ymd("2018-02-01"), by = '1 day'), 
                                 unique(NAB_tx_pol$NAB_station)) %>% 
  `colnames<-`(c("date", "NAB_station")) %>%
  filter(!is.na(NAB_station)) %>% 
  ungroup()

nrevss_data4 <- left_join(date_station_grid, nrevss_data3)%>% 
  dplyr::select(NAB_station, date, 
                v_pos_prop_RSV = RSV_pos_prop,
                v_pos_rel_RSV, v_pos_rel_adj_RSV,
                v_pos_prop_flu = flu_pos_prop,
                v_pos_rel_flu, v_pos_rel_adj_flu,
                v_pos_prop_Rhinovirus = Rhino_pos_prop,
                v_pos_rel_Rhinovirus, v_pos_rel_adj_Rhinovirus,
                v_pos_prop_corona = CoV_pos_prop,
                v_pos_rel_corona, v_pos_rel_adj_corona) %>% 
arrange(NAB_station, date) %>% 
group_by(NAB_station) %>% #switch to daily values from weekly values
  mutate( v_pos_prop_RSV = round(rollapply(v_pos_prop_RSV, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
          v_pos_rel_RSV = round(rollapply(v_pos_rel_RSV, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
          v_pos_rel_adj_RSV = round(rollapply(v_pos_rel_adj_RSV, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
          v_pos_prop_flu = round(rollapply(v_pos_prop_flu, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
          v_pos_rel_flu = round(rollapply(v_pos_rel_flu, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
          v_pos_rel_adj_flu = round(rollapply(v_pos_rel_adj_flu, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 4),
          v_pos_prop_Rhinovirus = round(rollapply(v_pos_prop_Rhinovirus, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 5),
          v_pos_rel_Rhinovirus = round(rollapply(v_pos_rel_Rhinovirus, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 5),
          v_pos_rel_adj_Rhinovirus = round(rollapply(v_pos_rel_adj_Rhinovirus, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 5),
          v_pos_prop_corona = round(rollapply(v_pos_prop_corona, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 5),
          v_pos_rel_corona = round(rollapply(v_pos_rel_corona, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 5),
          v_pos_rel_adj_corona = round(rollapply(v_pos_rel_adj_corona, 7, align = "left", FUN=function(x) mean(x, na.rm=TRUE), fill=NA), 5)) %>% 
  
    mutate(v_pos_prop_RSV_m = na_interpolation(v_pos_prop_RSV), #linear interpolation to fill in missing data
           v_pos_rel_RSV_m = na_interpolation(v_pos_rel_RSV),
           v_pos_rel_adj_RSV_m = na_interpolation(v_pos_rel_adj_RSV),
           v_pos_prop_flu_m = na_interpolation(v_pos_prop_flu),
           v_pos_rel_flu_m = na_interpolation(v_pos_rel_flu),
           v_pos_rel_adj_flu_m = na_interpolation(v_pos_rel_adj_flu),
           v_pos_prop_Rhinovirus_m = na_interpolation(v_pos_prop_Rhinovirus),
           v_pos_rel_Rhinovirus_m = na_interpolation(v_pos_rel_Rhinovirus),
           v_pos_rel_adj_Rhinovirus_m = na_interpolation(v_pos_rel_adj_Rhinovirus),
           v_pos_prop_corona_m = na_interpolation(v_pos_prop_corona),
           v_pos_rel_corona_m = na_interpolation(v_pos_rel_corona),
           v_pos_rel_adj_corona_m = na_interpolation(v_pos_rel_adj_corona)) %>% 
           
    #doing some moving averages
    
    mutate(v_pos_rel_adj_Rhinovirus_m14 = rollmean(v_pos_rel_adj_Rhinovirus_m, 15, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_RSV_m14 = rollmean(v_pos_rel_adj_RSV_m, 15, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_corona_m14 = rollmean(v_pos_rel_adj_corona_m, 15, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_flu_m14 = rollmean(v_pos_rel_adj_flu_m, 15, na.pad=TRUE, align = "center"),
           
           v_pos_rel_adj_Rhinovirus_m14r = rollmean(v_pos_rel_adj_Rhinovirus_m, 15, na.pad=TRUE, align = "right"),
           v_pos_rel_adj_RSV_m14r = rollmean(v_pos_rel_adj_RSV_m, 15, na.pad=TRUE, align = "right"),
           v_pos_rel_adj_corona_m14r = rollmean(v_pos_rel_adj_corona_m, 15, na.pad=TRUE, align = "right"),
           v_pos_rel_adj_flu_m14r = rollmean(v_pos_rel_adj_flu_m, 15, na.pad=TRUE, align = "right"),
           
           v_pos_rel_adj_Rhinovirus_m14l = rollmean(v_pos_rel_adj_Rhinovirus_m, 15, na.pad=TRUE, align = "left"),
           v_pos_rel_adj_RSV_m14l = rollmean(v_pos_rel_adj_RSV_m, 15, na.pad=TRUE, align = "left"),
           v_pos_rel_adj_corona_m14l = rollmean(v_pos_rel_adj_corona_m, 15, na.pad=TRUE, align = "left"),
           v_pos_rel_adj_flu_m14l = rollmean(v_pos_rel_adj_flu_m, 15, na.pad=TRUE, align = "left"),
           
           v_pos_rel_adj_Rhinovirus_m21 = rollmean(v_pos_rel_adj_Rhinovirus_m, 22, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_RSV_m21 = rollmean(v_pos_rel_adj_RSV_m, 22, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_corona_m21 = rollmean(v_pos_rel_adj_corona_m, 22, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_flu_m21 = rollmean(v_pos_rel_adj_flu_m, 22, na.pad=TRUE, align = "center"),
           
           v_pos_rel_adj_Rhinovirus_m28 = rollmean(v_pos_rel_adj_Rhinovirus_m, 29, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_RSV_m28 = rollmean(v_pos_rel_adj_RSV_m, 29, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_corona_m28 = rollmean(v_pos_rel_adj_corona_m, 29, na.pad=TRUE, align = "center"),
           v_pos_rel_adj_flu_m28 = rollmean(v_pos_rel_adj_flu_m, 29, na.pad=TRUE, align = "center"),
           )%>% 
  dplyr::select(NAB_station, date, contains("_m"))

nrevss_data4 %>%
  ggplot(aes(x = date, y =  v_pos_rel_adj_corona_m, color = NAB_station)) + geom_line() + theme_bw()
  #ylab("RSV tests (proportion positive)") #+ facet_wrap(~NAB_station)

nrevss_data4 %>% group_by(NAB_station, date) %>% 
  dplyr::select(contains("v_")) %>% 
  filter(NAB_station == "San Antonio A" | NAB_station == "Flower Mound") %>% 
  ungroup() %>% 
  pivot_longer(cols = c(v_pos_prop_RSV_m, v_pos_rel_RSV_m, v_pos_rel_adj_RSV_m,
                        v_pos_prop_flu_m, v_pos_rel_flu_m, v_pos_rel_adj_flu_m, 
                        v_pos_prop_Rhinovirus_m, v_pos_rel_Rhinovirus_m, v_pos_rel_adj_Rhinovirus_m, 
                        v_pos_prop_corona_m, v_pos_rel_corona_m, v_pos_rel_adj_corona_m)) %>% 
  ggplot(aes( x = date, y = value, color = NAB_station)) + geom_step() + facet_wrap(~name, scales = "free_y")  + theme_bw()


### load in flu data ###############################################################################
# #downloaded via CDCfluview package, script is here: https://github.com/dankatz/TX_epi/blob/master/cdc_flu_data_acquisition191111.R
# flu <- read.csv("C:/Users/dsk856/Desktop/misc_data/flu_daily_200916.csv") %>% 
#   mutate(date = ymd(date)) %>% 
#   dplyr::select(-proportion_positive, -total_positive)
# 
# #it appears a few days at the start didn't have data, taking data from the next week which looks like a pretty reasonable assumption
# flu$flu_d_prop_pos[flu$date == "2015-10-01" | flu$date == "2015-10-02" | flu$date == "2015-10-03" ] <- 
#   flu$flu_d_prop_pos[flu$date == "2015-10-04" ] 
# 
# flu$flu_d_total_pos[flu$date == "2015-10-01" | flu$date == "2015-10-02" | flu$date == "2015-10-03" ] <- 
#   flu$flu_d_total_pos[flu$date == "2015-10-04" ] 
# 
# 
# #summary(flu)
# # flu %>% filter(date > mdy("10-31-2015") & date < mdy("01-01-2018")) %>%
# # ggplot(aes(x = date, y = flu_d_prop_pos)) + geom_step() + theme_bw()

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
opa_day <- left_join(opa_day, nrevss_data4) #str(nrevss_data4) 
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
                  pbir_py = (pbir / agegroup_x_pop) * 100000)
# write_csv(opa_day_agegroup_x, paste0("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_",age_low,"_",age_hi, "_25km_201008.csv"))

#summary(opa_day_agegroup_x)

### exploring data ###################################################
# opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_5_17_25km_201008.csv", guess_max = 8260)
# opa_day_adult <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_18_99_25km_201008.csv", guess_max = 8260)
# opa_day_youngchildren <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_0_4_25km_201008.csv", guess_max = 8260)

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
  coord_cartesian(ylim = c(0, 0.5)) +  scale_color_grey(name = "NAB station") +
  ylab("Asthma ED \n visits \n (per 10,000)") +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position= "none",
        axis.title.x=element_blank(), axis.text.x=element_blank())

#time series for ED visits: school aged children
pbir_global_mean <- opa_day %>% #the average across all the study areas
  group_by(date) %>%
  summarize(pbir_global_mean = mean(pbir))
panel_ed <-
  opa_day %>%
  ggplot(aes(x = date, y = pbir, col = NAB_station)) + theme_few() +
  geom_line(aes(x = date, y=rollmean((pbir ), 7, na.pad=TRUE)), alpha = 0.3) +
  geom_line(data = pbir_global_mean, aes(x = date, y = rollmean(pbir_global_mean, 7, na.pad=TRUE)), col = "black") +
  coord_cartesian(ylim = c(0, 0.55)) +  scale_color_grey() +
  ylab("Asthma ED \n visits \n (per 10,000)") +
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
  coord_cartesian(ylim = c(0, 0.12)) +  scale_color_grey(name = "NAB station") +
  ylab("Asthma ED \n visits \n (per 10,000)") +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position= "none",
        axis.title.x=element_blank(), axis.text.x=element_blank())

## time series for pollen
pol_global_mean <- opa_day %>% group_by(date) %>%
  summarize(cup_all_m_global = mean(cup_all_m),
            trees_m_global = mean(trees_m),
            pol_other_m_global = mean(pol_other_m))

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
  opa_day %>% ungroup() %>%
   mutate(flu_d_perc_pos = flu_d_prop_pos * 100) %>%
  #        v_tests_adj_pos_Corona_ms = v_tests_adj_pos_Corona_m/130.6684,
  #        v_tests_adj_pos_Rhinovirus_ms = v_tests_adj_pos_Rhinovirus_m/130.6684 ,
  #        v_tests_adj_pos_RSV_ms = v_tests_adj_pos_RSV_m/130.6684 ) %>%
    dplyr::select(date, v_tests_perc_pos_Corona_m, v_tests_perc_pos_Rhinovirus_m, flu_d_perc_pos) %>%
    pivot_longer(cols = c(v_tests_perc_pos_Corona_m, v_tests_perc_pos_Rhinovirus_m , flu_d_perc_pos),
                 names_to = "virus_type", values_to = "positive_tests") %>%
    distinct() %>%
    #filter(date > mdy("10 - 31 - 2015")) %>%
    arrange(virus_type, date) %>%
    ggplot(aes(x = date, y = positive_tests, color = virus_type)) + theme_few() +
    geom_step() + #geom_point() +
    ylab(expression(atop("positive tests", "(%)"))) +
    scale_color_viridis_d(breaks = c("flu_d_perc_pos", "v_tests_perc_pos_Corona_m", "v_tests_perc_pos_Rhinovirus_m"), 
                          option = "viridis",
                       labels = c("Influenza" ,"Seasonal coronavirus", "Rhinovirus"), name = "virus type") +
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
ggsave(file = "C:/Users/dsk856/Desktop/thcic_analysis/time_series_fig_201008.jpg", plot = ts_panels,
       height =20, width = 17, units = "cm", dpi = 300)


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




#### analysis with a distributed lags model using dlnm package #######################################
library(dlnm)
library(splines)
library(MASS)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

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

#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_25km_200918.csv", guess_max = 8260) #opa_day_child_200910.csv
#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_10km_200918.csv", guess_max = 8260) 
#opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_child_50km_200918.csv", guess_max = 8260) 
opa_day <- opa_day_agegroup_x 
opa_day %>% #group_by(NAB_station) %>% 
  summarize(total_cases = sum(n_cases), PBIR_mean = mean(pbir)) #%>% ungroup() %>%  summarize( total_n = sum (total_cases))
ggplot(opa_day, aes( x = date, y = trees + 1))  + facet_wrap(~NAB_station) + theme_bw()+ scale_y_log10() + 
  geom_point(color = "black") +
  geom_point(aes(x = date, y = trees_m + 1), color = "red", size = 0.5) 

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
                child_pop, log_child_pop, adult_pop, log_adult_pop, agegroup_x_pop, log_agegroup_x_pop,
                week_day,
                #cup_other_lm, #cup_other_lms, 
                cup_all_lm, cup_all_m,
                trees_lm, trees_m,
                pol_other_lm, pol_other_m,
                # v_tests_pos_Rhinovirus_ms,
                # v_tests_pos_RSV_ms,
                # v_tests_pos_Corona_ms,
                # v_tests_perc_pos_Rhinovirus_m,
                # v_tests_perc_pos_RSV_m,
                # v_tests_perc_pos_Corona_m,
                # v_tests_adj_pos_Rhinovirus_m,
                # v_tests_adj_pos_RSV_m,
                # v_tests_adj_pos_Corona_m,
                v_pos_prop_Rhinovirus_m, v_pos_rel_Rhinovirus_m, v_pos_rel_adj_Rhinovirus_m, v_pos_rel_adj_Rhinovirus_m14,  
                v_pos_rel_adj_Rhinovirus_m14l, v_pos_rel_adj_Rhinovirus_m14r, v_pos_rel_adj_Rhinovirus_m21, 
                v_pos_rel_adj_Rhinovirus_m28, 
                
                v_pos_prop_RSV_m, v_pos_rel_RSV_m, v_pos_rel_adj_RSV_m, v_pos_rel_adj_RSV_m14, 
                v_pos_rel_adj_RSV_m14l, v_pos_rel_adj_RSV_m14r, v_pos_rel_adj_RSV_m21,v_pos_rel_adj_RSV_m28,#names(nrevss_data4)
                
                v_pos_prop_corona_m, v_pos_rel_corona_m, v_pos_rel_adj_corona_m, v_pos_rel_adj_corona_m14, 
                v_pos_rel_adj_corona_m14l, v_pos_rel_adj_corona_m14r, v_pos_rel_adj_corona_m21,v_pos_rel_adj_corona_m28,
                
                v_pos_prop_flu_m, v_pos_rel_flu_m, v_pos_rel_adj_flu_m,v_pos_rel_adj_flu_m14, v_pos_rel_adj_flu_m14l,
                v_pos_rel_adj_flu_m14r, v_pos_rel_adj_flu_m21,v_pos_rel_adj_flu_m28,
                
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
         all_pol_lm = log10(cup_all_m + trees_m + pol_other_m + 1)) 


# names(data_for_model) 
# summary(data_for_model)
# ggplot(data_for_model, aes( x = date, y = cup_all_lm )) + geom_point() + facet_wrap(~NAB_station) 
data_for_model %>% group_by(NAB_station, date) %>% 
  dplyr::select(contains("v_")) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(v_pos_prop_Rhinovirus_m, v_pos_rel_Rhinovirus_m, v_pos_rel_adj_Rhinovirus_m21, 
                        v_pos_prop_RSV_m, v_pos_rel_RSV_m, v_pos_rel_adj_RSV_m21,
                        v_pos_prop_corona_m, v_pos_rel_corona_m, v_pos_rel_adj_corona_m21,
                        v_pos_prop_flu_m, v_pos_rel_flu_m, v_pos_rel_adj_flu_m21
                        )) %>% 
 ggplot(aes( x = date, y = value, color = NAB_station)) + geom_step() + facet_wrap(~name, scales = "free_y")  + theme_bw()



## set up dlnm crossbasis object for use in glm
max_lag <- 7
cup_lag <- crossbasis(data_for_model$cup_all_lm, lag = max_lag, #log10 transformed & imputed pollen concentration for Cupressaceae
                     argvar=list(fun = "ns", knots = 3), #shape of response curve
                     arglag = list(fun = "ns", knots = 3)) #shape of lag
                    # argvar=list(fun = "poly", degree = 3), #shape of response curve
                    # arglag = list(fun = "poly", degree = 3)) #shape of lag

trees_lag <- crossbasis(data_for_model$trees_lm, lag = max_lag, 
                        argvar=list(fun = "ns", knots = 3), #shape of response curve
                        arglag = list(fun = "ns", knots = 3)) #shape of lag
                      # argvar=list(fun = "poly", degree = 3), #shape of response curve
                      # arglag = list(fun = "poly", degree = 3)) #shape of lag
                    

pol_other_lag <- crossbasis(data_for_model$pol_other_lm, lag = max_lag, 
                            argvar=list(fun = "ns", knots = 2), #shape of response curve
                            arglag = list(fun = "ns", knots = 2)) #shape of lag
                      # argvar=list(fun = "poly", degree = 3), #shape of response curve
                      # arglag = list(fun = "poly", degree = 3)) #shape of lag

#viruses in dlnm format for use with attrdl function for AR
rhino_lag <- crossbasis(data_for_model$v_pos_rel_adj_Rhinovirus_m21, lag = 0, #percent of tests positive for rhinovirus
                        # argvar=list(fun = "poly", degree = 3), #shape of response curve
                        # arglag = list(fun = "poly", degree = 3)) #shape of lag
                        argvar=list(fun = "lin"), arglag = list(fun = "lin")) #using a linear response
# rhino_interaction_lag <- crossbasis(data_for_model$v_tests_perc_pos_Rhinovirus_m *
#                                     data_for_model$v_tests_pos_Rhinovirus_ms, lag = 0, #percent of tests positive for rhinovirus
#                                    argvar=list(fun = "poly", degree = 3), #shape of response curve
#                                    arglag = list(fun = "poly", degree = 3)) #shape of lag
#                     #               argvar=list(fun = "lin"), arglag = list(fun = "lin")) #using a linear response

corona_lag <- crossbasis(data_for_model$v_pos_rel_adj_corona_m21, lag = 0, #percent of tests positive for rhinovirus
                         # argvar=list(fun = "poly", degree = 3), #shape of response curve
                         # arglag = list(fun = "poly", degree = 3)) #shape of lag
                         argvar=list(fun = "lin"), arglag = list(fun = "lin"))
rsv_lag <- crossbasis(data_for_model$v_pos_rel_adj_RSV_m21, lag = 0, #percent of tests positive for rhinovirus
                         # argvar=list(fun = "poly", degree = 3), #shape of response curve
                         # arglag = list(fun = "poly", degree = 3)) #shape of lag
                        argvar=list(fun = "lin"), arglag = list(fun = "lin"))

flu_lag <- crossbasis(data_for_model$v_pos_rel_adj_flu_m21, lag = 0, #percent of tests positive for influenza (separate dataset)
                      # argvar=list(fun = "poly", degree = 3), #shape of response curve
                      # arglag = list(fun = "poly", degree = 3)) #shape of lag
                      argvar=list(fun = "lin"), arglag = list(fun = "lin"))


#quasiposson glm with included variables
model1 <- glm(n_cases ~  #number of cases at a station on an observed day
                    NAB_station + #effect of station
                    offset(log(agegroup_x_pop)) +  #offset for the population of a study area
                    cup_lag +  trees_lag  + pol_other_lag + #dlnm crossbasis for each pollen type
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
                    # ns(time, df = 12) + #not including spline anymore
              week_day, #day of week term
              family = quasipoisson, #quasipoisson
              data = data_for_model)  
fqaic(model1)
summary(model1)


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
# resid_explor <- bind_cols(data_for_model, resid = c(rep(NA, 0), residuals(model1, type = "deviance")))#for the model version when pollen isn't included
# resid_explor <- bind_cols(data_for_model, resid = c(rep(NA, max_lag), residuals(model1, type = "deviance")))# with pollen
# resid_explor %>% 
#   mutate(doy = yday(date),
#          syear = year(date)) %>% 
#   ##filter(NAB_station == "San Antonio A") %>% 
#  # filter(date > ymd("2015-12-15") & date < ymd("2016-01-15")) %>% 
#   
#   ggplot(aes(x = date, y = resid * .5 + 2, col = resid)) +  theme_bw() + 
#   scale_x_date(breaks = pretty(resid_explor$date, n = 22)) + scale_color_viridis_c() + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_wrap(~NAB_station)+
#   #geom_point() +
#   geom_line(aes(y=rollmean(resid + 2, 7, na.pad=TRUE))) +
#   geom_line(aes(y=rollmean(resid + 2, 14, na.pad=TRUE)), alpha = 0.3, lwd = 2) +
#   geom_line(aes(y=rollmean( pbir * 3, 7, na.pad=TRUE)), col = "black") +
#   
#   geom_line(aes(y=rollmean( cup_all_lm * .5, 1, na.pad=TRUE, align = "left")), col = "red")+
#   geom_line(aes(y=rollmean( trees_lm * .5, 1, na.pad=TRUE, align = "left")), col = "green") +
#   geom_line(aes(y=rollmean( pol_other_lm * .5, 1, na.pad=TRUE, align = "left")), col = "yellow") +
#   geom_line(aes(y=rollmean( v_pos_rel_adj_Rhinovirus_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "blue") +
#   geom_line(aes(y=rollmean( v_pos_rel_adj_corona_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "brown") +
#   geom_line(aes(y=rollmean( v_pos_rel_adj_RSV_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "purple") +
#   geom_line(aes(y=rollmean( v_pos_rel_adj_flu_m21 * 1000 - 1, 7, na.pad=TRUE, align = "left")), col = "orange") 
#   
#   
# resid_explor %>% 
#   mutate(doy = yday(date),
#          syear = year(date)) %>% 
#   ggplot(aes(x = v_pos_rel_adj_flu_m14, y = resid, col = doy)) + geom_point() + theme_bw() + geom_smooth(se = FALSE, color = "red") +
#   facet_wrap(~NAB_station) + scale_color_viridis_c()
#   # geom_line(aes(y=rollmean(cup_all_lm, 7, na.pad=TRUE, align = "left")), col = "red") +
#   # geom_line(aes(y=rollmean(cup_all_lm, 14, na.pad=TRUE, align = "left")), col = "orange") +
#   # geom_line(aes(y=rollmean(cup_all_lm, 21, na.pad=TRUE, align = "left")), col = "yellow") 
# 
# 
# # resid_explor %>%
# #   mutate(doy = yday(date)) %>% 
# #   mutate(cup_all_lm_14d = rollmean(cup_all_lm, 14, na.pad=TRUE)) %>% 
# #   #filter(date < ymd("2016-07-15")) %>% 
# #   filter(doy < 70 | doy > 330) %>% 
# #   ggplot(aes(x = cup_all_lm_14d, y = resid)) + geom_point() + theme_bw() + facet_wrap(~NAB_station) +
# #   geom_smooth(se = FALSE)


### visualize effects of pollen ###############################################################
#cup all  
pred1_cup <- crosspred(cup_lag,  model2, #at = 1,
                      at = seq(from = 0, to = max(data_for_model$cup_all_lm), by = 0.10), 
                      bylag = 0.2, cen = 0, cumul = TRUE) #str(pred1_cup)

child_RR_x_cup_25km <- 
  data.frame(pol_conc = 10^(pred1_cup$predvar), 
                     mean = pred1_cup$allRRfit,
                     lower = pred1_cup$allRRlow,
                     upper = pred1_cup$allRRhigh) %>% 
ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() + 
  annotation_logticks(sides = "b")  
  #ggtitle(paste0("ages ", age_low, "-", age_hi, "  n_cases = ", sum(data_for_model$n_cases))) 

child_RR_x_cup_25km <- child_RR_x_cup_25km 
    #+ geom_rug(data = data_for_model, aes(x = cup_all_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

child_lag_RR_x_cup_25km <-
  as.data.frame(exp(pred1_cup$cumfit)) %>% mutate(pol_conc = pred1_cup$predvar) %>% 
         pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
         mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
                pol_conc_exp = 10^pol_conc) %>% 
ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("Cupressaceae (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + #, begin = 0.3, end = 1)  #automatically bins and turns to factor
  annotation_logticks(sides = "b")  
  #ggtitle(paste0("  n_pop = ", sum(pop_near_NAB_agegroup_x$agegroup_x_pop)))

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
  xlab(expression(paste("trees (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10()+ 
  annotation_logticks(sides = "b")  

child_RR_x_trees_25km <-  child_RR_x_trees_25km 
#+  geom_rug(data = data_for_model, aes(x = trees_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

# plot.crosspred(pred1_trees, "contour", cumul = TRUE,
#      plot.title = title(xlab = expression(paste("log10(tree pollen grains m"^"3",")")),
#                         ylab = "Lag", main = "Cumulative RR across lags for trees"), key.title = title("RR"))
child_lag_RR_x_trees_25km <-
  as.data.frame(exp(pred1_trees$cumfit)) %>% mutate(pol_conc = pred1_trees$predvar) %>% 
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>% 
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>% 
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("tree pollen (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + 
  annotation_logticks(sides = "b")  


## pol_other
pred1_pol_other <- crosspred(pol_other_lag,  model2, cen = 0, cumul = TRUE,
                         at = seq(from = 0, to = max(data_for_model$pol_other_lm), by = .10))
child_RR_x_pol_other_25km <-
  data.frame(pol_conc = 10^(pred1_pol_other$predvar),
             mean = pred1_pol_other$allRRfit,
             lower = pred1_pol_other$allRRlow,
             upper = pred1_pol_other$allRRhigh) %>%
  ggplot(aes(x = pol_conc, y = mean, ymin = lower, ymax = upper))+
  geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ ylab('RR')+theme_few() + scale_x_log10() + 
  annotation_logticks(sides = "b")  
  
child_RR_x_pol_other_25km <- child_RR_x_pol_other_25km 
#+geom_rug(data = data_for_model, aes(x = pol_other_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)


child_lag_RR_x_pol_other_25km <-
  as.data.frame(exp(pred1_pol_other$cumfit)) %>% mutate(pol_conc = pred1_pol_other$predvar) %>%
  pivot_longer(., cols = contains("lag"), names_to = "lag", values_to = "RR") %>%
  mutate(lag = as.numeric(gsub(pattern = "lag", replacement = "", x = lag)),
         pol_conc_exp = 10^pol_conc) %>%
  ggplot(aes(x = pol_conc_exp, y = lag, z = RR)) + geom_contour_filled(bins = 10) + theme_few() +
  xlab(expression(paste("other pollen (pollen grains / m"^"3",")")))+ scale_x_log10() +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "RR") + 
  annotation_logticks(sides = "b")  


#saving the figs 
child_pol_25km <-
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
child_pol_25km
# ggsave(file = "C:/Users/dsk856/Desktop/thcic_analysis/child_pol_25km_210513.jpg", plot = child_pol_25km,
#        height = 25, width = 21, units = "cm", dpi = 300)

  
### visualize effects of viruses ###############################################################
  
  ## rhinovirus
  pred1_rhino <- crosspred(rhino_lag,  model2, cen = 0, cumul = TRUE,
                               at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_Rhinovirus_m14), by = 0.0005))
  data.frame(rhino_conc = pred1_rhino$predvar,
               mean = pred1_rhino$allRRfit,
               lower = pred1_rhino$allRRlow,
               upper = pred1_rhino$allRRhigh) %>%
    ggplot(aes(x = rhino_conc, y = mean, ymin = lower, ymax = upper))+
    geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
    xlab("rhino")+ ylab('RR')+theme_few() 

  ## corona
  pred1_corona <- crosspred(corona_lag,  model2, cen = 0, cumul = TRUE,
                           at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_corona_m14), by = 0.0005))
  data.frame(corona_conc = pred1_corona$predvar,
             mean = pred1_corona$allRRfit,
             lower = pred1_corona$allRRlow,
             upper = pred1_corona$allRRhigh) %>%
    ggplot(aes(x = corona_conc, y = mean, ymin = lower, ymax = upper))+
    geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
    xlab("corona")+ ylab('RR')+theme_few() 

  ## RSV
  pred1_rsv <- crosspred(rsv_lag,  model2, cen = 0, cumul = TRUE,
                         at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_RSV_m14), by = 0.0005))
  data.frame(flu_conc = pred1_rsv$predvar,
             mean = pred1_rsv$allRRfit,
             lower = pred1_rsv$allRRlow,
             upper = pred1_rsv$allRRhigh) %>%
    ggplot(aes(x = flu_conc, y = mean, ymin = lower, ymax = upper))+
    geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
    xlab("rsv")+ ylab('RR')+theme_few()

  ## flu
  pred1_flu <- crosspred(flu_lag,  model2, cen = 0, cumul = TRUE,
                            at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_flu_m14), by = 0.0005))
  data.frame(flu_conc = pred1_flu$predvar,
             mean = pred1_flu$allRRfit,
             lower = pred1_flu$allRRlow,
             upper = pred1_flu$allRRhigh) %>%
    ggplot(aes(x = flu_conc, y = mean, ymin = lower, ymax = upper))+
    geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
    xlab("flu")+ ylab('RR')+theme_few() 

### attributable risk ##########################################
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

#rhinovirus
rhino_attr <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases, 
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
                     model = model1, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_trees <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases, 
                       model = model1, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_other_pol <- attrdl(x = data_for_model$pol_other_lm, basis = pol_other_lag, cases = data_for_model$n_cases,
                        model = model1, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_rhino <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases, 
                       model = model1, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_corona <- attrdl(x = data_for_model$v_pos_rel_adj_corona_m21, basis = corona_lag, cases = data_for_model$n_cases, 
                       model = model1, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_flu <- attrdl(x = data_for_model$v_pos_rel_adj_flu_m21, basis = flu_lag, cases = data_for_model$n_cases, 
                       model = model1, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_rsv <- attrdl(x = data_for_model$v_pos_rel_adj_RSV_m21, basis = rsv_lag, cases = data_for_model$n_cases, 
                     model = model1, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)


predicted_n_cases_t <- c(rep(NA, max_lag + 0), model1$fitted.values) #hist(model2$fitted.values)
#predicted_n_cases_t <- c(rep(NA, max_lag + 1), model2$fitted.values) #hist(model2$fitted.values)

fit <- lm(data_for_model$n_cases ~ predicted_n_cases_t)
summary(fit)
plot(jitter(data_for_model$n_cases), predicted_n_cases_t)

attr_df <- data.frame(attr_t_cup, attr_t_trees, attr_t_other_pol, attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_rsv) #attr_t_other_pol, , predicted_n_cases_t
#attr_df[attr_df < 0] <- 0 #removing net protective effects of all variables
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
                                              Cupressaceae = "attr_t_cup", trees = "attr_t_trees", other_pollen = "attr_t_other_pol"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "RSV","Influenza",
                                                                       "Cupressaceae", "trees", "other_pollen")), #"other_pollen"  "unexplained"
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))
         
observed_ncases_t <- data_for_model %>% 
  dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
  mutate(week = week(date),
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 )) %>% 
  group_by(NAB_station, agegroup_x_pop, week, date2) %>% 
  summarize(mean_cases = mean(n_cases)) %>% 
  mutate(attr_risk_var = "Rhinovirus") 

ggplot(attr_full_df, aes(x = date2, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) + 
  facet_wrap(~NAB_station) + geom_area() + theme_bw() + scale_fill_viridis_d(name = "attributable risk") + 
  ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
  scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(-0.02, .4)) + #c(-0.01, .15) #c(-0.02, .6)
  geom_step(data = observed_ncases_t, aes( x = date2, y =(mean_cases / agegroup_x_pop) * 10000, 
                                           color = "observed cases")) + scale_color_discrete(name = "") 
  
## a table giving the percent of AR for each pollen type by each city across the study period
bind_cols(data_for_model, attr_df) %>% 
group_by(NAB_station) %>% 
  mutate(att_t_cup_risk_cases = (attr_t_cup/agegroup_x_pop) * 10000,
         att_t_tree_risk_cases = (attr_t_trees/agegroup_x_pop) * 10000,
         att_t_other_pol_risk_cases = (attr_t_other_pol/agegroup_x_pop) * 10000) %>% 
  summarize(attr_t_cup_mean = mean(att_t_cup_risk_cases, na.rm = TRUE), 
            attr_t_trees_mean = mean(att_t_tree_risk_cases, na.rm = TRUE),
            attr_t_other_pol_mean = mean(att_t_other_pol_risk_cases, na.rm = TRUE))

## a table giving the percent of AR for each pollen type by each city across the main pollen season

#the cumulative y axis in the risk over time figure
average_cases_pred_jan <- bind_cols(data_for_model, attr_df) %>% 
  # mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu - attr_t_other_pol) %>% 
  dplyr::select(date, NAB_station, agegroup_x_pop, 
                #attr_t_unexplained, 
                attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_other_pol) %>%   #attr_t_other_pol,
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
  mutate(date_month = month(date)) %>% 
  filter(date_month == 1) %>% 
  group_by(NAB_station, date) %>% 
  mutate(risk_cases_per_pop = (risk_cases/agegroup_x_pop)*10000) %>% 
  summarize(total_risk_cases = sum(risk_cases_per_pop, na.rm = TRUE)) %>% 
  group_by(NAB_station) %>% 
  summarize(mean_total_risk_cases_jan = mean(total_risk_cases))

bind_cols(data_for_model, attr_df) %>% 
  mutate(date_month = month(date)) %>% 
  filter(date_month == 1) %>% 
  group_by(NAB_station) %>% 
  mutate(att_t_cup_risk_cases = (attr_t_cup/agegroup_x_pop) * 10000) %>% 
  summarize(attr_t_cup_mean = mean(att_t_cup_risk_cases, na.rm = TRUE)) %>% 
  left_join(., average_cases_pred_jan) %>% 
mutate(prop_cases_cup = attr_t_cup_mean / mean_total_risk_cases_jan) 




### a figure of just one city for the entire time period  
NAB_station_filter <- "San Antonio A"
attr_full_ex_df <- bind_cols(data_for_model, attr_df) %>% 
  filter(NAB_station == NAB_station_filter) %>% 
  # mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu - attr_t_other_pol) %>% 
  dplyr::select(date, NAB_station, agegroup_x_pop, 
                #attr_t_unexplained, 
                attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_other_pol) %>%   #attr_t_other_pol,
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
  mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
                                                     Rhinovirus = "attr_t_rhino", Corona = "attr_t_corona", Influenza = "attr_t_flu",
                                                     Cupressaceae = "attr_t_cup", trees = "attr_t_trees", other_pollen = "attr_t_other_pol"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza",
                                                 "Cupressaceae", "trees", "other_pollen"))) #"other_pollen"  "unexplained"


observed_ncases_ex_t <- data_for_model %>% 
  filter(NAB_station == NAB_station_filter) %>% 
  dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
  mutate(attr_risk_var = "Rhinovirus")


test_data <- data_for_model 
test_model_pred <- predict.glm(object = model2, newdata = test_data, type = "response")  #str(model_pred)

predicted_ncases_ex_t <-  data_for_model %>% ungroup() %>% 
  mutate(predicted_ncases_vector_model1 = c(rep(NA, max_lag + 0), model1$fitted.values),
         predicted_ncases_resid_model1 =  c(rep(NA, max_lag + 0),  model1$residuals),   
         predicted_ncases_vector_model2 = c(rep(NA, max_lag + 1), model2$fitted.values),
         predicted_ncases_resid_model2 =  c(rep(NA, max_lag + 1),  model2$residuals),   
         predicted_ncases_glmpred = test_model_pred) %>% 
  filter(NAB_station == NAB_station_filter) %>% 
  mutate(attr_risk_var = "Rhinovirus")

ggplot(attr_full_ex_df, aes(x = date, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) + 
  facet_wrap(~NAB_station) +  theme_bw() + scale_fill_viridis_d(name = "attributable risk") + # geom_area() +
  ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
  #coord_cartesian(ylim = c(-0.02, .9)) + #c(-0.01, .15) #c(-0.02, .6)
  geom_line(data = observed_ncases_ex_t, aes( x = date, y =(n_cases / agegroup_x_pop) * 10000, 
                                           color = "observed cases"), color = "black") +
  geom_line(data = predicted_ncases_ex_t, aes( x = date, y =(predicted_ncases_vector_model1 / agegroup_x_pop) * 10000, 
                                              color = "predicted cases"), color = "red") +
  # geom_line(data = predicted_ncases_ex_t, aes( x = date, y =(predicted_ncases_resid_model1 / agegroup_x_pop) * 10000, 
  #                                              color = "predicted cases"), color = "green") +
  geom_line(data = predicted_ncases_ex_t, aes( x = date, y = cup_all_lm/3.7, 
                                               color = "predicted cases"), color = "blue") +
  geom_line(data = predicted_ncases_ex_t, aes( x = date, y = trees_lm/3.7, 
                                               color = "predicted cases"), color = "green") +
  geom_line(data = predicted_ncases_ex_t, aes( x = date, y = v_tests_perc_pos_Rhinovirus_m/100, 
                                               color = "predicted cases"), color = "pink") +
  
  #scale_color_discrete(name = "") +
  scale_x_date(limits = c(mdy("1/10/2016"), mdy("12/1/2016")))# labels = date_format("%b")) 

max(predicted_ncases_ex_t$cup_all_lm)

hist(model2$effects)
str(model2)

model2$residuals[1100:1110] 
(model2$fitted.values[1100:1110] )
model2$y[1100:1110]


### figure of all time series as a kind of check
bind_cols(data_for_model, attr_df) %>% 
  ggplot(aes(x = date, y = pbir)) + geom_point(color = "gray") + theme_bw() + facet_wrap(~NAB_station) +
  #geom_smooth(se = F, color = "black")+
  geom_line(aes(y=zoo::rollmean(pbir, 7, na.pad=TRUE)), color = "black") +
  
  geom_line( aes( x = date, y = (cup_all_lm)/5,  color = "predicted cases"), color = "blue") +
  geom_line( aes( x = date, y = (trees_lm)/5,  color = "predicted cases"), color = "green") +
  
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_date(limits = c(mdy("1/10/2016"), mdy("12/1/2016")))# labels = date_format("%b")) 




### for the animation
attr_full_df_example <- bind_cols(data_for_model, attr_df) %>% 
  #mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>%  
  #- attr_t_other_pol) %>% #attr_t_other_pol
  dplyr::select(date, NAB_station, agegroup_x_pop, 
                attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu) %>%   #attr_t_other_pol,
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
  mutate(week = week(date)) %>% 
  group_by(NAB_station, agegroup_x_pop, week, var) %>% 
  summarize(risk_cases = mean(risk_cases)) %>% 
  mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
                                                     Corona = "attr_t_corona", Influenza = "attr_t_flu",
                                                     Rhinovirus = "attr_t_rhino", Cupressaceae = "attr_t_cup", trees = "attr_t_trees"),  #other_pollen = "attr_t_other_pol"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Corona", "Influenza","Rhinovirus", 
                                                                        "trees", "Cupressaceae")), #"other_pollen"
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))

observed_ncases_t_example <-filter(observed_ncases_t, NAB_station == "San Antonio B") 

filter(attr_full_df_example, NAB_station == "San Antonio B") %>% 
ggplot( aes(x = date2, y = (risk_cases / agegroup_x_pop) * 100000, fill = attr_risk_var)) + 
 geom_area() + theme_bw() + 
  scale_fill_manual(name = "attributable risk", values = c(rgb(68, 1, 84, alpha = 0, names = NULL, maxColorValue = 255), #corona
                                                           rgb(253, 231, 37, alpha = 1, names = NULL, maxColorValue = 255), #cupr
                                                           rgb(72, 40, 120, alpha = 0, names = NULL, maxColorValue = 255), #flu
                                                           rgb(49, 104, 142, alpha = 0, names = NULL, maxColorValue = 255),  #rhino
                                                           rgb(109, 205, 89, alpha = 0, names = NULL, maxColorValue = 255))) + #trees
  ylab("asthma-related ED visits (per 100,000 people per day)") + xlab("date") +
  scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(-0.02, 8)) + #c(-0.01, .15) #c(-0.02, .6)
  geom_step(data = observed_ncases_t_example, aes( x = date2, y =(mean_cases / agegroup_x_pop) * 100000,
                                                   color = "observed cases", fill = NA), 
            color = rgb(1,0,0, alpha = 1, maxColorValue = 1)) +
  theme(text = element_text(size=18))




bind_cols(data_for_model, attr_df) %>% 
  mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>%  
  #- attr_t_other_pol) %>% #attr_t_other_pol
  dplyr::select(date, NAB_station, agegroup_x_pop, 
                attr_t_unexplained, attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu) %>%   #attr_t_other_pol,
  pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
  mutate(week = week(date)) %>% 
  group_by(NAB_station, agegroup_x_pop, week, var) %>% 
  summarize(risk_cases = mean(risk_cases)) %>% 
  filter(attr_full_df, NAB_station == "San Antonio B") %>%
  filter(var != "attr_t_unexplained")  %>% 
  mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
                                                     Rhinovirus = "attr_t_rhino", Corona = "attr_t_corona", Influenza = "attr_t_flu",
                                                     Cupressaceae = "attr_t_cup", trees = "attr_t_trees"),  #other_pollen = "attr_t_other_pol"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza",
                                                                       "Cupressaceae", "trees"))) %>% 
ggplot(aes(x = date2, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) +
  geom_area() +
  theme_bw() + scale_fill_viridis_d(name = "attributable risk") +
  ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
  scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(-0.02, .7)) +





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

#using predict.glm to get the mean and then following Gavin Simpson's instructions to get the CI
# #https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
# 
# # str(model2)
# # model2$effects
# # ?predict.glm
# # getAnywhere(predict.glm())
# # summary(model2)
# # summary.glm
# 
# #No rhinovirus #hist(data_for_model$v_tests_perc_pos_Rhinovirus_ms)
# newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_Rhinovirus_ms #str(data_for_model$v_tests_pos_Rhinoviruss)
#   #mutate(v_tests_perc_pos_Rhinovirus_m = 0) 
#   mutate(v_tests_perc_pos_Rhinovirus_m = 0)#min(data_for_model$v_tests_perc_pos_Rhinovirus_m) )
# model_pred_no_rhino <- predict.glm(object = model2, newdata = newdata, type = "response")  #str(model_pred)
# table2$yc_n_cases_mean[4] <- sprintf("%.0f", sum(model2$fitted.values) - sum(model_pred_no_rhino, na.rm = TRUE))
# table2$yc_p_cases_mean[4] <- sprintf("%.1f", 100*((sum(model2$fitted.values) - sum(model_pred_no_rhino, na.rm = TRUE) ) / 
#                                                     sum(model2$fitted.values)) )
# 
# # #No RSV
# # newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_RSV_ms #str(data_for_model$v_tests_pos_RSV)
# #   mutate(v_tests_perc_pos_RSV_ms = 0)
# # model_pred_no_RSV <- predict.glm(object = model2, newdata = newdata, type = "response") 
# # sum(model2$fitted.values) 
# # sum(model_pred_no_RSV, na.rm = TRUE)
# # (sum(model2$fitted.values) - sum(model_pred_no_RSV, na.rm = TRUE) ) / sum(model2$fitted.values) 
# 
# #No corona
# newdata <- data_for_model %>%  #data_for_model$v_tests_perc_pos_RSV_ms #str(data_for_model$v_tests_pos_RSV)
#   mutate(v_tests_perc_pos_Corona_m = 0) 
# model_pred_no_corona <- predict.glm(object = model2, newdata = newdata, type = "response") 
# table2$yc_n_cases_mean[5] <- sprintf("%.0f", sum(model2$fitted.values) - sum(model_pred_no_corona, na.rm = TRUE))
# table2$yc_p_cases_mean[5] <- sprintf("%.1f", 100*((sum(model2$fitted.values) - sum(model_pred_no_corona, na.rm = TRUE) ) / 
#                                                     sum(model2$fitted.values))  )
# # 
# # #MESSING AROUND WITH TRYING TO GET CIs FOR VIRUSES. NO LUCK SO FAR
# # model_pred_no_corona <- predict.glm(object = model2, newdata = newdata, type = "response", se.fit = TRUE) 
# # plot(model_pred_no_corona$fit)
# # plot(model_pred_no_corona$se.fit)
# # 
# # family(model2)
# # ?predict.glm
# # # model_pred_no_corona <- predict.glm(object = model2, newdata = newdata, type = "response", se.fit = TRUE) 
# # # test <- ginv(na.omit(model_pred_no_corona[[1]] - 1.96 * model_pred_no_corona[[2]]))
# # # sum(model_pred_no_corona[[1]] + 1.96 * model_pred_no_corona[[2]], na.rm = TRUE)
# # var(data_for_model$n_cases)
# # 
# # model_pred_no_corona <- predict.glm(object = model2, newdata = newdata, type = "response", se.fit = TRUE) 
# # test <- data.frame(stan_fit = model_pred_no_corona$fit, 
# #                    stan_fit_lo = model_pred_no_corona$fit - 1.96 * model_pred_no_corona$se.fit,
# #                    stan_fit_hi = model_pred_no_corona$fit + 1.96 * model_pred_no_corona$se.fit)
# # test %>% ungroup() %>% mutate(rowid = 1:8250) %>% 
# #   ggplot(aes(x = rowid, y = stan_fit, ymax = stan_fit_hi, ymin = stan_fit_lo)) + geom_linerange() + theme_bw() + 
# #   coord_cartesian(xlim=c(800,850)) + geom_point()
# # 
# # ## grad the inverse link function
# # ilink <- family(model2)$linkinv
# # ## add fit and se.fit on the **link** scale
# # ndata <- bind_cols(newdata, setNames(as_tibble(predict(model2, newdata, se.fit = TRUE)[1:2]),
# #                                    c('fit_link','se_link')))
# # ## create the interval and backtransform
# # ndata <- mutate(ndata,
# #                 fit_resp  = ilink(fit_link),
# #                 right_upr = ilink(fit_link + (1.96 * se_link)),
# #                 right_lwr = ilink(fit_link - (1.96 * se_link)))
# # sum(ndata$fit_resp, na.rm = TRUE)
# # sum(ndata$fit_resp, na.rm = TRUE) - sum(ndata$right_lwr, na.rm = TRUE)
# # sum(ndata$right_upr, na.rm = TRUE) - sum(ndata$fit_resp, na.rm = TRUE)
# # 
# # #model_pred_no_corona <- predict.glm(object = model2, newdata = newdata, type = "response", se.fit = TRUE)
# # 
# # 
# # test3 <- cbind(ndata, test)
# # 
# # test3 %>% ungroup() %>% mutate(rowid = 1:8250) %>% 
# #   ggplot(aes(x = rowid, y = stan_fit, ymax = stan_fit_hi, ymin = stan_fit_lo)) + geom_linerange() + theme_bw() + 
# #   coord_cartesian(xlim=c(800,850)) +
# #   geom_point(aes(x=rowid, y = right_upr), col = "red") +
# #   geom_point(aes(x=rowid, y = right_lwr), col = "blue") +
# #   geom_point(aes(x=rowid, y = fit_resp), col = "black") 
# # plot(test)
# # 
# # ggplot(ndata, aes(x = row_id, ymax ))
# # plot(ndata$right_upr)
# # plot(ndata$right_lwr)
# 
# 
# #No flu
# newdata <- data_for_model %>%  #data_for_model$flu_ds #str(data_for_model$v_tests_pos_RSV)
#   mutate(flu_d_prop_pos = min(data_for_model$flu_d_prop_pos)) 
# model_pred_no_flu <- predict.glm(object = model2, newdata = newdata, type = "response") 
# table2$yc_n_cases_mean[6] <- sprintf("%.0f", sum(model2$fitted.values) - sum(model_pred_no_flu, na.rm = TRUE))
# table2$yc_p_cases_mean[6] <- sprintf("%.1f", 100*((sum(model2$fitted.values) - sum(model_pred_no_flu, na.rm = TRUE) ) / 
#                                                     sum(model2$fitted.values))  )
# 
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
# #block_groups_near_NAB_nostation <- unlist(block_groups_near_NAB$GEOID)
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


# ### does the birthday of patient correspond to allergy season? ###########
# names(opa_raw)
# test <- 
# opa_raw %>% 
#   filter(as.numeric(PAT_AGE_YEARS) < 18,
#          lon_imp < -97) %>% 
#   mutate(birth_doy = yday(ymd(date) - as.numeric(PAT_AGE_DAYS)),
#          birth_week = week(as.Date("2016-01-01") + birth_doy)) %>% 
#   #ggplot(aes(x = birth_doy)) + geom_histogram(bins = 366) + theme_bw() 
#   group_by(birth_week) %>% 
#   summarize(n_birth_week_west = n()) 
#   #ggplot(aes(x = birth_week, y = n_birth_week)) + geom_step() + theme_bw() 
#   
# test2 <- opa_raw %>% 
#   filter(as.numeric(PAT_AGE_YEARS) < 18,
#          lon_imp > -97) %>% 
#   mutate(birth_doy = yday(ymd(date) - as.numeric(PAT_AGE_DAYS)),
#          birth_week = week(as.Date("2016-01-01") + birth_doy)) %>% 
#   #ggplot(aes(x = birth_doy)) + geom_histogram(bins = 366) + theme_bw() 
#   group_by(birth_week) %>% 
#   summarize(n_birth_week_east = n()) 
# #ggplot(aes(x = birth_week, y = n_birth_week)) + geom_step() + theme_bw() 
# 
# 
# #compare to injuries as a control group
# # opi <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/op_injuries.csv")
# # test2 <-
# #   opi %>% mutate(birth_doy = yday(ymd(date) - as.numeric(PAT_AGE_DAYS)),
# #                birth_week = week(as.Date("2016-01-01") + birth_doy)) %>% 
# #   filter(as.numeric(PAT_AGE_YEARS) < 15) %>% 
# #   #ggplot(aes(x = birth_doy)) + geom_histogram(bins = 366) + theme_bw() 
# #   group_by(birth_week) %>% 
# #   summarize(n_birth_week_in = n()) #%>% 
# #   #ggplot(aes(x = birth_week, y = n_birth_week)) + geom_step() + theme_bw() 
# left_join(test, test2) %>% 
#   ggplot(aes(x = birth_week, y = n_birth_week_west)) + geom_step(color = "red") + theme_bw() +
#   geom_step(aes(x = birth_week, y = n_birth_week_east*1.2), color = "blue")
# 
# #the two graphs look qualitatively similar; there are only minor differences between the E and W half of the state
# #the differences between asthma and injury show potentially a slightly higher number of ED visits for asthma for people 
# #born in ~November