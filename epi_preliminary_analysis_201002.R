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
age_low <- 18 # >= #young kids = 0, school-aged kids = 5, adults = 18
age_hi <- 99 # <= #young kids = 5, school-aged kids = 17, adults = 99

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
# NAB_tx %>% 
#   #filter(NAB_station == "College Station") %>% 
#   filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
#   ggplot(aes(x = date, y = cup_all_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
#   geom_point(aes(x = date, y = cup_log10), color = "black")
# 
# NAB_tx %>% 
#   #filter(NAB_station == "College Station") %>% 
#   filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
#   ggplot(aes(x = date, y = trees_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
#   geom_point(aes(x = date, y = trees_log10), color = "black")
# 
# NAB_tx %>% 
#   #filter(NAB_station == "College Station") %>% 
#   filter(date > mdy("10/1/2015") & date < mdy("1/1/2018")) %>% 
#   ggplot(aes(x = date, y = pol_other_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
#   geom_point(aes(x = date, y = pol_other_log10), color = "black")


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
# cup_mtc_fit <- lm(NAB_tx_mtc$cup_log10 ~ NAB_tx_mtc$cup_withheld_lm )
# sqrt(mean(cup_mtc_fit$residuals^2)); summary(cup_mtc_fit) #RMSE and R2
# cup_panel <- ggplot(NAB_tx_mtc, aes(x= 10^(cup_withheld_lm) - 1, y = 10^(cup_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
#   geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
#   scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000)) 
# 
# trees_mtc_fit <- lm(NAB_tx_mtc$trees_log10 ~ NAB_tx_mtc$trees_withheld_lm)
# sqrt(mean(trees_mtc_fit$residuals^2)); summary(trees_mtc_fit) #RMSE and R2
# tree_panel <- ggplot(NAB_tx_mtc, aes(x= 10^(trees_withheld_lm) - 1, y = 10^(trees_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
#   geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
#   scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000)) 
# 
# pol_other_mtc_fit <- lm(NAB_tx_mtc$pol_other_log10 ~ NAB_tx_mtc$pol_other_withheld_lm)
# sqrt(mean(pol_other_mtc_fit$residuals^2)); summary(pol_other_mtc_fit) #RMSE and R2
# other_panel <- ggplot(NAB_tx_mtc, aes(x= 10^(pol_other_withheld_lm) - 1, y = 10^(pol_other_log10) - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
#   geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) + 
#   scale_x_log10(limits = c(1, 500)) + scale_y_log10(limits = c(1, 500)) 
# 
# cowplot::plot_grid(cup_panel, tree_panel, other_panel, labels = c("A", "B", "C"))




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


## Using census tract centroids when the block group isn't available but the census tract is (23967 records)
census_tract_coord <- read_csv("C:/Users/dsk856/Desktop/misc_data/TX_census_tract_centroids.csv",  
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

#how many records in each group
# test <- filter(opa_raw, is.na(lon) ) #28817/277232   
# test <- filter(opa_raw, is.na(lon) & !is.na(lon_tract) ) #23967/277232
# test <- filter(opa_raw, is.na(lon) & is.na(lon_tract) & !is.na(lon_zip)) #4662/277232 #188/277232

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
c_vars_youngkids <- c(paste0("B0100", 1003), #males 0 - 4: 1003; 5 - 17: 1004:1006, 18 +: 1007:1025
                       paste0("B0100", 1027)) %>%  #females 0 - 4: 1027; 5 - 17: 1028:1030, 18 +: 1031:1049
                        gsub(pattern = "B01001", replacement ="B01001_", x = .) #adding the underscore back in
c_vars_schoolkids <- c("B01001_004", "B01001_005", "B01001_006", #males from 5-17 years old
            "B01001_028", "B01001_029", "B01001_030") #females from 5-17 years old
c_vars_adults <- c(paste0("B0100", 1007:1025), #males
                  paste0("B0100", 1031:1049)) %>%  #females
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

pop_near_NAB_young_kids <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  filter(variable %in% c_vars_youngkids) %>%  #only select variables that are population of children between 5 and 17
  group_by(NAB_station) %>%
  summarize(young_kids_pop = sum(estimate)) #names(pop_near_NAB)

pop_near_NAB_schoolkids <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
    filter(variable %in% c_vars_schoolkids) %>%  #only select variables that are population of children between 5 and 17
    group_by(NAB_station) %>%
    summarize(schoolkids_pop = sum(estimate)) #names(pop_near_NAB)

pop_near_NAB_adult <- census_All_2017_sf %>% filter(NAB_min_dist_bg < NAB_min_dist_threshold) %>%
  filter(variable %in% c_vars_adult) %>%  #only select variables that are population of children between 5 and 17
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


#making sure the population is set for the correct age group. 
#WARNING: BE CAUTIOUS IF NOT USING THE EXACT AGEGROUPS
if(age_hi < 5){opa_day_agegroup_x <- mutate(opa_day, agegroup_x_pop = young_kids_pop)} #PIBR per 1,000,000 
if(age_hi == 17){opa_day_agegroup_x <- mutate(opa_day, agegroup_x_pop = schoolkids_pop)} #PIBR per 1,000,000 
if(age_hi > 90){opa_day_agegroup_x <- mutate(opa_day, agegroup_x_pop = adult_pop)} #PIBR per 1,000,000 

opa_day_agegroup_x <- opa_day_agegroup_x %>% mutate(pbir =  (n_cases/agegroup_x_pop) * 1000000)


csv_file_name <- paste0("C:/Users/dsk856/Desktop/thcic_analysis/",
                        "opa_day_ages_",age_low,"_",age_hi,"_dist_", NAB_min_dist_threshold, "_",Sys.Date(),".csv")
write_csv(opa_day_agegroup_x, csv_file_name)

#summary(opa_day_agegroup_x)

### exploring data ###################################################
# opa_day <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_5_17_25km_201008.csv", guess_max = 8260)
# opa_day_adult <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_18_99_25km_201008.csv", guess_max = 8260)
# opa_day_youngchildren <- read_csv("C:/Users/dsk856/Desktop/thcic_analysis/opa_day_ages_0_4_25km_210524.csv", guess_max = 8260)

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




#### setting up analysis with a distributed lags model using dlnm package #######################################
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

opa_day <- opa_day_agegroup_x 

## data for Table 1 ============================================================
#needs to be run for each age group
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
    #log_child_pop = log(children_pop),
    #child_pop = children_pop,
    #log_adult_pop = log(adult_pop),
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
                agegroup_x_pop, log_agegroup_x_pop, #child_pop, log_child_pop, adult_pop, log_adult_pop, 
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

## SI: viral prevalence by health service region (3 wk moving average) ----------------------------------------
data_for_model %>% group_by(NAB_station, date) %>% 
  dplyr::select(contains("v_")) %>% 
  ungroup() %>% 
  rename(Rhinovirus = v_pos_rel_adj_Rhinovirus_m21,
         Corona = v_pos_rel_adj_corona_m21,
         RSV = v_pos_rel_adj_RSV_m21,
         Influenza = v_pos_rel_adj_flu_m21
         ) %>% 
  pivot_longer(cols = c(Rhinovirus, Corona, RSV, Influenza)) %>% 
 ggplot(aes( x = date, y = value, color = NAB_station)) + geom_step() + facet_wrap(~name, scales = "free_y", ncol = 1)  + theme_bw() +
  ylab("viral prevalence index (scaled positive tests per day)")


## dlnm model -----------------------------------------------------------------
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
                            argvar=list(fun = "ns", knots = 3), #shape of response curve
                            arglag = list(fun = "ns", knots = 3)) #shape of lag
                      # argvar=list(fun = "poly", degree = 3), #shape of response curve
                      # arglag = list(fun = "poly", degree = 3)) #shape of lag

#viruses in dlnm format for use with attrdl function for AR
rhino_lag <- crossbasis(data_for_model$v_pos_rel_adj_Rhinovirus_m21, lag = 0, #percent of tests positive for rhinovirus
                        # argvar=list(fun = "poly", degree = 3), #shape of response curve
                        # arglag = list(fun = "poly", degree = 3)) #shape of lag
                        argvar=list(fun = "lin"), arglag = list(fun = "lin")) #using a linear response

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


### Fig 2,3,4: visualize effects of pollen ###############################################################
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

child_RR_x_cup_25km <- child_RR_x_cup_25km + geom_rug(data = data_for_model, aes(x = cup_all_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

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

child_RR_x_trees_25km <-  child_RR_x_trees_25km +  geom_rug(data = data_for_model, aes(x = trees_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)

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
  
child_RR_x_pol_other_25km <- child_RR_x_pol_other_25km +geom_rug(data = data_for_model, aes(x = pol_other_m + 1), sides = "t", alpha = 0.1, inherit.aes = FALSE)


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
fig234 <-
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
#fig234
fig_234_name <- paste0("C:/Users/dsk856/Desktop/thcic_analysis/results/",
                       "fig234_pol_ages",age_low,"_",age_hi,"_dist_", NAB_min_dist_threshold, "_",Sys.Date(),".jpg")
ggsave(file = fig_234_name, plot = fig234,
       height = 25, width = 21, units = "cm", dpi = 300)

  
### visualize effects of viruses ###############################################################
  # 
  # ## rhinovirus
  # pred1_rhino <- crosspred(rhino_lag,  model2, cen = 0, cumul = TRUE,
  #                              at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_Rhinovirus_m21), by = 0.0005))
  # data.frame(rhino_conc = pred1_rhino$predvar,
  #              mean = pred1_rhino$allRRfit,
  #              lower = pred1_rhino$allRRlow,
  #              upper = pred1_rhino$allRRhigh) %>%
  #   ggplot(aes(x = rhino_conc, y = mean, ymin = lower, ymax = upper))+
  #   geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  #   xlab("rhino")+ ylab('RR')+theme_few() 
  # 
  # ## corona
  # pred1_corona <- crosspred(corona_lag,  model2, cen = 0, cumul = TRUE,
  #                          at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_corona_m21), by = 0.0005))
  # data.frame(corona_conc = pred1_corona$predvar,
  #            mean = pred1_corona$allRRfit,
  #            lower = pred1_corona$allRRlow,
  #            upper = pred1_corona$allRRhigh) %>%
  #   ggplot(aes(x = corona_conc, y = mean, ymin = lower, ymax = upper))+
  #   geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  #   xlab("corona")+ ylab('RR')+theme_few() 
  # 
  # ## RSV
  # pred1_rsv <- crosspred(rsv_lag,  model2, cen = 0, cumul = TRUE,
  #                        at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_RSV_m21), by = 0.0005))
  # data.frame(flu_conc = pred1_rsv$predvar,
  #            mean = pred1_rsv$allRRfit,
  #            lower = pred1_rsv$allRRlow,
  #            upper = pred1_rsv$allRRhigh) %>%
  #   ggplot(aes(x = flu_conc, y = mean, ymin = lower, ymax = upper))+
  #   geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  #   xlab("rsv")+ ylab('RR')+theme_few()
  # 
  # ## flu
  # pred1_flu <- crosspred(flu_lag,  model2, cen = 0, cumul = TRUE,
  #                           at = seq(from = 0, to = max(data_for_model$v_pos_rel_adj_flu_m21), by = 0.0005))
  # data.frame(flu_conc = pred1_flu$predvar,
  #            mean = pred1_flu$allRRfit,
  #            lower = pred1_flu$allRRlow,
  #            upper = pred1_flu$allRRhigh) %>%
  #   ggplot(aes(x = flu_conc, y = mean, ymin = lower, ymax = upper))+
  #   geom_ribbon(alpha=0.1)+ geom_line()+ geom_hline(lty=2, yintercept = 1)+ # horizontal reference line at no change in odds
  #   xlab("flu")+ ylab('RR')+theme_few() 

### table 2: attributable risk ##########################################
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
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_trees <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases, 
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_other_pol <- attrdl(x = data_for_model$pol_other_lm, basis = pol_other_lag, cases = data_for_model$n_cases,
                        model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_rhino <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases, 
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_corona <- attrdl(x = data_for_model$v_pos_rel_adj_corona_m21, basis = corona_lag, cases = data_for_model$n_cases, 
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_flu <- attrdl(x = data_for_model$v_pos_rel_adj_flu_m21, basis = flu_lag, cases = data_for_model$n_cases, 
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

attr_t_rsv <- attrdl(x = data_for_model$v_pos_rel_adj_RSV_m21, basis = rsv_lag, cases = data_for_model$n_cases, 
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "an", range = NULL)

#predicted_n_cases_t <- c(rep(NA, max_lag + 0), model1$fitted.values) #hist(model2$fitted.values)
#predicted_n_cases_t <- c(rep(NA, max_lag + 1), model2$fitted.values) #hist(model2$fitted.values)

# fit <- lm(data_for_model$n_cases ~ predicted_n_cases_t)
# summary(fit)
# plot(jitter(data_for_model$n_cases), predicted_n_cases_t)

attr_df <- data.frame(attr_t_cup, attr_t_trees, attr_t_other_pol, attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_rsv) #attr_t_other_pol, , predicted_n_cases_t
attr_df[attr_df < 0] <- 0 #removing net protective effects of all variables
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
                                              Cupressaceae = "attr_t_cup", 
                                              trees = "attr_t_trees", 
                                              'other pollen' = "attr_t_other_pol"),
         attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "RSV","Influenza",
                                                                       "Cupressaceae", "trees", "other pollen")), #"other_pollen"  "unexplained"
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))
         
observed_ncases_t <- data_for_model %>% 
  dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
  mutate(week = week(date),
         date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 )) %>% 
  group_by(NAB_station, agegroup_x_pop, week, date2) %>% 
  summarize(mean_cases = mean(n_cases)) %>% 
  mutate(attr_risk_var = "Rhinovirus") 


fig567_ymax <-  ifelse(age_low == 0,   20, #setting figure ymax manually
                (ifelse(age_low == 5,  55, 
                (ifelse(age_low == 18, 10, 
                        print("error in age_low"))))))
fig567 <- ggplot(attr_full_df, aes(x = date2, y = (risk_cases / agegroup_x_pop) * 1000000, fill = attr_risk_var)) + 
  facet_wrap(~NAB_station) + geom_area() + theme_bw() + scale_fill_viridis_d(name = "attributable risk") + 
  ylab("asthma-related ED visits (per 1,000,000 people per day)") + xlab("date") +
  scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(0, fig567_ymax)) + #c(-0.01, .15) #c(-0.02, .6)
  geom_step(data = observed_ncases_t, aes( x = date2, y =(mean_cases / agegroup_x_pop) * 1000000, 
                                           color = "observed cases")) + scale_color_discrete(name = "") 

#fig567
fig_567_name <- paste0("C:/Users/dsk856/Desktop/thcic_analysis/results/",
                       "fig567_AR_ages",age_low,"_",age_hi,"_dist_", NAB_min_dist_threshold, "_",Sys.Date(),".jpg")
ggsave(file = fig_567_name, plot = fig567, height = 25, width = 21, units = "cm", dpi = 300)






### attributable risk fraction over time ################################################
attr_tf_cup <- attrdl(x = data_for_model$cup_all_lm, basis = cup_lag, cases = data_for_model$n_cases,
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_trees <- attrdl(x = data_for_model$trees_lm, basis = trees_lag, cases = data_for_model$n_cases,
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_other_pol <- attrdl(x = data_for_model$pol_other_lm, basis = pol_other_lag, cases = data_for_model$n_cases,
                           model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_rhino <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases,
                       model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_corona <- attrdl(x = data_for_model$v_pos_rel_adj_corona_m21, basis = corona_lag, cases = data_for_model$n_cases,
                        model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_flu <- attrdl(x = data_for_model$v_pos_rel_adj_flu_m21, basis = flu_lag, cases = data_for_model$n_cases,
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_tf_rsv <- attrdl(x = data_for_model$v_pos_rel_adj_RSV_m21, basis = rsv_lag, cases = data_for_model$n_cases,
                     model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)

attr_f_df <- data.frame(attr_tf_cup, attr_tf_trees, attr_tf_other_pol, attr_tf_rhino, attr_tf_corona, attr_tf_flu, attr_tf_rsv) 
attr_f_df[attr_f_df < 0] <- 0 #removing net protective effects of all variables

### SI table: the percent of AR for each pollen type by each city across the main pollen season ##################
attr_f_cup_jan <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_cup) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  filter(date_month == 1) %>%
  group_by(NAB_station) %>%
  summarize(cup_prop_cases_jan = mean(attr_tf_cup))
attr_f_cup_allyr <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_cup) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  group_by(NAB_station, date_month) %>%
  summarize(cup_prop_cases_all_mo_r = mean(attr_tf_cup, na.rm = TRUE)) %>% #so the extra fall months aren't given more weight
  group_by(NAB_station) %>% 
  summarize(cup_prop_cases_all_mo = mean(cup_prop_cases_all_mo_r))


attr_f_trees_mar <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_trees) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  filter(date_month == 3) %>%
  group_by(NAB_station) %>%
  summarize(trees_prop_cases_mar = mean(attr_tf_trees))
attr_f_trees_allyr <- bind_cols(data_for_model, attr_f_df) %>%
  dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_trees) %>%   #attr_t_other_pol,
  mutate(date_month = month(date)) %>%
  group_by(NAB_station, date_month) %>%
  summarize(trees_prop_cases_all_mo_r = mean(attr_tf_trees, na.rm = TRUE)) %>% #so the extra fall months aren't given more weight
  group_by(NAB_station) %>% 
  summarize(trees_prop_cases_all_mo = mean(trees_prop_cases_all_mo_r))


#prepare for pasting into excel/word
cup_by_city_jan_wholeyear <- sprintf("%.1f", c(100*attr_f_cup_jan$cup_prop_cases_jan, 100*attr_f_cup_allyr$cup_prop_cases_all_mo))
write.table(cup_by_city_jan_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

trees_by_city_mar_wholeyear <- sprintf("%.1f", c(100*attr_f_trees_mar$trees_prop_cases_mar, 
                                                 100*attr_f_trees_allyr$trees_prop_cases_all_mo))
write.table(trees_by_city_mar_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)



### model/Table 2 SI version without viruses ########################################################

model1 <- glm(n_cases ~  #number of cases at a station on an observed day
                NAB_station + #effect of station
                offset(log(agegroup_x_pop)) +  #offset for the population of a study area
                cup_lag +  trees_lag  + pol_other_lag + #dlnm crossbasis for each pollen type
                #rhino_lag + corona_lag  + rsv_lag +  flu_lag +
              week_day, #day of week term
              family = quasipoisson, #quasipoisson
              data = data_for_model)  

# include the 1-day lagged residual in the model
resid_model1 <- c(rep(NA, max_lag), residuals(model1, type = "deviance"))
model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1))  #length(resid_model1) #length(residuals(model1, type = "deviance"))

table2 <- data.frame(variable = c("Cupressaceae", "Trees", "Other_pollen", "Rhinovirus", "Corona", "RSV",
                                  "Influenza"), 
                     yc_n_cases_mean = rep(NA, 7), yc_n_cases_2.5 = rep(NA, 7), yc_n_cases_97.5 = rep(NA, 7),
                     yc_p_cases_mean = rep(NA, 7), yc_p_cases_2.5 = rep(NA, 7), yc_p_cases_97.5 = rep(NA, 7),
                     c_n_cases_mean = rep(NA, 7), c_n_cases_2.5 = rep(NA, 7), c_n_cases_97.5 = rep(NA, 7),
                     c_p_cases_mean = rep(NA, 7), c_p_cases_2.5 = rep(NA, 7), c_p_cases_97.5 = rep(NA, 7),
                     a_n_cases_mean = rep(NA, 7), a_n_cases_2.5 = rep(NA, 7), a_n_cases_97.5 = rep(NA, 7),
                     a_p_cases_mean = rep(NA, 7), a_p_cases_2.5 = rep(NA, 7), a_p_cases_97.5 = rep(NA, 7))

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



# version without pollen -------------------------------------------------------
model1 <- glm(n_cases ~  #number of cases at a station on an observed day
                NAB_station + #effect of station
                offset(log(agegroup_x_pop)) +  #offset for the population of a study area
                #cup_lag +  trees_lag  + pol_other_lag + #dlnm crossbasis for each pollen type
                rhino_lag + corona_lag  + rsv_lag +  flu_lag +
                week_day, #day of week term
              family = quasipoisson, #quasipoisson
              data = data_for_model)  

# include the 1-day lagged residual in the model
resid_model1 <- c(rep(NA, 0), residuals(model1, type = "deviance"))#for the model version when pollen isn't included
model2 <- update(model1, .~. + tsModel::Lag(resid_model1, 1))  #length(resid_model1) #length(residuals(model1, type = "deviance"))

#rhinovirus
rhino_attr <- attrdl(x = data_for_model$v_pos_rel_adj_Rhinovirus_m21, basis = rhino_lag, cases = data_for_model$n_cases, 
                     model = model2, dir = "back", sim = TRUE, cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 10000)
table2$yc_n_cases_mean[4] <- sprintf("%.0f", mean(rhino_attr))
table2$yc_n_cases_2.5[4] <- sprintf("%.0f", as.numeric(quantile(rhino_attr, probs = 0.025)))
table2$yc_n_cases_97.5[4] <- sprintf("%.0f", as.numeric(quantile(rhino_attr, probs = 0.975)))
table2$yc_p_cases_mean[4] <- sprintf("%.1f", 100*mean(rhino_attr) / sum(model2$fitted.values))
table2$yc_p_cases_2.5[4] <- sprintf("%.1f", 100*as.numeric(quantile(rhino_attr, probs = 0.025))/sum(model2$fitted.values))
table2$yc_p_cases_97.5[4] <- sprintf("%.1f", 100*as.numeric(quantile(rhino_attr, probs = 0.975))/sum(model2$fitted.values))

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
table2_SI_paste <- data.frame(col1 = paste0(table2$yc_n_cases_mean, " (", table2$yc_n_cases_2.5, " - ", table2$yc_n_cases_97.5, ")"),
                           col2 = paste0(table2$yc_p_cases_mean, " (", table2$yc_p_cases_2.5, " - ", table2$yc_p_cases_97.5, ")"))
write.table(table2_SI_paste, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)
table2_SI_paste




### another version of everything that needs to be pasted into the figures in word ####################☻

#table 2
write.table(table2_paste, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

#table 2 SI (only pollen or only viruses)
write.table(table2_SI_paste, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

#effects of pollen during highest month per city
cup_by_city_jan_wholeyear <- sprintf("%.1f", c(100*attr_f_cup_jan$cup_prop_cases_jan, 100*attr_f_cup_allyr$cup_prop_cases_all_mo))
write.table(cup_by_city_jan_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)

trees_by_city_mar_wholeyear <- sprintf("%.1f", c(100*attr_f_trees_mar$trees_prop_cases_mar, 
                                                 100*attr_f_trees_allyr$trees_prop_cases_all_mo))
write.table(trees_by_city_mar_wholeyear, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE)






# attr_full_df <- bind_cols(data_for_model, attr_df) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 attr_t_cup, attr_t_trees, attr_t_other_pol, 
#                 attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_rsv) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(week = week(date)) %>% 
#   group_by(NAB_station, agegroup_x_pop, week, var) %>% 
#   summarize(risk_cases = mean(risk_cases)) %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Rhinovirus = "attr_t_rhino", 
#                                                      Corona = "attr_t_corona",
#                                                      RSV = "attr_t_rsv",
#                                                      Influenza = "attr_t_flu",
#                                                      Cupressaceae = "attr_t_cup", 
#                                                      trees = "attr_t_trees", 
#                                                      'other pollen' = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "RSV","Influenza",
#                                                                        "Cupressaceae", "trees", "other pollen")), #"other_pollen"  "unexplained"
#          date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))
# ggplot(attr_full_df, aes(x = date2, y = (risk_cases), fill = attr_risk_var)) + 
#   facet_wrap(~NAB_station) + geom_area() + theme_bw() + scale_fill_viridis_d(name = "attributable risk") + 
#   ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
#   scale_x_date(labels = date_format("%b")) 


  
# ## a table giving the percent of AR for each pollen type by each city across the study period
# bind_cols(data_for_model, attr_f_df) %>% 
#   group_by(NAB_station) %>% 
#   summarize(attr_tf_cup_mean = mean(attr_tf_cup, na.rm = TRUE), 
#             attr_tf_trees_mean = mean(attr_tf_trees, na.rm = TRUE),
#             attr_tf_other_pol_mean = mean(attr_tf_other_pol, na.rm = TRUE))
# 
# ## table: the percent of AR for each pollen type by each city across the main pollen season
# bind_cols(data_for_model, attr_f_df) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop,  attr_tf_cup) %>%   #attr_t_other_pol,
#   mutate(date_month = month(date)) %>% 
#   filter(date_month == 1) %>% 
#   group_by(NAB_station) %>% 
#   summarize(cup_prop_cases_jan = mean(attr_tf_cup))
# 
# bind_cols(data_for_model, attr_f_df) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop, attr_tf_trees) %>%   #attr_t_other_pol,
#   mutate(date_month = month(date)) %>% 
#   filter(date_month == 3) %>% 
#   group_by(NAB_station) %>% 
#   summarize(trees_prop_cases_march = mean(attr_tf_trees))


# #testing why cup AR isn't higher for adults (dose response curve is sig, table 2)
# hist(attrdl(x = data_for_model$cup_all_lm[data_for_model$cup_all_lm > 2.5], basis = cup_lag, 
#                cases = data_for_model$n_cases[data_for_model$cup_all_lm > 2.5], model = model2,
#                    dir = "back", sim = TRUE,
#                    cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 1000), breaks = 50)
# hist(data_for_model$cup_all_lm, breaks = 50)
# 
# cup_attr <- attrdl(x = data_for_model$cup_all_lm[data_for_model$cup_all_lm > 2.5], basis = cup_lag, 
#                    cases = data_for_model$n_cases[data_for_model$cup_all_lm > 2.5], model = model2,
#                    dir = "back", sim = TRUE,
#                    cen = 0, tot = TRUE, type = "an", range = NULL, nsim = 1000)
# sprintf("%.1f", 100*mean(cup_attr) / sum(model2$fitted.values))
# sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.025))/sum(model2$fitted.values))
# sprintf("%.1f", 100*as.numeric(quantile(cup_attr, probs = 0.975))/sum(model2$fitted.values))
# 
# plot(data_for_model$n_cases, c(rep(NA, 8,), model2$fitted.values))
# length(data_for_model$n_cases); length(model2$fitted.values)
# sum(data_for_model$n_cases); sum(model2$fitted.values)
# 
# attr_t_cup <- attrdl(x = data_for_model$cup_all_lm , basis = cup_lag, cases = data_for_model$n_cases *2, 
#                      model = model2, dir = "back", sim = FALSE, cen = 0, tot = FALSE, type = "af", range = NULL)
# #hist(attr_t_cup, breaks = 50)
# plot(attr_t_cup)
# mean(attr_t_cup, na.rm = TRUE)
# 
# ### a figure of just one city for the entire time period  
# NAB_station_filter <- "San Antonio A"
# attr_full_ex_df <- bind_cols(data_for_model, attr_df) %>% 
#   filter(NAB_station == NAB_station_filter) %>% 
#   # mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu - attr_t_other_pol) %>% 
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 #attr_t_unexplained, 
#                 attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu, attr_t_other_pol) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Rhinovirus = "attr_t_rhino", Corona = "attr_t_corona", Influenza = "attr_t_flu",
#                                                      Cupressaceae = "attr_t_cup", trees = "attr_t_trees", other_pollen = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza",
#                                                  "Cupressaceae", "trees", "other_pollen"))) #"other_pollen"  "unexplained"
# 
# 
# observed_ncases_ex_t <- data_for_model %>% 
#   filter(NAB_station == NAB_station_filter) %>% 
#   dplyr::select(NAB_station, agegroup_x_pop, date, n_cases) %>% 
#   mutate(attr_risk_var = "Rhinovirus")
# 
# 
# test_data <- data_for_model 
# test_model_pred <- predict.glm(object = model2, newdata = test_data, type = "response")  #str(model_pred)
# 
# predicted_ncases_ex_t <-  data_for_model %>% ungroup() %>% 
#   mutate(predicted_ncases_vector_model1 = c(rep(NA, max_lag + 0), model1$fitted.values),
#          predicted_ncases_resid_model1 =  c(rep(NA, max_lag + 0),  model1$residuals),   
#          predicted_ncases_vector_model2 = c(rep(NA, max_lag + 1), model2$fitted.values),
#          predicted_ncases_resid_model2 =  c(rep(NA, max_lag + 1),  model2$residuals),   
#          predicted_ncases_glmpred = test_model_pred) %>% 
#   filter(NAB_station == NAB_station_filter) %>%                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#   mutate(attr_risk_var = "Rhinovirus")
# 
# ggplot(attr_full_ex_df, aes(x = date, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) + 
#   facet_wrap(~NAB_station) +  theme_bw() + scale_fill_viridis_d(name = "attributable risk") + # geom_area() +
#   ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
#   #coord_cartesian(ylim = c(-0.02, .9)) + #c(-0.01, .15) #c(-0.02, .6)
#   geom_line(data = observed_ncases_ex_t, aes( x = date, y =(n_cases / agegroup_x_pop) * 10000, 
#                                            color = "observed cases"), color = "black") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y =(predicted_ncases_vector_model1 / agegroup_x_pop) * 10000, 
#                                               color = "predicted cases"), color = "red") +
#   # geom_line(data = predicted_ncases_ex_t, aes( x = date, y =(predicted_ncases_resid_model1 / agegroup_x_pop) * 10000, 
#   #                                              color = "predicted cases"), color = "green") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y = cup_all_lm/3.7, 
#                                                color = "predicted cases"), color = "blue") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y = trees_lm/3.7, 
#                                                color = "predicted cases"), color = "green") +
#   geom_line(data = predicted_ncases_ex_t, aes( x = date, y = v_tests_perc_pos_Rhinovirus_m/100, 
#                                                color = "predicted cases"), color = "pink") +
#   
#   #scale_color_discrete(name = "") +
#   scale_x_date(limits = c(mdy("1/10/2016"), mdy("12/1/2016")))# labels = date_format("%b")) 
# 
# max(predicted_ncases_ex_t$cup_all_lm)
# 
# hist(model2$effects)
# str(model2)
# 
# model2$residuals[1100:1110] 
# (model2$fitted.values[1100:1110] )
# model2$y[1100:1110]
# 
# 
# ### figure of all time series as a kind of check
# bind_cols(data_for_model, attr_df) %>% 
#   ggplot(aes(x = date, y = pbir)) + geom_point(color = "gray") + theme_bw() + facet_wrap(~NAB_station) +
#   #geom_smooth(se = F, color = "black")+
#   geom_line(aes(y=zoo::rollmean(pbir, 7, na.pad=TRUE)), color = "black") +
#   
#   geom_line( aes( x = date, y = (cup_all_lm)/5,  color = "predicted cases"), color = "blue") +
#   geom_line( aes( x = date, y = (trees_lm)/5,  color = "predicted cases"), color = "green") +
#   
#   scale_y_continuous(limits = c(0, 1)) +
#   scale_x_date(limits = c(mdy("1/10/2016"), mdy("12/1/2016")))# labels = date_format("%b")) 
# 
# 
# 
# 
# ### for the animation
# attr_full_df_example <- bind_cols(data_for_model, attr_df) %>% 
#   #mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>%  
#   #- attr_t_other_pol) %>% #attr_t_other_pol
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(week = week(date)) %>% 
#   group_by(NAB_station, agegroup_x_pop, week, var) %>% 
#   summarize(risk_cases = mean(risk_cases)) %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Corona = "attr_t_corona", Influenza = "attr_t_flu",
#                                                      Rhinovirus = "attr_t_rhino", Cupressaceae = "attr_t_cup", trees = "attr_t_trees"),  #other_pollen = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Corona", "Influenza","Rhinovirus", 
#                                                                         "trees", "Cupressaceae")), #"other_pollen"
#          date2 = lubridate::ymd( "2016-01-01" ) + lubridate::weeks( week - 1 ))
# 
# observed_ncases_t_example <-filter(observed_ncases_t, NAB_station == "San Antonio B") 
# 
# filter(attr_full_df_example, NAB_station == "San Antonio B") %>% 
# ggplot( aes(x = date2, y = (risk_cases / agegroup_x_pop) * 100000, fill = attr_risk_var)) + 
#  geom_area() + theme_bw() + 
#   scale_fill_manual(name = "attributable risk", values = c(rgb(68, 1, 84, alpha = 0, names = NULL, maxColorValue = 255), #corona
#                                                            rgb(253, 231, 37, alpha = 1, names = NULL, maxColorValue = 255), #cupr
#                                                            rgb(72, 40, 120, alpha = 0, names = NULL, maxColorValue = 255), #flu
#                                                            rgb(49, 104, 142, alpha = 0, names = NULL, maxColorValue = 255),  #rhino
#                                                            rgb(109, 205, 89, alpha = 0, names = NULL, maxColorValue = 255))) + #trees
#   ylab("asthma-related ED visits (per 100,000 people per day)") + xlab("date") +
#   scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(-0.02, 8)) + #c(-0.01, .15) #c(-0.02, .6)
#   geom_step(data = observed_ncases_t_example, aes( x = date2, y =(mean_cases / agegroup_x_pop) * 100000,
#                                                    color = "observed cases", fill = NA), 
#             color = rgb(1,0,0, alpha = 1, maxColorValue = 1)) +
#   theme(text = element_text(size=18))
# 
# 
# 
# 
# bind_cols(data_for_model, attr_df) %>% 
#   mutate(attr_t_unexplained = predicted_n_cases_t - attr_t_cup - attr_t_trees - attr_t_rhino - attr_t_corona - attr_t_flu) %>%  
#   #- attr_t_other_pol) %>% #attr_t_other_pol
#   dplyr::select(date, NAB_station, agegroup_x_pop, 
#                 attr_t_unexplained, attr_t_cup, attr_t_trees,  attr_t_rhino, attr_t_corona, attr_t_flu) %>%   #attr_t_other_pol,
#   pivot_longer(cols = contains("attr_t_"), names_to = "var", values_to = "risk_cases") %>% 
#   mutate(week = week(date)) %>% 
#   group_by(NAB_station, agegroup_x_pop, week, var) %>% 
#   summarize(risk_cases = mean(risk_cases)) %>% 
#   filter(attr_full_df, NAB_station == "San Antonio B") %>%
#   filter(var != "attr_t_unexplained")  %>% 
#   mutate(attr_risk_var_unorder = forcats::fct_recode(var, #unexplained = "attr_t_unexplained",
#                                                      Rhinovirus = "attr_t_rhino", Corona = "attr_t_corona", Influenza = "attr_t_flu",
#                                                      Cupressaceae = "attr_t_cup", trees = "attr_t_trees"),  #other_pollen = "attr_t_other_pol"),
#          attr_risk_var = forcats::fct_relevel(attr_risk_var_unorder, c("Rhinovirus", "Corona", "Influenza",
#                                                                        "Cupressaceae", "trees"))) %>% 
# ggplot(aes(x = date2, y = (risk_cases / agegroup_x_pop) * 10000, fill = attr_risk_var)) +
#   geom_area() +
#   theme_bw() + scale_fill_viridis_d(name = "attributable risk") +
#   ylab("asthma-related ED visits (per 10,000 people per day)") + xlab("date") +
#   scale_x_date(labels = date_format("%b")) + coord_cartesian(ylim = c(-0.02, .7)) +
# 
# 
# 
# 

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







