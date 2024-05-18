#extract data from NAB 2019 - 2021 data request and combine with 2009-2019 data request
# the old version of this script is on box: C:/Users/dsk273/Box/texas/NAB/extract_pollen_data_from_2021_data_request.R

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(purrr)
library(lubridate)
library(stringr)
library(ggplot2)
library(here)
library(imputeTS)
#library(tidyverse)
library(ggthemes)
library(zoo)

setwd("C:/Users/danka/Box")
here::i_am("katz_photo.jpg")


### extract data from 2019 to 2021 data request ###########################################################
### set working directory to folder with all of the NAB files in original NAB format
setwd(here("texas", "NAB", "NAB_2019_2021"))

#list of files
file_list_raw <- dir()#folder is pretty clean, nothing here besides .xls
file_list <- file_list_raw[!file_list_raw %in% "Attribution List- Katz.pdf"]
file_list <- file_list[!file_list %in% "old_format"]

#start file loop
d <- NULL
for(i in 1:length(file_list)){
  #for(i in 1:4){
  file_i <-file_list[i] #file_i <-file_list[1]
  
  ## extract data from a single spreadsheet
  
  #extract and clean column names
  p_names <- file_i %>%
    excel_sheets() %>%
    map_df(~ read_excel(path = file_i, range = cell_rows(2:3), col_names = FALSE)) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(pnames_clean = case_when(!is.na(V2) ~ repair_names(V2),
                                    is.na(V2) ~ repair_names(V1))) %>% 
    dplyr::select(pnames_clean) %>% 
    pull(pnames_clean)
  
  #extract data from file and add in names
  p <- file_i %>%
    excel_sheets() %>%
    set_names() %>% 
    map_df(~ read_excel(path = file_i, range = cell_rows(6:20000), col_names = p_names, .name_repair = "universal")) %>% 
    mutate(Station.Postal.Code = as.character(Station.Postal.Code))
  
  p$file <- file_i
  if(i == 1){d <- p}else{d <- bind_rows(d, p)}
} #end file list loop

#check the number of real obs per station
test <-
  d %>% 
  filter(!is.na(Station.City)) %>% 
  group_by(Station.City) %>% 
  summarize(n = n())

unique(d$Station.City)

#clean out white space
nab2019_2021 <- d %>% filter(!is.na(Station.City))

unique(nab2019_2021$Station.City)
nab2019_2021 <- nab2019_2021 %>% 
  mutate(NAB_station = case_when(Station.City == "San Antonio (2)" ~ "San Antonio B",
                                 Station.City == "San Antonio (3)" ~ "San Antonio A",
                                 Station.City == "Waco" ~ "Waco A",
                                 Station.City == "Houston (Station 2)" ~ "Houston",
                                 Station.City == "Georgetown" ~ "Austin",
                                 TRUE ~ Station.City))


write_csv(nab2019_2021, here("texas", "NAB", "NAB2019_2021_pollen_220810.csv")) #save collated file as csv

tx_NAB_stations <- c("Austin", "College Station","Dallas", "Flower Mound", "Houston", "San Antonio A", "San Antonio B", 
               "Waco A", "Waco B")

nab2019_2021_tx <- nab2019_2021 %>% filter(NAB_station %in% tx_NAB_stations) 
unique(nab2019_2021$NAB_station)
unique(nab2019_2021_tx$NAB_station)


### combining with the 2009-2019 data #################################################################################
tx_cities <- c("Austin", "College Station","Dallas", "Flower Mound", "Houston", "San Antonio 2", "San Antonio 3", "San Antonio (2)", "San Antonio (3)", "Waco", "Waco 2")

nab2009_2019 <- read_csv(here("texas", "NAB", "NAB2009_2019_pollen_200508.csv")) #processed in the "extract_pollen_data_from_xls.R" script
nab2009_2019_tx <- filter(nab2009_2019, site2 %in% tx_cities) %>% 
  mutate(NAB_station = case_when(site2 == "San Antonio 2" ~ "San Antonio B",
                                 site2 == "San Antonio 3" ~ "San Antonio A",
                                 site2 == "Waco" ~ "Waco A",
                                 site2 == "Waco 2" ~ "Waco B",
                                 TRUE ~ site2)) 

unique(nab2009_2019_tx$NAB_station)
unique(nab2019_2021_tx$NAB_station) #missing from new request: Dallas, College Station, Waco B

names(nab2019_2021_tx)
names(nab2009_2019_tx)

nab2009_2019_tx_join <- nab2009_2019_tx %>% dplyr::select(-"...1",-date, -city, -state, -site2, -lat, -long, -file)
nab2019_2021_tx_join <- nab2019_2021_tx %>% dplyr::select(-Station.ID, -Station.Name, - Station.City, -Station.State, - Station.Postal.Code, -Station.Country, -file)


nab2009_2021_tx <-  bind_rows(nab2009_2019_tx_join, nab2019_2021_tx_join)

names(nab2009_2021_tx)


### summarizing plant taxa at each NAB station for an SI section #####################################################
NAB_tx_pol <- left_join(nab2009_2021_tx, NAB_locations) %>%
  filter(!is.na(NAB_station)) %>% 
  mutate(date = ymd(Date))

names(NAB_tx_pol)

NAB_tx_pol_long <- NAB_tx_pol %>% 
  select(-c(48:85)) %>% 
  select(-site) %>% 
  pivot_longer(cols = 3:44, names_to = "taxon", values_to = "pollen") %>% 
  mutate(year_s = year(date)) %>% 
  filter(!is.na(pollen))


mean_annual_pollen <- NAB_tx_pol_long %>% 
  group_by(taxon, NAB_station, year_s) %>% 
  summarize(sum_annual_pollen = sum(pollen)) %>% 
  group_by(NAB_station, taxon) %>% 
  summarize(mean_annual_pollen = mean(sum_annual_pollen)) %>% 
  group_by(NAB_station) %>% 
  summarize(total_mean_annual_pollen = sum(mean_annual_pollen))

  
#Fig SI 2
  NAB_tx_pol_long %>% 
  group_by(taxon, NAB_station, year_s) %>% 
  summarize(sum_annual_pollen = sum(pollen)) %>% 
  group_by(NAB_station, taxon) %>% 
  summarize(mean_annual_pollen = mean(sum_annual_pollen)) %>% 
  arrange(NAB_station, -mean_annual_pollen) %>% 
  left_join(., mean_annual_pollen) %>% 
  mutate(rel_pol = round(mean_annual_pollen/total_mean_annual_pollen, 3)) %>% 
  filter(rel_pol > 0.01) %>% 
  filter(NAB_station != "Waco B") %>% 
  mutate(taxon = case_when(taxon == "Gramineae...Poaceae" ~ "Poaceae",
                           taxon ==  "Asteraceae..Excluding.Ambrosia.and.Artemisia." ~ "Asteraceae",
                           taxon == "Other.Grass.Pollen" ~ "Poaceae",
                           TRUE ~ taxon)) %>% 
   #group_by(taxon) %>% summarize(mean_rel_pol = mean(rel_pol))  #proportion of each taxon
  ggplot(aes(x= reorder(taxon, - rel_pol), y = rel_pol*100)) + geom_col() + theme_bw() + facet_wrap(~NAB_station, scales = "free")+
    scale_x_discrete(guide = guide_axis(angle = 45)) + xlab("pollen type") + ylab("relative pollen abundance (%)")



### adding in station metadata ########################################################################################
NAB_locations <- read_csv(here("texas", "NAB", "EPHT_Pollen Station Inventory_062019_dk220318.csv")) %>% 
  filter(Station_name != "Family Allergy & Asthma Care") %>% #prevent duplication of Flower Mound 
  dplyr::select(NAB_station, Lat, Long, FIPS, MSA, county_name, ZIP, City) %>% 
  mutate(FIPS = sprintf("%03s",FIPS), #NAB_tx_pol$FIPS
         FIPS = sub(" ", "0",FIPS),
         FIPS = sub(" ", "0",FIPS))


NAB_tx_pol <- left_join(nab2009_2021_tx, NAB_locations) %>%
  filter(!is.na(NAB_station)) %>% 
  mutate(date = ymd(Date)) %>%
  rowwise()%>%
  mutate(Fagales = sum(Alnus, Betula, Carpinus.Ostrya, Corylus, Quercus, Fagus, Myrica, na.rm = TRUE),
         other_trees = sum(Acer, Fraxinus, Juglans, Liquidambar, Other.Tree.Pollen, Pinaceae, Populus, Pseudotsuga, Salix, 
                           Carya, Ligustrum, Olea, Platanus, Tilia, Celtis, Tsuga, Prosopis, na.rm = TRUE),
         trees = sum(abs(Morus), abs(Ulmus), Fagales, other_trees, na.rm = TRUE), #fixing an erroneous entry of -6 for Morus
         grass = sum(Gramineae...Poaceae, Other.Grass.Pollen , na.rm = TRUE),
         herbaceous = sum( Artemisia, Asteraceae..Excluding.Ambrosia.and.Artemisia., Chenopodiaceae.Amaranthaceae,
                           Other.Weed.Pollen, Plantago, Rumex,  Arecaceae, Cyperaceae, Typha,
                           Eupatorium, Urticaceae, na.rm = TRUE ),
         pol_other = sum(grass, Ambrosia, Unidentified.Pollen, herbaceous, na.rm = TRUE),
         
         #log10 transform final selected categories
         cup_log10 = log10(abs(Cupressaceae) + 1),  #for checking negative entry error: test <- NAB_tx_pol[27071, ]
         trees_log10 = log10(trees + 1),
         pol_other_log10 = log10(pol_other + 1)
         
         #a quick check to make sure that I'm not losing any pollen columns in this section
         #test_pol = Total.Pollen.Count - Cupressaceae - trees - pol_other #this check shows my math is fine, but there are ~10 rows
         #...  where the Total.Pollen.Count column appears to have incorrect sums (based on manual inspection of original .xls files)
  )  %>% 
  ungroup() %>% 
  #arrange(desc(test_pol)) #for manual check, described in lines just above
  dplyr::select(NAB_station, date, #mo, doy, year, #City, FIPS, county_name, MSA, Lat, Long, NAB_station,
                Cupressaceae, trees, pol_other,
                cup_log10, trees_log10, pol_other_log10,
                Lat, Long, FIPS, MSA, county_name, ZIP, City) 


write_csv(NAB_tx_pol, here("texas", "NAB", "NAB2009_2021_tx_epi_pollen_220810.csv"))



### processing data: interpolating small amounts of missing data #######################################################
#this was originally done in the main analysis script, but moving here for better organization
# before 8/5/22 this was included in this script and before 3/15/21 this was a separate script: ~/TX_epi/NAB_missing_data.R
# I originally used random forest instead of linear imputation, but the results were only a little better
# and it was much more difficult to explain, so I switched to linear imputation

NAB_gaps <- read_csv(here("texas", "NAB", "NAB2009_2021_tx_epi_pollen_220810.csv"))
 #read_csv("Z:/THCIC/Katz/data_pollen/NAB2009_2021_tx_epi_pollen_220805.csv", guess_max = 92013)

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


#expand to include missing dates
date_station_grid <- expand_grid(seq(min(NAB_gaps$date),max(NAB_gaps$date), by = '1 day'), 
                                 unique(NAB_gaps$NAB_station)) %>% 
  `colnames<-`(c("date", "NAB_station")) %>%
  filter(!is.na(NAB_station)) %>% 
  ungroup()

NAB <- left_join(date_station_grid, NAB_gaps) %>%
  arrange(NAB_station, date)



### simple linear interpolation ##############################################################################################
# using a random forest is slightly better but far more complicated and I don't think it's worth it because it is just so much more complicated to explain
# the marginal benefit is probably an R2 improvement of ~0.02 
# so, as of 10/2/20, I'm going to switch over to just a linear interpolation:

#linear interpolation of pollen data
NAB_tx <- NAB %>% 
  group_by(NAB_station) %>% 
  mutate(
    mo = month(date),
    doy = yday(date),
    year = year(date),
    year_f = paste0("y_", year(date)),
    
    cup_all_m = na_interpolation(Cupressaceae, maxgap = 7),
    cup_all_m2 = na_interpolation(Cupressaceae, maxgap = 2),
    cup_all_lm = na_interpolation(cup_log10, maxgap = 7),
    trees_m = na_interpolation(trees, maxgap = 7),
    trees_m2 = na_interpolation(trees, maxgap = 2),
    trees_lm = na_interpolation(trees_log10, maxgap = 7),
    pol_other_m = na_interpolation(pol_other, maxgap = 7),
    pol_other_m2 = na_interpolation(pol_other, maxgap = 2),
    pol_other_lm = na_interpolation(pol_other_log10, maxgap = 7)
  ) 


#check on data
NAB_tx %>%
  #filter(NAB_station == "College Station") %>%
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>%
  ggplot(aes(x = date, y = cup_all_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") + #red = interpolated missing data
  geom_point(aes(x = date, y = cup_log10), color = "black")

NAB_tx %>%
  #filter(NAB_station == "College Station") %>%
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>%
  ggplot(aes(x = date, y = trees_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
  geom_point(aes(x = date, y = trees_log10), color = "black")

NAB_tx %>%
  #filter(NAB_station == "College Station") %>%
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>%
  ggplot(aes(x = date, y = pol_other_lm))  + facet_wrap(~NAB_station) + theme_bw() + geom_point(color = "red") +
  geom_point(aes(x = date, y = pol_other_log10), color = "black")


write_csv(NAB_tx, here("texas", "NAB", "NAB2009_2021_tx_epi_pollen_220831.csv"))


#some stats for paper
#amount of missing data per station
NAB_tx %>% 
  filter(date > mdy("10-1-2015") & date < mdy("1/1/21")) %>% 
  group_by(NAB_station) %>% 
  mutate(missing_pollen_data = case_when(is.na(Cupressaceae) | is.na(trees) | is.na(pol_other) ~ 1,
                                         !is.na(Cupressaceae) & !is.na(trees) & !is.na(pol_other) ~ 0)) %>% 
  summarize(mean_missing = mean(missing_pollen_data)) %>% 
  arrange(mean_missing)

#last date of data provided
date_end <- NAB_tx %>% 
  filter(date > mdy("10-1-2015") & date < mdy("1/1/21")) %>% 
  group_by(NAB_station) %>% 
  mutate(missing_pollen_data = case_when(is.na(Cupressaceae) | is.na(trees) | is.na(pol_other) ~ 1,
                                         !is.na(Cupressaceae) & !is.na(trees) & !is.na(pol_other) ~ 0)) %>% 
  filter(missing_pollen_data == 0) %>% 
  slice_max(date) %>% 
  mutate(date_end = date + 1) %>% 
  dplyr::select(date_end, NAB_station)

#amount of missing data per station not in the truncate period
left_join(NAB_tx, date_end) %>% 
  filter(date > mdy("10-1-2015") & date < mdy("1/1/21")) %>% 
  filter(date < date_end) %>% 
  group_by(NAB_station) %>% 
  mutate(missing_pollen_data = case_when(is.na(Cupressaceae) | is.na(trees) | is.na(pol_other) ~ 1,
                                         !is.na(Cupressaceae) & !is.na(trees) & !is.na(pol_other) ~ 0)) %>% 
  summarize(mean_missing = mean(missing_pollen_data))  %>% 
  arrange(mean_missing)


### a figure of linear interpolated data  #####################################################
NAB_tx %>%
  filter(date > mdy("10-1-2015") & date < mdy("1/1/21")) %>% 
  rowwise() %>% 
  mutate(all_pollen = sum(Cupressaceae, trees, pol_other + 1),
         all_pollen_m = sum(cup_all_m, trees_m, pol_other_m + 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = all_pollen, group = NAB_station)) + theme_few() + scale_y_log10() +
  
  geom_point(aes(x = date, y=all_pollen_m ), color = "red", alpha = 0.3, size = 0.9) +
  geom_point(aes(x = date, y=all_pollen ), color = "black") +
  ylab(expression(atop(pollen, (grains/m^3)))) +  xlab("date") +
  theme(legend.position= "none" ) + #theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + 
  facet_wrap(~NAB_station)

NAB_tx %>%
  filter(date > mdy("10-1-2015") & date < mdy("1/1/21")) %>% 
  rowwise() %>% 
  mutate(all_pollen = sum(Cupressaceae, trees, pol_other + 1),
         all_pollen_m2 = sum(cup_all_m2, trees_m2, pol_other_m2 + 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = all_pollen, group = NAB_station)) + theme_few() + scale_y_log10() +
  geom_point(aes(x = date, y=all_pollen_m2 ), color = "red", alpha = 0.3, size = 0.9) +
  geom_point(aes(x = date, y=all_pollen ), color = "black") +
  ylab(expression(atop(pollen, (grains/m^3)))) +  xlab("date") +
  theme(legend.position= "none" ) + #theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + 
  facet_wrap(~NAB_station)



### assess how good of a job the linear interpolation does on log 10 data######################
#select a portion of the data to withhold 
NAB_tx_withheld <- NAB %>%  #str(NAB_tx)
  ungroup() %>%  #get rid of rowwise
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>% 
  sample_frac(0.1) %>% #withold 10% of data
  dplyr::select(date, NAB_station) %>% 
  mutate(withheld = 1)
NAB_tx_mt <- left_join(NAB, NAB_tx_withheld) %>%  #str(NAB_tx_mt)
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>% 
  mutate(#withheld2 = case_when(withheld == 1 ~ withheld, is.na(withheld) ~ 0),
    cup_withheld = case_when(is.na(withheld) ~ cup_log10), #by not specifying a value, it defaults to NA
    trees_withheld = case_when(is.na(withheld) ~ trees_log10),
    pol_other_withheld = case_when(is.na(withheld) ~ pol_other_log10)
  )

#linear interpolation of time series that had extra data removed
NAB_tx_mt <- NAB_tx_mt %>% 
  group_by(NAB_station) %>% 
  mutate(
    cup_withheld_lm = na_interpolation(cup_withheld, maxgap = 7),
    trees_withheld_lm = na_interpolation(trees_withheld, maxgap = 7),
    pol_other_withheld_lm = na_interpolation(pol_other_withheld, maxgap = 7))

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





### assess how good of a job the linear interpolation does on untransformed data######################
#select a portion of the data to withhold 
NAB_tx_withheld <- NAB %>%  #str(NAB_tx)
  ungroup() %>%  #get rid of rowwise
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>% 
  sample_frac(0.1) %>% #withold 10% of data
  dplyr::select(date, NAB_station) %>% 
  mutate(withheld = 1)
NAB_tx_mt <- left_join(NAB, NAB_tx_withheld) %>%  #str(NAB_tx_mt)
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>% 
  mutate(#withheld2 = case_when(withheld == 1 ~ withheld, is.na(withheld) ~ 0),
    cup_withheld = case_when(is.na(withheld) ~ Cupressaceae ), #by not specifying a value, it defaults to NA
    trees_withheld = case_when(is.na(withheld) ~ trees),
    pol_other_withheld = case_when(is.na(withheld) ~ pol_other)
  )

#linear interpolation of time series that had extra data removed
NAB_tx_mt <- NAB_tx_mt %>% 
  group_by(NAB_station) %>% 
  mutate(
    cup_withheld_m = na_interpolation(cup_withheld, maxgap = 7),
    trees_withheld_m = na_interpolation(trees_withheld, maxgap = 7),
    pol_other_withheld_m = na_interpolation(pol_other_withheld, maxgap = 7))

NAB_tx_mtc <- filter(NAB_tx_mt, withheld == 1) 

#compare withheld data with linear interp estimates
cup_mtc_fit <- lm(NAB_tx_mtc$Cupressaceae ~ NAB_tx_mtc$cup_withheld_m )
sqrt(mean(cup_mtc_fit$residuals^2)); summary(cup_mtc_fit) #RMSE and R2
cup_panel <- ggplot(NAB_tx_mtc, aes(x= cup_withheld_m - 1, y = Cupressaceae - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) +
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000))

trees_mtc_fit <- lm(NAB_tx_mtc$trees ~ NAB_tx_mtc$trees_withheld_m)
sqrt(mean(trees_mtc_fit$residuals^2)); summary(trees_mtc_fit) #RMSE and R2
tree_panel <- ggplot(NAB_tx_mtc, aes(x= trees_withheld_m - 1, y = trees - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) +
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000))

pol_other_mtc_fit <- lm(NAB_tx_mtc$pol_other ~ NAB_tx_mtc$pol_other_withheld_m)
sqrt(mean(pol_other_mtc_fit$residuals^2)); summary(pol_other_mtc_fit) #RMSE and R2
other_panel <- ggplot(NAB_tx_mtc, aes(x= pol_other_withheld_m - 1, y = pol_other - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) +
  scale_x_log10(limits = c(1, 500)) + scale_y_log10(limits = c(1, 500))

cowplot::plot_grid(cup_panel, tree_panel, other_panel, labels = c("A", "B", "C"))





### assess how good of a job the linear interpolation does on untransformed data with a max gap of 2######################
#select a portion of the data to withhold 
NAB_tx_withheld <- NAB %>%  #str(NAB_tx)
  ungroup() %>%  #get rid of rowwise
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>% 
  sample_frac(0.1) %>% #withold 10% of data
  dplyr::select(date, NAB_station) %>% 
  mutate(withheld = 1)
NAB_tx_mt <- left_join(NAB, NAB_tx_withheld) %>%  #str(NAB_tx_mt)
  filter(date > mdy("10/1/2015") & date < mdy("1/1/2021")) %>% 
  mutate(#withheld2 = case_when(withheld == 1 ~ withheld, is.na(withheld) ~ 0),
    cup_withheld = case_when(is.na(withheld) ~ Cupressaceae ), #by not specifying a value, it defaults to NA
    trees_withheld = case_when(is.na(withheld) ~ trees),
    pol_other_withheld = case_when(is.na(withheld) ~ pol_other)
  )

#linear interpolation of time series that had extra data removed
NAB_tx_mt <- NAB_tx_mt %>% 
  group_by(NAB_station) %>% 
  mutate(
    cup_withheld_m = na_interpolation(cup_withheld, maxgap = 2),
    trees_withheld_m = na_interpolation(trees_withheld, maxgap = 2),
    pol_other_withheld_m = na_interpolation(pol_other_withheld, maxgap = 2))

NAB_tx_mtc <- filter(NAB_tx_mt, withheld == 1) 

#compare withheld data with linear interp estimates
cup_mtc_fit <- lm(NAB_tx_mtc$Cupressaceae ~ NAB_tx_mtc$cup_withheld_m )
sqrt(mean(cup_mtc_fit$residuals^2)); summary(cup_mtc_fit) #RMSE and R2
cup_panel <- ggplot(NAB_tx_mtc, aes(x= cup_withheld_m - 1, y = Cupressaceae - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) +
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000))

trees_mtc_fit <- lm(NAB_tx_mtc$trees ~ NAB_tx_mtc$trees_withheld_m)
sqrt(mean(trees_mtc_fit$residuals^2)); summary(trees_mtc_fit) #RMSE and R2
tree_panel <- ggplot(NAB_tx_mtc, aes(x= trees_withheld_m - 1, y = trees - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) +
  scale_x_log10(limits = c(1, 10000)) + scale_y_log10(limits = c(1, 10000))

pol_other_mtc_fit <- lm(NAB_tx_mtc$pol_other ~ NAB_tx_mtc$pol_other_withheld_m)
sqrt(mean(pol_other_mtc_fit$residuals^2)); summary(pol_other_mtc_fit) #RMSE and R2
other_panel <- ggplot(NAB_tx_mtc, aes(x= pol_other_withheld_m - 1, y = pol_other - 1)) + geom_point(alpha = .2) + theme_bw() + #geom_smooth(method = "lm") +
  geom_abline(slope = 1, lty = 2) +xlab(interpolated~(pollen~grains~per~m^3)) + ylab(observed~(pollen~grains~per~m^3)) +
  scale_x_log10(limits = c(1, 500)) + scale_y_log10(limits = c(1, 500))

cowplot::plot_grid(cup_panel, tree_panel, other_panel, labels = c("A", "B", "C"))




