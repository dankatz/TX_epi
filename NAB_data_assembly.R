#extract data from NAB 2019 - 2021 data request and combine with 2009-2019 data request
# the old version of this script is on box: C:/Users/dsk273/Box/texas/NAB/extract_pollen_data_from_2021_data_request.R

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
#library(tidyverse)
library(purrr)
library(lubridate)
library(stringr)
library(ggplot2)


### extract data from 2019 to 2021 data request ###########################################################
### set working directory to folder with all of the NAB files in original NAB format
setwd("C:/Users/dsk273/Box/texas/NAB/NAB_2019_2021")

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

write_csv(nab2019_2021, "C:/Users/dsk273/Box/texas/NAB/NAB2019_2021_pollen_220804.csv") #save collated file as csv
unique(nab2009_2019$city)

tx_cities <- c("Austin", "College Station","Dallas", "Flower Mound", "Houston", "San Antonio 2", "San Antonio 3", "San Antonio (2)", "San Antonio (3)", "Waco", "Waco 2")


nab2019_2021_tx <- nab2019_2021 %>% filter(Station.City %in% tx_cities) %>% 
  mutate(NAB_station = case_when(Station.City == "San Antonio (2)" ~ "San Antonio B",
                                 Station.City == "San Antonio (3)" ~ "San Antonio A",
                                 Station.City == "Waco" ~ "Waco A",
                                 TRUE ~ Station.City))

unique(nab2019_2021_tx$NAB_station)

### combining with the 2009-2019 data #################################################################################
nab2009_2019 <- read_csv("C:/Users/dsk273/Box/texas/NAB/NAB2009_2019_pollen_200508.csv") #processed in the "extract_pollen_data_from_xls.R" script
nab2009_2019_tx <- filter(nab2009_2019, site2 %in% tx_cities) %>% 
  mutate(NAB_station = case_when(site2 == "San Antonio 2" ~ "San Antonio B",
                                 site2 == "San Antonio 3" ~ "San Antonio A",
                                 site2 == "Waco" ~ "Waco A",
                                 site2 == "Waco 2" ~ "Waco B",
                                 TRUE ~ site2)) 

unique(nab2009_2019_tx$NAB_station)
unique(nab2019_2021_tx$NAB_station)

names(nab2019_2021_tx)
names(nab2009_2019_tx)

nab2009_2019_tx_join <- nab2009_2019_tx %>% dplyr::select(-"...1",-date, -city, -state, -site2, -lat, -long, -file)
nab2019_2021_tx_join <- nab2019_2021_tx %>% dplyr::select(-Station.ID, -Station.Name, - Station.City, -Station.State, - Station.Postal.Code, -Station.Country, -file)


nab2009_2021_tx <-  bind_rows(nab2009_2019_tx_join, nab2019_2021_tx_join)

names(nab2009_2021_tx)

### adding in station metadata ########################################################################################
NAB_locations <- read_csv("C:/Users/dsk273/Box/texas/NAB/EPHT_Pollen Station Inventory_062019_dk220318.csv") %>% 
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


write_csv(NAB_tx_pol, "C:/Users/dsk273/Box/texas/NAB/NAB2009_2021_tx_epi_pollen_220805.csv")
