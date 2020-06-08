#data assembly for preliminary epidemiological analysis
#install.packages("noncensus")
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

### create the categories of pollen ##################################################################
names(NAB)
NAB_tx <- left_join(NAB, NAB_locations) %>%
  filter(State == "TX") %>%
  mutate(date = ymd(Date),
         doy = yday(date),
         year = year(date)) %>%  #, FIPS = as.character(FIPS
  dplyr::select(date, doy, year, City, FIPS, county_name, MSA, Lat, Long, NABStation_ID, file, 
                Total.Pollen.Count, Cupressaceae, 
                grass = Gramineae...Poaceae, 
                Ambrosia, Quercus, Ulmus, Acer, Platanus, NAB_station) 
