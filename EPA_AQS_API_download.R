# downloading air quality measurements from the EPA AQS API
# https://aqs.epa.gov/aqsweb/documents/data_api.html

#work environment
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
#devtools::install_github("jpkeller/aqsr")
library(aqsr)
rm(list = ls()) 

myuser <- create_user(email="dan.katz@austin.utexas.edu",
                      key="bolefrog98")

#parameters of interest: Criteria variables (CO, SO2, NO2, O3, PM10, PM2.5)
#aq_param_list <- c("44201")
aq_param_list_raw <- aqs_list_parameters(myuser, pc="CRITERIA")
aq_param_list <- aq_param_list_raw %>% filter(!grepl("Lead", value_represented )) %>%  #remove the lead related variables
                  #filter(!grepl("Carbon monoxide", value_represented )) %>%  #remove CO so I can fit to the 5 param max
                  dplyr::mutate(code = as.numeric(as.character(code))) %>%
                  select(code) %>% unlist()

focal_param <- aq_param_list[6]



# #get list of sites that are in each county
# 
# site_list <- aqsr::aqs_list_sites(myuser, state = "48", county = "113") %>% filter(!is.na(value_represented))
# focal_site <- as.character(site_list[1,1])

# list of counties that I want data from:
county_list <- c("029", #Bexar
                 "113", #Dallas
                 "491", #Williamson (Georgetown)
                 "309", #McLennan (Waco)
                 "201", #"Harris" (Houston)
                 "041",#Brazos (College Station)
                 "121")  #Denton (Flower Mound)

#get list of sites that measure the focal parameter in that county
#https://aqs.epa.gov/data/api/monitors/byCounty?email=test@aqs.api&key=test&param=42401&bdate=20150501&edate=20150502&state=15&county=001
#list_services()
# aqsr::aqs_list_parameters(myuser, param = focal_param, bdate = "20160101", edate = "20161231",
#                           state = "48", county = "113", site = "0069")
# 
# ?aqs_list_parameters

#download the data for that variable in that county
# test <- aqs_dailyData_bySite(myuser, param = focal_param, bdate = "20160101", edate = "20161231",
#                              state = "48", county = "113", site = "0069")#, site = focal_site)

#create a list of dates
bdate_list <- c("20150930", "20160101", "20170101")

# create a dataframe with one row for each API query
county_list
aq_param_list
bdate_list

aqi_download_df <- expand.grid(county_list, aq_param_list, bdate_list)  %>%
                    rename(county_d = Var1, param_d = Var2, bdate_d = Var3) %>%
                    mutate(edate_d = case_when(bdate_d == "20150930" ~ "20151231",
                                               bdate_d == "20160101" ~ "20161231",
                                               bdate_d == "20170101" ~ "20171231")) %>%
  arrange(param_d, county_d, bdate_d)
# test <- aqs_dailyData_byCounty(myuser, param = focal_param, bdate = "20160101", edate = "20161231",
#                                state = "48", county = "113")#, site = focal_site)
# aqi_download_df <- filter(aqi_download_df, county_d == "113",
#                                           param_d == "88101" | param_d == "81102") #param_d == "88101"
#aqi_download_df <- aqi_download_df[1,]
# aqi_download_df <- sample_n(aqi_download_df, 3) #for testing
# aqi_download_df <- sample_n(aqi_download_df, 3)

aqi_batch_download_fun <- function(aqi_download_df, param_d, county_d, bdate_d, edate_d){
  start_fn_time <- Sys.time() #aqi_download_df <- aqi_download_df[1,]

  aqi_download_focal <- aqs_dailyData_byCounty(myuser, param = param_d, bdate = bdate_d, edate = edate_d,
                                               state = "48", county = county_d)
  end_fn_time <- Sys.time()
  dif_time <- end_fn_time - start_fn_time
  #sleep_for_time <- 5 - dif_time
  if(dif_time < 6){Sys.sleep(6 - dif_time)}

  return(aqi_download_focal)
  print(param_d, county_d, bdate_d)
}

downloaded_aqi_files <- pmap_dfr(aqi_download_df, aqi_batch_download_fun)
#backup_version <- downloaded_aqi_files
summary(downloaded_aqi_files)

unique(downloaded_aqi_files$parameter)
downloaded_aqi_files %>%
  filter(parameter == "PM2.5 - Local Conditions") %>%
  #filter(parameter != "Sulfur dioxide" & )
  filter(pollutant_standard == "PM25 24-hour 2012") %>%
  #group_by(parameter, sample_duration) %>%
  #select(parameter, sample_duration, pollutant_standard) %>%
  #distinct()
  mutate(date =ymd(date_local)) %>%
  ggplot(aes(x = date, y = arithmetic_mean, color = local_site_name, shape = pollutant_standard)) + 
  facet_wrap(~county_code) + geom_point() + theme_bw()

write_csv(downloaded_aqi_files, "C:/Users/dsk856/Box/texas/preliminary_epi/air_quality/downloaded_aqi_files200411.csv")
#


# ?pmap
# test <- aqs_dailyData_byCounty(myuser, param = focal_param, bdate = "20160101", edate = "20161231",
#                              state = "48", county = "113")#, site = focal_site)
# 
# 
# 
# test <- aqs_dailyData_bySite(myuser, param = "44201", bdate = "20170618", edate = "20170618",
#                              state = "48", county = "113", site = "0050")
# 
# names(test)
# test2 <- test %>%
#   mutate(date = ymd(date_local)) %>%
#   filter(pollutant_standard == "PM25 24-hour 2012") %>%
#   #filter(date > ymd("2016/12/1")) %>%
#   filter(sample_duration == "24 HOUR") %>%
#   group_by(date, local_site_name) %>%
#   summarize(arithmetic_mean_date = mean(arithmetic_mean)) %>%
# ggplot(aes(x = date, y = arithmetic_mean_date, color = local_site_name)) + geom_point() +
#   xlab("date") + ylab("PM 2.5 (mg/m3)") +
#   scale_color_discrete(name = "measurement site") + theme_bw()
# 
# test <- aqs_dailyData_bySite(myuser, param = focal_param, bdate = "20150930", edate = "20151231",
#                      state = "48", county = "113", site = "0050")
# 
# 
# 
# list_endpoints(service="dailyData") 
# aqs_dailyData()
# ?aqs_dailyData_bySite
# 
# 
# 
# 
# aqsr::list_endpoints()
# ?list_endpoints
# aqsr::aqs_list_sites(myuser, state = "48", county = "113")
# 
# #https://aqs.epa.gov/data/api/dailyData/bySite?email=test@aqs.api&key=test&param=44201&bdate=20170618&edate=20170618&state=37&county=183&site=0014
# 
# test <- aqs_dailyData_bySite(myuser, param = "44201", bdate = "20170618", edate = "20170618",
#                              state = "37", county = "183", site = "0014")
