# assembling THCIC data for both outpatient and inpatient THCIC data

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)




### outpatient: Using RAZ's script for the 2018-2020 THCIC data and then adding in the encounter date ##############################################
# using the script "Z:\THCIC\Outpatient THCIC data 2018-2020\OP THCIC clean  up 4.6.2022.R"
# this script was probably written by RAZ:

# library(tidyverse)
#
# ################### Creating cleaned file for Asthma all of Texas ######################
# # Steps:
# # reading in large files 
# # adding year column 
# # filtering based on the Dx code of interest
# # removing large file from global environment 
# 
# # We forgot to request vital variables from our last THCIC data request. We received the variables but only the missing variables. 
# #So those couple variables need to be merged with the bulk of the dataaset. (We forgot to request hospital ID so we recieved that and record ID which can be used to join to the large dataset.)
# 
# op2018full <- read_tsv("OP_2018_base.txt")
# op2018hospitalID <- read_tsv("op_2018_enc.txt")
# op2018 <- left_join(op2018hospitalID, op2018full, by = "RECORD_ID")
# op2018$Year <- 2018
# op2018r <-filter(op2018, str_detect(PRINC_DIAG_CODE, "^J45."))
# 
# #write.csv(op2018, file = "op2018_full.csv")
# 
# op2019full <- read_tsv("OP_2019_base.txt")
# op2019hospitalID <- read_tsv("op_2019_enc.txt")
# op2019 <- left_join(op2019hospitalID, op2019full, by = "RECORD_ID")
# op2019$Year <- 2019
# op2019r <-filter(op2019, str_detect(PRINC_DIAG_CODE, "^J45."))
# 
# #op2019r <-filter(op2019, str_detect(PRINC_DIAG_CODE, "^J45."))
# 
# #write.csv(op2019, file = "op2019_full.csv")
# 
# op2020full <- read_tsv("OP_2020_base.txt")
# op2020hospitalID <- read_tsv("op_2020_enc.txt")
# op2020 <- left_join(op2020hospitalID, op2020full, by = "RECORD_ID")
# op2020$Year <- 2020
# op2020r <-filter(op2020, str_detect(PRINC_DIAG_CODE, "^J45."))
# 
# #write.csv(op2020, file = "op2020_full.csv")
# 
# op_data_2018_2020 <- plyr :: rbind.fill(op2018r, op2019r, op2020r)
# rm(op2018, op2019, op2020)
# 
# #------- Cleaning up some errors in the THCIC data ---------------#
# # there are some errors, there should only be 0-5 options, see page 7 of RDF_OP.pdf for references
# op_data_2018_2020$RACE <- factor(ifelse(op_data_2018_2020$RACE%in% c(6,7,8,9,"O"), 0, op_data_2018_2020$RACE))
# op_data_2018_2020$RACE <- droplevels(op_data_2018_2020$RACE) #drop unused levels then relabel them
# op_data_2018_2020$RACE <- factor(op_data_2018_2020$RACE, labels = c("Missing", "American Indian", "Asian", "Black", "White", "Other"))
# 
# # there are some errors, there should only be 0-3 options, see page 8 of RDF_OP.pdf for references
# op_data_2018_2020$ETHNICITY <- factor(ifelse(op_data_2018_2020$ETHNICITY%in% c(3,4,5), 0, op_data_2018_2020$ETHNICITY))
# op_data_2018_2020$ETHNICITY <- droplevels(op_data_2018_2020$ETHNICITY) #drop unused levels then relabel them
# op_data_2018_2020$ETHNICITY <- factor(op_data_2018_2020$ETHNICITY, labels = c("Missing", "Latinx", "Non-Latinx"))
# 
# 
# table(op_data_2018_2020$RACE, op_data_2018_2020$ETHNICITY)
# # there are 6926 cases that are Black and Latinx
# 
# # creating a new variable that combines race and ethnicity 
# # if a person is Black and Latinx, they are categorized as Black 
# # if a person is Latinx, they could be any race EXCEPT Black
# op_data_2018_2020$RACE_ETHNICITY[op_data_2018_2020$ETHNICITY == "Non-Latinx" &  op_data_2018_2020$RACE == "White"] <- "White"   
# op_data_2018_2020$RACE_ETHNICITY[op_data_2018_2020$ETHNICITY == "Latinx"] <- "Latinx"   
# op_data_2018_2020$RACE_ETHNICITY[op_data_2018_2020$RACE == "Black"] <- "Black"   
# 
# #fixing census block input errors -- removing periods
# op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- str_replace_all(op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, "[^[:alnum:]]", "")
# 
# #filling in any missing zeros at the end (numbers should be at least 11 digits long -- the 12th digit is an added level of specificity we can't use with census data)
# library(stringi)
# op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- stri_pad_right(op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, 11, 0)
# 
# OPTexasAsthma18_20 <- op_data_2018_2020
# #save file as .csv and .RData file so steps above do not need to be repeated
# #create main asthma file for all of Texas
# write.csv(OPTexasAsthma18_20, file = "OPTexasAsthma18_20.csv")
# save(OPTexasAsthma18_20, file = "OPTexasAsthma18_20.RData")###

#reading in the output from RAZ's script (see above)
OPTexasAsthma18_20 <- read_csv("Z:/THCIC/Outpatient THCIC data 2018-2020/OPTexasAsthma18_20.csv")

#adding in the patient encounter date
THCIC_encounter_date_2018 <- read_tsv("Z:/SChambliss/thcicdata6/OP_2018_2020/op_2018_enc.txt")
THCIC_encounter_date_2019 <- read_tsv("Z:/SChambliss/thcicdata6/OP_2018_2020/op_2019_enc.txt")
THCIC_encounter_date_2020 <- read_tsv("Z:/SChambliss/thcicdata6/OP_2018_2020/op_2020_enc.txt")

THCIC_encounter_date <- bind_rows(THCIC_encounter_date_2018, THCIC_encounter_date_2019)
THCIC_encounter_date <- bind_rows(THCIC_encounter_date, THCIC_encounter_date_2020)

OPTexasAsthma18_20_enc <- left_join(OPTexasAsthma18_20, THCIC_encounter_date)
#test <- head(OPTexasAsthma18_20_enc)

write_csv(OPTexasAsthma18_20_enc, "Z:/THCIC/Outpatient THCIC data 2018-2020/OPTexasAsthma18_20_enc_date.csv")


### outpatient:  extracting asthma records from the 2015 q4 - 2017 data ##################################################################################
#we want the 'base' files; 'charges' contain revenue codes

setwd("Z:/THCIC/Outpatient THCIC data 2015Q4-2017/OP Decrypted")
yearly_files <- c("OP4q2015_Base.txt", "OP2016_Base.txt", "OP2017_Base.txt")

icd10_codes <- "J45"  #paste(c("J45")) #Asthma - not bothering to exclude any of them  collapse = "|") 

for(i in 1:length(yearly_files)){
  file_i <- read_tsv(yearly_files[i], #n_max = 10000,
                     col_types = cols(
                         PROVIDER_ZIP = col_character(),
                         ETHNICITY = col_double(),
                         RACE = col_double(),
                         PAT_AGE_GROUP = col_double()),
                       guess_max = 10000)
                    
  file_i$PRINC_DIAG_CODE_first <- substr(file_i$PRINC_DIAG_CODE, 1, 3)
  file_i_filt <- file_i %>% filter(str_detect(PRINC_DIAG_CODE_first, icd10_codes)) 
  if(i == 1) {file_all <- file_i_filt}
  if(i > 1){file_all <- bind_rows(file_all, file_i_filt)}
}

op_data_2015_2017 <- file_all 

#------- Cleaning up some errors in the THCIC data ---------------#
# there are some errors, there should only be 0-5 options, see page 7 of RDF_OP.pdf for references
op_data_2015_2017$RACE <- factor(ifelse(op_data_2015_2017$RACE%in% c(6,7,8,9,"O"), 0, op_data_2015_2017$RACE))
op_data_2015_2017$RACE <- droplevels(op_data_2015_2017$RACE) #drop unused levels then relabel them
op_data_2015_2017$RACE <- factor(op_data_2015_2017$RACE, labels = c("Missing", "American Indian", "Asian", "Black", "White", "Other"))

# there are some errors, there should only be 0-3 options, see page 8 of RDF_OP.pdf for references
op_data_2015_2017$ETHNICITY <- factor(ifelse(op_data_2015_2017$ETHNICITY%in% c(3,4,5,8), 0, op_data_2015_2017$ETHNICITY))
op_data_2015_2017$ETHNICITY <- droplevels(op_data_2015_2017$ETHNICITY) #drop unused levels then relabel them
op_data_2015_2017$ETHNICITY <- factor(op_data_2015_2017$ETHNICITY, labels = c("Missing", "Latinx", "Non-Latinx"))


table(op_data_2015_2017$RACE, op_data_2015_2017$ETHNICITY)
# there are 6926 cases that are Black and Latinx

# creating a new variable that combines race and ethnicity
# if a person is Black and Latinx, they are categorized as Black
# if a person is Latinx, they could be any race EXCEPT Black
op_data_2015_2017$RACE_ETHNICITY[op_data_2015_2017$ETHNICITY == "Non-Latinx" &  op_data_2015_2017$RACE == "White"] <- "White"
op_data_2015_2017$RACE_ETHNICITY[op_data_2015_2017$ETHNICITY == "Latinx"] <- "Latinx"
op_data_2015_2017$RACE_ETHNICITY[op_data_2015_2017$RACE == "Black"] <- "Black"
write.csv(op_data_2015_2017, "Z:/THCIC/Katz/op_asthma_2015q4_2017.csv", row.names = FALSE)

### outpatient: combining the outpatient 2015 q4 - 2017 data with the 2018-2020 data #################################################################################
str(op_data_2015_2017)
str(OPTexasAsthma18_20_enc)

op_data_2015_2017 <- op_data_2015_2017 %>% mutate(SOURCE_OF_ADMISSION= as.numeric(SOURCE_OF_ADMISSION))
OPTexasAsthma18_20_enc <- OPTexasAsthma18_20_enc %>% mutate(PAT_ADDR_CENSUS_BLOCK_GROUP= as.character(PAT_ADDR_CENSUS_BLOCK_GROUP),
                                                            PAT_AGE_GROUP = as.numeric(PAT_AGE_GROUP))

op_asthma_2015q4_2020 <- bind_rows(OPTexasAsthma18_20_enc, op_data_2015_2017)
write_csv(op_asthma_2015q4_2020, "Z:/THCIC/Katz/op_asthma_2015q4_2020.csv")



### inpatient: Using RAZ's script for the 2018-2020 THCIC data and then adding in the encounter date ##############################################
# using the script "Z:\THCIC\Inpatient THCIC data 2018-2020\IP THCIC clean  up 4.6.2022.R"
# this script was probably written by RAZ:

# library(tidyverse)
# 
# 
# ################### Creating cleaned file for Asthma all of Texas ######################
# # Steps:
# # reading in large files
# # adding year column
# # filtering based on the Dx code of interest
# # removing large file from global environment
# 
# # We forgot to request vital variables from our last THCIC data request. We received the variables but only the missing variables. So those couple variables need to be merged with the bulk of the dataaset. (We forgot to request hospital ID so we recieved that and record ID which can be used to join to the large dataset.)
# 
# ip2018full <- read_tsv("IP_2018_base.txt")
# ip2018hospitalID <- read_tsv("ip_2018_enc.txt")
# ip2018 <- left_join(ip2018hospitalID, ip2018full, by = "RECORD_ID")
# ip2018$Year <- 2018
# ip2018r <-filter(ip2018, str_detect(PRINC_DIAG_CODE, "^J45."))
# 
# #write.csv(ip2018, file = "ip2018_full.csv")
# 
# ip2019full <- read_tsv("IP_2019_base.txt")
# ip2019hospitalID <- read_tsv("ip_2019_enc.txt")
# ip2019 <- left_join(ip2019hospitalID, ip2019full, by = "RECORD_ID")
# ip2019$Year <- 2019
# ip2019r <-filter(ip2019, str_detect(PRINC_DIAG_CODE, "^J45."))
# 
# #write.csv(ip2019, file = "ip2019_full.csv")
# 
# ip2020full <- read_tsv("IP_2020_base.txt")
# ip2020hospitalID <- read_tsv("ip_2020_enc.txt")
# ip2020 <- left_join(ip2020hospitalID, ip2020full, by = "RECORD_ID")
# ip2020$Year <- 2020
# ip2020r <-filter(ip2020, str_detect(PRINC_DIAG_CODE, "^J45."))
# 
# #write.csv(ip2020, file = "ip2020_full.csv")
# 
# ip_data_2018_2020 <- plyr :: rbind.fill(ip2018r, ip2019r, ip2020r)
# 
# rm(ip2018, ip2019, ip2020)
# #------- Cleaning up some errors in the THCIC data ---------------#
# # there are some errors, there should only be 0-5 options, see page 7 of RDF_IP.pdf for references
# ip_data_2018_2020$RACE <- factor(ifelse(ip_data_2018_2020$RACE%in% c(6,7,8,9,"O"), 0, ip_data_2018_2020$RACE))
# ip_data_2018_2020$RACE <- droplevels(ip_data_2018_2020$RACE) #drop unused levels then relabel them
# ip_data_2018_2020$RACE <- factor(ip_data_2018_2020$RACE, labels = c("American Indian", "Asian", "Black", "White", "Other"))
# 
# # there are some errors, there should only be 0-3 options, see page 8 of RDF_IP.pdf for references
# ip_data_2018_2020$ETHNICITY <- factor(ifelse(ip_data_2018_2020$ETHNICITY%in% c(3,4,5), 0, ip_data_2018_2020$ETHNICITY))
# ip_data_2018_2020$ETHNICITY <- droplevels(ip_data_2018_2020$ETHNICITY) #drop unused levels then relabel them
# ip_data_2018_2020$ETHNICITY <- factor(ip_data_2018_2020$ETHNICITY, labels = c("Missing", "Latinx", "Non-Latinx"))
# 
# 
# table(ip_data_2018_2020$RACE, ip_data_2018_2020$ETHNICITY)
# # there are 6926 cases that are Black and Latinx
# 
# # creating a new variable that combines race and ethnicity
# # if a person is Black and Latinx, they are categorized as Black
# # if a person is Latinx, they could be any race EXCEPT Black
# ip_data_2018_2020$RACE_ETHNICITY[ip_data_2018_2020$ETHNICITY == "Non-Latinx" &  ip_data_2018_2020$RACE == "White"] <- "White"
# ip_data_2018_2020$RACE_ETHNICITY[ip_data_2018_2020$ETHNICITY == "Latinx"] <- "Latinx"
# ip_data_2018_2020$RACE_ETHNICITY[ip_data_2018_2020$RACE == "Black"] <- "Black"
# 
# 
# 
# #fixing census block input errors -- removing periods
# ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- str_replace_all(ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, "[^[:alnum:]]", "")
# 
# #filling in any missing zeros at the end (numbers should be at least 11 digits long -- the 12th digit is an added level of specificity we can't use with census data)
# library(stringi)
# ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- stri_pad_right(ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, 11, 0)
# 
# IPTexasAsthma18_20 <- ip_data_2018_2020
# #save file as .csv and .RData file so steps above do not need to be repeated
# #create main asthma file for all of Texas
# write.csv(ip_data_2018_2020, file = "IPTexasAsthma18_20.csv")
# save(ip_data_2018_2020, file = "IPTexasAsthma18_20.RData")

#reading in the output from RAZ's script (see above)
IPTexasAsthma18_20 <- read_csv("Z:/THCIC/Inpatient THCIC data 2018-2020/IPTexasAsthma18_20.csv")

#adding in the patient encounter date
THCIC_encounter_date_2018 <- read_tsv("Z:/SChambliss/thcicdata6/IP_2018_2020/ip_2018_enc.txt")
THCIC_encounter_date_2019 <- read_tsv("Z:/SChambliss/thcicdata6/IP_2018_2020/ip_2019_enc.txt")
THCIC_encounter_date_2020 <- read_tsv("Z:/SChambliss/thcicdata6/IP_2018_2020/ip_2020_enc.txt")

THCIC_encounter_date <- bind_rows(THCIC_encounter_date_2018, THCIC_encounter_date_2019)
THCIC_encounter_date <- bind_rows(THCIC_encounter_date, THCIC_encounter_date_2020)

IPTexasAsthma18_20_enc <- left_join(IPTexasAsthma18_20, THCIC_encounter_date)
#test <- head(OPTexasAsthma18_20_enc)

write_csv(IPTexasAsthma18_20_enc, "Z:/THCIC/Inpatient THCIC data 2018-2020/IPTexasAsthma18_20_enc_date.csv")


### inpatient:  extracting asthma records from the 2015 q4 - 2017 data ##################################################################################
#we want the 'base' files; 'charges' contain revenue codes

setwd("Z:/THCIC/Inpatient THCIC data 2004-2017/raw data")
yearly_files <- c("IP_2015Q4_base.txt", "IP_2016_base.txt", "IP_2017_base.txt")

icd10_codes <- "J45"  #paste(c("J45")) #Asthma - not bothering to exclude any of them  collapse = "|") 

for(i in 1:length(yearly_files)){
  file_i <- read_tsv(yearly_files[i], #n_max = 10000 #test <- read_tsv(yearly_files[1]); names(test)
                     col_types = cols(
                       #PROVIDER_ZIP = col_character(),
                       ETHNICITY = col_double(),
                       RACE = col_double(),
                       PAT_AGE_GROUP = col_double()),
                     guess_max = 10000)
  
  file_i$PRINC_DIAG_CODE_first <- substr(file_i$PRINC_DIAG_CODE, 1, 3)
  file_i_filt <- file_i %>% filter(str_detect(PRINC_DIAG_CODE_first, icd10_codes)) 
  if(i == 1) {file_all <- file_i_filt}
  if(i > 1){file_all <- bind_rows(file_all, file_i_filt)}
}

ip_data_2015_2017 <- file_all 

#------- Cleaning up some errors in the THCIC data ---------------#
# there are some errors, there should only be 1-5 options, see page 9 of RDF_IP.pdf for references
ip_data_2015_2017$RACE <- factor(ifelse(ip_data_2015_2017$RACE%in% c(6,7,8,9,"O", NA), 0, ip_data_2015_2017$RACE)) #unique(ip_data_2015_2017$RACE)
ip_data_2015_2017$RACE <- droplevels(ip_data_2015_2017$RACE) #drop unused levels then relabel them
ip_data_2015_2017$RACE <- factor(ip_data_2015_2017$RACE, labels = c("Missing", "American Indian", "Asian", "Black", "White", "Other"))

# there are some errors, there should only be 0-3 options, see page 8 of RDF_OP.pdf for references
ip_data_2015_2017$ETHNICITY <- factor(ifelse(ip_data_2015_2017$ETHNICITY%in% c(3,4,5,6,8, NA), 0, ip_data_2015_2017$ETHNICITY)) #unique(ip_data_2015_2017$ETHNICITY)
ip_data_2015_2017$ETHNICITY <- droplevels(ip_data_2015_2017$ETHNICITY) #drop unused levels then relabel them
ip_data_2015_2017$ETHNICITY <- factor(ip_data_2015_2017$ETHNICITY, labels = c("Missing", "Latinx", "Non-Latinx"))


table(ip_data_2015_2017$RACE, ip_data_2015_2017$ETHNICITY)
# there are 445 cases that are Black and Latinx

# creating a new variable that combines race and ethnicity
# if a person is Black and Latinx, they are categorized as Black
# if a person is Latinx, they could be any race EXCEPT Black
ip_data_2015_2017$RACE_ETHNICITY[ip_data_2015_2017$ETHNICITY == "Non-Latinx" &  ip_data_2015_2017$RACE == "White"] <- "White"
ip_data_2015_2017$RACE_ETHNICITY[ip_data_2015_2017$ETHNICITY == "Latinx"] <- "Latinx"
ip_data_2015_2017$RACE_ETHNICITY[ip_data_2015_2017$RACE == "Black"] <- "Black"
write.csv(ip_data_2015_2017, "Z:/THCIC/Katz/ip_asthma_2015q4_2017.csv", row.names = FALSE)

### inpatient: combining the inpatient 2015 q4 - 2017 data with the 2018-2020 data #################################################################################
str(ip_data_2015_2017)
str(IPTexasAsthma18_20_enc)

ip_data_2015_2017 <- ip_data_2015_2017 %>% mutate(SOURCE_OF_ADMISSION= as.character(SOURCE_OF_ADMISSION))
IPTexasAsthma18_20_enc <- IPTexasAsthma18_20_enc %>% mutate(PAT_ADDR_CENSUS_BLOCK_GROUP= as.character(PAT_ADDR_CENSUS_BLOCK_GROUP),
                                                            PAT_AGE_GROUP = as.numeric(PAT_AGE_GROUP))

ip_asthma_2015q4_2020 <- bind_rows(IPTexasAsthma18_20_enc, ip_data_2015_2017)
write_csv(ip_asthma_2015q4_2020, "Z:/THCIC/Katz/ip_asthma_2015q4_2020.csv")

