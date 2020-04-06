# downloading the available non-influenza data from Texas DSHS
# https://www.dshs.state.tx.us/IDCU/disease/influenza/surveillance/2015---2016-Texas-Influenza-Surveillance-Activity-Report.xls
# following this example: https://www.brodrigues.co/blog/2018-06-10-scraping_pdfs/

#set up work environment
library(glue)
library(pdftools)
library(purrr)
library(stringr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggplot2)


### downloading and processing all 2015-2016 weekly reports ######################################################################
# following this example for scraping pdfs: https://www.brodrigues.co/blog/2018-06-10-scraping_pdfs/

# All weekly reports for 2015-2016 are provided as pdfs from links given here: 
# https://www.dshs.state.tx.us/IDCU/disease/influenza/surveillance/2015---2016-Texas-Influenza-Surveillance-Activity-Report.xls

# get a list of all individual links; to do so, type this in to the console on chrome: 
# urls = $$('a'); for (url in urls) console.log (urls[url].href );
# for 2015-2016 the pdfs are numbered in order from 1571:1623
pdf_numbers <- as.character(1571:1623) #
base_url <- "https://www.dshs.state.tx.us/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=859000{pdf_numbers}"
pdf_urls <- glue(base_url) #replace everything in the bracket with the list of pdf numbers
pdf_names <- glue("report_2015_2016_n{pdf_numbers}.pdf")#"first_pdf.pdf" #glue("report_{country}.pdf")

#test with extracting data from a single pdf
# focal_pdf_url <- "https://www.dshs.state.tx.us/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=8590001574"
# pdf_urls <- focal_pdf_url #
# pdf_names <- "test_pdf.pdf"
# walk2(pdf_urls, pdf_names, download.file, mode = "wb")

#download all of the pdfs, takes a minute
walk2(pdf_urls, pdf_names, download.file, mode = "wb")

#raw_text[[1]]
#str_locate(raw_text[[1]], "Table 3: Non-Influenza")
#raw_text[[1]][2]
table_3_col_names <- c("Virus", "labs_testing_n", "tests_performed", "tests_positive", "tests_percent_positive")


#create a functino to extract just table 3 from the report and turn it into a dataframe
#NOTE: this function is very hard coded and should only be used for reports in this EXACT format
clean_table <- function(pdf_to_process){
  #extract raw text from pdf
  raw_text <- pdf_text(pdf_to_process) #raw_text <- pdf_text(pdf_names[33])
  
  #get the date of the report
  week_of_test <- substr(raw_text[[1]], 
                 start = as.numeric(str_locate(raw_text[[1]], fixed("              ("))[1,2]) ,
                 stop = as.numeric(str_locate(raw_text[[1]], fixed(")\r\n ")))[1] )[1]
  
  #get Table 3 from report
  table <- unlist(raw_text) #str_split(table, "\n", simplify = TRUE) #table <- unlist(raw_text)
  table_place <- stringr::str_which(table, "Table 3: Non-Influenza Respiratory Virus Testing Performed by Texas NREVSS Laboratories for the Current Week")
  table <- table[table_place]
  table_start <- str_locate(table, "Table 3: Non-Influenza Respiratory Virus Testing Performed by Texas NREVSS Laboratories for the Current Week")[1]
  table_end <- str_locate(table, "RSV tests displayed in the table are")[2]
    if(is.na(table_end)){table_end <- str_locate(table, "RSV  tests displayed in the table are")[2]} #some of the pdfs parsed an extra space here
  table3 <- str_sub(table, start = table_start, end = table_end)
  table3b <- str_replace_all(table3, "\\s{2,}", "|")
  table3b <- gsub("RSV|†^|", "RSV†^|", table3b, fixed = TRUE)
#extract each row and turn it in to a dataframe  
  adenovirus <- str_sub(table3b, 
                        (start = str_locate(table3b, "Adenovirus")[1]),
                        (end = str_locate(table3b, "HMPV")[1] - 2)) %>%
                      str_split(pattern = fixed("|")) %>%
                      do.call(rbind.data.frame, .) %>%
                      set_colnames(table_3_col_names)
  
  HMPV <- str_sub(table3b, 
                        (start = str_locate(table3b, "HMPV")[1]),
                        (end = str_locate(table3b, "Parainfluenza virus")[1] - 2))%>%
              str_split(pattern = fixed("|")) %>%
              do.call(rbind.data.frame, .) %>%
              set_colnames(table_3_col_names)
  
  Parainfluenza <- str_sub(table3b, 
                  (start = str_locate(table3b, "Parainfluenza")[1]),
                  (end = str_locate(table3b, "Rhinovirus")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_3_col_names)
  
  rhinovirus <- str_sub(table3b, 
                           (start = str_locate(table3b, "Rhinovirus")[1]),
                           (end = str_locate(table3b, "RSV")[1] - 5))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_3_col_names)
  
  RSV <- str_sub(table3b, 
                           (start = str_locate(table3b, "RSV")[1]),
                           (end = str_locate(table3b, "Seasonal coronavirus")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_3_col_names)
  
  seasonal_corona <- str_sub(table3b, 
                           (start = str_locate(table3b, "Seasonal coronavirus")[1]),
                           (end = str_locate(table3b, "not include MERS-CoV")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_3_col_names)
  
  #create single dataframe
  table3_df <- bind_rows(adenovirus, HMPV, Parainfluenza, rhinovirus, RSV, seasonal_corona) %>%
    mutate(tests_percent_positive = gsub(x = tests_percent_positive, pattern ="%", "", fixed = TRUE), #get rid of "%"
           week_of_testing = week_of_test) 
  
  #return the output
  return(table3_df)
}

# apply the function to each pdf that has been downloaded
# use a for loop to check where the errors are:
# 
# for(i in 1:53){
#   test <-  clean_table(pdf_names[i]) #clean_table(pdf_names[33])
# }


virus_2015_2016 <- map_dfr(pdf_names, clean_table) #apply the clean_table function to each pdf and create a df by adding on rows

#manually spot checking pdfs
#looks good for the 6 pdfs I spot checked.  The percent positive column has a decimal place cut off sometimes
virus_2015_2016b <- virus_2015_2016 %>% mutate(virus = gsub("†^", "", Virus, fixed = TRUE),
                                               virus = gsub("(does", "", virus, fixed = TRUE),
                                               Virus = NULL,
                                               labs_testing_n = as.numeric(labs_testing_n),
                                               tests_performed = as.numeric(tests_performed),
                                               tests_positive = as.numeric(tests_positive),
                                               tests_percent_positive = (tests_positive/tests_performed) * 100,
                                               week_of_testing = gsub("Sept", "Sep", week_of_testing),
                                               week_start_date = mdy(substr(week_of_testing, 2, 15)),
                                               week_end_date = mdy(substr(week_of_testing, 16, 30)),
                                               week_midpoint = (week_end_date - week_start_date )/2 + week_start_date)

ggplot(virus_2015_2016b, aes(x = week_midpoint, y = labs_testing_n, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2015_2016b, aes(x = week_midpoint, y = tests_performed, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2015_2016b, aes(x = week_midpoint, y = tests_percent_positive, color = virus)) + geom_point() + geom_line() + theme_bw()

write.csv(virus_2015_2016, "C:/Users/dsk856/Box/texas/preliminary_epi/virus/virus_2015_2016.csv", row.names = FALSE)


### downloading and processing all 2016-2017 weekly reports ######################################################################
#https://dshs.texas.gov/idcu/disease/influenza/surveillance/2017/
# get a list of all individual links; to do so, type this in to the console on chrome: 
# urls = $$('a'); for (url in urls) console.log (urls[url].href );

#NOTE: some of the links were broken or pointed to the wrong place (internal website that isn't publicly accessible)
#and the format is inconsistent, so I did some manual cleaning (e.g., switching internal urls to the public directory, where the files 
#were also placed and accessible):
#original link: https://wwwstage.dshs.internal/IDCU/disease/influenza/surveillance/2017/16wk50Dec17.pdf
#fixed link: https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/16wk50Dec17.pdf

#problem urls remaining after all trouble shooting completed:
#week ending on: 2017-03-25, 2017-09-02
#https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk12Mar25.pdf #"Full report on Friday"
#https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk35Sep02.pdf #"Full report on Friday"


urls_2016_2017_clean <- (
"https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk39Oct06.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk40Oct13.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk39Oct06.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk38Sept09.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk37Sept22.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk36Sep15.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk34Sep01.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk33Aug25.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk32Aug18.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk31Aug11.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk30Aug04.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk29Jul28.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk27July08.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk24Jun23.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk23Jun16.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk22Jun09.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk21Jun02.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk20May26.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk19May19.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk18May12.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk16Apr28.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk15Apr24.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk14Apr17.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk13Apr07.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk11Mar24.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk10Mar11.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk09Mar04.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk08Feb25.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk07Feb18.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk06Feb11.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk05Feb04.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk04Jan28.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk03Jan21.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17wk02Jan14.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/2018/17Wk01Jan07.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/16wk52Dec31.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/16wk51Dec24.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/16wk50Dec17.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/16wk49Dec10.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/16Wk48Dec3.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/16wk47Nov26.pdf
https://dshs.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=12884905688
https://dshs.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=12884905687
https://dshs.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=12884905686
https://dshs.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=12884905685
https://dshs.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=12884905684
https://dshs.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=12884905683
https://dshs.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=12884905682
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk17May05.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk25Jun30.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk26July01.pdf")

urls_2016_2017_cleanb <- unlist(str_split(urls_2016_2017_clean, pattern = "\n"))
pdf_number1617 <- 1:length(urls_2016_2017_cleanb) #note that these aren't necessarily in perfect order
pdf_names1617 <- glue("report_2016_2017_n{pdf_number1617}.pdf")#"first_pdf.pdf" #glue("report_{country}.pdf")

#download all of the pdfs, takes a minute
walk2(urls_2016_2017_cleanb, pdf_names1617, download.file, mode = "wb")

#create a functino to extract just table 3 from the report and turn it into a dataframe
#NOTE: this function is very hard coded and should only be used for reports in this EXACT format
table_4_col_names <- c("Virus", "labs_testing_n", "tests_performed", "tests_positive", "tests_percent_positive")

clean_table1617 <- function(pdf_to_process){
  #extract raw text from pdf
  raw_text <- pdf_text(pdf_to_process) #raw_text <- pdf_text(pdf_names1617[24])
  
  #get the date of the report
  week_of_test <- substr(raw_text[[1]], 
                         start = as.numeric(str_locate(raw_text[[1]], fixed("              ("))[1,2]) ,
                         stop = as.numeric(str_locate(raw_text[[1]], fixed(")\r\n ")))[1] )[1]
  
  #get Table 4 from report
  table <- unlist(raw_text) #str_split(table, "\n", simplify = TRUE) #table <- unlist(raw_text)
  table_place <- stringr::str_which(table, "Table 4: Non-Influenza Respiratory Virus Testing Performed by Texas NREVSS Laboratories for the Current Week")
  table <- table[table_place]
  table_start <- str_locate(table, "Table 4: Non-Influenza Respiratory Virus Testing Performed by Texas NREVSS Laboratories for the Current Week")[1]
  table_end <- str_locate(table, "RSV tests displayed in the table are")[2]
  if(is.na(table_end)){table_end <- str_locate(table, "RSV  tests displayed in the table are")[2]} #some of the pdfs parsed an extra space here
  table4 <- str_sub(table, start = table_start, end = table_end)
  table4b <- str_replace_all(table4, "\\s{2,}", "|")
  table4b <- gsub("RSV|†^|", "RSV†^|", table4b, fixed = TRUE)
  
  #manually fixing some bad parsing of pdf
  if(str_detect(table4b, "|†^|") == TRUE){
    table4b <- sub(pattern =     "|†^|", 
                   replacement = "|RSV†^|",
                   x = table4b, fixed = TRUE)
    table4b <- sub(pattern =     "|RSV|Seasonal coronavirus", 
                   replacement = "|Seasonal coronavirus",
                   x = table4b, fixed = TRUE)
    }
  
  #extract each row and turn it in to a dataframe  
  adenovirus <- str_sub(table4b, 
                        (start = str_locate(table4b, "Adenovirus")[1]),
                        (end = str_locate(table4b, "HMPV")[1] - 2)) %>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  HMPV <- str_sub(table4b, 
                  (start = str_locate(table4b, "HMPV")[1]),
                  (end = str_locate(table4b, "Parainfluenza")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  Parainfluenza <- str_sub(table4b, 
                           (start = str_locate(table4b, "Parainfluenza")[1]),
                           (end = str_locate(table4b, "Rhinovirus")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  rhinovirus <- str_sub(table4b, 
                        (start = str_locate(table4b, "Rhinovirus")[1]),
                        (end = str_locate(table4b, "RSV")[1] - 5))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
         
  RSV <- str_sub(table4b, 
                 (start = str_locate(table4b, "RSV")[1]),
                 (end = str_locate(table4b, "Seasonal coronavirus")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  seasonal_corona <- str_sub(table4b, 
                             (start = str_locate(table4b, "Seasonal coronavirus")[1]),
                             (end = str_locate(table4b, "not include MERS-CoV")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  #create single dataframe
  table4_df <- bind_rows(adenovirus, HMPV, Parainfluenza, rhinovirus, RSV, seasonal_corona) %>%
    mutate(tests_percent_positive = gsub(x = tests_percent_positive, pattern ="%", "", fixed = TRUE), #get rid of "%"
           week_of_testing = week_of_test) 
  
  #return the output
  return(table4_df)
}

# apply the function to each pdf that has been downloaded
# use a for loop to check where the errors are:
# 
# for(i in 1:48){
#   test <-  clean_table1617(pdf_names1617[i]) #clean_table1617(pdf_names1617[24])
# }

virus_2016_2017 <- map_dfr(pdf_names1617, clean_table1617) #apply the clean_table function to each pdf and create a df by adding on rows

date_df <- virus_2016_2017$week_of_testing %>% #separate(sep = "–")#
        str_split_fixed(pattern = "–", n = 2) %>% 
  gsub("[^[:alnum:] ]", "", .) %>%
  as.data.frame() %>%
  set_names(value = c("week_start_date", "week_end_date")) %>%
  mutate(
    week_start_date = mdy(week_start_date),
    week_end_date = mdy(week_end_date)
  )
  
#missing dates
date_df %>% select(week_end_date) %>% distinct() %>% arrange(week_end_date)
#missing dates: week ending on: 2017-03-25, 2017-04-29, 2017-06-24, 2017-07-01, 2017-09-02

### PICK UP HERE
#manually spot checked several pdfs, didn't see any errors

virus_2016_2017b <- cbind(virus_2016_2017, date_df) %>% 
  mutate(virus = gsub("†^", "", Virus, fixed = TRUE),
                                               virus = gsub("(does", "", virus, fixed = TRUE),
                                               virus = recode(virus, "Parainfluenza virus" = "Parainfluenza"),
                                               Virus = NULL,
                                               labs_testing_n = as.numeric(labs_testing_n),
                                               tests_performed = as.numeric(tests_performed),
                                               tests_positive = as.numeric(tests_positive),
                                               tests_percent_positive = (tests_positive/tests_performed) * 100,
                                               week_midpoint = (week_end_date - week_start_date )/2 + week_start_date)

ggplot(virus_2016_2017b, aes(x = week_midpoint, y = labs_testing_n, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2016_2017b, aes(x = week_midpoint, y = tests_performed, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2016_2017b, aes(x = week_midpoint, y = tests_percent_positive, color = virus)) + geom_point() + geom_line() + theme_bw()

write.csv(virus_2016_2017b, "C:/Users/dsk856/Box/texas/preliminary_epi/virus/virus_2016_2017.csv", row.names = FALSE)


### 2017 to 2018 data #################################################################
### downloading and processing 2017-2018 weekly reports that were from 2017  ###########################################################
# https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017---2018-Texas-Influenza-Surveillance-Activity-Report.aspx
# get a list of all individual links; to do so, type this in to the console on chrome: 
# urls = $$('a'); for (url in urls) console.log (urls[url].href );


urls_2017_2018_clean <- (
"https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk52Jan05.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk51Dec28.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk50Dec22.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk49Dec15.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017/17Wk48Dec08.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk47Dec01.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk46Nov29.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk45Nov11.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk44Nov10.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk43Nov03.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk42Oct27-REVISED.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk41Oct20.pdf
https://dshs.texas.gov/IDCU/disease/influenza/surveillance/2017-2018-Texas-Influenza-Surveillance-Activity-Report/17Wk40Oct13.pdf")

urls_2017_2018_cleanb <- unlist(str_split(urls_2017_2018_clean, pattern = "\n"))
pdf_number1718 <- 1:length(urls_2017_2018_cleanb) #note that these aren't necessarily in perfect order
pdf_names1718 <- glue("report_2017_2018_n{pdf_number1718}.pdf")#"first_pdf.pdf" #glue("report_{country}.pdf")

#download all of the pdfs, takes a minute
walk2(urls_2017_2018_cleanb, pdf_names1718, download.file, mode = "wb")

#create a functino to extract just table 3 from the report and turn it into a dataframe
#NOTE: this function is very hard coded and should only be used for reports in this EXACT format
table_4_col_names <- c("Virus", "labs_testing_n", "tests_performed", "tests_positive", "tests_percent_positive")

clean_table1718 <- function(pdf_to_process){
  #extract raw text from pdf
  raw_text <- pdf_text(pdf_to_process) #raw_text <- pdf_text(pdf_names1718[24])
  
  #get the date of the report
  week_of_test <- substr(raw_text[[1]], 
                         start = as.numeric(str_locate(raw_text[[1]], fixed("              ("))[1,2]) ,
                         stop = as.numeric(str_locate(raw_text[[1]], fixed(")\r\n ")))[1] )[1]
  
  #get Table 4 from report
  table <- unlist(raw_text) #str_split(table, "\n", simplify = TRUE) #table <- unlist(raw_text)
  table_place <- stringr::str_which(table, "Table 4: Non-Influenza Respiratory Virus Testing Performed by Texas NREVSS Laboratories for the Current Week")
  table <- table[table_place]
  table_start <- str_locate(table, "Table 4: Non-Influenza Respiratory Virus Testing Performed by Texas NREVSS Laboratories for the Current Week")[1]
  table_end <- str_locate(table, "RSV tests displayed in the table are")[2]
  if(is.na(table_end)){table_end <- str_locate(table, "RSV  tests displayed in the table are")[2]} #some of the pdfs parsed an extra space here
  table4 <- str_sub(table, start = table_start, end = table_end)
  table4b <- str_replace_all(table4, "\\s{2,}", "|")
  table4b <- gsub("RSV|†^|", "RSV†^|", table4b, fixed = TRUE)
  
  #manually fixing some bad parsing of pdf
  if(str_detect(table4b, "|†^|") == TRUE){
    table4b <- sub(pattern =     "|†^|", 
                   replacement = "|RSV†^|",
                   x = table4b, fixed = TRUE)
    table4b <- sub(pattern =     "|RSV|Seasonal coronavirus", 
                   replacement = "|Seasonal coronavirus",
                   x = table4b, fixed = TRUE)
  }
  
  #extract each row and turn it in to a dataframe  
  adenovirus <- str_sub(table4b, 
                        (start = str_locate(table4b, "Adenovirus")[1]),
                        (end = str_locate(table4b, "HMPV")[1] - 2)) %>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  HMPV <- str_sub(table4b, 
                  (start = str_locate(table4b, "HMPV")[1]),
                  (end = str_locate(table4b, "Parainfluenza")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  Parainfluenza <- str_sub(table4b, 
                           (start = str_locate(table4b, "Parainfluenza")[1]),
                           (end = str_locate(table4b, "Rhinovirus")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  rhinovirus <- str_sub(table4b, 
                        (start = str_locate(table4b, "Rhinovirus")[1]),
                        (end = str_locate(table4b, "RSV")[1] - 5))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  RSV <- str_sub(table4b, 
                 (start = str_locate(table4b, "RSV")[1]),
                 (end = str_locate(table4b, "Seasonal coronavirus")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  seasonal_corona <- str_sub(table4b, 
                             (start = str_locate(table4b, "Seasonal coronavirus")[1]),
                             (end = str_locate(table4b, "not include MERS-CoV")[1] - 2))%>%
    str_split(pattern = fixed("|")) %>%
    do.call(rbind.data.frame, .) %>%
    set_colnames(table_4_col_names)
  
  #create single dataframe
  table4_df <- bind_rows(adenovirus, HMPV, Parainfluenza, rhinovirus, RSV, seasonal_corona) %>%
    mutate(tests_percent_positive = gsub(x = tests_percent_positive, pattern ="%", "", fixed = TRUE), #get rid of "%"
           week_of_testing = week_of_test) 
  
  #return the output
  return(table4_df)
}

# apply the function to each pdf that has been downloaded
# use a for loop to check where the errors are:
# for(i in 1:13){
#   test <-  clean_table1718(pdf_names1718[i]) #clean_table1718(pdf_names1718[24])
# }

virus_2017_2018 <- map_dfr(pdf_names1718, clean_table1718) #apply the clean_table function to each pdf and create a df by adding on rows

date_df <- virus_2017_2018$week_of_testing %>% #separate(sep = "–")#
  str_split_fixed(pattern = "–", n = 2) %>% 
  gsub("[^[:alnum:] ]", "", .) %>%
  as.data.frame() %>%
  set_names(value = c("week_start_date", "week_end_date")) %>%
  mutate(
    week_start_date = mdy(week_start_date),
    week_end_date = mdy(week_end_date)
  )

# no missing dates
# date_df %>% select(week_end_date) %>% distinct() %>% arrange(week_end_date)

#manually spot checked 4 pdfs, no errors 

virus_2017_2018b <- cbind(virus_2017_2018, date_df) %>% 
  mutate(virus = gsub("†^", "", Virus, fixed = TRUE),
         virus = gsub("(does", "", virus, fixed = TRUE),
         virus = recode(virus, "Parainfluenza virus" = "Parainfluenza"),
         Virus = NULL,
         labs_testing_n = as.numeric(labs_testing_n),
         tests_performed = as.numeric(tests_performed),
         tests_positive = as.numeric(tests_positive),
         tests_percent_positive = (tests_positive/tests_performed) * 100,
         week_midpoint = (week_end_date - week_start_date )/2 + week_start_date)

ggplot(virus_2017_2018b, aes(x = week_midpoint, y = labs_testing_n, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2017_2018b, aes(x = week_midpoint, y = tests_performed, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2017_2018b, aes(x = week_midpoint, y = tests_percent_positive, color = virus)) + geom_point() + geom_line() + theme_bw()

write.csv(virus_2017_2018b, "C:/Users/dsk856/Box/texas/preliminary_epi/virus/virus_2017_2018.csv", row.names = FALSE)

#combine all the different years that I've downloaded
virus_2015_2017 <- bind_rows(virus_2015_2016b, virus_2016_2017b, virus_2017_2018b) %>%
                    mutate(virus = recode(virus, "Parainfluenza virus" = "Parainfluenza"))
ggplot(virus_2015_2017, aes(x = week_midpoint, y = labs_testing_n, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2015_2017, aes(x = week_midpoint, y = tests_performed, color = virus)) + geom_point() + geom_line() + theme_bw()
ggplot(virus_2015_2017, aes(x = week_midpoint, y = tests_percent_positive, color = virus)) + geom_point() + geom_line() + theme_bw()

write.csv(virus_2015_2017, "C:/Users/dsk856/Box/texas/preliminary_epi/virus/virus_2015_2017.csv", row.names = FALSE)


#files are all stored here: C:\Users\dsk856\Documents\TX_epi