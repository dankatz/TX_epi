#assemble weekly flu data for Texas
install.packages("cdcfluview")
library(cdcfluview)
library(ggplot2)
library(zoo)


# test <- get_weekly_flu_report(year = 2016)
# gsd <- get_state_data(2010:2014)
# surveillance_areas()
# test <- hospitalizations(surveillance_area = c("flusurv", "eip", "ihsp"),
#                  region = "TX", years = NULL)
test <- who_nrevss(region = "state",  years = NULL)
# test2 <- test$combined_prior_to_2015_16
# test3 <- test$public_health_labs
test4 <- test$clinical_labs

test5 <- filter(test4, region == "Texas") %>%
        mutate(date = ymd(wk_date),
               total_specimens = as.numeric(total_specimens),
               proportion_positive = as.numeric(percent_positive)/100,
               total_positive = total_specimens * proportion_positive) %>%
        select(date, total_positive)
str(test5)


ggplot(test5, aes(x= date, y = total_positive)) + geom_point() + theme_bw()

#convert to daily value from a weekly value (so it aligns with other data)
start_date <- mdy("01-01-2015")
end_date <- mdy("12-31-2019")
date <-  ymd(seq(as.Date(start_date), as.Date(end_date), by="days"))

flu_df <- data.frame(date) #str(flu_df)
flu_df <- left_join(flu_df, test5)

#get daily value from weekly value by a moving average window
flu_df2 <- arrange(flu_df, date) %>%
mutate( flu_d = round(rollapply(total_positive, 7, align = "right", FUN=function(x) mean(x, na.rm=TRUE), fill=NA) / 7, 2))
#the previous function fills in the average daily value from the weekly total 
#note, based on the cdc graphs that I downloaded, it appears that the date given is the date that the week starts


getwd()
write.csv(flu_df2, "C:/Users/dsk856/Box/texas/preliminary_epi/flu_total_positive191111.csv", row.names = FALSE)
?write.csv
