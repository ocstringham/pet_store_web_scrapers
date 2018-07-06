library(rvest)
library(stringr)
library(httr)
library(RSelenium)
library(XML)
library(magrittr)
library(pipeR)



#### Part 0: Get timestamps and preload functions ####


## load timestamp function - req internet bc API, also load other funs
source("scripts/web_archive/wbm_get_snapshots.R")
source("scripts/web_archive/functions/make_vec_same_length_function.R")
source("scripts/web_archive/functions/wbm_underground_reptiles_close_popup_fun.R")

url1 = "http://www.undergroundreptiles.com/animalsforsale.htm"
url2 = "http://undergroundreptiles.com/an.html"
url3 = "http://undergroundreptiles.com/"
url4 = "http://undergroundreptiles.com/product-category/animals/"



# call function to get time stamps
time_stamps_df_url1 = wbm_get_snapshots(url1)
time_stamps_df_url2 = wbm_get_snapshots(url2)
time_stamps_df_url3 = wbm_get_snapshots(url3)
time_stamps_df_url4 = wbm_get_snapshots(url4)

# combine them, first add field for version
time_stamps_df_url1$version = 1
time_stamps_df_url2$version = 2
time_stamps_df_url3$version = 3
time_stamps_df_url4$version = 4



# subset out desired dates
# make date field
time_stamps_df_url1$date = time_stamps_df_url1$timestamp  %>% 
  str_sub(start = 1, end = 8)  %>% 
  as.Date(format =  "%Y%m%d")

time_stamps_df_url2$date = time_stamps_df_url2$timestamp  %>% 
  str_sub(start = 1, end = 8)  %>% 
  as.Date(format =  "%Y%m%d")

time_stamps_df_url3$date = time_stamps_df_url3$timestamp  %>% 
  str_sub(start = 1, end = 8)  %>% 
  as.Date(format =  "%Y%m%d")

time_stamps_df_url4$date = time_stamps_df_url4$timestamp  %>% 
  str_sub(start = 1, end = 8)  %>% 
  as.Date(format =  "%Y%m%d")


# remove dates btwn specificied dates
time_stamps_df_url1 = subset(time_stamps_df_url1, date < "2007-03-21")
time_stamps_df_url2 = subset(time_stamps_df_url2, date < "2012-09-02")
time_stamps_df_url3 = subset(time_stamps_df_url3, date > "2012-09-01" & date < "2014-10-06") #,date < "2016-06-16"
time_stamps_df_url4 = subset(time_stamps_df_url4, date >= "2014-10-06")

time_stamps_df = rbind.data.frame(time_stamps_df_url1, time_stamps_df_url2,
                                  time_stamps_df_url3, time_stamps_df_url4)

# filter out only working snapshots, with statuscode == 200
time_stamps_df = subset(time_stamps_df, statuscode == "200")

## remove duplicated dates
# 
# # make date field
# time_stamps_df$date = time_stamps_df$timestamp  %>% 
#   str_sub(start = 1, end = 8)  %>% 
#   as.Date(format =  "%Y%m%d")

## remove ones not in time periods of verisons

# remove dupes
time_stamps_df = time_stamps_df[!duplicated(time_stamps_df$date),]

# add id for debugging/coding
time_stamps_df$ID<-seq.int(nrow(time_stamps_df))

# subset
time_stamps_df = time_stamps_df[, -which(names(time_stamps_df) %in% c("original", "mimetype", "digest", "length"))]

rm(time_stamps_df_url1, time_stamps_df_url2,
   time_stamps_df_url3, time_stamps_df_url4)


# # save timestamp df
# saveRDS(time_stamps_df, file = "data/web_archive/underground/underground_timestamps_df.rds")
