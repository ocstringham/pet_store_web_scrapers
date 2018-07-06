##### script to get all available timestamps from web archive for http://www.backwaterreptiles.com ####


library(rvest)
library(stringr)
library(httr)
library(RSelenium)
library(XML)
library(magrittr)


#### Part 0: Get timestamps and preload some data####

url = "http://www.backwaterreptiles.com/"

## pre load back water main category urls for later loop
backwater_main_cats_url = readRDS("data/web_archive/backwater/backwater_main_cats_url.rds")
backwater_main_cats_url = backwater_main_cats_url[c(-10:-14)]


## load timestamp function - req internet bc API
source("scripts/web_archive/wbm_get_snapshots.R")

# call function to get time stamps
time_stamps_df = wbm_get_snapshots(url)

# filter out only working snapshots, with statuscode == 200
time_stamps_df = subset(time_stamps_df, statuscode == "200")

## remove duplicated dates

# make date field
time_stamps_df$date = time_stamps_df$timestamp  %>% 
  str_sub(start = 1, end = 8)  %>% 
  as.Date(format =  "%Y%m%d")

# remove dupes
time_stamps_df = time_stamps_df[!duplicated(time_stamps_df$date),]

# add id for debugging/coding
time_stamps_df$ID<-seq.int(nrow(time_stamps_df))



# # save timestamp df
# saveRDS(time_stamps_df, file = "data/web_archive/backwater/backwater_timestamps_df.rds")
