
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



url1 = "http://www.lllreptile.com/animals.html"
url2 = "http://lllreptile.com/v2/content/catalog/?section_string=animals"
url3 = "http://www.lllreptile.com/store/catalog/animals/"
url4 = "http://www.lllreptile.com/catalog/101-animals"


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

time_stamps_df = rbind.data.frame(time_stamps_df_url1, time_stamps_df_url2,
                                  time_stamps_df_url3, time_stamps_df_url4)

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
# saveRDS(time_stamps_df, file = "data/web_archive/lll_reptiles//lll_timestamps_df.rds")
