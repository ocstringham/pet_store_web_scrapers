#-##########################################################################-#
#-##########################################################################-#
#-############ Scraper for web archive http://www.backwaterreptiles.com ####-#
#-##########################################################################-#
#-##########################################################################-#


library(rvest)
library(stringr)
library(httr)
library(RSelenium)
library(XML)
library(magrittr)
library(reshape2)



#### load data ####
## load timestamps from web archive
time_stamps_df = readRDS("data/web_archive/backwater/backwater_timestamps_df.rds")
## load main categories of website
backwater_main_cats_url = readRDS("data/web_archive/backwater/backwater_main_cats_url.rds")




#### Part A: Start RSelenium ####

## first kill open ports
if(Sys.info()[1] == "Windows") { system("Taskkill /IM java.exe /F") 
} else if(Sys.info()[1] == "Linux") { system("killall java") }

### Start RSelenium
selenium <- rsDriver(version='3.0.1', browser='phantomjs')   #, port = 4444L
# server <- selenium[['server']]
remDr <- selenium[['client']]

#---------------------------------------------------------------------------#

#-###########################################################################-#
###### Outer loop through each timestamp #####################################
#-###########################################################################-#

# start loop wherever it left off
for(q in 40:nrow(time_stamps_df))
{

  
#### Step 0: Remove variable except for ones that I need to keep ####
rm(list=setdiff(ls(), c("q", "remDr", "selenium", 
                        "time_stamps_df", "url",
                        "backwater_main_cats_url")))
  
  
  
#### Step 1: Navigate to main website ####
# create url depending on timestamp in the loop: q
url_timestamp = paste0("http://web.archive.org/web/",
                       time_stamps_df$timestamp[q], "/",
                       url)  

# navigate to current timestamp
remDr$navigate(url_timestamp)

print(paste0("Start scrape of timestamp ", q, "/", nrow(time_stamps_df)))


#### Step 2: Loop through main cats to either 1. get sub cat urls or scrape data if no sub cats####

# define main cat urls variable for current timestamp
main_cats_url = paste0("http://web.archive.org/web/",
                       time_stamps_df$timestamp[q], "/",
                       backwater_main_cats_url)  

## define css selector for data depending on website date
# v1 works for current (3/2/17), older: (3/28/13), and oldest (9/24/11) so I shouldnt have to change this
v1_data_css = "div.stacks_div.stacks_middle div.stacks_div div.stacks_out:nth-of-type(1) div.stacks_in"


### Step 2A: loop through each main cat

# inits
sub_cats_url_temp = vector()
sub_cats_url = list()
main_cats_without_subcats = vector()
f=1
data_main_cat = list()
date_main_cat = list()

# loop through main cats
for(i in 1:length(main_cats_url))
{
  ## navigate to main cat url
  remDr$navigate(main_cats_url[i])
  
  # find sub cat elements
  sub_cats_elem = remDr$findElements(using="css selector", value = "div.centered_image a")
  # find data elem if not sub cats there
  data_elem = remDr$findElements(using="css selector", value = v1_data_css)
  
  
  ## if subcats exist
  if(length(sub_cats_elem) > 0)
  {
    ## get subcat urls
    
    #init
    sub_cats_url_temp = vector()
    
    # loop through each elem to get each subcat url
    for(j in 1:length(sub_cats_elem))
    {
      # get sub cat urls for each element
      sub_cats_url_temp[j] = sub_cats_elem[[j]]$getElementAttribute("href") %>% unlist()
    }
    
    # save sub cat urls
    sub_cats_url[[i]] = sub_cats_url_temp
  
    
  ### Step 2B: else if no sub cats, but direct data on page, then scrape
  }else if(length(data_elem) > 0)
  {
    # save index of main cats without subcats so that can keep them in master url list
    main_cats_without_subcats[f] = i
    
    # ALSO scrape data here so i dont have to revist sites later
    data_temp = list()
    
    for(k in 1:length(data_elem))
    {
      data_temp[[k]] = data_elem[[k]]$getElementText() %>%
        unlist()
    }
    
    # store data
    data_main_cat[[f]] = data_temp %>% unlist()
    
    # store date of data
    date_main_cat[[f]] = main_cats_url[i]  %>% 
      str_sub(start = 28, end = 35)  %>% 
      as.Date(format =  "%Y%m%d")
    
    f = f+1
    
    print(paste0("Scraping main category ", i, "/", length(main_cats_url), 
                 " of ", q, "/", nrow(time_stamps_df), " timestamp" ))
    
  }

}



#### Step 3: Loop through subcats for data ####

# get subcats in a vector
sub_cats_url = sub_cats_url %>% unlist()


### Step 3A: loop through subcats

# inits
data_sub_cat = list()
date_sub_cat = list()


## loop though sub cats
for(i in 1:length(sub_cats_url))
{
  ## nav to subcats
  remDr$navigate(sub_cats_url[i])
  
  ## get data
  data_elem = remDr$findElements(using="css selector", value=v1_data_css)
  
  data_temp = list()
  for(k in 1:length(data_elem))
  {
    data_temp[[k]] = data_elem[[k]]$getElementText() %>%
      unlist()
  }
  
  # store data
  data_sub_cat[[i]] = data_temp %>% unlist()
  
  # store date of data
  date_sub_cat[[i]] = sub_cats_url[i]  %>% 
    str_sub(start = 28, end = 35)  %>% 
    as.Date(format =  "%Y%m%d")
  
  print(paste0("Scraping sub category ", i, "/", length(sub_cats_url), 
               " of ", q, "/", nrow(time_stamps_df), " timestamp" ))
  
}

print(paste0("Finish scrape of timestamp ", q, "/", nrow(time_stamps_df)))

#------------------------------------------------------------------------------------------#

#### Step 4: Compile data from all listings (main and sub categories) ####

# 1. resave variables
x.main = data_main_cat ; x.sub = data_sub_cat
y.main = date_main_cat ; y.sub = date_sub_cat

# 2. melt to get common id
x.main.melt = melt(x.main) ; x.sub.melt = melt(x.sub)
y.main.melt = melt(y.main) ; y.sub.melt = melt(y.sub)

# 3. merge within main and sub cats
main.merge = merge(x.main.melt, y.main.melt, by = "L1")
sub.merge = merge(x.sub.melt, y.sub.melt, by = "L1")

# 4. merge togther main and sub cats
finaldf = rbind(main.merge, sub.merge)

# 5. rename fields and add field for timestamp 
colnames(finaldf)[2] = "data"
colnames(finaldf)[3] = "date_from_url"

finaldf$wbm_timestamp = time_stamps_df$timestamp[q] %>%
                          as.Date(format =  "%Y%m%d")

print(paste0("Data manipulation success of timestamp ", q, "/", nrow(time_stamps_df)))



#### Step 5: Save ####

# load function
source("scripts/web_archive/wbm_save_function.R")


# set parameters
x = "backwater"
y = url_timestamp
z = finaldf

  # call function
  wbm_save(x,y,z)

print(paste0("Save success of timestamp ", q, "/", nrow(time_stamps_df)))

}


#---------------------------------------------------------------------------#

#### Part B: Close RSelenuim ####
#close browser
remDr$close()
# stop the selenium server
rD[["server"]]$stop()


