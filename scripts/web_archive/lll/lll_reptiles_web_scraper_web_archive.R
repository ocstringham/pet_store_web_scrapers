#-##########################################################################-#
#-##########################################################################-#
#-#################### Scraper for WBM LLL Reptile  ########################-#
#-##########################################################################-#
#-##########################################################################-#


library(rvest)
library(stringr)
library(httr)
library(RSelenium)
library(XML)
library(magrittr)
library(pipeR)



#### load data ####

## load timestamps from web archive
time_stamps_df = readRDS("data/web_archive/lll_reptiles/lll_timestamps_df.rds")

## load functions
source("scripts/web_archive/functions/make_vec_same_length_function.R")




#---------------------------------------------------------------------------#

#### Part A: Start RSelenium ####

## first kill open ports
if(Sys.info()[1] == "Windows") { system("Taskkill /IM java.exe /F")
} else if(Sys.info()[1] == "Linux") { system("killall java") }

### Start RSelenium
selenium <- rsDriver(version='3.0.1', browser='phantomjs', port = 4444L)
# server <- selenium[['server']]
remDr <- selenium[['client']]

#---------------------------------------------------------------------------#

#-###########################################################################-#
###### Outer loop through each timestamp #####################################
#-###########################################################################-#

for(q in 320:nrow(time_stamps_df))
{

#### Step 0: Remove variable except for ones that I need to keep ####
rm(list=setdiff(ls(), c("q", "remDr", "selenium",
                        "time_stamps_df", "make_vec_same_length",
                        "url1", "url2", "url3","url4")))



#### Step 1: Navigate to main website ####

### Depending on what version I'm at
  
if(time_stamps_df$version[q] == 1)
{
# create url depending on timestamp in the loop: q
url_timestamp = paste0("http://web.archive.org/web/",
                       time_stamps_df$timestamp[q], "/",
                       url1)  
}else if(time_stamps_df$version[q] == 2)
{
  url_timestamp = paste0("http://web.archive.org/web/",
                         time_stamps_df$timestamp[q], "/",
                         url2)  
}else if(time_stamps_df$version[q] == 3)
{
  url_timestamp = paste0("http://web.archive.org/web/",
                         time_stamps_df$timestamp[q], "/",
                         url3)  
}else if(time_stamps_df$version[q] == 4)
{
  url_timestamp = paste0("http://web.archive.org/web/",
                         time_stamps_df$timestamp[q], "/",
                         url4)  
}

  
print(paste0("Start scrape of timestamp ", q, "/", nrow(time_stamps_df)))
    
# navigate to current timestamp
remDr$navigate(url_timestamp)



#### Step 2: Get main category URLs ####


### _Step 2a: Define main category css ----

# version 1 has actual listings on that url

# version 2 css 
v2_css = "table.auto-margin table table a"

# version 3
v3_css1 = "h3.catalogCategoryHeader a" # 8/2004 to 3/2008
v3_css2 = "li li li:nth-of-type(1) li a" # 12/2008 to 3/2010   **some are missing
v3_css3 = "ul.jdm_events ul.jdm_events li:nth-of-type(1) li a" #7/2010 to 7/31/2013 
v3_css4 = "h3.catalogCategoryHeader a" #8/1/2013 to 6/25/2014

# version 4
v4_css1 = "h3.catalogCategoryHeader a" #7/29/2104 - 9/26/2015
v4_css2 = "h3.card-header a" #10/6/2015 - present



### _Step 2b: Get main category URLs ----

## inits 
main_cat_url = vector()

## find main cats depending on version

# skip version 1 bc listings directly on those pages

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

# start with version 2
if(time_stamps_df$version[q] == 2)
{
  # find elements
  main_cat_elem = remDr$findElements(using="css selector", value = v2_css)
  
  ## get URLs
  
  #inits
  main_cat_url_temp = vector()
  
  for(j in 1:length(main_cat_elem))
  {
    # get main cat url for each element
    main_cat_url_temp[j] = main_cat_elem[[j]]$getElementAttribute("href") %>% unlist()
  }
  
  # get rid of view all or not, can remove duplicates later/duplicates don't matter
  
  # save sub cat urls
  main_cat_url = main_cat_url_temp
  
  
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
}else if(time_stamps_df$version[q] == 3)
{

  # find elements depeding on which css it is
  main_cat_elem = remDr$findElements(using="css selector", 
                                value = v3_css1)
  ## the else if only works if no older css's are present, this fails for q3
  # add search for chameleons in urls, if present proceed with if else
  main_cat_temp0 = vector()
  for(j in 1:length(main_cat_elem))
  {main_cat_temp0[j] = main_cat_elem[[j]]$getElementAttribute("href") %>% unlist()}
  actual_main_cats = !(str_detect(main_cat_temp0, "chameleons") %>% which() %>>% `[`(1) %>% is.na())
  
  if(length(main_cat_elem) == 0 ||  !actual_main_cats )
  {
    main_cat_elem = remDr$findElements(using="css selector", 
                                       value = v3_css2)
  }else if(length(main_cat_elem) == 0)
  {
    main_cat_elem = remDr$findElements(using="css selector", 
                                       value = v3_css3)
  }else if(length(main_cat_elem) == 0)
  {
    main_cat_elem = remDr$findElements(using="css selector", 
                                       value = v3_css4)
  }
  
  ## get URLs
  
  #inits
  main_cat_url_temp = vector()
  
  for(j in 1:length(main_cat_elem))
  {
    # get main cat url for each element
    main_cat_url_temp[j] = main_cat_elem[[j]]$getElementAttribute("href") %>% unlist()
  }
  
  # get rid of view all or not, can remove duplicates later/duplicates don't matter
  
  # save sub cat urls
  main_cat_url = main_cat_url_temp
  

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
}else if(time_stamps_df$version[q] == 4)
{
  # find elements depeding on which css it is
  main_cat_elem = remDr$findElements(using="css selector", 
                                     value = v4_css1)
  if(length(main_cat_elem) == 0)
  {
    main_cat_elem = remDr$findElements(using="css selector", 
                                       value = v4_css2)
  }
  
  ## get URLs
  
  #inits
  main_cat_url_temp = vector()
  
  for(j in 1:length(main_cat_elem))
  {
    # get main cat url for each element
    main_cat_url_temp[j] = main_cat_elem[[j]]$getElementAttribute("href") %>% unlist()
  }
  
  # get rid of view all or not, can remove duplicates later/duplicates don't matter
  
  # save sub cat urls
  main_cat_url = main_cat_url_temp
  
}


#------------------------------------------------------------------------------#


#### Step 3: Get data from main urls ####

## define outer if - for if there is no website/main cats
if(length(main_cat_url > 0)){
  
### _Step 3A: define inits ----

# version 1 inits
v1_css_info = "td:nth-of-type(1)"
v1_css_price = "td:nth-of-type(2)"
info = vector()
price = vector()
# date_from_url1 = vector()

# version 2 inits
v2_css_name = "td:nth-of-type(3) td a"
v2_css_details = "tr:nth-of-type(n+2) td:nth-of-type(4)"
v2_css_price = "tr:nth-of-type(n+2) td:nth-of-type(6)"
name_v2 = vector()
details_v2 = vector()
price_v2 = vector()
# date_from_url2 = vector()

# version 3 inits
v3_css_info_1 = "div#bodyContent td:nth-of-type(2)"
v3_css_price_1 = "ul.prices"
v3_css_price_2 = "table.productsTable td:nth-of-type(3)"
#v3_css_info_2 = q2 css = q1 css at q=169 to 177, no wbm data for 178 to 191
#v3_css_price_2 =   q2 css = q1 css at q=169 to 177, no wbm data for 178 to 191
#v3_css_info_3 = q3 css = q1 css
#v3_css_price_3 = q3 css = q1 css
# v3 q4 css = q1

info_v3 = list()
price_v3 = list()
date_from_url3 = list()

# version 4 inits
v4_css_info_1 = "table.productsTable td:nth-of-type(2)"
v4_css_price_1 = "table.productsTable td:nth-of-type(3)"  #, ul.prices

v4_css_all_2 = "div.product-wrap"
# v4_css_title_2 = "div.product-title"
# v4_css_species_2 = "p.product-species em"
# v4_css_size_2 = "p.product-size em"
# v4_css_descr_2 = "div.product-desc p"
# v4_css_price_2 = "ul.prices li"
# # v4_css_price_2 = "ul.prices" # same as q1


info_v4 = list()
price_v4 = list()
date_from_url4 = list()

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

### _Part 1: scrape version 1 ----
# version 1 if statement -- test to see if string funs work as intended
if(time_stamps_df$version[q] == 1)
{
  # q = 1 to 32
  
  ## look for elements and get text
  # info
  infotemp1 = vector()
  infotemp1 = remDr$findElements(using="css selector", value = v1_css_info)
  for(j in 1:length(infotemp1))
  {
    info[j] = infotemp1[[j]]$getElementText() %>% unlist()
  }
  # price
  pricetemp1 = vector()
  pricetemp1 = remDr$findElements(using="css selector", value = v1_css_price)
  for(j in 1:length(pricetemp1))
  {
    price[j] = pricetemp1[[j]]$getElementText() %>% unlist()
  }

## clean data to make sure that both field are aligned ##

# tidbit on why and how i did this:
# the main problem was that there was no organization in version 1 of the website, so i had to grab a lot of unwanted data. sometimes, they dont line up and are off by one.

# # get start index for info, should be right after wbm info about captures
# start_info = 1 + str_detect(info, "^[0-9]+ captures") %>% which()

# start price get first instance of $
start_price = str_detect(price, "\\$") %>% which() %>>% `[`(1)

# assume start_price index: I checked. for v1 this always works.
start_info = start_price

# redefine info and price with new starting points
info = info[start_info:length(info)]
price = price[start_price:length(price)]

# make vecs same length
info = make_vec_same_length(info, price) %>>% `[`(1) %>% unlist()
price = make_vec_same_length(info, price) %>>% `[`(2) %>% unlist()

## create finaldf
# initial define: using indices gathered from above
finaldf = cbind.data.frame(info,price) 
                          

# add url timestamp
finaldf$date_from_url = remDr$getCurrentUrl() %>% 
  str_sub(start = 28, end = 35)  %>% 
  as.Date(format =  "%Y%m%d")

# add version number
finaldf$version = 1

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

### _Part 2: scrape version 2 ----
}else if(time_stamps_df$version[q] == 2)
{
  # q = 33 to 40
  
  # just get the url with view all
  view_all_url_index = str_detect(main_cat_url, "view-all") %>% which() %>>% `[`(1)
  view_all_url = main_cat_url[view_all_url_index]
  
  # navigate to view all
  remDr$navigate(view_all_url)
  
  # look for elements and get text
  # name
  nametemp = vector()
  nametemp = remDr$findElements(using = "css selector", value = v2_css_name)
  for (j in 1:length(nametemp))
  {
    name_v2[j] = nametemp[[j]]$getElementText() %>% unlist()
  }
  # details
  detailstemp = vector()
  detailstemp = remDr$findElements(using = "css selector", value = v2_css_details)
  for (j in 1:length(detailstemp))
  {
    details_v2[j] = detailstemp[[j]]$getElementText() %>% unlist()
  }
  # price
  pricetemp = vector()
  pricetemp = remDr$findElements(using = "css selector", value = v2_css_price)
  for (j in 1:length(pricetemp))
  {
    price_v2[j] = pricetemp[[j]]$getElementText() %>% unlist()
  }

  ## combine all info fields
  # first make sure details_v2 starts at right index
  details_start = str_detect(details_v2, "^Size") %>% which() %>>% `[`(1)
  details_v2 = details_v2[details_start:length(details_v2)]
  
  # combine
  info_v2 = paste(name_v2, details_v2, sep = "\n")
  
  ## create finaldf 
  # create df
  finaldf = cbind.data.frame(info = info_v2,
                   price = price_v2)
  
  # add url timestamp
  finaldf$date_from_url = remDr$getCurrentUrl() %>% 
                            str_sub(start = 28, end = 35)  %>% 
                            as.Date(format =  "%Y%m%d")
  
  # add version to df
  finaldf$version = 2
  
  
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
### _Part 3: scrape version 3 ----
}else if(time_stamps_df$version[q] == 3)
{
  # q overall = 41 to 278 
  
  # q1 = 41 to 168 - css's dont change :)
  # q2 = 169 to 191. css = q1 css. no data on wbm for q = 178 to 191 :(
  # q3 = 192 to 267. css = q1 css for all
  # q4 = 268 to 278. css = q1 for all
  
  
  ## loop through each main category
  for(k in 1:length(main_cat_url))
  {
    
    print(paste0("Scraping main category ", k, "/", length(main_cat_url), 
                 " of ", q, "/", nrow(time_stamps_df), " timestamp" ))
    
    # nav to main category
    remDr$navigate(main_cat_url[k])
    
    ## get data
    
    # info
    # try each info css & extract info data
    infotemp3 = vector(); infotemp_v3 = vector()
    
    infotemp3 = remDr$findElements(using = "css selector", value = v3_css_info_1) 
    # add in code to skip if no cotent = wbm didn't get data
    if(length(infotemp3) != 0)
    {
    # get text
    for(j in 1:length(infotemp3))
    {infotemp_v3[j] = infotemp3[[j]]$getElementText() %>% unlist()}

    info_v3[[k]] = infotemp_v3
    
    # price
    # try each price css & extract price data
    pricetemp3 = vector(); pricetemp_v3 = vector()
    pricetemp3 = remDr$findElements(using = "css selector", value = v3_css_price_1)
    
    ### fix bug of not being same length as info
    if(length(pricetemp3) != length(info_v3[[k]]))
    {pricetemp3 = vector()
    pricetemp3 = remDr$findElements(using = "css selector", value = v3_css_price_2)}
    
    # get price data
    for(j in 1:length(pricetemp3))
    {pricetemp_v3[j] = pricetemp3[[j]]$getElementText() %>% unlist()}
    
    price_v3[[k]] = pricetemp_v3
    
    # get url timestamp of main cat
    date_temp = remDr$getCurrentUrl() %>% 
      str_sub(start = 28, end = 35)  %>% 
      as.Date(format =  "%Y%m%d") %>%
      as.character()
    date_from_url3[[k]] = rep(date_temp, each = length(info_v3[[k]]))
    
    }else{
      info_v3[[k]] = NA
      price_v3[[k]] = NA
      date_from_url3[[k]] = NA
    }
  
  }
  
  ## make final df
  finaldf = cbind.data.frame(info = (info_v3 %>% unlist()),
                             price = (price_v3 %>% unlist()),
                             date_from_url = (date_from_url3 %>% unlist()) )
  # add version to df
  finaldf$version = 3

  
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
### _Part 4: scrape version 4 ----
}else if(time_stamps_df$version[q] == 4)
{
  # q overall = 279 to 371
  # q1 = 279 to 318, #7/29/2104 - 9/26/2015
  # q2 = 319 to 371, #10/6/2015 - present
  
  ## loop through each main category
  for(k in 1:length(main_cat_url))
  {
    
    print(paste0("Scraping main category ", k, "/", length(main_cat_url), 
                 " of ", q, "/", nrow(time_stamps_df), " timestamp" ))
    
    # nav to main category
    remDr$navigate(main_cat_url[k])
    
    ## get data
    # info
    # try each info css & extract info data
    alltemp4=vector() 
    alltemp_v4=vector()
    #--#
    infotemp4=vector()
    infotemp_v4 = vector()   
    infotemp4 = remDr$findElements(using = "css selector", value = v4_css_info_1) 

    ## for q2
    alltemp4 = remDr$findElements(using = "css selector", value = v4_css_all_2)
    
    ## if neither q1 or q2 skip page
    if(length(infotemp4) == 0 & length(alltemp4) == 0)
    {
      info_v4[[k]] = NA
      price_v4[[k]] = NA
      date_from_url4[[k]] = NA
      
      # if q >= 319 AKA q2
    }else if(q >= 319) 
    {
      # get text
      for(j in 1:length(alltemp4))
        {alltemp_v4[j] = alltemp4[[j]]$getElementText() %>% unlist()}

      info_v4[[k]] = alltemp_v4
      # fill in price later for v4q2
      price_v4[[k]] = rep(NA, each = length(info_v4[[k]]))
      
      
    }else{ # else do q1

      # info
      for(j in 1:length(infotemp4))
      {infotemp_v4[j] = infotemp4[[j]]$getElementText() %>% unlist()}

      info_v4[[k]] = infotemp_v4
      
      # price
      # find element & extract price data
      pricetemp4 = vector()
      pricetemp_v4 = vector()
      pricetemp4 = remDr$findElements(using = "css selector", value = v4_css_price_1)
      
      for(j in 1:length(pricetemp4))
      {pricetemp_v4[j] = pricetemp4[[j]]$getElementText() %>% unlist()}
      
      price_v4[[k]] = pricetemp_v4

    }
    

    # get url timestamp of main cat
    date_temp = remDr$getCurrentUrl() %>% 
      str_sub(start = 28, end = 35)  %>% 
      as.Date(format =  "%Y%m%d") %>%
      as.character()
    date_from_url4[[k]] = rep(date_temp, each = length(info_v4[[k]]))
    
    
  } # end loop through indiv cats
  

  ## make final df
  finaldf = cbind.data.frame(info = (info_v4 %>% unlist()),
                             price = (price_v4 %>% unlist()),
                             date_from_url = (date_from_url4 %>% unlist()) )
  # add version to df
  finaldf$version = 4
  
}

#-------------------------------------------------------------------------#

#### Step 4: Save ####

# add timestamp to df
finaldf$wbm_timestamp = time_stamps_df$timestamp[q] %>%
  as.Date(format =  "%Y%m%d")

# load function
source("scripts/web_archive/wbm_save_function.R")


# set parameters
x = "lll_reptiles"
y = url_timestamp
z = finaldf

# call function
wbm_save(x,y,z)

print(paste0("Save success of timestamp ", q, "/", nrow(time_stamps_df)))



# end if main cat url >0
}else{
  print(paste0("No data for timestamp ", q, "/", nrow(time_stamps_df)))

  } 

} # end for loop through timestamps

#---------------------------------------------------------------------------#

#### Part B: Close RSelenuim ####
#close browser
remDr$close()
# stop the selenium server
rD[["server"]]$stop()

