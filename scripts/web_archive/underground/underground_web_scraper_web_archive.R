#-##########################################################################-#
#-##########################################################################-#
#-                    Scraper for WBM Underground Reptiles                  -#
#-##########################################################################-#
#-##########################################################################-#



library(rvest)
library(stringr)
library(httr)
library(RSelenium)
library(XML)
library(magrittr)
library(pipeR)


## load timestamps from web archive
time_stamps_df = readRDS("data/web_archive/underground/underground_timestamps_df.rds")

## load functions
source("scripts/web_archive/functions/make_vec_same_length_function.R")
source("scripts/web_archive/get_text_RSelenium_fun.R")



#---------------------------------------------------------------------------#

#### Part A: Start RSelenium ####

# ## first kill open ports
# if(Sys.info()[1] == "Windows") { system("Taskkill /IM java.exe /F")
# } else if(Sys.info()[1] == "Linux") { system("killall java") }

### Start RSelenium
selenium <- rsDriver(version='3.0.1', browser='phantomjs', port = 4447L)
# selenium <- rsDriver(browser='firefox', port = 4449L)
# selenium <- rsDriver(browser='chrome', port = 4449L)
# selenium <- rsDriver(browser='phantomjs', port = 4448L)
# selenium <- rsDriver(version='3.0.1', browser='chrome', port = 4449L)
# server <- selenium[['server']]
remDr <- selenium[['client']]

#---------------------------------------------------------------------------#

#-###########################################################################-#
###### Outer loop through each timestamp #####################################
#-###########################################################################-#

for(q in 91:nrow(time_stamps_df)) # start from where i left off
{
  
  #### Step 0: Remove variable except for ones that I need to keep ####
  rm(list=setdiff(ls(), c("q", "remDr", "selenium",
                          "time_stamps_df", "make_vec_same_length",
                          "wbm_underground_reptiles_close_popup",
                          "url1", "url2", "url3")))
  
  
  
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
  }
  # skip bc it's after June 2016 and there are only 2 of them.
  # else if(time_stamps_df$version[q] == 4)
  # {
  #   url_timestamp = paste0("http://web.archive.org/web/",
  #                          time_stamps_df$timestamp[q], "/",
  #                          url4)  
  # }
  
  
  print(paste0("Start scrape of timestamp ", q, "/", nrow(time_stamps_df)))
  
  # navigate to current timestamp
  
  # first go to base page to load cookies
  remDr$navigate("http://web.archive.org")
  
  # load cookies
  remDr$addCookie(name = "yit-popup-access-underground-reptiles",
                  value = "1", 
                  domain = "web.archive.org", 
                  path = "/", 
                  httpOnly = FALSE)
  
  # go to timestamp
  remDr$navigate(url_timestamp)
  
  # # # close popup if there is one
  # wbm_underground_reptiles_close_popup()
  
  # # working with cookies
  # cookies_list = remDr$getAllCookies()
  # cookie_popup = cookies_list[[1]]
  

  
#### Step 2: Get main category URLs ####


### _Step 2a: Define main category css ----


# version 1 css 
v1_css = "p:nth-of-type(n+2) a" # 10/28/2003 to 3/17/2007 # q = 1 to 20

# skip q =21 for now, "h2 a"
  
# version 2
# 2011-07-06 to 2012-08-02    # q = 22 to 37
v2_css1 = "div:nth-of-type(3) a:nth-of-type(1), center a:nth-of-type(n+2)" 



# version 3
# q 38, 39 not working

# q 40 to 76  # q 77 not working sometimes but has same css
v3_css1 = "ul.level2 a.orphan, ul.level3 a.orphan" # main cats, sub cats


# q = 78 to 111 new css
v3_css2 = "ul.level-1 li#menu-item-2268.menu-item a"




# 
# # q 4
# # q = 112 to current
# v4_css1 = "a.product-category-link"
# v4_css1_sub = "a.product-category-link"
# 
# # main cats ALSO same as sub cats, some have sub cats some dont 
# # but this css doesn't show up in listings :)
# # the listing are going to be a huge pain bc pagination :(



### _Step 2b: Get main category URLs ----

## inits 
main_cat_url = vector()

## find main cats depending on version


# start with version 1
if(time_stamps_df$version[q] == 1)
{
  # find elements
  main_cat_elem = remDr$findElements(using="css selector", value = v1_css)
  
  ## get URLs
  # get main cat url for each element
  main_cat_url =unlist(lapply(main_cat_elem, 
                              function(x){x$getElementAttribute("href")}))
  
  
  
  #--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
}else if(time_stamps_df$version[q] == 2)
{
  # find elements
  main_cat_elem = remDr$findElements(using="css selector", value = v2_css1)
  
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
  # find elements
  main_cat_elem = remDr$findElements(using="css selector", value = v3_css1)
  
  if(length(main_cat_elem) == 0){
    main_cat_elem = remDr$findElements(using="css selector", value = v3_css2)
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
  
  # get unique
  main_cat_url = main_cat_url %>% unique()

}

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#


#### Step 3: Get data from main urls ####

## define outer if - for if there is no website/main cats
if(length(main_cat_url) > 0){
  
  ### _Step 3A: define inits ----
  # version 1 inits
  v1_css_name = "td div > table > tbody > tr:nth-of-type(1) > td"
  v1_css_price = "tr:nth-of-type(4) td:nth-of-type(2)"
  # v1_css_details = "td div tr:nth-of-type(3) td:nth-of-type(2)"
  name_v1 = list()
  price_v1 = list()
  # details_v1 = list()  
  date_from_url_v1 = list()
  
  # version 2 inits
  v2_css_name = "div p:nth-of-type(1)"
  v2_css_details = "div p:nth-of-type(2)"
  v2_css_price = "p strong"
  name_v2 = list()
  details_v2 = list()
  price_v2 = list()
  date_from_url_v2 = list()
  
  # version 3 inits
  v3_css_name_1 = "h2.sprocket-mosaic-title a"
  v3_css_name_1_alt = "div.item h2"
  v3_css_price_1 = "div.item p:nth-of-type(3)" #no price data
  
  v3_css_name_2 = "div.product-meta h3"
  v3_css_price_2 = "span.price"
  
  name_v3 = list()
  price_v3 = list()
  date_from_url_v3 = list()
  
  
  
  #--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
  
  
  
  ### _Part 1: scrape version 1 ----

  # version 1 if statement
  if(time_stamps_df$version[q] == 1)
  {
    # q = 1 to 20
    
    
    # loop through main urls
    for(k in 1:length(main_cat_url))
    {
      # nav to main cat url
      remDr$navigate(main_cat_url[k])
      
      # name
      name_v1[[k]] = get_text_RSelenium(remDr, v1_css_name)
      
      # # details
      # details_v1[[k]] = get_text_RSelenium(remDr, v1_css_details)
      
      # price
      price_v1[[k]] = get_text_RSelenium(remDr, v1_css_price)
      
      
      # make sure price and name are same length
      name_v1[[k]] = make_vec_same_length(name_v1[[k]], price_v1[[k]]) %>>% `[`(1) %>% unlist()
      price_v1[[k]] = make_vec_same_length(price_v1[[k]], name_v1[[k]]) %>>% `[`(1) %>% unlist()
      
      # date
      datetemp = remDr$getCurrentUrl() %>% 
                    str_sub(start = 28, end = 35)  %>% 
                    as.Date(format =  "%Y%m%d")
      date_from_url_v1[[k]] = rep(datetemp, times = length(name_v1[[k]]))

    }
    
    ## compile into finaldf
    
    # unlist for finaldf
    name_v1 = unlist(name_v1)
    price_v1 = unlist(price_v1)
    date_from_url_v1 = do.call("c", date_from_url_v1)
    
    ## create finaldf
    finaldf = cbind.data.frame(name_v1, price_v1, date_from_url_v1,
                               stringsAsFactors = F) 
    
    # add version number
    finaldf$version = 1
    
    
  ### _Part 2: scrape version 2 ----
  }else if(time_stamps_df$version[q] == 2)
  {
    # q = 21 to 38
    # loop through main urls
    for(k in 1:length(main_cat_url))
    {
      # nav to main cat url
      remDr$navigate(main_cat_url[k])
      
      # name
      name_v2[[k]] = get_text_RSelenium(remDr, v2_css_name)
      
      # # details
      # details_v2[[k]] = get_text_RSelenium(remDr, v2_css_details)
      
      # # price - is in name, clean later
      # price_v2[[k]] = get_text_RSelenium(remDr, v2_css_price)
      
      # date
      datetemp = remDr$getCurrentUrl() %>% 
        str_sub(start = 28, end = 35)  %>% 
        as.Date(format =  "%Y%m%d")
      date_from_url_v2[[k]] = rep(datetemp, times = length(name_v2[[k]]))
      
    }
    
    ## compile into finaldf
    
    # unlist for finaldf
    name_v2 = unlist(name_v2)
    # details_v2 = unlist(details_v2)
    # price_v2 = unlist(price_v2)
    date_from_url_v2 = do.call("c", date_from_url_v2)
    
    ## create finaldf
    finaldf = cbind.data.frame(name_v2, date_from_url_v2, 
                               stringsAsFactors = F) 
    
    # add version number
    finaldf$version = 2
    
    
  ### _Part 3: scrape version 3 ----
  }else if(time_stamps_df$version[q] == 3)
  {
    
    # 3.1 = q 40 to 76  # q 38, 39 not working # q 77 not working sometimes but has same css
    # 3.2 = q = 78 to 111 new css

    if(q >= 38 && q <= 77){
      
      v3_css_name = v3_css_name_1
    
    }else if(q >= 78){
      
      v3_css_name = v3_css_name_2
      
    }
    
    
    ## progress bar
    total = length(main_cat_url)
    # create progress bar
    pb = txtProgressBar(min = 0, max = total, style = 3)
    
    
    # loop through main urls
    for(k in 1:length(main_cat_url))
    {

      
      
      # nav to main cat url
      
      # first add cookie
      remDr$addCookie(name = "yit-popup-access-undergrournd-reptiles",
                      value = "1", 
                      domain = "web.archive.org", 
                      path = "/", 
                      httpOnly = FALSE)
      
      remDr$navigate(main_cat_url[k])
      
      # # close popup
      # wbm_underground_reptiles_close_popup()
      
      # name
      name_v3_temp = get_text_RSelenium(remDr, v3_css_name)
      
      # try alt css if NA
      if(is.na(name_v3_temp)){ 
       
         name_v3[[k]] = get_text_RSelenium(remDr, v3_css_name_1_alt)
      
      }else{
        
        name_v3[[k]] =  name_v3_temp 
        
      }
      
      # # details
      # details_v2[[k]] = get_text_RSelenium(remDr, v2_css_details)
      
      # price - is in name, clean later
      if(q >= 38 && q <= 77){
        
        price_v3[[k]] = rep(NA, times = length(name_v3[[k]]))
        
        # price_v3_temp =  get_text_RSelenium(remDr, v3_css_price_1)
        # 
        # if(is.na(price_v3_temp)){
        #   
        #   price_v3[[k]] = rep(NA, times = length(name_v3[[k]]))
        # 
        # }else{
        #   
        #   price_v3[[k]] = price_v3_temp
        #   
        # }
        
      }else if(q >= 78){
        
        price_v3[[k]] = get_text_RSelenium(remDr, v3_css_price_2)
      
      }
      
      
      # date
      datetemp = remDr$getCurrentUrl() %>% 
        str_sub(start = 28, end = 35)  %>% 
        as.Date(format =  "%Y%m%d")
      date_from_url_v3[[k]] = rep(datetemp, times = length(name_v3[[k]]))
      
      
      # update progress bar
      setTxtProgressBar(pb, k)
      
    }
    
    # close progress bar
    close(pb)
    
    ## compile into finaldf
    
    # unlist for finaldf
    name_v3 = unlist(name_v3)
    # details_v3 = unlist(details_v3)
    price_v3 = unlist(price_v3)
    date_from_url_v3 = do.call("c", date_from_url_v3)
    
    ## create finaldf
    finaldf = cbind.data.frame(name_v3, price_v3, date_from_url_v3, 
                               stringsAsFactors = F) 
    
    # add version number
    if(q >= 38 && q <= 77){
      finaldf$version = 3.1
    }else if(q >= 78){
      finaldf$version = 3.2
    }

  
  }else if(time_stamps_df$version[q] == 4){
    
    
    
    
    
    
  }
  
  
  
  
  
  #-------------------------------------------------------------------------# 
  
  #### Step 4: Save ####
  
  # add timestamp to df
  finaldf$wbm_timestamp = time_stamps_df$timestamp[q] %>%
    as.Date(format =  "%Y%m%d")
  
  # load function
  source("scripts/web_archive/wbm_save_function.R")
  
  
  # set parameters
  x = "underground"
  y = url_timestamp
  z = finaldf
  
  # call function
  wbm_save(x,y,z)
  
  print(paste0("Save success of timestamp ", q, "/", nrow(time_stamps_df)))

  
  
  
  
# end if main cat url !> 0
}else{
  print(paste0("No data for timestamp ", q, "/", nrow(time_stamps_df)))
  
} 

  
  
  

} # end loop through timestamps




#---------------------------------------------------------------------------#

#### Part B: Close RSelenuim ####
#close browser
remDr$close()
# stop the selenium server
rD[["server"]]$stop()

