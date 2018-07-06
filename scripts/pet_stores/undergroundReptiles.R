#-########################################################-#
#-### web scraper for https://undergroundreptiles.com/ ###-#
#-########################################################-#


library(rvest)
library(stringr)
library(RSelenium)

#### start up RSelenium for later

# ## first kill open ports
# if(Sys.info()[1] == "Windows") { system("Taskkill /IM java.exe /F") 
# } else if(Sys.info()[1] == "Linux") { system("killall java") }

selenium <- rsDriver(version='3.0.1', browser='chrome', port = 4451L)
server <- selenium[['server']]
remDr <- selenium[['client']]

#### Get URLs for all listings ####
site = read_html("http://undergroundreptiles.com/product-category/animals/")
links = html_nodes(site, css = "div.shop-sidebar li.cat-item.cat-item-179 > ul.children > li.cat-item > a") %>%
  html_attr("href") %>%
  unique()

#unique listings
links = unique(links)

## sub links

# inits
sl = list()
for(x in 1:length(links)){
  
  site_temp = read_html(links[x])
  sl[[x]] = html_nodes(site_temp, css = "div.shop-sidebar ul.children ul.children a") %>%
    html_attr("href") %>%
    unique()
  rm(site_temp)
  print(x)
}

links = unique(unlist(sl))

#### Extract data from each listing ####

#inits
commonName1 = vector()
latinName1 = vector()
#size1 = vector()
price1 = vector()
description1 = vector()
stock1 = vector()
sp_link = vector()
# picture1 = list()
# s = list()
# inits 
dd = 1

#loop through each listing
for(j in 1:length(links)){
  
  
  ### get links to indiv listings within each subgroup using RSelenium bc now website uses JS to load
  
  # first nav to page
  remDr$navigate(links[j])
  
  # scroll down
  webElem <- remDr$findElement("css", "body")
  # webElem$sendKeysToElement(list(key = "end"))
  
  # this might not be working the way I want it to. 
  for(r in 1: 200) webElem$sendKeysToElement(list(key = "down_arrow"))
  
  ## need to wait for rest of listings to load
  # check if span loading exist
  
  # inits
  proceed = FALSE
  d = 1
  
  while( proceed == FALSE ){
    
    # find elem that says "No more items available."
    elem_done_loading  = remDr$findElements(using="css selector", value="a.disabled")
    temp = unlist(lapply(elem_done_loading, function(x){x$getElementText()}))
    
    # if done loading
    if( is.character(temp) ) {
        if( temp == "No more items available."){ 
          
          proceed = TRUE 
        }
      
    # else if still loading
    } else{
      # scroll down
      for(r in 1: 100) webElem$sendKeysToElement(list(key = "down_arrow"))
      # wait 5 seconds for it to load
      Sys.sleep(2)
      d = d+1
    }
    if(d > 30) break # break if loads for too long
  
  } # end while loop
  
  rm(elem_done_loading, temp, d)
  
  
  # get individual listings within that link
  elem <- remDr$findElements(using="css selector", value="a.shop_product_title")
  new_links = unlist(lapply(elem, function(x){x$getElementAttribute("href")}))
  
  # use rvest to get info
  for(k in 1:length(new_links)){
  
    # set session for each listing
    s = html_session(new_links[k])
    
    ## save info
    
    commonName1[dd] = s %>%
      html_nodes(css = "h1.product_title") %>%
      html_text() %>%
      toString()
    
    latinName1[dd] = s %>%
      html_node(css = "em strong") %>%
      html_text() %>%
      toString()
    
    # size1[j] = s[[j]] %>%
    #   html_nodes(css = ".product-size") %>%
    #   html_text() %>%
    #   toString()
    # 
    price1[dd] = s %>%
      html_nodes(css = "div.product_price") %>%
      html_text() %>%
      toString()
    
    description1[dd] = s %>%
      html_nodes(css = "div.product_infos") %>%
      html_text() %>%
      toString()
    
    stock1[dd] = s %>%
      html_nodes(css = "div.product_excerpt div.out_of_stock") %>%
      html_text() %>%
      toString()
    
    sp_link[dd] = new_links[k]
    
    # update index
    dd = dd +1
    
    # remove temp vars
    rm(s)
  }
  
  # remove temp vars
  rm(webElem, elem, new_links)
  
}




#cbind into db
finaldf = cbind.data.frame(commonName1, latinName1, price1, description1, 
                                     stock1, sp_link, stringsAsFactors=FALSE)



#### save ####
saveRDS(finaldf, paste0("data/underground_reptiles_", Sys.Date(), ".rds") )


