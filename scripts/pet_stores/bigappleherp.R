#-#########################################################-#
#-### web scraper for http://www.bigappleherp.com/ ########-#
#-#########################################################-#


library(rvest)
library(stringr)

#### Step 1: get URLs to main categories of animals for sale ####
site = read_html("http://www.bigappleherp.com/ANIMALS-LIVE-REPTILES")
links = html_nodes(site, css = "a.categorylistcell-link") %>%
  html_attr("href") %>%
  unique()


links = paste0("http://www.bigappleherp.com", links)


#### Step 2: figure out with main category has subcategories ####

#inits
subcats = list()
d=1
r = vector()

for(i in 1:length(links)){
  
  s = html_session(links[i])
  
  #check to see if main cat has sub cat
  if(s %>% html_nodes(css = "a.categorylistcell-link") %>% html_attr("href") %>% toString() != "") 
  {
    #if yes, record URLs for subcats
    subcats[[d]] = s %>% 
      html_nodes(css = "a.categorylistcell-link") %>% 
      html_attr("href") %>% 
      unique() 
    
    #index which cats have sub cats, so that i can later remove from list
    r[d] = i
    
    #count dummy variable
    d = d+1
  }
}


#### Step 3: remove main cats that have sub cats and add sub cats to links ####

#remove main cats that have sub cats, stored in var r
links = links[-r]

## add sub cats

#first unlist subcats
subcats = unlist(subcats)

#add beginning of url
subcats = paste0("http://www.bigappleherp.com" , subcats)

#join sub cats with main cats URLs
links = c(links, subcats)


#### Step 4: get URL for individual listings ####
#inits
d = 1
listings_links = list()

#loop through each main categories to get URLs for all listings
for(i in 1:length(links)){
  
  listings_links[[d]] =  read_html(links[i]) %>%
    html_nodes(css = "a.itemlistcell-link") %>%
    html_attr("href") %>%
    unique()
  
  d = d+1
}

#get URLs for all listings in a vector called sub_links
listings_links = unlist(listings_links)
listings_links = paste0("http://www.bigappleherp.com", listings_links)

#should get unique ones
listings_links = unique(listings_links)

#### Step 5: Extract data from each listing ####

#inits
commonName1 = vector()
latinName1 = vector()
size1 = vector()
price1 = vector()
description1 = vector()
picture1 = list()
s = list()

#loop through each listing
for(j in 1:length(listings_links)){
  
  #set session for each listing
  s[[j]] = html_session(listings_links[j])
  
  #get details
  commonName1[j] = s[[j]] %>%
    html_nodes(css = "h2") %>%
    html_text() %>%
    toString()
  
  # latinName1[j] = s[[j]] %>%
  #   html_nodes(css = "p.product-species") %>%
  #   html_text() %>%
  #   toString()
  
  # size1[j] = s[[j]] %>%
  #   html_nodes(css = ".product-size") %>%
  #   html_text() %>%
  #   toString()
  
  price1[j] = s[[j]] %>%
    html_nodes(css = "div.itemdetail-salespricehtml") %>%
    html_text() %>%
    toString()
  
  description1[j] = paste( s[[j]] %>%
    html_nodes(css = "div.tab-pane.active p:nth-of-type(1)") %>%
    html_text() %>%
    toString() ,
    s[[j]] %>%
      html_nodes(css = "div.tab-pane.active ul:nth-of-type(1)") %>%
      html_text() %>%
      toString()
    )
  
}

#cbind into db
finaldf = cbind.data.frame(commonName1, price1, description1, 
                                      listings_links, stringsAsFactors=FALSE)


#### save ####
saveRDS(finaldf, paste0("data/bigappleherp_", Sys.Date(), ".rds"))