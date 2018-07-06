#-#########################################################-#
#-### web scraper for http://www.backwaterreptiles.com/ ###-#
#-#########################################################-#


library(rvest)
library(stringr)


#### Step 1: get URLs to main categories of animals for sale ####
site = read_html("http://www.backwaterreptiles.com/")
links = html_nodes(site, css = "div#stacks_out_109460_page0.stacks_out span a") %>%
  html_attr("href") %>%
  unique()


links = paste0("http://www.backwaterreptiles.com/", links)


#### Step 2: figure out with main category has subcategories ####

#inits
subcats = list()
d=1
r = vector()

for(i in 1:length(links)){
  
  s = html_session(links[i])
  
  #check to see if main cat has sub cat
  if(s %>% html_nodes(css = "div.stacks_out.stacks_s_hidden div.stacks_in.com_yourhead_stacks_two_columns_stack span a, p span a") %>% 
     html_attr("href") %>% toString() != "") 
  {
    #if yes, record URLs for subcats
    subcats[[d]] = s %>% 
      html_nodes(css = "div.stacks_out.stacks_s_hidden div.stacks_in.com_yourhead_stacks_two_columns_stack span a, p span a") %>% 
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

#remove .. at beginning of url
subcats = str_replace(subcats, "\\.\\.", "")

#add beginning of url
subcats = paste0("http://www.backwaterreptiles.com" , subcats)

#join sub cats with main cats URLs
links = c(links, subcats)



#### Step 4: get URL for individual listings ####
#inits
d = 1
listings_links = list()

#loop through each main categories to get URLs for all listings
for(i in 1:length(links)){
  
  listings_links[[d]] =  read_html(links[i]) %>%
    html_nodes(css = "div.stacks_div a") %>%
    html_attr("href") %>%
    unique()
  
  d = d+1
}

#get URLs for all listings in a vector called sub_links
listings_links = unlist(listings_links)
listings_links = paste0("http://www.backwaterreptiles.com/", listings_links)

#unique listings
listings_links = unique(listings_links)



#### Step 5: Extract data from each listing ####

#inits
commonName1 = vector()
latinName1 = vector()
size1 = vector()
price1 = vector()
description1 = vector()
stock1 = vector()
picture1 = list()
s = list()

# Start the clock!
ptm <- proc.time()

#loop through each listing
for(j in 1:length(listings_links)){
  
  #set session for each listing
  s[[j]] = html_session(listings_links[j])
  
  #get details
  commonName1[j] = s[[j]] %>%
    html_node(css = ".text_stack") %>%
    html_text() %>%
    toString()
   
  
  latinName1[j] = s[[j]] %>%
    html_nodes(css = "div.stacks_div div.stacks_div em") %>%
    html_text() %>%
    toString()
  
  # size1[j] = s[[j]] %>%
  #   html_nodes(css = ".product-size") %>%
  #   html_text() %>%
  #   toString()
  
  price1[j] = s[[j]] %>%
    html_nodes(css = "form.cartloom-addtocart > select") %>%
    html_text() %>%
    toString()
  
  description1[j] = s[[j]] %>%
    html_nodes(css = "div.stacks_div div.stacks_div p") %>%
    html_text() %>%
    toString()
  
  
  stock1[j] = s[[j]] %>%
    html_nodes(css = "img") %>%
    toString() %>%
    str_detect("http://www.backwaterreptiles.com/images/pet-reptile.jpg")
    
  if(stock1[j] == TRUE){stock1[j] = "Out of stock"}else{stock1[j] = ""}
  
  
  # #pictures
  # picture1[[j]] = s[[j]] %>%
  #   html_nodes(css = "div.stacks_div.stacks_left img") %>%
  #   html_attr("src") 
  
}

# Stop the clock
proc.time() - ptm

#cbind into db
finaldf = cbind.data.frame(commonName1, latinName1, price1, description1, 
                                           stock1, listings_links, stringsAsFactors=FALSE)


#### save ####
saveRDS(finaldf, paste0("data/backwater_reptiles_", Sys.Date(), ".rds"))








