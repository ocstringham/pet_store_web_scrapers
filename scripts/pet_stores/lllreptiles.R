#-##################################################-#
#-### web scraper for http://www.lllreptile.com/ ###-#
#-##################################################-#


library(rvest)
library(stringr)

## overall method: get URLs for each listing ##

#### Step 1: get URLs to main categories of animals for sale ####
site = read_html("http://www.lllreptile.com/catalog/101-animals")
links = html_nodes(site, css = ".card-deck-wrapper+ .card-deck-wrapper a , .card:nth-child(1) a") %>%
          html_attr("href") %>%
          unique()


links = paste0("http://www.lllreptile.com", links)

#### Step 2: get URLs to every pet listing ####

#inits
d = 1
sub_links = list()

#loop through each main categories to get URLs for all listings
for(i in 1:length(links)){
  
  sub_links[[d]] =  read_html(links[i]) %>%
                html_nodes(css = ".product-wrap~ .product-wrap+ .product-wrap a , .product-wrap:nth-child(1) a") %>%
                html_attr("href") %>%
                unique()
              
  d = d+1
}

#get URLs for all listings in a vector called sub_links
sub_links = unlist(sub_links)
sub_links = paste0("http://www.lllreptile.com", sub_links)

#unique listings
sub_links = unique(sub_links)


#### Step 3: Extract data from each listing ####

#inits
commonName1 = vector()
latinName1 = vector()
size1 = vector()
price1 = vector()
description1 = vector()
picture1 = list()
s = list()

#loop through each listing
for(j in 1:length(sub_links)){
  
  #set session for each listing
  s[[j]] = html_session(sub_links[j])
  
  #get details
  commonName1[j] = s[[j]] %>%
            html_nodes(css = "h1") %>%
            html_text() %>%
            toString()
  
  latinName1[j] = s[[j]] %>%
                html_nodes(css = "p.product-species") %>%
                html_text() %>%
                toString()
  
  size1[j] = s[[j]] %>%
            html_nodes(css = ".product-size") %>%
            html_text() %>%
            toString()
  
  price1[j] = s[[j]] %>%
            html_nodes(css = ".form-check-label") %>%
            html_text() %>%
            toString()
  
  description1[j] = s[[j]] %>%
                  html_nodes(css = "div.markdown p") %>%
                  html_text() %>%
                  toString()

}

#cbind into db
finaldf = cbind.data.frame(commonName1, latinName1, size1, price1, 
                             description1, sub_links, stringsAsFactors=FALSE)


#### save ####
saveRDS(finaldf, paste0("data/lll_reptiles_", Sys.Date(), ".rds"))
