#### close popup for wbm underground reptiles

wbm_underground_reptiles_close_popup = function(){
  
  Sys.sleep(5)
  
  pop_up_exists =
    remDr$findElements(using="css selector", value = "div.close-popup") %>% 
    length()
  
  if(pop_up_exists > 0){
    pu = remDr$findElement(using="css selector", value = "div.close-popup")
    pu$clickElement()
  }
  
  
}