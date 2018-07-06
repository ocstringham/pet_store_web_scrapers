
get_text_RSelenium = function(remDr, css){
  y = vector()
  y = remDr$findElements(using="css selector", value = css)
  y =unlist(lapply(y, function(x){x$getElementText()}))
  
  if(is.null(y)){y = NA}
  
  return(y)
}

