##########################################
#- get all snapshot instances into a df -#
##########################################

# internet required bc calling API

wbm_get_snapshots = function(url)
{
  library(jsonlite)
  library(magrittr)
  
  #### Step 1, get time stamps from WBM CDX API
  call_url = paste0("http://web.archive.org/cdx/search/cdx?url=", url, "&output=json")
  
  snapshots = fromJSON(call_url) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  #### Step 2, rename cols
  colnames(snapshots) = as.character(unlist(snapshots[1,])) # the first row will be the header
  snapshots = snapshots[-1, ]          # removing the first row.
  
  return(snapshots)
}
