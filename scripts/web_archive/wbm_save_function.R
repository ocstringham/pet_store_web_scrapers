#######################################################
#- Script to save data from all timestamps from WBM scrape -#
#######################################################

wbm_save = function(dir_and_save_name, url_timestamp, finaldf){
  
  # set wd for storage
  setwd( paste0("data/web_archive/", dir_and_save_name, "store_data") )
  
  
  ## create dir for current timestamp IF doesn't already exist
  
  # get name of current timestamp
  current_time_stamp = time_stamps_df$timestamp[q] %>%
                          as.Date(format =  "%Y%m%d") %>%
                          as.character() 
  
  # create folder if not yet created
  if(current_time_stamp %>% file.exists() != TRUE)
  {
    current_time_stamp %>%
      dir.create()
  }
  
  #navigate to new folder
  current_time_stamp %>%
    setwd()
  
  
  #save txt of metadata for data and time
  metadata = paste("Website: ", url_timestamp ,"\n",
                   "WBM date: ", current_time_stamp, "\n",
                   "Scraped date/time: ", Sys.time(), " ", Sys.timezone() )
  
  #save metadata
  write(metadata, file = paste0(dir_and_save_name, "_metadata_", current_time_stamp, ".txt"))
  
  #save scraped data
  saveRDS(finaldf, file = paste0(dir_and_save_name, "_", current_time_stamp, ".rds"))
  
  #save as csv too
  write.csv(finaldf, file = paste0(dir_and_save_name, "_", current_time_stamp, ".csv"))
  
}