###Data generating process from CAIDA's IODA API
###Rebecca Strauch
###24.06.2021

rm(list = ls())

library(jsonlite)
library(tidyverse)
library(haven)

###1. Load data from CAIDA's IODA API (only sample requested for time frame of Afrobarometer)
###read URls 
url <- read.csv("regionid.csv")
url <- url %>%
  dplyr::select(url)



#download URLs to folder (run 2x times to catch all of the requested data)
for (url in url) {
  if (!file.exists(".json")) {
    test <- download.file(url, destfile = paste0(basename(url), ".json"))
  }
}


#Load all JSON files to R
temp = list.files(pattern="*.json")

for (files in temp){ 
  file <- fromJSON(files, flatten=TRUE)
}

#create list of JSON files
ioda <- purrr::map_df(temp, function(x) { 
  purrr::map(jsonlite::fromJSON(x), function(y) ifelse(is.null(y), NA, y)) 
})

#drop empty lists
ioda <- ioda$data[ sapply(ioda$data, length) >0]

#unnest lists to dataframes 
ioda <- ioda %>% 
  bind_rows(ioda) %>%    # make larger sample data
  mutate_if(is.list, simplify_all) %>%    # flatten each list element internally 
  unnest(cols = c()) %>%
  distinct()

write.csv(ioda,"ioda_data.csv", row.names = F)






















