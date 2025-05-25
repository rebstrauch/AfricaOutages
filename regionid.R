###List of regional identifiers (CAIDA - IODA): List of all regions CAIDA tests 
#22.06.2021

rm(list = ls())
library(tidyverse)
library(stringr)
library(countrycode)
library(readxl)
library(lubridate)
library(chron)
library(haven)


#Load data: The dataset provides information on regional identifiers which are used by CAIDA to locate Internet outages.
regionid <- read.csv(file.path("ioda_regionidx_to_regionname.gz"), stringsAsFactors = F) #on request by CAIDA

regionid <- regionid %>%
  rename(region_code=X0, location_code=X......0, region_name=X.) 

###add country location
regionid$country_code <- substring(regionid$location_code, 4,5)
regionid$country_name <- countrycode(regionid$country_code, origin = 'iso2c', destination = 'country.name')


###Load data: Afrobarometer (7th survey wave)
afro <- read_sav("r7_merged_data_34ctry.release.sav")
afro <- dplyr::select(afro, RESPNO, COUNTRY, REGION, LOCATION.LEVEL.1, DATEINTR, STRTIME)
names(afro) <- c("respno", "country", "region", "sub_region", "day", "time")
head(afro)

#identify time period covered by interviews per country
afro$time <- format(strptime(afro$time,"%H:%M:%S"), '%H:%M:%S')
afro$day <- as.Date(afro$day)


#combine date and hour variables 
afro$date <- as.POSIXct(paste(afro$day, afro$time), format="%Y-%m-%d %H:%M:%S")
head(afro)

###assign value label to Afrobarometer (original file is in SPSS)
afro$country
c <- stack(attr(afro$country, 'labels')) #labels of country names
names(c) <- c("country", "country_name" )
afro <- left_join(afro, c, by="country")
head(afro)

#get overview for interviews per country
dates <- afro %>%
  group_by(country_name) %>%
  summarise(start = min(day), end = max(day))
head(dates)
print(tbl_df(dates), n=34)

#add country-code 
dates$country_code <- countrycode(dates$country_name, origin = 'country.name', destination = 'iso2c')
dates$country_name <- NULL

#only include those country/regions to the region ID which are part of the Afrobarometer
regionid <- regionid %>%
  filter(country_code %in% dates$country_code) %>% #only select countries which are part of the Afrobarometer
  filter(!region_name=="?" & !region_name=="") #drop non-observations

###add frame for respective country: create list of country-regions which I like to request data on
regionid <- regionid %>%
  left_join(dates, by="country_code") #leaves me with 616 regions I check on for Internet outages


regionid$start_large <- as.Date(regionid$start) %m-% months(3)
regionid$end_large <- regionid$start

###modify time frame 
regionid$begin_time <- ("00:00:00")
regionid$end_time <- ("23:59:59")

regionid <- regionid %>%
  mutate(start=(paste(start, begin_time)), 
         end=(paste(end, end_time)), 
         start_large=(paste(start_large, begin_time)),
         end_large=(paste(end_large, end_time))) %>%
  dplyr::select(-c(begin_time, end_time))

###change to date-time variable 
regionid$start <- as.POSIXct(strptime(regionid$start, "%Y-%m-%d %H:%M:%S"))
regionid$end <- as.POSIXct(strptime(regionid$end, "%Y-%m-%d %H:%M:%S"))
regionid$start_large <- as.POSIXct(strptime(regionid$start_large, "%Y-%m-%d %H:%M:%S"))
regionid$end_large <- as.POSIXct(strptime(regionid$end_large, "%Y-%m-%d %H:%M:%S"))

###convert to UTC time stamps 
tz(regionid$start) <- "GMT"
regionid$start <- as.numeric(regionid$start)

tz(regionid$end) <- "GMT"
regionid$end <- as.numeric(regionid$end)

tz(regionid$start_large) <- "GMT"
regionid$start_large <- as.numeric(regionid$start_large)

tz(regionid$end_large) <- "GMT"
regionid$end_large <- as.numeric(regionid$end_large)


###create URL (only short time)
regionid$url = paste("https://ioda.caida.org/ioda/data/events?from=", regionid$start,
                     "&until=", regionid$end, "&human=true&meta=region/", regionid$region_code, sep = "")

###save file
write.csv(regionid,"regionid.csv", row.names = F)

