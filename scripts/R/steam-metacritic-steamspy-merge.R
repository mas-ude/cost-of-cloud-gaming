# Merge Steam API, Metacritic, and SteamSpy data into one big .csv.gz.
# Ask https://github.com/mas-ude/game-data-collection/tree/master/gamedata
# and https://github.com/lifezoned4 for the input data.

library(stringr)
library(stringdist)
library(foreach)
library(tibble)
library(lubridate)
library(dplyr)
library(ggplot2)


# set the timezone to something that produces no warnings (and is correct)
Sys.setenv(TZ="Europe/Berlin")
options(tz="Europe/Berlin")

setwd("~/git/cost-of-cloud-gaming/scripts/")
datafolder <- "../data/gamedata"

getData <- function() {
  steam.files <- list.files(recursive = TRUE, datafolder, pattern = "steamdata.\\d*.csv")
  steam.files <- steam.files[order(steam.files,decreasing = TRUE)]
  
  df <- data.frame()
  for(f in steam.files){
    d <- read.csv(paste(datafolder, "/" , f, sep = ""),stringsAsFactors = FALSE, sep = ";")
    
    # don't forget to add the steamspy data
    fold <- paste(datafolder, "/", str_extract(f, "(201[7-9][0-1][0-9][0-3][0-9]-[0-9]{6}-GameCollection/)"), sep = "")
    spyfile <- list.files(fold, pattern = "steamSpyData.\\d*.csv")
    spy <- read.csv(paste(fold, spyfile, sep = ""), stringsAsFactors = FALSE)
    
    merged <- left_join(d, spy, by = "appid")
    
    
    ## add the recording date to the current set
    datestring <- str_extract(f, "(201[7-9][0-1][0-9][0-3][0-9])")
    print(datestring)
    merged$date <- ymd(datestring)
    df <- rbind(df, merged)
  }
  return(df)
}


##########################################
## Write out the data
# XXX In its current form, all cells are quoted!
# XXX Read in the source data with proper colClasses to fix this.

steamdata <- getData()
gz <- gzfile("../data/steamdata_combined.csv.gz", "w")
write.csv(steamdata, gz)
close(gz)
