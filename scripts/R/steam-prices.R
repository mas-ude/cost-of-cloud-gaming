library(stringr)
library(stringdist)
library(foreach)
library(tibble)
library(lubridate)
library(dplyr)


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
## store and load

steamdata <- getData()
gz <- gzfile("../data/steamdata_combined.csv.gz", "w")
write.csv(steamdata, gz)
close(gz)

steamdata <- read.csv("../data/steamdata_combined.csv.gz", stringsAsFactors = FALSE)
# TODO: restore columen data types
steamdata$date <- ymd(steamdata$date)


##########################################
## housekeeping

library(dplyr)

steamdata$price <- as.integer(steamdata$price)
steamdata$metacritic_score <- as.integer(steamdata$metacritic_score)
steamdata$recommendations <- as.integer(steamdata$recommendations)
steamdata$release_date <- dmy(steamdata$release_date)
steamdata <- as.tibble(steamdata)

## keep one name column
## currency column seems to contain genres, but only sometimes and very inconsistent, drop it

steamdata <- steamdata %>% 
  select(-name.y, -currency) %>%
  rename(name = name.x)





#########################################
## plotting
library(ggplot2)

meanprices <- steamdata %>% 
  group_by(date) %>% 
  summarise(mean = mean(price/100), sd = sd(price/100))

ggplot(meanprices, aes(x = date, y = mean)) + geom_point() + scale_x_date() + scale_y_log10()
ggsave("../plots/mean-steam-prices.pdf")


#########################################
## day-to-day owner difference 

newowners <- steamdata %>%
  group_by(appid) %>%
  select(appid, name, date, owners) %>%
  arrange(date) %>%
  mutate(new_owners = owners - lag(owners)) %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(total_new_owners = sum(new_owners, na.rm = TRUE))

ggplot(newowners, aes(x = date, y = total_new_owners)) + geom_line()
ggsave("../plots/total-game-owners.pdf")

#########################################
## select only the latest set of data

latest_date <- max(steamdata$date)
steamdata_latest <- steamdata %>% 
  filter(date == latest_date)