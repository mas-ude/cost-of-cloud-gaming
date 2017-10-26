# Extract parts of the big steamdata_combined dataset.
# The output format is compatible to other scripts that
# expect "steamdata-... .csv" files as input.

library(stringr)
library(stringdist)
library(foreach)
library(tibble)
library(lubridate)
library(dplyr)
library(ggplot2)

steamdata <- read.csv("../data/steamdata_combined.csv",
                      stringsAsFactors = FALSE)

##########################################
## housekeeping

steamdata$price <- as.integer(steamdata$price)
steamdata$metacritic_score <- as.integer(steamdata$metacritic_score)
steamdata$recommendations <- as.integer(steamdata$recommendations)
steamdata$is_free <- as.logical(steamdata$is_free)
steamdata$release_date <- dmy(steamdata$release_date)
steamdata <- as.tibble(steamdata)

## Remove all the stuff we don't need
# (Now that's specific!)
steamdata <- steamdata %>% 
  select(-X, -recommendations, -release_date, -is_free, -metacritic_score, -name.y, -currency) %>%
  rename(name = name.x)

steamdata.latest <- steamdata %>% 
  filter(date == max(steamdata$date))

steamdata.earliest <- steamdata %>% 
  filter(date == min(steamdata$date))

# This is the selection and order of columns we require in steam.R
thecolumnnames <- c("appid", "name", "owners", "owners_variance",
    "players_forever", "players_forever_variance", "players_2weeks",
    "players_2weeks_variance", "average_forever", "average_2weeks",
    "median_forever", "median_2weeks", "price")

write.csv(steamdata.latest[,thecolumnnames],
    file="steamdata-20171009.csv", row.names=FALSE)

write.csv(steamdata.earliest[,thecolumnnames],
          file="steamdata-20170419.csv", row.names=FALSE)
