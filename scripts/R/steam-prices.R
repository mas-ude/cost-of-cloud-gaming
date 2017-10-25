# First evaluations of Steam prices, owners, review scores,
# and recommendations in the 2017-04 to 2017-10 timeframe.

library(stringr)
library(stringdist)
library(foreach)
library(tibble)
library(lubridate)
library(dplyr)
library(ggplot2)

# Shorthands for column class declarations
# XXX Unused ATM, the cells in the file are all quoted (also the numeric ones)
CHR = "character"
INT = "integer"
NUM = "numeric"
BOOL = "logical"
NIL = "NULL"
steamdata <- read.csv("../data/steamdata_combined.csv.gz",
    stringsAsFactors = FALSE)
# XXX These would be the required colClasses
#, colClasses = c(
#        NIL, INT, CHR, INT, NIL,
#        BOOL, INT, INT, CHR, NIL,
#        INT, NUM, INT, NUM, INT,
#        NUM, INT, NUM, INT, NUM,
#        CHR))
steamdata$date <- ymd(steamdata$date)


##########################################
## housekeeping
# XXX Remove once the source's colClasses are fixed

steamdata$price <- as.integer(steamdata$price)
steamdata$metacritic_score <- as.integer(steamdata$metacritic_score)
steamdata$recommendations <- as.integer(steamdata$recommendations)
steamdata$is_free <- as.logical(steamdata$is_free)
steamdata$release_date <- dmy(steamdata$release_date)
steamdata <- as.tibble(steamdata)

## keep one name column
## currency column seems to contain genres, but only sometimes
## and very inconsistent, drop it

steamdata <- steamdata %>% 
  select(-name.y, -currency) %>%
  rename(name = name.x)





#########################################
## plotting

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
steamdata.latest <- steamdata %>% 
  filter(date == latest_date)

# A quick stat
quantile(steamdata.latest$price/100, probs=seq(0,1,0.01), na.rm=TRUE)
# Some modes:
#         10% of games are free
# (18-13)= 5% of games cost  0.99
# (30-26)= 4% of games cost  2.99
# (48-36)=12% of games cost  4.99
# (76-63)=13% of games cost  9.99
# (87-82)= 5% of games cost 14.99
# (93-89)= 4% of games cost 19.99
# The top  6% of games cost more than that

#########################################
## correlogramm of steamdata.latest

library(ggcorrplot)

# remove non-numeric variables for cor
corrdata <- steamdata.latest %>%
  select(-name, -date) %>%
  #select(-release_date) %>%
  mutate(is_free = as.integer(is_free)) %>%
  mutate(release_date = as.integer(format(release_date, "%Y%m%d")))

corr <- round(cor(corrdata, use = "complete.obs"), 1)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of steamdata.latest", 
           ggtheme=theme_bw)
ggsave("../plots/correlogram-steamdata-latest.pdf")


#########################################
## timeseries #1
## owners of (larger) games on observed date

# attempt to filter out all games that
# have at least N owners on any of the observed days

larger.games <- steamdata %>%
  group_by(appid) %>%
  filter(any(owners >= 100000)) %>%
  ungroup

ggplot(larger.games, aes(x = date, y = owners, group = name)) + geom_line(alpha = 0.1) + scale_y_log10()
ggsave("../plots/timeseries-owners-100k.pdf")


## plot as timeseries based on x = date - release_date
## this should shift all starts to the zero mark


#########################################
## bivariate plots

latest.filtered <- steamdata.latest %>%
  filter(price <= 15000) %>% # price out of the usual game price range
  filter(metacritic_score > 0) # no or invalid metacritic score present
  
## let's try price vs recommendations vs total average playtime
ggplot(latest.filtered, aes(x = price/100, y = metacritic_score)) + geom_point(aes(size = average_forever))
ggsave("../plots/price-vs-metacritic-vs-playtime.pdf")

ggplot(latest.filtered, aes(x =recommendations, y = metacritic_score)) + geom_point(aes(size = average_forever)) + scale_x_log10()
ggsave("../plots/recommendations-vs-metacritic-vs-playtime.pdf")

ggplot(latest.filtered, aes(x =players_2weeks, y = recommendations)) + geom_point() + geom_smooth() + scale_x_log10()
ggsave("../plots/players2weeks-vs-recommendations.pdf")
