library(ggplot2)





################################################################################
#### Geforce Now
# German Perspective
gfnow.monthly <- 9.99
gfnow.hw <- 201.99   # shield tv box, 16GB (the smallest one)

# number of available games
nrow(gfnow.games)

# number of games included in subscription
nrow(subset(gfnow.games, price == 0))

# calculate worth in steam prices
gfnow.steamprices <- merge(gfnow.games, df.steam, by ="name", all.x = TRUE)
sum(gfnow.steamprices$price.y, na.rm = TRUE) 
mean(gfnow.steamprices$price.y, na.rm = TRUE)

# calculate average metacritic score of gf now
df.metacritic.pc <- subset(df.metacritic, platform == "pc")
gfnow.metacritic  <- merge(gfnow.games, df.metacritic.pc, by.x ="name", by.y = "title", all.x = TRUE)
mean(gfnow.metacritic$score, na.rm = TRUE)

# calculate game lengths
df.hltb.pc <- subset(df.hltb, platform == "PC")
gfnow.hltb  <- merge(gfnow.games, df.hltb.pc, by.x ="name", by.y = "title", all.x = TRUE)
sum(gfnow.hltb$combined_length, na.rm = TRUE)
mean(gfnow.hltb$combined_length, na.rm = TRUE)





################################################################################
#### ps now streaming
# US, CA, UK only; using US as data source, as info on UK is insufficient:
# https://en.wikipedia.org/wiki/List_of_PlayStation_Now_games
# http://www.ign.com/wikis/playstation-4/List_of_PlayStation_Now_Games

psnow.monthly <- 12.99 * exchangerate.GBPtoEUR

## specific hardware requirements
# either a ps4, ps3, or specific Sony TV models (plus controller)
# going with the ps4 option, as its the most common and includes a controller
psnow.hw <- 323.9 # as of 2016/02/11

## number of available games
nrow(psnow.games)

## number of included in subscription
length(psnow.games$Included.In.Subscription[psnow.games$Included.In.Subscription == TRUE])
  
## cost of renting additional games
psnow.rental.price.48h <- 2.99 * exchangerate.GBPtoEUR
psnow.rental.price.30d <- 7.99 * exchangerate.GBPtoEUR
psnow.rental.numgames <- nrow(psnow.games) - length(psnow.games$Included.In.Subscription[psnow.games$Included.In.Subscription == TRUE])
  





################################################################################
## gamefly
# CAPEX: firetv + controller: 150 (over 10 years): 150/10
# OPEX: 6 packets, 7 games each, 7$ per packet



