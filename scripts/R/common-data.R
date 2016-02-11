
#setwd("git/cost-of-cloud-gaming/data/")

################################################################################
##  load all data fales
df.steam <- read.csv(file="steamdata-20160206.csv")
df.hltb <- read.csv("howlongtobeat-20160209.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "character"))
df.metacritic <- read.csv("metacritic-20160209.csv", header=TRUE, sep=";", colClasses=c("numeric", "character", "character", "character", "numeric", "character", "character"))


gfnow.games <- read.csv("gfnow-games.csv", header = TRUE, sep = ",", colClasses = c("character", "numeric"))
psnow.games <- read.csv("psnow-games.csv", header = TRUE, sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "logical"))


################################################################################
## all cost calculated for region 1 EU in Euros; cost converted from other regions (UK/US) with current exchange rates plus taxes
## update to current exchange rates here
# rates of 20160211
exchangerate.GBPtoEUR <- 1.27270
exchangerate.USDtoEUR <- 0.882388
