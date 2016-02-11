
#setwd("git/cost-of-cloud-gaming/data/")

df.steam <- read.csv(file="steamdata-20160206.csv")
df.hltb <- read.csv("howlongtobeat-20160209.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "character"))
df.metacritic <- read.csv("metacritic-20160209.csv", header=TRUE, sep=";", colClasses=c("numeric", "character", "character", "character", "numeric", "character", "character"))


gfnow.games <- read.csv("gfnow-games.csv", header = TRUE, sep = ",", colClasses = c("character", "numeric"))
psnow.games <- read.csv("psnow-games.csv", header = TRUE, sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "logical"))
