library(ggplot2)
library(stringr)

# set the working directory
#setwd("/Users/sv/Desktop/cost-of-cloud-gaming/scripts/R/")
#setwd("git/cost-of-cloud-gaming/data/")

# === LOADING DATA ===

df.steamdata <- read.csv(file="steamdata-20171009.csv", head=TRUE, sep=",", colClasses=c("numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

df.metacritic <- read.csv("metacritic.20171008230001.csv", header=TRUE, sep=";", colClasses=c("numeric", "character", "character", "character", "numeric", "character", "character"))

df.hltb <- read.csv("howlongtobeat-20171027.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "character"))

df.gfnow <- read.csv("gfnow-games-DE-shield-2017.csv", header=TRUE, sep=",", colClasses=c("character", "numeric"))

df.psnow <- read.csv("psnow-titles-de-2017.txt", header=TRUE, sep=";", colClasses=c("character"))

## convert date strings to objects and calculate year
df.metacritic$release <- as.Date(df.metacritic$release, format = "%B %d, %Y")
df.metacritic$year = strftime(df.metacritic$release, "%Y")


# === DATA PREPARATION ===
# improve matching of game titles by better aligning the strings

## lowercase all titles
df.hltb$title <- tolower(df.hltb$title)
df.metacritic$title <- tolower(df.metacritic$title)
df.gfnow$name <- tolower(df.gfnow$name)
df.psnow$name <- tolower(df.psnow$name)
df.steamdata$name <- tolower(df.steamdata$name)

## trim leading/trailing whitespaces to increase matching
df.hltb$title <- trimws(df.hltb$title)
df.metacritic$title <- trimws(df.metacritic$title)
df.gfnow$name <- trimws(df.gfnow$name)
df.psnow$name <- trimws(df.psnow$name)
df.steamdata$name <- trimws(df.steamdata$name)

## strip all "-" and ":" from the strings, as this is the most common mismatch
df.hltb$title <- str_replace_all(df.hltb$title, "[:-]", "")
df.metacritic$title <- str_replace_all(df.metacritic$title, "[:-]", "")
df.gfnow$name <- str_replace_all(df.gfnow$name, "[:-]", "")
df.psnow$name <- str_replace_all(df.psnow$name, "[:-]", "")
df.steamdata$name <- str_replace_all(df.steamdata$name, "[:-]", "")

## merge double space to one
df.hltb$title <- str_replace_all(df.hltb$title, "  ", " ")
df.metacritic$title <- str_replace_all(df.metacritic$title, "  ", " ")
df.gfnow$name <- str_replace_all(df.gfnow$name, "  ", " ")
df.psnow$name <- str_replace_all(df.psnow$name, "  ", " ")
df.steamdata$name <- str_replace_all(df.steamdata$name, "  ", " ")


# === SUBSETTING ===
## generate subsets of metacritic/hltb to match against gfnow/psnow/steam

df.hltb.pc = subset(df.hltb, platform == "PC")
df.metacritic.pc = subset(df.metacritic, platform == "pc")

df.hltb.ps = subset(df.hltb, platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3"))
df.metacritic.ps = subset(df.metacritic, platform %in% c("ps3", "ps2"))


# === MERGING DATA SETS ===

df.consolidated.gfnow <- merge(df.gfnow, df.hltb.pc, by.x = "name", by.y = "title", all.x = TRUE)
df.consolidated.gfnow <- merge(df.consolidated.gfnow, df.metacritic.pc, by.x = "name", by.y = "title", all.x = TRUE)

df.consolidated.psnow <- merge(df.psnow, df.hltb.ps, by.x = "name", by.y = "title", all.x = TRUE)
df.consolidated.psnow <- merge(df.consolidated.psnow, df.metacritic.ps, by.x = "name", by.y = "title", all.x = TRUE)

df.consolidated.steam <- merge(df.steamdata, df.hltb.pc, by.x = "name", by.y = "title", all.x = TRUE)
df.consolidated.steam <- merge(df.consolidated.steam, df.metacritic.pc, by.x = "name", by.y = "title", all.x = TRUE)


# === DATAFRAME GENERATION ===
# combine all the invididual dfs into one big df

df.everything <- data.frame(df.consolidated.steam, delivery_platform = "Steam")
tmp <- data.frame(df.consolidated.gfnow, delivery_platform = " Geforce Now", appid = NA, owners = NA, owners_variance = NA, players_forever = NA, players_forever_variance = NA, players_2weeks = NA, players_2weeks_variance = NA, average_forever = NA, average_2weeks = NA, median_forever = NA, median_2weeks = NA)
df.everything <- rbind(df.everything, tmp)
tmp <- data.frame(price = NA, name = df.consolidated.psnow$Title, appid = NA, owners = NA, owners_variance = NA, players_forever = NA, players_forever_variance = NA, players_2weeks = NA, players_2weeks_variance = NA, average_forever = NA, average_2weeks = NA, median_forever = NA, median_2weeks = NA, main_story_length = df.consolidated.psnow$main_story_length, mainextra_length = df.consolidated.psnow$mainextra_length, completionist_length = df.consolidated.psnow$completionist_length, combined_length = df.consolidated.psnow$combined_length, platform.x = df.consolidated.psnow$platform.x, user_score = df.consolidated.psnow$user_score, publisher = df.consolidated.psnow$publisher, genre = df.consolidated.psnow$genre, score = df.consolidated.psnow$score, release = df.consolidated.psnow$release, platform.y = df.consolidated.psnow$platform.y, year = df.consolidated.psnow$year, delivery_platform = "PlayStation Now")
df.everything <- rbind(df.everything, tmp)



# == CUSTOM BINNING ===

# Prices
# (df has prices in cents, we label categories as Euros though)
df.everything$price_category <-
  ifelse(df.everything$price>4000,"above 40",
    ifelse(df.everything$price>2000,"20 to 40",
      ifelse(df.everything$price>1000,"10 to 20",
        ifelse(df.everything$price>500,"5 to 10",
          ifelse(df.everything$price>0,"<5","free")))))



# Scores
df.tmp <- data.frame()
for (i in 1:nrow(df.everything)) {
  row <- df.everything[i,]
  # First check if price has value or is NA
  if(!is.na(row$score)){
    if (row$score >= 0 & row$score <= 10){
      row$score_category <- "0-10"
    } else if (row$score > 10 & row$score <= 20){
      row$score_category <- "11-20"
    } else if (row$score > 20 & row$score <= 30){
      row$score_category <- "21-30"
    } else if (row$score > 30 & row$score <= 40){
      row$score_category <- "31-40"
    } else if (row$score > 40 & row$score <= 50){
      row$score_category <- "41-50"
    } else if (row$score > 50 & row$score <= 60){
      row$score_category <- "51-60"
    } else if (row$score > 60 & row$score <= 70){
      row$score_category <- "61-70"
    } else if (row$score > 70 & row$score <= 80){
      row$score_category <- "71-80"
    } else if (row$score > 80 & row$score <= 90){
      row$score_category <- "81-90"
    } else if (row$score > 90 & row$score <= 100){
      row$score_category <- "91-100"
    } 
  } else {
    row$score_category <- NA
  }
  df.tmp <- rbind(df.tmp, data.frame(row))
}
df.everything = df.tmp


# === BASIC STATISTICS ===

# Correlation coefficient between owners and Metacritic score
df.cons.tmp <- df.consolidated.steam[!(is.na(df.consolidated.steam$score)),]
cor(df.cons.tmp$score, df.cons.tmp$owners)
# ~0.1998204

# Correlation coefficient between owners and Metacritic user score
df.cons.tmp <- df.consolidated.steam[!(is.na(df.consolidated.steam$user_score)),]
cor(df.cons.tmp$user_score, df.cons.tmp$owners)
# ~ 0.1010024

# Correlation coefficient between owners and game length
df.cons.tmp <- df.consolidated.steam[!(is.na(df.consolidated.steam$combined_length)),]
cor(df.cons.tmp$combined_length, df.cons.tmp$owners)
# ~0.175819

# Correlation coefficient between game length and Metacritic score
df.cons.tmp <- df.consolidated.steam[!(is.na(df.consolidated.steam$combined_length)),]
df.cons.tmp <- df.cons.tmp[!(is.na(df.cons.tmp$score)),]
cor(df.cons.tmp$score, df.cons.tmp$combined_length)
# ~0.1952609

# Correlation coefficient between Metacritic score and user score
df.cons.tmp <- df.consolidated.steam[!(is.na(df.consolidated.steam$user_score)),]
df.cons.tmp <- df.cons.tmp[!(is.na(df.cons.tmp$score)),]
cor(df.cons.tmp$score, df.cons.tmp$user_score)
# ~ 0.6220127

# Correlation coefficient between price and owners
df.cons.tmp <- df.consolidated.steam[!(is.na(df.consolidated.steam$price)),]
df.cons.tmp <- df.cons.tmp[!(is.na(df.cons.tmp$owners)),]
cor(df.cons.tmp$owners, df.cons.tmp$price)
# ~ -0.02780982


# Correlation coefficient between age and owners
# df.consolidated$year = as.numeric(df.consolidated$year)
# df.cons.tmp <- df.consolidated[!(is.na(df.consolidated$year)),]
# df.cons.tmp <- df.cons.tmp[!(is.na(df.cons.tmp$owners)),]
# cor(df.cons.tmp$owners, df.cons.tmp$year)
# ~

# === PLOTS ===

ggplot(df.consolidated, aes(x=score, y=owners, size=combined_length, color=price)) + geom_point() + scale_x_log10() + scale_y_log10() + geom_smooth(method='lm',formula=y~x)
ggsave("rel-score-owners.pdf", width=12, height=8)

ggplot(df.consolidated.steam, aes(x=combined_length, y=owners)) + geom_point() + scale_x_log10(breaks=c(1,10,100)) + scale_y_log10() + geom_smooth(method='lm',formula=y~x) + xlab("combined game length (h)") + ylab("number of owners")
ggsave("rel-combinedlength-owners.pdf", width=12, height=8)

ggplot(df.consolidated, aes(x=price, y=owners, color=price)) + geom_point() + scale_x_log10() + scale_y_log10() + geom_smooth(method='lm',formula=y~x)
ggsave("rel-price-owners.pdf", width=12, height=8)

ggplot(data = subset(df.consolidated, !is.na(score_category)), aes(score_category)) + stat_summary_bin(aes(y = owners), fun.y = "mean", geom = "bar")
ggsave("rel-score-category-owners.pdf", width=12, height=8)
ggplot(data = subset(df.consolidated, !is.na(price_category)), aes(price_category)) + stat_summary_bin(aes(y = owners), fun.y = "mean", geom = "bar")
ggsave("rel-price-category-owners.pdf", width=12, height=8)

#df.consolidated$year = as.numeric(df.consolidated$year)
#ggplot(data = subset(df.consolidated, !is.na(year)), aes(year)) + stat_summary_bin(aes(y = owners), fun.y = "mean", geom = "bar")
#ggsave("rel-year-owners.pdf", width=12, height=8)

#releases.peryear = count(df.consolidated, vars = c("year", "score_category"))
#ggplot(releases.peryear, aes(x=year, y=freq, fill=score_category)) + geom_bar(stat="identity", position = "stack")
#ggsave("releases-per-year.pdf", width=12, height=8)

#ggplot(df.consolidated, aes(x=score, y=user_score, size=combined_length, color=price)) + geom_point() + geom_smooth(method='lm',formula=y~x)