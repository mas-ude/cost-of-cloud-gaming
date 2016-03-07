library(ggplot2)
library(plyr)

### load and analyse metacritic data
# Note: file not provided here
# scrape the data with https://github.com/mas-ude/metacritic_scraper
# and place it in the data folder


df.metacritic <- read.csv("metacritic-20160302.csv", header=TRUE, sep=";", colClasses=c("numeric", "character", "character", "character", "numeric", "character", "character"))
df.metacritic$platform <- as.factor(df.metacritic$platform)
df.metacritic$release <- as.Date(df.metacritic$release, format = "%B %d, %Y")
ggplot(df.metacritic, aes(x=score, color=platform)) + stat_ecdf()
ggplot(df.metacritic, aes(x=score, color=platform)) + stat_density(position = "dodge", fill=NA, lwd=1)


## quick plot for releases per year
#df.metacritic$month = strftime(df.metacritic$release, "%b")
df.metacritic$year = strftime(df.metacritic$release, "%Y")
releases.peryear = count(df.metacritic, vars = c("year", "platform"))
ggplot(releases.peryear, aes(x=year, y=freq, fill=platform)) + geom_bar(stat="identity", position = "stack")
ggsave("releases-per-year.pdf", width=12, height=8)


##############
df.psnow <- read.csv("data/psnow-games.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "logical"))
df.metacritic.ps3 <- subset(df.metacritic, platform == "ps3")
df.merged <- merge(df.psnow, df.metacritic.ps3, by.x = "Title", by.y = "title", all.x = TRUE)

## scores
mean(df.merged$score, na.rm = TRUE)

## age
df.merged$year <- as.numeric(strftime(df.merged$release, "%Y"))
df.merged$age <- 2015 - df.merged$year

hist(df.merged$age)
mean(df.merged$age, na.rm = TRUE)
median(df.merged$age, na.rm = TRUE)




#########################
df.gfnow <- read.csv("gfnow-games.csv", header=TRUE, sep=",", colClasses=c("character", "numeric"))
df.psnow <- read.csv("psnow-games.csv", header=TRUE, sep=";", colClasses=c("character", "numeric", "numeric", "numeric", "numeric", "logical"))
df.steamdata <- read.csv(file="steamdata-20160206.csv", head=TRUE, sep=",", colClasses=c("numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))


df.hltb$title <- tolower(df.hltb$title)
df.metacritic$title <- tolower(df.metacritic$title)
df.gfnow$name <- tolower(df.gfnow$name)
df.psnow$Title <- tolower(df.psnow$Title)
df.steamdata$name <- tolower(df.steamdata$name)

## trim leading/trailing whitespaces to increase matching
df.hltb$title <- trimws(df.hltb$title)
df.metacritic$title <- trimws(df.metacritic$title)
df.gfnow$name <- trimws(df.gfnow$name)
df.psnow$Title <- trimws(df.psnow$Title)
df.steamdata$name <- trimws(df.steamdata$name)

## strip all "-" and ":" from the strings, as this is the most common mismatch
df.hltb$title <- str_replace_all(df.hltb$title, "[:-]", "")
df.metacritic$title <- str_replace_all(df.metacritic$title, "[:-]", "")
df.gfnow$name <- str_replace_all(df.gfnow$name, "[:-]", "")
df.psnow$Title <- str_replace_all(df.psnow$Title, "[:-]", "")
df.steamdata$name <- str_replace_all(df.steamdata$name, "[:-]", "")

## merge double space to one
df.hltb$title <- str_replace_all(df.hltb$title, "  ", " ")
df.metacritic$title <- str_replace_all(df.metacritic$title, "  ", " ")
df.gfnow$name <- str_replace_all(df.gfnow$name, "  ", " ")
df.psnow$Title <- str_replace_all(df.psnow$Title, "  ", " ")
df.steamdata$name <- str_replace_all(df.steamdata$name, "  ", " ")


df.metacritic.pc = subset(df.metacritic, platform == "pc")
df.metacritic.ps = subset(df.metacritic, platform %in% c("ps3", "ps2"))

df.consolidated.gfnow <- merge(df.gfnow, df.metacritic.pc, by.x = "name", by.y = "title", all.x = TRUE)
df.consolidated.psnow <- merge(df.psnow, df.metacritic.ps, by.x = "Title", by.y = "title", all.x = TRUE)
df.consolidated.steam <- merge(df.steamdata, df.metacritic.pc, by.x = "name", by.y = "title", all.x = TRUE)

## generate data frame for multi-plat density plot

df.scores <- data.frame()
df.scores <- rbind(df.scores, data.frame(title = df.consolidated.psnow$Title, platform = "PlayStation Now", score = df.consolidated.psnow$score, user_score = df.consolidated.psnow$user_score * 10))
df.scores <- rbind(df.scores, data.frame(title = df.consolidated.gfnow$name, platform = "Geforce Now", score = df.consolidated.gfnow$score, user_score = df.consolidated.gfnow$user_score * 10))
df.scores <- rbind(df.scores, data.frame(title = df.consolidated.steam$name, platform = "Steam", score = df.consolidated.steam$score, user_score = df.consolidated.steam$user_score * 10))
df.scores <- rbind(df.scores, data.frame(title = df.metacritic$title, platform = "overall", score = df.metacritic$score, user_score = df.metacritic$user_score * 10))


## density plot
p <- ggplot(df.scores, aes(x = score, fill = as.factor(platform), color = as.factor(platform))) + geom_density(alpha = 0.4) #+ scale_x_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("score") + ylab("density")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamelengths-by-platform-density.pdf", width=12, height=8)

## violins
p <- ggplot(df.scores, aes(x = as.factor(platform), y = score)) + geom_violin(adjust = .5, scale = "area", draw_quantiles = c(0.25,0.5,0.75), na.rm = TRUE) #+ scale_y_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("platform") + ylab("score")
p <- p + theme(text = element_text(size=20))
p
ggsave("scores-by-platform-violin.pdf", width=12, height=8)


## violins user score
p <- ggplot(df.scores, aes(x = as.factor(platform), y = user_score)) + geom_violin(adjust = .5, scale = "area", draw_quantiles = c(0.25,0.5,0.75), na.rm = TRUE) #+ scale_y_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("platform") + ylab("score")
p <- p + theme(text = element_text(size=20))
p
ggsave("scores-by-platform-violin-userscore.pdf", width=12, height=8)
