library(ggplot2)
library(plyr)

### load data with data_merge.R first! ###


df.metacritic$platform <- as.factor(df.metacritic$platform)
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




##### age calculation

# from https://stackoverflow.com/questions/3611314/calculating-ages-in-r
age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

# gfnow
mean(age(df.consolidated.gfnow$release, Sys.Date()), na.rm = TRUE) # 2.38
var(age(df.consolidated.gfnow$release, Sys.Date()), na.rm = TRUE) # 3.80
sd(age(df.consolidated.gfnow$release, Sys.Date()), na.rm = TRUE) # 1.95

# psnow
mean(age(df.consolidated.psnow$release, Sys.Date()), na.rm = TRUE) # 4.27
var(age(df.consolidated.psnow$release, Sys.Date()), na.rm = TRUE) # 5.00
sd(age(df.consolidated.psnow$release, Sys.Date()), na.rm = TRUE) # 2.24

# steam
mean(age(df.consolidated.steam$release, Sys.Date()), na.rm = TRUE) # 2.86
var(age(df.consolidated.steam$release, Sys.Date()), na.rm = TRUE) # 15.71
sd(age(df.consolidated.steam$release, Sys.Date()), na.rm = TRUE) # 3.96


### score stats ###

# gfnow
mean(df.consolidated.gfnow$score, na.rm = TRUE) # 76.12
var(df.consolidated.gfnow$score, na.rm = TRUE) # 91.60
sd(df.consolidated.gfnow$score, na.rm = TRUE) # 9.57

mean(df.consolidated.gfnow$user_score, na.rm = TRUE) * 10 # 72.55
var(df.consolidated.gfnow$user_score, na.rm = TRUE) * 10 # 15.90
sd(df.consolidated.gfnow$user_score, na.rm = TRUE) * 10 # 12.61


# psnow
mean(df.consolidated.psnow$score, na.rm = TRUE) # 73.33
var(df.consolidated.psnow$score, na.rm = TRUE) # 173.66
sd(df.consolidated.psnow$score, na.rm = TRUE) # 13.18

mean(df.consolidated.psnow$user_score, na.rm = TRUE) * 10 # 70.77
var(df.consolidated.psnow$user_score, na.rm = TRUE) * 10 # 17.66
sd(df.consolidated.psnow$user_score, na.rm = TRUE) * 10 # 13.29


# steam
mean(df.consolidated.steam$score, na.rm = TRUE) # 71.00
var(df.consolidated.steam$score, na.rm = TRUE) # 143.96
sd(df.consolidated.steam$score, na.rm = TRUE) # 12.00

mean(df.consolidated.steam$user_score, na.rm = TRUE) * 10 # 69.04
var(df.consolidated.steam$user_score, na.rm = TRUE) * 10 # 23.23
sd(df.consolidated.steam$user_score, na.rm = TRUE) * 10 # 15.24
