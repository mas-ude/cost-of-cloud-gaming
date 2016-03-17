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
df.scores <- rbind(df.scores, data.frame(title = df.consolidated.gfnow$name, platform = "Geforce Now", score = df.consolidated.gfnow$score, user_score = df.consolidated.gfnow$user_score * 10))
df.scores <- rbind(df.scores, data.frame(title = df.consolidated.psnow$name, platform = "PlayStation Now", score = df.consolidated.psnow$score, user_score = df.consolidated.psnow$user_score * 10))
df.scores <- rbind(df.scores, data.frame(title = df.consolidated.steam$name, platform = "Steam", score = df.consolidated.steam$score, user_score = df.consolidated.steam$user_score * 10))
df.scores <- rbind(df.scores, data.frame(title = df.metacritic$title, platform = "overall", score = df.metacritic$score, user_score = df.metacritic$user_score * 10))

# Show how many games per platform we have Metacritic scores for.
# https://github.com/mas-ude/cost-of-cloud-gaming/issues/4 : I'd love 
#   for this information to go into the x labels, but multiline labels 
#   are too hard for me (or R.)
# Result (also showing that we couldn't match 100% of games between the datasets)
#    Geforce Now PlayStation Now           Steam         overall 
#             68             209            7759           46197 
summary(df.scores$platform)


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
age_in_integer_years = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

# Adapted function to return the age as a real number of years.
# XXX I'm glossing over the intricacies here.
age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = as.numeric(to_lt - from_lt)/365
}

# Fix TODAY for the calculations so the results are reproducible.
# Update with `TODAY <- as.POSIXlt(Sys.Date())` when you redo this!
TODAY = "2016-03-17 UTC"

# gfnow
mean(age(df.consolidated.gfnow$release, TODAY), na.rm = TRUE) # 2.87
var(age(df.consolidated.gfnow$release, TODAY), na.rm = TRUE) # 3.82
sd(age(df.consolidated.gfnow$release, TODAY), na.rm = TRUE) # 1.95

# psnow
mean(age(df.consolidated.psnow$release, TODAY), na.rm = TRUE) # 5.24
var(age(df.consolidated.psnow$release, TODAY), na.rm = TRUE) # 6.49
sd(age(df.consolidated.psnow$release, TODAY), na.rm = TRUE) # 2.55

# steam
mean(age(df.consolidated.steam$release, TODAY), na.rm = TRUE) # 3.36
var(age(df.consolidated.steam$release, TODAY), na.rm = TRUE) # 15.63
sd(age(df.consolidated.steam$release, TODAY), na.rm = TRUE) # 3.95

# Age plot
df.ages <- NULL
df.ages <- data.frame()
df.ages <- rbind(df.ages, data.frame(title=df.consolidated.gfnow, platform="Geforce Now", age=age(df.consolidated.gfnow$release, Sys.Date())))
df.ages <- rbind(df.ages, data.frame(title=df.consolidated.psnow, platform="PlayStation Now", age=age(df.consolidated.psnow$release, Sys.Date())))
subset_of_columns <- c("name", "price","main_story_length","mainextra_length",
    "completionist_length","combined_length","platform.x","user_score",
    "publisher","genre","score","release","platform.y","year")
df.ages <- rbind(df.ages, data.frame(title=df.consolidated.steam[,subset_of_columns], platform="Steam", age=age(df.consolidated.steam$release, Sys.Date())))
p <- ggplot(df.ages, aes(x=age, color=as.factor(platform))) + geom_density()
p <- p + xlab("Game age (years)") + ylab("density")
p <- p + theme(text = element_text(size=20))
p
ggsave("game-ages-by-platform-density.pdf", width=12, height=8)

p <- ggplot(df.ages, aes(x=as.factor(platform), y=age)) + geom_violin()
p <- p + xlab("platform") + ylab("Game age (years)")
p <- p + theme(text = element_text(size=20))
p
ggsave("game-ages-by-platform-violin.pdf", width=12, height=8)


### score stats ###

# gfnow
mean(df.consolidated.gfnow$score, na.rm = TRUE) # 75.90
var(df.consolidated.gfnow$score, na.rm = TRUE) # 89.01
sd(df.consolidated.gfnow$score, na.rm = TRUE) # 9.44

mean(df.consolidated.gfnow$user_score * 10, na.rm = TRUE) # 72.41
var(df.consolidated.gfnow$user_score * 10, na.rm = TRUE) # 156.1
sd(df.consolidated.gfnow$user_score * 10, na.rm = TRUE) # 12.49


# psnow
mean(df.consolidated.psnow$score, na.rm = TRUE) # 76.72
var(df.consolidated.psnow$score, na.rm = TRUE) # 130.69
sd(df.consolidated.psnow$score, na.rm = TRUE) # 11.43

mean(df.consolidated.psnow$user_score * 10, na.rm = TRUE) # 73.1
var(df.consolidated.psnow$user_score * 10, na.rm = TRUE) # 165.5
sd(df.consolidated.psnow$user_score * 10, na.rm = TRUE) # 12.86


# steam
mean(df.consolidated.steam$score, na.rm = TRUE) # 71.00
var(df.consolidated.steam$score, na.rm = TRUE) # 143.16
sd(df.consolidated.steam$score, na.rm = TRUE) # 12

mean(df.consolidated.steam$user_score * 10, na.rm = TRUE) # 69
var(df.consolidated.steam$user_score * 10, na.rm = TRUE) # 233.2
sd(df.consolidated.steam$user_score * 10, na.rm = TRUE) # 15.27
