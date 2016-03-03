library(ggplot2)

## TODO: unify category names across datasets and merge by title AND category
## TODO: remove outliers, e.g. runescape @ 100000h

#setwd("git/cost-of-cloud-gaming/data/")

df.hltb <- read.csv("howlongtobeat-20160301.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "character"))

summary(df.hltb)
mean(df.hltb$combined_length, na.rm = TRUE)
median(df.hltb$combined_length, na.rm = TRUE)

orig_df.hltb <- df.hltb
#df.hltb <- subset(df.hltb, platform %in% c("PC", "OnLive", "PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PlayStation Now", "PSP"))

ggplot(df.hltb, aes(x = combined_length, color=as.factor(platform))) + stat_ecdf() + scale_x_log10()



ggplot(df.hltb, aes(x = combined_length, color=as.factor(platform))) + stat_ecdf() + scale_x_log10()



p <- ggplot(df.hltb, aes(x = combined_length)) + stat_density()  + scale_x_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("avg. combined playthrough length (h)") + ylab("density")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamelengths-density.pdf", width=12, height=8)

ggplot(df.hltb, aes(x=combined_length, fill=as.factor(platform))) + stat_density() + scale_x_log10()

## dataset merging efforts
# work-in-progress and TODO!


df.merged2 <- merge(df.merged, df.lengths.cross, by.x = "Title", by.y = "title", all.x = TRUE)
df.merged2 <- merge(df.metacritic, df.hltb, by ="title", all.x = TRUE, all.y = TRUE)

tmp <- df.hltb[order(-df.hltb$combined_length),]

####  combine some stuff with steam datasets
## TODO: consider using adist() for fuzzy string matching the titles
## http://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/
string.matches <- adist(subset(df.priced, date = "20151015")$name, subset(df.metacritic, platform == "pc")$title, ignore.case = TRUE)

df.steammetascore <- merge(df.priced, subset(df.metacritic, platform == "pc"), by.x = "name", by.y = "title", all.x = TRUE)
df.steammetascorehltb <- merge(df.steammetascore, subset(df.hltb, platform == "PC"), by.x = "name", by.y = "title", all.x = TRUE)

# hist of steam games metacritic score
ggplot(df.steammetascorehltb, aes(x = score)) + geom_histogram(binwidth = 1)
ggplot(df.steammetascorehltb, aes(x = score)) + stat_ecdf()




#############################
df.gfnow <- read.csv("gfnow-games.csv", header=TRUE, sep=",", colClasses=c("character", "numeric"))
df.psnow <- read.csv("psnow-games.csv", header=TRUE, sep=";", colClasses=c("character", "numeric", "numeric", "numeric", "numeric", "logical"))
df.steamdata <- read.csv(file="steamdata-20160206.csv", head=TRUE, sep=",", colClasses=c("numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

df.hltb$title <- tolower(df.hltb$title)
df.gfnow$name <- tolower(df.gfnow$name)
df.psnow$Title <- tolower(df.psnow$Title)
df.steamdata$name <- tolower(df.steamdata$name)

df.hltb.pc = subset(df.hltb, platform == "PC")
df.hltb.ps = subset(df.hltb, platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3"))

df.consolidated.gfnow <- merge(df.gfnow, df.hltb.pc, by.x = "name", by.y = "title", all.x = TRUE)
df.consolidated.psnow <- merge(df.psnow, df.hltb.ps, by.x = "Title", by.y = "title", all.x = TRUE)
df.consolidated.steam <- merge(df.steamdata, df.hltb.pc, by.x = "name", by.y = "title", all.x = TRUE)

## generate data frame for multi-plat density plot

df.lengths <- data.frame()
df.lengths <- rbind(df.lengths, data.frame(title = df.consolidated.psnow$Title, platform = "PlayStation Now", combined_length = df.consolidated.psnow$combined_length))
df.lengths <- rbind(df.lengths, data.frame(title = df.consolidated.gfnow$name, platform = "Geforce Now", combined_length = df.consolidated.gfnow$combined_length))
df.lengths <- rbind(df.lengths, data.frame(title = df.consolidated.steam$name, platform = "Steam", combined_length = df.consolidated.steam$combined_length))
df.lengths <- rbind(df.lengths, data.frame(title = df.hltb$title, platform = "overall", combined_length = df.hltb$combined_length))


## density plot
p <- ggplot(df.lengths, aes(x = combined_length, fill = as.factor(platform), color = as.factor(platform))) + geom_density(alpha = 0.4) + scale_x_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("avg. combined playthrough length (h)") + ylab("density")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamelengths-by-platform-density.pdf", width=12, height=8)

## violins
# p <- ggplot(df.lengths, aes(x = as.factor(platform), y = combined_length)) + geom_violin(scale = "count") + scale_y_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- ggplot(df.lengths, aes(x = as.factor(platform), y = combined_length)) + geom_violin(adjust = .8, scale = "area", draw_quantiles = c(0.25,0.5,0.75), na.rm = TRUE) + scale_y_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("platform") + ylab("avg. combined playthrough length (h)")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamelengths-by-platform-violin.pdf", width=12, height=8)
