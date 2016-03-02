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


df.hltb$title <- tolower(df.hltb$title)
df.gfnow$name <- tolower(df.gfnow$name)
df.psnow$Title <- tolower(df.psnow$Title)

df.hltb.pc = subset(df.hltb, platform == "PC")
df.hltb.ps = subset(df.hltb, platform %in% c("PlayStation", "PlayStation 2", "PlayStation 3"))



df.consolidated.gfnow <- merge(df.gfnow, df.hltb.pc, by.x = "name", by.y = "title", all.x = TRUE)

df.consolidated.psnow <- merge(df.psnow, df.hltb.ps, by.x = "Title", by.y = "title", all.x = TRUE)
