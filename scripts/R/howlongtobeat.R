library(ggplot2)
library(stringr)

### load data with data_merge.R first! ###


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


# hist of steam games metacritic score
ggplot(df.steammetascorehltb, aes(x = score)) + geom_histogram(binwidth = 1)
ggplot(df.steammetascorehltb, aes(x = score)) + stat_ecdf()





## generate data frame for multi-plat density plot

df.lengths <- data.frame()
df.lengths <- rbind(df.lengths, data.frame(title = df.consolidated.gfnow$name, platform = "Geforce Now", combined_length = df.consolidated.gfnow$combined_length))
df.lengths <- rbind(df.lengths, data.frame(title = df.consolidated.psnow$name, platform = "PlayStation Now", combined_length = df.consolidated.psnow$combined_length))
df.lengths <- rbind(df.lengths, data.frame(title = df.consolidated.steam$name, platform = "Steam", combined_length = df.consolidated.steam$combined_length))
df.lengths <- rbind(df.lengths, data.frame(title = df.hltb$title, platform = "overall", combined_length = df.hltb$combined_length))


# Show how many games per price category we have.
# https://github.com/mas-ude/cost-of-cloud-gaming/issues/4 : I'd love 
#   for this information to go into the x labels, but multiline labels 
#   are too hard for me (or R.)
# Result (also showing that we couldn't match 100% of games between the datasets)
#    Geforce Now PlayStation Now           Steam        overall 
#             68             209            7764          18433 
summary(df.lengths$platform)



## density plot
p <- ggplot(df.lengths, aes(x = combined_length, fill = as.factor(platform), color = as.factor(platform))) + geom_density(alpha = 0.4) + scale_x_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("avg. combined playthrough length (h)") + ylab("density")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamelengths-by-platform-density.pdf", width=12, height=8)

## violins
# p <- ggplot(df.lengths, aes(x = as.factor(platform), y = combined_length)) + geom_violin(scale = "count") + scale_y_log10() #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
nice_breaks <- c(0.1, 1, 10, 100, 1000)
p <- ggplot(df.lengths, aes(x = as.factor(platform), y = combined_length)) + geom_violin(adjust = .8, scale = "area", draw_quantiles = c(0.25,0.5,0.75), na.rm = TRUE) + scale_y_log10(breaks=nice_breaks, labels=nice_breaks) #+ xlim(0, 75)#+ scale_x_log10() # + xlim(0, 100)
p <- p + xlab("platform") + ylab("avg. combined playthrough length (h)")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamelengths-by-platform-violin.pdf", width=12, height=8)



### lengths stats per set ###

# gfnow
mean(df.consolidated.gfnow$combined_length, na.rm = TRUE) # 14.65
var(df.consolidated.gfnow$combined_length, na.rm = TRUE) # 208.50
sd(df.consolidated.gfnow$combined_length, na.rm = TRUE) # 14.44


# psnow
mean(df.consolidated.psnow$combined_length, na.rm = TRUE) # 13.28
var(df.consolidated.psnow$combined_length, na.rm = TRUE) # 206.85
sd(df.consolidated.psnow$combined_length, na.rm = TRUE) # 14.38


# steam
mean(df.consolidated.steam$combined_length, na.rm = TRUE) # 13.02
var(df.consolidated.steam$combined_length, na.rm = TRUE) # 419.79
sd(df.consolidated.steam$combined_length, na.rm = TRUE) # 20.49
