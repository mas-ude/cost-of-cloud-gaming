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
