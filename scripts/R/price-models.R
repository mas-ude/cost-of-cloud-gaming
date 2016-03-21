library(ggplot2)
library(Cairo)

# Use `data_merge.R` to read in the required platform dataframes.

exchangerate.GBPtoEUR <- 1.27270
exchangerate.USDtoEUR <- 0.882388


### console cycle years
# according to https://en.wikipedia.org/wiki/History_of_video_games
console.lifetime <- round(mean(c(4, 7, 4, 6, 5, 7, 7))) # 5.7 years -> 6




### gamesperyear over budget model ###

# basis a yearly budget 
budget <- seq(0, 1500, by = 1)

########
# ps now

psnow.hw <- 329.9 # as of 2016/02/11
psnow.hw.peryear <- psnow.hw / console.lifetime
psnow.monthly <- 12.99 * exchangerate.GBPtoEUR
psnow.yearly <- psnow.monthly * 12
psnow.rental.price.30d <- 7.99 * exchangerate.GBPtoEUR
# no numbers for uk service, approximate by ratio of jp included vs rental titles as of
# http://www.jp.playstation.com/psnow/list.html
# included: 145, rental: 185
# rental proportion: .56
psnow.maxgames <- nrow(df.psnow)
psnow.excluded <- psnow.maxgames * 0.56
psnow.included <- psnow.maxgames - psnow.excluded
#psnow.included <- sum(df.psnow$Included.In.Subscription == TRUE)
#psnow.extra <- sum(df.psnow$Included.In.Subscription == FALSE)

psnow <- pmax(((budget - psnow.hw.peryear) - psnow.yearly),0)
psnow[psnow > 0] <- pmin(psnow[psnow > 0] / psnow.rental.price.30d + psnow.included, psnow.maxgames)


########
# gf now
gfnow.monthly <- 9.99
gfnow.yearly <- gfnow.monthly * 12
gfnow.hw <- 201.99
gfnow.hw.peryear <- gfnow.hw / console.lifetime

gfnow.maxgames <- nrow(df.gfnow)
gfnow.included <- nrow(subset(df.gfnow, price == 0))
gfnow.extra <- gfnow.maxgames - gfnow.included
gfnow.extraprice.mean <- mean(subset(df.gfnow, price != 0)$price)

gfnow <- pmax(((budget - gfnow.hw.peryear) - gfnow.yearly), 0)
gfnow[gfnow > 0] <- gfnow.included + pmin(gfnow[gfnow > 0]/gfnow.extraprice.mean , gfnow.extra)


########
# steam
steam.hw <- 500
steam.hw.peryear <- steam.hw / 3
steam.meanprice <- mean(df.steamdata$price, na.rm = TRUE) / 100

steam <- pmax((budget - steam.hw.peryear),0) / steam.meanprice



## plot

df <- data.frame(budget = budget, gamesperyear = psnow, platform = "GF Now")
tmp <- data.frame(budget=budget, gamesperyear = gfnow, platform = "PS Now")
df <- rbind(df, tmp)
tmp <- data.frame(budget = budget, gamesperyear = steam, platform = "Steam")
df <- rbind(df, tmp)

p <- ggplot(df, aes(x = budget, y = gamesperyear, color = platform, lty = platform)) + geom_line(size = 1) #+ geom_point(size=2)
p <- p + xlab("budget (€)") + ylab("games per year")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamesperyear-over-budget.pdf", width=12, height=8, device = cairo_pdf)




##########################################
## over a 10 year period at a fixed budget
money <- 500
year <- 1:10
budget.annual <- year * money

steam.annual <- ((budget.annual) - steam.hw.peryear * year) / steam.meanprice

psnow.annual <- pmax((year * money) - (psnow.hw.peryear + psnow.yearly) * year, 0)
psnow.annual[psnow.annual > 0] <- pmin(pmin(psnow.annual[psnow.annual > 0] / psnow.rental.price.30d, psnow.excluded) + psnow.included, psnow.maxgames)

gfnow.annual <- pmax((year * money) - (gfnow.hw.peryear + gfnow.yearly) * year, 0)
gfnow.annual[gfnow.annual > 0] <- pmin(gfnow.annual[gfnow.annual > 0] / gfnow.extraprice.mean, gfnow.extra) + gfnow.included

df <- data.frame(year=year, games = psnow.annual, platform = "GF Now")
tmp <- data.frame(year=year, games = gfnow.annual, platform = "PS Now")
df <- rbind(df, tmp)
tmp <- data.frame(year=year, games = steam.annual, platform = "Steam")
df <- rbind(df, tmp)

p <- ggplot(df, aes(x = year, y = games, color = platform, lty = platform)) + geom_line(size = 1) + geom_point(size = 2) + ylim(0, max(df$games)) + scale_x_continuous(breaks=year)
p <- p + xlab("year") + ylab("games")
p <- p + theme(text = element_text(size=20))
p
ggsave("games-over-year.pdf", width=12, height=8)
