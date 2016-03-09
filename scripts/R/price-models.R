library(ggplot2)
library(Cairo)

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
psnow.included <- sum(df.psnow$Included.In.Subscription == TRUE)
psnow.extra <- sum(df.psnow$Included.In.Subscription == FALSE)

psnow <- pmax(((budget - psnow.hw.peryear) - psnow.yearly),0)
psnow[psnow > 0] <- psnow[psnow > 0] / psnow.rental.price.30d + psnow.included


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

df <- data.frame(budget = budget, gamesperyear = psnow, platform = "PS Now")
tmp <- data.frame(budget=budget, gamesperyear = gfnow, platform = "GF Now")
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
budget.annual <- year*money

pc.annual <- ((year*money) - 167*year)/10.1
console.annual <- pmax((year*money-80*year),0)/50

psnow.annual <- (year*money)-(15+240)*year
psnow.annual[psnow.annual>0] <- psnow.annual[psnow.annual>0]/15+134
psnow.annual[psnow.annual <0] <- 0

df <- data.frame(year=year, games = pc.annual, platform = "pc")

tmp <- data.frame(year=year, games = console.annual, platform = "consoles")
df <- rbind(df, tmp)
tmp <- data.frame(year=year, games = psnow.annual, platform = "ps now")
df <- rbind(df, tmp)


ggplot(df, aes(x=year, y=games, color=platform)) + geom_line() + geom_point(size=2)






## script intended to look at the reverse model of games-per-year.R:
# assume a fixed number of games
# (maybe even certain individual games: a selection of recent popular titles for that timeframe (via e.g. metacritic))
# and then calculate the cost for each individual service platform

# TODO: update games-per-year.R first