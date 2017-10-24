library(ggplot2)
library(Cairo)

# Use `data_merge.R` to read in the required platform dataframes.

### Foreign-currency exchange rates
# via https://www.oenb.at/zinssaetzewechselkurse/zinssaetzewechselkurse
exchangerate.GBPtoEUR <- 1/0.89303
exchangerate.USDtoEUR <- 1/1.1761


### console cycle years
# according to https://en.wikipedia.org/wiki/History_of_video_games
console.lifetime <- round(mean(c(4, 7, 4, 6, 5, 7, 7))) # 5.7 years -> 6




### gamesperyear over budget model ###

# basis a yearly budget 
budget <- seq(0, 1500, by = 1)

########
# ps now

psnow.hw <- 299 # as of 2017/10/24 on Amazon.de and Libro.at
psnow.hw.peryear <- psnow.hw / console.lifetime
psnow.monthly <- 16.99
psnow.yearly <- psnow.monthly * 12

# XXX Hardcoded to the latest manual count
psnow.maxgames <- 432

psnow <- pmax(((budget - psnow.hw.peryear) - psnow.yearly),0)
psnow[psnow > 0] <- psnow.maxgames


########
# gf now
gfnow.monthly <- 9.99
gfnow.yearly <- gfnow.monthly * 12
gfnow.hw <- 229
gfnow.hw.peryear <- gfnow.hw / console.lifetime

# XXX More hardcoded numbers ahead
# XXX Fix this when the main `df` is updated!
gfnow.maxgames <- 55+63 # nrow(df.gfnow)
gfnow.included <- 55 # nrow(subset(df.gfnow, price == 0))
gfnow.extra <- 63 # gfnow.maxgames - gfnow.included
# XXX From a few glances into GFnow's game offering and
# XXX Steam's prices
gfnow.extraprice.mean <- 15 # mean(subset(df.gfnow, price != 0)$price)

gfnow <- pmax(((budget - gfnow.hw.peryear) - gfnow.yearly), 0)
gfnow[gfnow > 0] <- gfnow.included + pmin(gfnow[gfnow > 0]/gfnow.extraprice.mean , gfnow.extra)


########
# steam
steam.hw <- 500
steam.hw.peryear <- steam.hw / 3
# XXX Do update this too! I snapped it from visual
# XXX inspection of the old graph.
steam.meanprice <- 6.25 # mean(df.steamdata$price, na.rm = TRUE) / 100

steam <- pmax((budget - steam.hw.peryear),0) / steam.meanprice



## plot
df <- NULL
df <- data.frame(budget = budget, gamesperyear = gfnow, platform = "GF Now")
tmp <- data.frame(budget=budget, gamesperyear = psnow, platform = "PS Now")
df <- rbind(df, tmp)
tmp <- data.frame(budget = budget, gamesperyear = steam, platform = "Steam")
df <- rbind(df, tmp)

p <- ggplot(df, aes(x = budget, y = gamesperyear, color = platform, lty = platform)) + geom_line(size = 1) #+ geom_point(size=2)
p <- p + xlab("Annual budget (€)") + ylab("Games per year")
p <- p + theme(text = element_text(size=20))
p
ggsave("gamesperyear-over-budget.pdf", width=12, height=8, device = cairo_pdf)




##########################################
## over a 10 year period at a fixed budget
money <- 500
year <- 1:10
budget.annual <- year * money

steam.annual <- ((budget.annual) - steam.hw.peryear * year) / steam.meanprice

psnow.annual <-  psnow.maxgames

gfnow.annual <- pmax((year * money) - (gfnow.hw.peryear + gfnow.yearly) * year, 0)
gfnow.annual[gfnow.annual > 0] <- pmin(gfnow.annual[gfnow.annual > 0] / gfnow.extraprice.mean, gfnow.extra) + gfnow.included

df <- data.frame(year=year, games = gfnow.annual, platform = "GF Now")
tmp <- data.frame(year=year, games = psnow.annual, platform = "PS Now")
df <- rbind(df, tmp)
tmp <- data.frame(year=year, games = steam.annual, platform = "Steam")
df <- rbind(df, tmp)

p <- ggplot(df, aes(x = year, y = games, color = platform, lty = platform)) + geom_line(size = 1) + geom_point(size = 2) + ylim(0, max(df$games)) + scale_x_continuous(breaks=year)
p <- p + xlab("year") + ylab("games")
p <- p + theme(text = element_text(size=20))
p
ggsave("games-over-year.pdf", width=12, height=8)
