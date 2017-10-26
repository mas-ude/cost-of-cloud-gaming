library(ggplot2)
library(Cairo)
library(dplyr)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# load the data file you want to plot

# navigate to data folder
#setwd("git/cost-of-cloud-gaming/data/")
df.priced <- data.frame()
csv.list <- Sys.glob("steamdata-*.csv")
for (csv.file in csv.list) {
  print(csv.file)
  tmp <- read.csv(file=csv.file)
  date <- unlist(strsplit(unlist(strsplit(csv.file, split="-"))[2], split="\\."))[1]
  tmp$date <- date
  df.priced <- rbind(df.priced, tmp)
}
df.priced$euro <- df.priced$price/100

# Let's take a look at average (aka "portfolio") prices, grouped by date
aggregate(df.priced$euro, list(Date=df.priced$date), mean)
aggregate(df.priced$euro, list(Date=df.priced$date), sd)


thedates <- c("2015-07-14", "2015-10-30", "2016-02-06", "2016-06-30",
    "2016-09-05", "2016-11-26", "2017-04-19", "2017-10-09")


# date as.factor color'd
ggplot(df.priced, aes(x=owners, color=as.factor(date))) + stat_ecdf() + scale_x_log10()

# prices ECDF
p <- ggplot(df.priced, aes(x=euro, color=as.factor(date)))
p <- p + stat_ecdf(lwd = 2) + scale_x_log10()
p <- p + xlab("price (€)") + ylab("ECDF")
p <- p + scale_color_manual(values = cbPalette, name="date", labels=thedates)
p <- p + theme(text = element_text(size=20))
p
ggsave("steam-prices.pdf", width=12, height=8, device = cairo_pdf)


# prices DF
# (Helps to explain why we categorized game price ranges the way we did.)
p <- ggplot(df.priced, aes(x=euro, color=as.factor(date)))
p <- p + geom_density() #+ scale_x_log10()
p <- p + xlim(c(0,40))
p <- p + xlab("price (€)") + ylab("Probability")
p <- p + scale_color_manual(values = cbPalette, name="date", labels=thedates)
p <- p + theme(text = element_text(size=20))
p
ggsave("steam-prices-density.pdf", width=12, height=8, device = cairo_pdf)



##############

df.steam <- read.csv(file="steamdata-20160206.csv")


ggplot(df.priced, aes(x=owners)) + stat_ecdf() + scale_x_log10()
ggplot(df.priced, aes(x=average_forever)) + stat_ecdf() + scale_x_log10()
ggplot(df.priced, aes(x=appid, y=owners)) + geom_point() + scale_y_log10() + xlim(0,400000)
ggplot(df.priced, aes(x=appid, y=average_forever)) + geom_point() + scale_y_log10() + xlim(0,400000)
ggplot(df.priced, aes(x=appid, y=average_2weeks)) + geom_point() + scale_y_log10() + xlim(0,400000)

ggplot(df.priced, aes(x=players_forever, y=average_forever)) + geom_point() + scale_x_log10() + scale_y_log10()
ggplot(df.priced, aes(x=players_2weeks, y=average_2weeks)) + geom_point() + scale_x_log10() + scale_y_log10()


### price data

ggplot(df.priced, aes(x=price, y=average_2weeks)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=average_forever)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=average_forever, size=players_forever, color=players_forever)) + geom_point() + scale_x_log10() + scale_y_log10() 

ggplot(df.priced, aes(x=price, y=players_forever)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=players_forever)) + geom_point() + xlim(0,10000) + scale_y_log10() + scale_x_log10()

prices <- as.data.frame(table(df.priced$price))
names(prices) <- c("price", "frequency")
ggplot(prices, aes(x=price, y=frequency)) + geom_point()
ggplot(df.priced, aes(x=price)) + stat_ecdf() + scale_x_log10()

ggplot(df.priced, aes(x=price, y=average_2weeks)) + geom_point() + xlim(0,10000) + scale_y_log10()

ggplot(df.priced, aes(x=price, y=players_forever)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=players_2weeks)) + geom_point() + xlim(0,10000) + scale_y_log10()



### heatmap
# be careful, the zero values are changed to 1 here, to allow for log scaling!
df.priced[df.priced["players_forever"]==0,"players_forever"] = 1
df.priced[df.priced["price"]==0,"price"] = 1
ggplot(df.priced, aes(x=price, y=players_forever)) + geom_bin2d() + scale_x_log10() + scale_y_log10()


### custom binning and setting levels
df.priced2 <- data.frame()
for (i in 1:nrow(df.priced)) {
  row <- df.priced[i,]
  
  if(row$price == 0) {
    row$price_category <- "free"
  } else if (row$price > 0 & row$price <= 500){
    row$price_category <- "<5"
  } else if (row$price > 500 & row$price <= 1000) {
    row$price_category <- "5 to 10"
  } else if (row$price > 1000 & row$price <= 2000) {
    row$price_category <- "10 to 20"
  } else if (row$price > 2000 & row$price <= 4000) {
    row$price_category <- "20 to 40"
  } else if (row$price > 4000) {
    row$price_category <- "above 40"
  }  
  tmp <- data.frame(row)
  df.priced2 <- rbind(df.priced2, tmp)
}
df.priced2$price_category <- as.factor(df.priced2$price_category)
df.priced2$price_category <- ordered(df.priced2$price_category, levels=c("free", "<5", "5 to 10", "10 to 20", "20 to 40", "above 40"))
ggplot(df.priced2, aes(x=price_category, y=players_forever)) + geom_violin(adjust=.5) + scale_y_log10()
ggsave("dampfviolinen-players.pdf")


d1 <- subset(df.priced, price==0  | (price>1000))
graph <- ggplot(d1, aes(x=average_forever, color=as.factor(sign(price)))) 
graph + stat_ecdf() + scale_x_log10()


#### paper violin price range vs average playtime violin plot
## standalone section

df.steamdata <- read.csv(file="steamdata-20171009.csv", head=TRUE, sep=",", colClasses=c("numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

df.steamdata$average_playtime_hours <- df.steamdata$average_forever / 60

df.steamdata$prizecategory <-
  ifelse(df.steamdata$price>4000,"above 40",
         ifelse(df.steamdata$price>2000,"20 to 40",
                ifelse(df.steamdata$price>1000,"10 to 20",
                       ifelse(df.steamdata$price>500,"5 to 10",
                              ifelse(df.steamdata$price>0,"<5","free")))))

df.steam.prizecategory <- df.steamdata$prizecategory

df.steam.prizecategory <- as.factor(df.steam.prizecategory)
df.steam.prizecategory <- ordered(df.steam.prizecategory, levels=c("free", "<5", "5 to 10", "10 to 20", "20 to 40", "above 40"))

df.steamdata$price_category <- df.steam.prizecategory

# Show how many games per price category we have.
# https://github.com/mas-ude/cost-of-cloud-gaming/issues/4 : I'd love 
#   for this information to go into the x labels, but multiline labels 
#   are too hard for me (or R.)
# Result:
# free       <5  5 to 10 10 to 20 20 to 40 above 40
# 1541     5269     4019     2445      658      188
summary(df.steamdata$price_category)

nice_breaks <- c(0.1, 1, 10, 100, 1000)
p <- ggplot(df.steamdata, aes(x=price_category, y=average_playtime_hours)) + geom_violin(adjust=.5, draw_quantiles = c(0.25,0.5,0.75), na.rm = TRUE)  + scale_y_log10(breaks = nice_breaks, labels = nice_breaks)
p <- p + xlab("price range (€)") + ylab("average playtime (h)")
p <- p + theme(text = element_text(size=20))
p
ggsave("steam-cost-vs-playtime-non-sale.pdf", width=12, height=8, device = cairo_pdf)
