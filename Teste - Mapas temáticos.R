library(WDI)
library(reshape2)
library(maptools)
library(RColorBrewer)


setwd("C:/Users/b2415110/Downloads/TM_WORLD_BORDERS_SIMPL-0.3")


WDIsearch(string = "co2 emissions", field = "name", short = TRUE,
cache = NULL)

d <- WDI_data[[2]]
countries <- d[d[, 'region'] != "Aggregates", 'iso2c']
indicators <- "SP.RUR.TOTL.ZS"  # c("SP.RUR.TOTL.ZS", "EN.ATM.CO2E.PC")
start <- 2000
end <- 2011

x <- WDI(countries, indicators, start, end)
x.1 <- dcast(x, iso2c ~ year, value.var = 'SP.RUR.TOTL.ZS')
names(x.1)[1] <- "ISO2"


wrl <- readShapePoly("TM_WORLD_BORDERS_SIMPL-0.3.shp")

wrl.data <- wrl@data
wrl.data$indice <- 1:nrow(wrl.data)

wrl.data$ISO2 <- paste(wrl.data$ISO2)
wrl.data <- merge(wrl.data, x.1, by = 'ISO2', all.x = TRUE)
wrl.data <- wrl.data[order(wrl.data$indice), ]

attr(wrl, "data") <- wrl.data


brks <- seq(0, 90, 10)
my.pal <- brewer.pal(length(brks)-1, "Blues")  # "YlGnBu"
ind.graph <- findInterval(wrl.data$'2010', brks)

pdf("teste.pdf", w = 11, h = 10)
plot(wrl, col = my.pal[ind.graph])
box()
title("Proporção de População rural")
legend(-190, -90, bty = "n", fill = my.pal, cex = 0.8,
       legend = leglabs(brks), horiz = F)
dev.off()