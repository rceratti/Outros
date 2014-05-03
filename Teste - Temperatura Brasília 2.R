library(dplyr)
library(weatherData)
library(ggplot2)
library(reshape2)



gWD <- function(station, Date) {
  tryCatch(getWeatherData(station, Date, 
                          opt_temperature_only = F),
           error = function(e) NULL) 
}


inicio <- as.Date('2014-04-01')
fim <- as.Date('2014-05-02')
datas <- seq.Date(inicio, fim, '1 day')


temp <- lapply(datas, function(d, station) {
  cat('Getting data --', paste(d), '... ')
  x <- gWD(station, d)
  cat('Done.\n')
  
  x <- filter(x, TemperatureC > -9999, Humidity != 'N/A') %.%
       select(TimeBRT, TemperatureC, Humidity) %.%
       mutate(Humidity = as.numeric(Humidity)) %.%
       group_by(TimeBRT) %.% 
       summarize(TemperatureC = mean(TemperatureC),
                 Humidity = mean(Humidity))
  x$Date <- d
  x
}, station = 'SBBR')


temp1 <- do.call(rbind, temp) %.%
         mutate(DT = paste(Date, TimeBRT)) %.%
         select(DT, TemperatureC, Humidity)

qplot(TemperatureC, Humidity, data = temp1)


temp2 <- melt(temp1, id.vars = 'DT')
temp2$DT <- strptime(temp2$DT, "%Y-%m-%d %I:%M %p")


ggplot(temp2, aes(DT, value)) + geom_line() + ylab("") +
  facet_grid(variable ~ ., scales = "free_y") + geom_smooth()
