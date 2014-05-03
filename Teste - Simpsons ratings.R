# Simpsons
library(XML)
library(RColorBrewer)
library(plyr)
library(ggplot2)


# Download data
tables <- readHTMLTable('http://www.imdb.com/title/tt0096697/eprate')

ratings <- tables[[1]][, 1:4]
names(ratings)[1:2] <- c("S.E", "Name")


# Data munging
ratings$UserRating <- as.numeric(paste(ratings$UserRating))
ratings$UserVotes <- as.numeric(gsub("\\,", "", ratings$UserVotes))

S.E <- as.character(ratings$S.E)
patt <- "^([0-9]*)\\.([0-9]*).*$"
ratings$Season <- as.numeric(gsub(patt, "\\1", S.E))
ratings$Episode <- as.numeric(gsub(patt, "\\2", S.E))

EpOrder <- with(ratings, order(Season, Episode))
ratings$EpNumber[EpOrder] <- 1:nrow(ratings)


# Plots
ggplot(ratings, aes(EpNumber, UserRating, col = paste(Season))) + 
       geom_point(size = 3.5) +
       theme_bw() + theme(legend.position = "none")


ratings.mean <- ddply(ratings, .(Season), summarize, 
                      mean = mean(UserRating), sd = sd(UserRating))

ggplot(ratings.mean, aes(Season, mean, col = paste(Season))) + 
       geom_point(size = 3.5) + 
       geom_errorbar(aes(Season, ymin = mean-sd, ymax = mean+sd, width = .2)) + 
       theme_bw() + theme(legend.position = "none")
  

ggplot(ratings, aes(EpNumber, UserVotes, col = paste(Season))) + 
       geom_point(size = 3.5) +
       theme_bw() + theme(legend.position = "none")


spectral <- brewer.pal(11, "Spectral")
ggplot(ratings, aes(UserVotes, UserRating, col = Season)) + 
       geom_point(size = 3.5) + theme_bw() +
       scale_colour_gradientn(colours = spectral)
