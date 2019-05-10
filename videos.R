library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)


dataset <- read_csv("videos.csv")

jhds <- mutate(dataset, just_hour = hour(dataset$hour))

nov_in_time <- jhds %>%
  group_by(just_hour) %>% 
  summarise(time = n())

x = c(nov_in_time$time)

# Clock plot function
clock.plot <- function (x, col = rainbow(n), ...) {
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a) )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
  segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
}

# Use the function on the created data
clock.plot(x, main = "Number of view each hour of the day")