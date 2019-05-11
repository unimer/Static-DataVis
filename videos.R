library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(gridGraphics)
library(tidyverse)
library(RColorBrewer)


#----------------------------------------------------------
#Number of views per each hour a of the day
#---------------------------------------------------------------------------------

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
  m <- 1.5
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
  p1 <- recordPlot()
  return (p1)
}

# Use the function on the created data
p <- clock.plot(x, main = "Number of view each hour of the day")



#----------------------------------------------------------
#Number of vews per video
#---------------------------------------------------------------------------------

nov_per_video <- dataset %>% group_by(episode) %>%
                    summarise(nov = n())



# Let's create a vector of data:
my_vector=c(nov_per_video$nov)
names(my_vector)=paste("episode: ", c(nov_per_video$episode),sep="")

#create color palette:

# plot
par(mar=c(7,3,3,3))
a=barplot(my_vector, col=coul , las=1, names.arg="") 
text(a[,1], -30, srt = 60, adj= 1, xpd = TRUE, labels = names(my_vector) , cex=1.2)

p2 <- recordPlot()

plot_grid(p, p2, ncol = 1, align = 'v')

#----------------------------------------------------------
## Number of logged vs number of unknown users
#-------------------------------------------------------------------


users <- jhds %>%
  group_by(logged, episode) %>% 
  summarise(nov = n())

users_spreaded <- spread(users, logged, nov )

ggplot(data=users, aes(x=episode, y=nov, fill=logged)) +
  labs(x="Episode", y = "Number of Views", fill="Logged") +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Set2") +
  
  theme_minimal()


#---------------------------------------------------------------
#Episode watching time
#--------------------------------------------------------------------

watching_time <- dataset %>%
                  group_by(episode) %>%
                    summarise(average = mean(watchingTime), duration_corrected = mean(duration))

watching_time <- mutate(watching_time, percentage = (watching_time$average / watching_time$duration_corrected) * 100)



# make data
data=data.frame(group=watching_time$episode , value=watching_time$percentage )

palette <- c("dodgerblue1", "skyblue4", "chocolate1", "seagreen4",
             "bisque3", "red4", "purple4", "mediumpurple3",
             "maroon", "dodgerblue4", "skyblue2", "darkcyan",
             "darkslategray3", "lightgreen", "bisque",
             "palevioletred1", "black", "gray79", "lightsalmon4",
             "darkgoldenrod1")

# Usual bar plot :
ggplot(data, aes(x = group, y = value ,fill = group )) + 
  geom_bar(width = 0.85, stat="identity")

ggplot(data=data, aes(x=group, y=value)) +
  labs(x="Episode", y = "Average Watching Time") +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(palette=palette) +
  theme_minimal()


#---------------------------------------------------------------
#Where the most people stopped watching
#--------------------------------------------------------------------








  