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

dataset <- read_csv("/home/nikola/famnit2/VIS/Projekt/DataVisVideos/videos.csv")

jhds <- mutate(dataset, just_hour = hour(dataset$hour))

nov_in_time <- jhds %>%
  group_by(just_hour) %>% 
  summarise(nov = n())

xx = c(nov_in_time$nov)

ii <- cut(nov_in_time$nov, breaks = seq(min(nov_in_time$nov), max(nov_in_time$nov), len = 100), 
          include.lowest = TRUE)

colors <- colorRampPalette(c("lightblue", "blue"))(99)[ii]

# Clock plot function
clock.plot <- function (x, col = heat.colors(315, alpha = 1, rev = TRUE), ...) {
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.7
  plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a), lty = 3  )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a), col = "light grey" )
  segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[x[i]] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
  p1 <- recordPlot()
  return (p1)
}

# Use the function on the created data
p <- clock.plot(xx)



#----------------------------------------------------------
#Number of vews per video
#---------------------------------------------------------------------------------

nov_per_video <- dataset %>% group_by(episode) %>%
                    summarise(number_of_views = n())
ID <- 1:13


ggplot(data=nov_per_video, aes(x=episode, y=number_of_views)) +
  ggtitle("                Number of views per episode\n") +
  labs(x="Episode", y = "Number of Views") +
  geom_bar(stat="identity", position=position_dodge())+
  #ylim(min=0, max=100) +
  scale_x_continuous("Episode", labels = as.character(ID), breaks = ID)+
  theme_minimal() +
  theme(legend.position="none") 



#----------------------------------------------------------
## Number of logged vs number of unknown users
#-------------------------------------------------------------------


users <- jhds %>%
  group_by(logged, episode) %>% 
  summarise(nov = n())

users_spreaded <- spread(users, logged, nov )


ID <- 1:13


ggplot(data=users, aes(x=episode, y=nov, fill=logged)) +
  ggtitle("            Registered vs Unregistered Users\n") +
  labs(x="Episode", y = "Number of Views", fill="Logged") +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Set2") +
  scale_x_continuous("Episode", labels = as.character(ID), breaks = ID)+
  theme_minimal()


#---------------------------------------------------------------
#Episode watching time
#--------------------------------------------------------------------

watching_time <- dataset %>%
                  group_by(episode) %>%
                    summarise(average = mean(watchingTime), duration_corrected = mean(duration))

watching_time <- mutate(watching_time, percentage = (watching_time$average / watching_time$duration_corrected) * 100)




ggplot(data=watching_time, aes(x=episode, y=percentage)) +
  
  labs(x="Episode", y = "Number of Views") +
  geom_bar(stat="identity", position=position_dodge())+
  ylim(min=0, max=100) +
  theme_minimal() +
  theme(legend.position="none") 




#---------------------------------------------------------------
#Where the most people stopped watching
#--------------------------------------------------------------------
stopped <- spread(dataset, episode, watchingTime )

stopped <- dataset[,c(7,9,10)]

stopped <- mutate(stopped, percentage = round((stopped$watchingTime / stopped$duration) * 100) )

how_much <- stopped %>%
              group_by(episode, percentage) %>%
                summarise(value = n())

first = filter(how_much, episode == 1)
second = filter(how_much, episode == 2)
third = filter(how_much, episode == 3)
four = filter(how_much, episode == 4)
five = filter(how_much, episode == 5)
six = filter(how_much, episode == 6)
seven = filter(how_much, episode == 7)
eight = filter(how_much, episode == 8)
nine = filter(how_much, episode == 9)
ten = filter(how_much, episode == 10)
eleven = filter(how_much, episode == 11)
twelve = filter(how_much, episode == 12)
thirteen = filter(how_much, episode == 13)



p1<-ggplot(first, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle = "Episode 1", x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 




p2<-ggplot(second,aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 2", x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 


p3<-ggplot(third, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 3",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 


p4<-ggplot(four,aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 4",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 

p5<-ggplot(five, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 5",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 

p6<-ggplot(six, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 6",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 

p7<-ggplot(seven, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 7",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 

p8<-ggplot(eight, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 8",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 


p9<-ggplot(nine, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 9",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 

p10<-ggplot(ten, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 10",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 


p11<-ggplot(eleven, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 11",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 


p12<-ggplot(twelve, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 12",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 


p13<-ggplot(thirteen, aes(x=percentage, y=value)) +
  geom_line(aes(color=episode))+
  labs(subtitle= "Episode 13",x="Video Length [%]", y = "Users")+
  theme_minimal() +
  theme(legend.position="none") 

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, ncol=3, scale=1)
  
