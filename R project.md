---
title: "Q1:Rush hour for each city to avoid in bikes shortage"
output:
  word_document: default
  pdf_document: default
  html_document: default
---


```{r Q1, warning=FALSE}

#preparing Dataset
WASH <- read.csv("C:/Users/Yasmin/Downloads/washington.csv")

NYC <- read.csv("C:/Users/Yasmin/Downloads/new-york-city.csv")

CHI <- read.csv("C:/Users/Yasmin/Downloads/chicago.csv")

#getting Libraries
options(repos=structure(c(CRAN="http://cran.r-project.org")))
install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(grid)
library(gridExtra)

#first we need to cut the time into hours as its our main interest.
hour <- function(data) {
  start_time <-
    sapply(strsplit(as.character(data$Start.Time), " "), "[", 2)
  #suset hour from the data
  hour <- substr(x = start_time, 1, 2)
}

##calling hour function for Chicago 
hour_CHI <- hour(CHI)
CHI['hour'] <- hour_CHI
##calling hour function for New York city
hour_NYC <- hour(NYC)
NYC['hour'] <- hour_NYC
##calling hour function for Washington DC
hour_WASH <- hour(WASH)
WASH['hour'] <- hour_WASH

#Second Changing 24 hrs format to 12 (AM or PM) & then subseting the new format into the ‘Hour’ column we had in each city data
old <- c('00','01', '02', '03', '04', '05', '06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
new <- c('12AM', '1AM', '2AM', '3AM', '4AM', '5AM','6AM', '7AM', '8AM', '9AM', '10AM','11AM', '12PM','1PM','2PM','3PM','4PM','5PM','6PM','7PM','8PM','9PM','10PM','11PM')
CHI$hour[CHI$hour %in% old] <-new[match(CHI$hour, old, nomatch = 0)] 

old <- c('00','01', '02', '03', '04', '05', '06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
new <- c('12AM', '1AM', '2AM', '3AM', '4AM', '5AM','6AM', '7AM', '8AM', '9AM', '10AM','11AM', '12PM','1PM','2PM','3PM','4PM','5PM','6PM','7PM','8PM','9PM','10PM','11PM')
WASH$hour[WASH$hour %in% old] <-new[match(WASH$hour, old, nomatch = 0)]

old <- c('00','01', '02', '03', '04', '05', '06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
new <- c('12AM', '1AM', '2AM', '3AM', '4AM', '5AM','6AM', '7AM', '8AM', '9AM', '10AM','11AM', '12PM','1PM','2PM','3PM','4PM','5PM','6PM','7PM','8PM','9PM','10PM','11PM')
NYC$hour[NYC$hour %in% old] <-new[match(NYC$hour, old, nomatch = 0)]


#Preparing for the visualizing the 3 summary tables above
level_order <- c('1AM', '2AM', '3AM', '4AM', '5AM','6AM', '7AM', '8AM', '9AM', '10AM','11AM', '12PM','1PM','2PM','3PM','4PM','5PM','6PM','7PM','8PM','9PM','10PM','11PM','12AM')



#Graph for each of the 3 cities
graph.CHI<-ggplot(data=CHI,aes(x = factor(hour, level = level_order)))+
  geom_histogram(aes(y = ..count..),stat = "count",color='White',fill='dark red')+
  scale_y_continuous(breaks = seq(0,35000,2000))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=1))+
  ggtitle('Chicago counts versus time ')+
  xlab('Hour of starting the trip')+
  ylab('# of users')

graph.NYC<-ggplot(data=NYC,aes(x = factor(hour, level = level_order)))+
  geom_histogram(aes(y = ..count..),stat = "count",color='White',fill='dark blue')+
  scale_y_continuous(breaks = seq(0,35000,2000))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=1))+
  ggtitle('New York city counts versus time ')+
  xlab('Hour of starting the trip')+
  ylab('# of users')

graph.WASH<-ggplot(data=WASH,aes(x = factor(hour, level = level_order)))+
  geom_histogram(aes(y = ..count..),stat = "count",color='White',fill='dark green')+
  scale_y_continuous(breaks = seq(0,35000,2000))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=1))+
  ggtitle('Washonton DC counts versus time ')+
  xlab('Hour of starting the trip')+
  ylab('# of users')

#setting the 3 graphs side by side for a better comparison
grid.arrange(graph.CHI,graph.NYC,graph.WASH,ncol=3)  

#The graph shows the rush hour for each of the cities regardless of the gender or user type, as we are interested here in knowing whether we need to backup some of our stations with extra bikes or the existing is enough already. Surprisingly the 3 cities doens't have the same rush hour.

```

