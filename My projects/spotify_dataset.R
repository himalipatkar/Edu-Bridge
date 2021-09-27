#Spotify dataset

library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(openintro)
library(tidyverse)
library(scales)
print(getwd)

# read and view the dataset:
data<-read.csv("C:/Users/himali/Documents/spotify_dataset.csv")
print(data)

# check the first and last rows 
print(head(data))
print(tail(data))

# check the no.of rows and columns of the dataset:
print(dim(data))

# check the summary:
print(summary(data))

# check the structure of dataset:
print(str(data))

# check the glimpse of the dataset:
print(glimpse(data))

#  check unique values
print(unique(data))

# statistical values
print(is.na(data))
print(is.data.frame(data))
print(is.name(data))
print(ncol(data))
print(nrow(data))
print(max(data$Streams))
print(min(data$Streams))
print(sort(data$Streams))
print(which.max(data$Streams))
print(which.min(data$Streams))
print(mean(data$Streams))
print(mean(data$Streams,trim=0.10))
print(var(data$Streams))
print(median(data$Streams))
print(mad(data$Streams))# mean absolute division
print(sd(data$AStreams))
print(mode(data$Streams))
print(range(data$Streams))
print(scale(data$Streams))
print(sd(data$Streams/sqrt(length(data$Genre))))
print(max(data$Streams-min(data$Genre)))
print(quantile(data$Streams))
print(quantile(data$Streams,c(0.75)))
print(IQR(data$Streams))
print(t.test(data$Streams))

# plotting of Streams
plot(data$Streams,col="red",xlab="X-axis",ylab="Y-axis",main="Streams")

# plotting of Loudness
plot(data$Loudness,col="red",xlab="X-axis",ylab="Y-axis",main="Loudness")

# Loudness and Danceability
plot(x=data$Loudness,y=data$Danceability,main="Song",xlab="Danceability",ylab="Loudness",col="blue")

# geographical plot of Loudness releated to Danceability
gsplot=data %>% group_by(Loudness) %>% summarise(Danceability)
View(gsplot)

# StreamsvsPopularity analysis
StreamsvsPopularity=data %>% group_by(Streams) %>% summarise(Popularity) %>% arrange((desc
                                                                           (Popularity)))
View(StreamsvsPopularity)

#ploting Song inStreams 
Song=ggplot(data,aes(x=Streams,y=Loudness.Energy,fill=Streams))+geom_col()
print(Song)

# Stream vs Popularity using bargraph:
StreamsvsPopularity=ggplot(data,aes(x=Streams,y=Popularity,fill=Closing))+geom_col()
print(StreamsvsPopularity)








