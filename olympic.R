install.packages('skimr')
install.packages("tidyverse")
install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)
library(tidyr)
library(DataExplorer)
library(ggplot2)
library(reactable)
library(GGally)
library(dplyr)

#import dataset
olympic<- read.csv("athlete_events.csv")

#take a look of our data
glimpse(olympic)
skim(olympic)
summary(olympic)
str(olympic)

#clean our data
clean_names(olympic)->olympic_df #clean names

#modify type of data
olympic_df$sex <- as.factor(olympic_df$sex)
olympic_df$season <- as.factor(olympic_df$season)
olympic_df$city <- as.factor(olympic_df$city)
olympic_df$sport <- as.factor(olympic_df$sport)
olympic_df$event <- as.factor(olympic_df$event)
olympic_df$team <- as.factor(olympic_df$team)
olympic_df$noc <- as.factor(olympic_df$noc)

summary(olympic_df)
glimpse(olympic_df)
str(olympic_df)

#To see if we have null value
sum(is.na(olympic_df)) #we have null value



#remove NULL value in medal 
#the NULL mean not winning any medals
olympic_df %>% 
  group_by(medal) %>% 
  count(medal)

olympic_df$medal[is.na(olympic_df$medal)]="Not win"
table(olympic_df$medal)

#modify type of data
olympic_df$medal <- as.factor(olympic_df$medal)
#take a look
summary(olympic_df)
