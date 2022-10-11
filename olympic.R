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

#take alook of our data
glimpse(olympic)
skim(olympic)
summary(olympic)
str(olympic)

#clean our data
clean_names(olympic)->olympic_df #clean names
#modify type of data
olympic_df$sex <- as.factor(olympic_df$sex)
olympic_df$season <- as.factor(olympic_df$season)
olympic_df$medal <- as.factor(olympic_df$medal)
olympic_df$city <- as.factor(olympic_df$city)
olympic_df$sport <- as.factor(olympic_df$sport)
olympic_df$event <- as.factor(olympic_df$event)
olympic_df$team <- as.factor(olympic_df$team)
olympic_df$noc <- as.factor(olympic_df$noc)

summary(olympic_df)
glimpse(olympic_df)

sum(is.na(olympic_df)) #we have null value
str(olympic_df)


# remove null value 
olympic_df %>% group_by(medal) %>% count(medal)

medalcount <- olympic_df %>% 
  filter(!is.na(medal))%>%
  group_by(medal) %>%
  summarize(count=length(medal))
medalcount

# or we can replace it
ans <- df %>% replace(.=="NULL", "Z") # replace with "Z"

muu<-olympic_df%>%
  replace(.=="NULL",'not_win')
muu
muu %>% group_by(medal) %>% count(medal)
