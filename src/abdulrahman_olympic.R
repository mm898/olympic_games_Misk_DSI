install.packages('skimr')
install.packages("tidyverse")
install.packages("janitor")
library("RColorBrewer")

library(tidyverse)
library(skimr)
library(janitor)
library(tidyr)
library(DataExplorer)
library(ggplot2)
library(reactable)
library(GGally)
library(dplyr)
library(forcats)

devtools::install_github("tidyverse/tidyverse")

#import dataset
olympic<- read.csv("data/athlete_events.csv")

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

olympic_df$id <- as.character(olympic_df$id)

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

#--------------------------
#G   Comparing men and women winners over the years?
#Summer
olympic_df %>% 
  filter(medal != "Not win", season == "Summer") %>% 
  group_by(year , sex , medal, season) %>%
  summarise(contestants = length(unique(id))) -> FVSM

ggplot(FVSM ,aes(x = year, y = contestants ,color = sex))+
  geom_point()+
  geom_line( )+
  ggtitle("Summer Men and Womens winners over the years") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(FVSM$medal)

#Winter
olympic_df %>% 
  filter(medal != "Not win", season == "Winter") %>% 
  group_by(year , sex , medal, season) %>%
  summarise(contestants = length(unique(id))) -> FVSMWinter

ggplot(FVSMWinter ,aes(x = year, y = contestants ,color = sex))+
  geom_point()+
  geom_line( )+
  ggtitle("Winter Men and Womens winners over the years") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(FVSMWinter$medal)

#----------------------------------------------------

#h  The most productive age for both male and female athletes in all sorts of sports?
olympic_df %>%
  filter(age >= 10 , age <= 75) %>% 
  group_by(age, sex) %>% 
  summarise(contestants = length(unique(id))) -> fvsminage


ggplot(fvsminage ,aes(x =age ,y = contestants))+
  geom_point()+
  #stat_summary( fun  = "mean")+
  geom_vline(aes(xintercept =23))+
  ggtitle("The most productive age for both male and female athletes") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(fvsminage$sex)
#-----------------------------------------------
#i Is there a relation between athletes' gender and their winning to their olympic game?


olympic_df %>% 
  filter(medal != "Not win") %>%
  group_by(sex , age) %>%
  summarise(contestants = length(unique(id))) -> fvsminevent


fvsminevent %>% 
  arrange(-contestants , n =15) %>% 
  head(15L) -> topevents


olympic_df %>% 
  filter(medal != "Not win", sex == 'F') %>%
  group_by(sex , event , medal) %>%
  summarise(contestants = length(unique(id))) -> Finevent

Finevent %>% 
  arrange(-contestants , n =10) %>% 
  head(30L) -> topFinevent

olympic_df %>% 
  filter(medal != "Not win", sex == 'M') %>%
  group_by(sex , event , medal) %>%
  summarise(contestants = length(unique(id))) -> Minevent

Minevent %>% 
  arrange(-contestants , n =10) %>% 
  head(30L) -> topMinevent


topFinevent %>% 
  ggplot(aes(x =contestants,y=fct_reorder(event,contestants) , fill = medal))+
  geom_col()+
  ggtitle("The most winning events for womens") +
  ylab('Event name')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#8B3E2F",'#CD9B1D','#C1CDC1'))


topMinevent %>% 
  ggplot(aes(x =contestants,y=fct_reorder(event,contestants) , fill = medal))+
  geom_col()+
  ggtitle("The most winning events for men") +
  ylab('Event name')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#8B3E2F",'#CD9B1D','#C1CDC1'))
#-------------------------------------------------------------------------------

# HYPOTHESIS QUESTION


#women grouth
olympic_df %>% 
  filter(sex =='F') %>% 
  group_by(year , sex ,season) %>%
  summarise(contestants = length(unique(id))) -> Fgrouth


ggplot(Fgrouth ,aes(x = year, y = contestants))+
  geom_point()+
  geom_line()+
  ggtitle("Womens contestants through years") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(Fgrouth$season)

#MEN GROUTH VS WOMENS
olympic_df %>%
 # filter(season == 'Summer') %>% 
  group_by(year , sex, season ) %>%
  summarise(contestants = length(unique(id))) -> FVSMinyears


ggplot(FVSMinyears ,aes(x = year, y = contestants ,color = sex ))+
  geom_point()+
  geom_line()+
  ggtitle("Mens and Womens contestants through years in both seasons") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(FVSMinyears$season)

#a