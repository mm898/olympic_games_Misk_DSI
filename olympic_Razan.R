install.packages('skimr')
install.packages("tidyverse")
install.packages("janitor")

#library(tidyverse)
library(skimr)
library(janitor)
library(tidyr)
library(DataExplorer)
library(ggplot2)
#library(reactable)
library(dplyr)

#import dataset
olympic<- read.csv("/home/rstudio/Documents/olympic_games_Misk_DSI/data/athlete_events.csv")

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


#--------------------------------------------------------------------------------------------------

#Total number of sporting events (for winter and summer)

#Unique Olympic sports
unique(olympic_df$sport)

#Number of Unique Sports
length(unique(olympic_df$sport))

#--------------------------------------------------------------------------------------------------

#Table that shows the total number of athletes and events each year
count_year_season <- olympic_df %>%
  group_by(year, season) %>%
  summarize(
    Athletes = length(unique(id)),
    Events = length(unique(event))
  )
count_year_season 


#Line plot that shows the number of sporting events over time
count_year_season %>% 
  ggplot(count_year_season, mapping = aes(x=year, y=Events,color=season)) + geom_line() 

#Line plot that shows the number of sporting events over time
count_year_season %>% 
  ggplot(count_year_season, mapping = aes(x=year, y=Athletes,color=season)) + geom_line() 

#--------------------------------------------------------------------------------------------------

# Top performing female athlete according to the number of medals they won
medals = olympic_df %>%filter(medal !='Not win',season=='Summer')
medals

female_top <- medals%>%group_by(name,sex,sport)%>%filter(sex == 'F')%>%summarize(total=n())%>%arrange(desc(total))%>%head(n=10)

female_top

# Top performing male athlete according to the number of medals they won
male_top <- medals%>%group_by(name,sex,sport)%>%filter(sex == 'M')%>%summarize(total=n())%>%arrange(desc(total))%>%head(n=10)
male_top

#--------------------------------------------------------------------------------------------------

#TOTAL NUMBER OF MEDALS WON BY EACH COUNTRY

country_medal_count <- olympic_df %>%
  filter(medal !='Not win')%>%
  group_by(noc,medal) %>% 
  summarise(Total = n())

country_medal_count 

```
country_medal_count %>%
  ggplot(country_medal_count,mapping = aes(x=noc, y=Total, fill=medal)) +
  geom_col() +
  ggtitle("Total number of Medal won by each country") 

by_country <- olympic_df %>%filter(medal !='Not win')%>% group_by(team) %>% summarise(Total = n())
by_country
```
#--------------------------------------------------------------------------------------------------

#Percentage of the distribution of male and female athletes throughout the years of Olympic history

Gender_Dist <- olympic_df %>%
  group_by(year, sex) %>%
  summarise(total_count=n()) %>% 
  mutate(total_Frequency_count = (total_count / sum(total_count) )*100)

ggplot(Gender_Dist, aes(x=year, y=total_Frequency_count ,color=sex)) + 
  geom_point() 

#--------------------------------------------------------------------------------------------------
