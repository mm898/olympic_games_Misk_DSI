#install.packages('skimr')
#install.packages("tidyverse")
#install.packages("janitor")

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

head(olympic_df, 10)



head(sort(table(olympic_df$noc), TRUE),5)

# USA has 18853 participants
# USA won 1358 bronze; 2638 gold; 1641 silver = 5637
USA_won_medals <- data.frame(olympic_df[olympic_df$noc == 'USA' & olympic_df$medal != 'Not win',])

USA_won_medals %>%
  select(medal, year) %>%
  group_by(medal) %>%
  count(year) -> USA_won_medals

table(USA_won_medals$year)

ggplot(data = USA_won_medals,
       mapping = aes(x=year, y=n, color=medal)) + 
  scale_color_manual(values = c("#8B3E2F",'#CD9B1D','#8B8386')) +
  ggtitle("Medals won over the years for USA") +
  geom_line() + facet_grid(rows = vars(medal))



# France has 12758 participants
# France won 666 bronze; 501 gold; 610 silver = 1777
FRA_won_medals <- data.frame(olympic_df[olympic_df$noc == 'FRA' & olympic_df$medal != 'Not win',])

summary(FRA_won_medals$medal)
table(FRA_won_medals$year)

FRA_won_medals %>%
  select(medal, year) %>%
  group_by(medal) %>%
  count(year) -> FRA_won_medals


ggplot(data = FRA_won_medals,
       mapping = aes(x=year, y=n, color=medal)) + 
  scale_color_manual(values = c("#8B3E2F",'#CD9B1D','#8B8386')) + 
  ggtitle("Medals won over the years for France") +
  geom_line() + facet_grid(rows = vars(medal)) 
  


# Great Brittin has 12256 participants
# Great Brittin won 651 bronze; 678 gold; 739 silver = 2068
GBR_won_medals <- data.frame(olympic_df[olympic_df$noc == 'GBR' & olympic_df$medal != 'Not win',])


summary(GBR_won_medals$medal)
table(GBR_won_medals$year)

GBR_won_medals %>%
  select(medal, year) %>%
  group_by(medal) %>%
  count(year) -> GBR_won_medals

ggplot(data = GBR_won_medals,
       mapping = aes(x=year, y=n, color=medal)) + 
  scale_color_manual(values = c("#8B3E2F",'#CD9B1D','#8B8386')) + 
  ggtitle("Medals won over the years for Great Brittin") +
  geom_line() + facet_grid(rows = vars(medal)) 




??geom_point()




# generate EDA report
create_report(olympic_df, 
              output_file = "olympics_EDA_report.html", 
              output_dir = "EDA_reports/")


calories_top[c("category", "item", "calories") ]