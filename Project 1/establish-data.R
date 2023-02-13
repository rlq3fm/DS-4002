######################################
### Trump twitter data exploration ###
#####################################

library(tidyverse)
library(stringr)
library(lubridate)

tweets <- read.csv("C:/Users/Student/OneDrive - University of Virginia/Documents/Data Science/DS-4002/Project 1/tweets.csv")
tweets <- tweets[1:(length(tweets)-2)]

# Data cleaning

## make all tweets lowercase, get rid of rt's
tweets <- tweets %>%
  filter(isRetweet == "f")

tweets$text <- tolower(tweets$text)

tweets$text <- gsub("https\\S*", "", tweets$text)
tweets$text <- gsub("@\\S*", "", tweets$text)
tweets$text <- gsub("amp", "", tweets$text)
tweets$text <- gsub("[\r\n]", "", tweets$text)
tweets$text <- gsub("[[:punct:]]", "", tweets$text)

## getting rid of empty rows
tweets <- tweets %>%
  filter(text != "")

words <- c("wall","fake news","media","democrat","great","proud boys")

tweets <- tweets %>% mutate(contains_word = case_when(str_detect(tweets$text, "wall") == TRUE ~ 1,
                                             str_detect(tweets$text, "fake news") == TRUE ~ 1,
                                             str_detect(tweets$text, "media") == TRUE ~ 1,
                                             str_detect(tweets$text, "democrat") == TRUE ~ 1,
                                             str_detect(tweets$text, "great") == TRUE ~ 1,
                                             str_detect(tweets$text, "proud boys") == TRUE ~ 1))

tweets["contains_word"][is.na(tweets["contains_word"])] <- 0
table(tweets$contains_word)

# now a column for each word:
tweets <- tweets %>%
  mutate(wall = if_else(str_detect(text, "wall"),1,0),
         fake_news = if_else(str_detect(text, "fake news"),1,0),
         media = if_else(str_detect(text, "media"),1,0),
         democrat = if_else(str_detect(text, "democrat"),1,0),
         great = if_else(str_detect(text, "great"),1,0),
         proud_boys = if_else(str_detect(text, "proud boys"),1,0))

# split into separate day and time columns
tweets <- separate(data=tweets, col=date, c("day", "time"), sep = " ")

# Adding PRE/POST/DURING election

class(tweets$day)
tweets$day <- dmy(tweets$day)
class(tweets$day)

tweets <- tweets %>%
  mutate(election_period = as.factor(case_when(day < "2015-06-16" ~ "Pre",
                                     (day >=  "2015-06-16" & day < "2016-11-08") ~ "During",
                                     day >= "2016-11-08" ~ "Post")))


# drop unnecessary columns
drop <- c("id","isRetweet","device","isFlagged")
tweets <- tweets[,!(names(tweets) %in% drop)]

ggplot(tweets, aes(x=election_period)) + geom_bar() + geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

pie(table(tweets$contains_word),labels = c("34598 FALSE","10843 TRUE"))
table(tweets$contains_word)
