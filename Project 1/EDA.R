####### EDA ###########

library(tidyverse)
tweets <- read.csv("C:/Users/Student/OneDrive - University of Virginia/Documents/Data Science/DS-4002/rlq3fm-DS-4002/Project 1/tweets_new.csv")

rate_summary <- tweets %>% group_by(election_period) %>% 
  summarize(total = n()) %>%
  mutate(num_months = case_when(election_period=="Pre" ~ interval("2009-05-04","2015-06-16") %/% months(1),
                                election_period == "During" ~ interval("2015-06-16","2016-11-08") %/% months(1),
                                election_period == "Post" ~ interval("2016-11-08","2021-01-08") %/% months(1))) %>%
  mutate(tweets_per_month = total/num_months)

#tweets per month
ggplot(rate_summary, aes(x=election_period,y=tweets_per_month)) +geom_col(fill="cornflowerblue") + geom_text(aes(label=round(tweets_per_month)),vjust=1.5,color="white")

# total tweets
ggplot(tweets, aes(x=election_period)) + geom_bar(fill="cornflowerblue") + geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

# proportion of tweets containing at least 1 word/phrase in our hypothesis
pie(table(tweets$contains_word),labels = c("34598 FALSE","10843 TRUE"), col=c("cadetblue1","cornflowerblue"))
table(tweets$contains_word)

# summary table for total count of tweets with each phrase
cols = c("wall","fake_news","media","democrat","great","proud_boys")
df <- tweets %>% summarize(sum(wall),sum(fake_news),sum(media),sum(democrat),sum(great),sum(proud_boys))


my_theme <- theme_bw() + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size=14))

# order levels to change order of plots to Pre -> During -> Post
tweets$election_period <- factor(tweets$election_period, 
                                          levels = c("Pre", "During", "Post"))

tweets <- tweets %>% mutate(trump = if_else(str_detect(text, "trump"),1,0))
tweets$contains_word <- as.factor(tweets$contains_word)

plot2 <- ggplot(tweets) +
  geom_bar(aes(x = contains_word, fill = election_period), position = "fill", color = "black") +
  scale_fill_manual("Election Period", values = c("Pre" = "lightblue", "During" = "navyblue",
                                                  "Post" = "blue")) +
  labs(x = "words present in Tweet", y = "Proportion of Tweets") +
  my_theme
plot2

plot3 <- ggplot(tweets) +
  geom_bar(aes(x = as.factor(trump), fill = election_period), position = "fill", color = "black") +
  scale_fill_manual("Election Period", values = c("Pre" = "lightblue", "During" = "navyblue",
                                                  "Post" = "blue")) +
  labs(x = "'Trump' present in Tweet", y = "Proportion of Tweets") +
  my_theme
plot3


