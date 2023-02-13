library(tidyverse)

data <- read_csv("DS-4002-Survey-Results.csv")

data <- data %>% mutate(Hotdog = case_when(`Is a hotdog a sandwich?` == "Yes" ~ 1,
                                   `Is a hotdog a sandwich?` == "No" ~ 0))
table(data$Hotdog)
# 160 people say no, 64 say yes

percent_yes <- 64/(64+160)
percent_yes

#now we can test difference between hypothesis and measured

t.test(data$Hotdog,mu=1/3)
# cannot reject H0 at 95% confidence