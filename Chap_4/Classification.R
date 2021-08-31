#### 13.
library(ISLR2)
# write.csv(Weekly, "Weekly.csv")


#A data frame with 1089 observations on the following 9 variables.
#it contains 1, 089weekly returns for 21 years, from the beginning of 1990 to the end of 2010.
#Year - The year that the observation was recorded
#Lag1 - Percentage return for previous week
#Lag2 - Percentage return for 2 weeks previous
#Lag3 - Percentage return for 3 weeks previous
#Lag4 - Percentage return for 4 weeks previous
#Lag5 - Percentage return for 5 weeks previous
#Volume - Volume of shares traded (average number of daily shares traded in billions)
#Today - Percentage return for this week
#Direction - A factor with levels Down and Up indicating whether the market had a positive or negative return on a given week

weekly <- Weekly

## a. 
library(tidyverse)
weekly_summary <- weekly %>%
  group_by(Year) %>%
  summarise(avg_return = mean(Today),avg_lag1 = mean(Lag1), avg_vol = mean(Volume))
weekly_summary

ggplot(data = weekly_summary, aes(x = Year, y = avg_return)) +
  geom_line() + 
  geom_point()

ggplot(data = weekly, aes(x = factor(Year), y = Today, color = factor(Year))) +
  geom_boxplot()

ggplot(data = weekly, aes(x = factor(Year), y = Lag1, color = factor(Year))) +
  geom_boxplot()