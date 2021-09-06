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


## b.
glm_1 <- glm(data = weekly, Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = "binomial")
summary(glm_1)
# seems like only the Lag2 variable is significant at 95%

# get the predicted probability in our dataset using the predict() function
pred_prob <- predict(glm_1,weekly, type = "response")

# create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main data frame
weekly$pred_dir <- ifelse(pred_prob >= 0.5, "Up", "Down")

# confusion matrix
table(weekly$Direction,weekly$pred_dir, dnn=c('True Status','Predicted Status')) # confusion matrix

# 430 cases where we predicted up instead of down, 48 or the reverse
# 557 cases where we predicted up correctly
# 54 cases where we predicted down correctly

## c.
weekly_sub <- filter(Weekly, Year >= 1990, Year <= 2008) # subset the data
weekly_rem <- filter(Weekly, Year >= 2009)  # remaining data
glm_2 <- glm(data = weekly, Direction ~ Lag2 , family = "binomial")
pred_prob_2 <- predict(glm_2,weekly_rem, type = "response")
weekly_rem$pred_dir <- ifelse(pred_prob_2 >= 0.5, "Up", "Down")
table(weekly_rem$Direction,weekly_rem$pred_dir, dnn=c('True Status','Predicted Status')) # confusion matrix

# 34 cases where we predicted up instead of down, 5 or the reverse
# 56 cases where we predicted up correctly
# 9 cases where we predicted down correctly