# Applied exercises from chapter 3 of introduction to statistical learning
# Focus is on linear regression

library(tidyverse)

#### 8.

auto <- read.csv('Auto.csv')

## a. 
model_1 <- lm(mpg ~ horsepower,data = auto)
model_1_sum <- summary(model_1)
print(model_1_sum)

# i. there is a relationship between horsepower and miles per gallon
# ii. the relationship is strong. p value less than 2e^-16
# iii. relationship is negative, higher horsepower means lower miles per gallon
39.935861 - 0.157845 *  98
# iv. for a horsepower of 98, the predicted mpg  = 39.935861 - 0.157845 *  98 = 24.46705

library(broom)
tidy(model_1, conf.int = TRUE)
38.5 - 0.171 * 98
41.3 - 0.145 * 98
# the associated confidence interval for a horspower of 98 is [38.5 - 0.0171*98, 41.3 - 0.0145 * 98] = [21.742, 27.09]

ggplot(data = auto, aes(x = horsepower, y = mpg)) + # plot linear model
  geom_point(color = 'blue')+
  geom_abline(mapping = aes(intercept = model_1_sum$coefficients[1,1], slope = model_1_sum$coefficients[2,1]),color = 'red')


plot(model_1) #diagnostic plots
# plot of residuals vs fitted values shows a curved pattern indicating that the relationship may not be linear
# none of the observations have leverage that is too high and there are no clear outliers
 

