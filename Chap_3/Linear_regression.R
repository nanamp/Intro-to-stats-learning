# Applied exercises from chapter 3 of introduction to statistical learning
# Focus is on linear regression

library(tidyverse)

#### 8.

auto <- read.csv('Auto.csv')
head(auto,5)


# mpg - miles per gallon
# cylinders - Number of cylinders between 4 and 8
# displacement - Engine displacement (cu. inches)
# horsepower - Engine horsepower
# weight - Vehicle weight (lbs.)
# acceleration - Time to accelerate from 0 to 60 mph (sec.)
# year - Model year (modulo 100) 
# origin - Origin of car (1. American, 2. European, 3. Japanese)
# name - Vehicle name

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


#### 9. 

auto_num <- select(auto, -(name),-(X)) # remove index and names
head(auto_num)

## a. 
pairs(auto_num) # matrix of scatterplots. disp and horsepower correlated, horsepower, weight, acceleration, displacement all correlated with mpg. weight correlated with horsepower

## b.
cor(auto_num) # correlation matrix

## c. 
model_2 <- lm(data = auto_num, mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + factor(origin))
summary(model_2)

# cylinders - not significant
# displacement - significant positive relationship at 1%
# horsepower - not significant
# weight - significant negative relationship
# acceleration - not significant
# year - significant positive relationship
# origin - significant

# coefficient of the year variable suggests that for each year increment, the miles per gallon increases by 0.777 

## d. 
plot(model_2)

# plot of fitted values vs residuals suggests that there might be a non-linear relationship not captured
# QQ plot suggests data is normally distributed?
# No significant outliers. There is one observation with high leverage


## e. 

model_3 <- lm(data = auto_num, mpg ~ cylinders + displacement + 
                horsepower + weight + acceleration + year + factor(origin) + 
                displacement:horsepower)

model_4 <- lm(data = auto_num, mpg ~ cylinders + displacement + 
                horsepower + weight + acceleration + year + factor(origin) + 
                displacement:horsepower + year:weight)

model_5 <- lm(data = auto_num, mpg ~ weight * displacement)

model_6 <- lm(data = auto_num, mpg ~ displacement * year)

model_7 <- lm(data = auto_num, mpg ~ origin * year)

model_8 <- lm(data = auto_num, mpg ~ origin * displacement)

model_9 <- lm(data = auto_num, mpg ~ year * weight)

summary(model_3)
summary(model_4)
summary(model_5)
summary(model_6)
summary(model_7)
summary(model_8)
summary(model_9)


# the interactions between weight/displacement and displacement/year are significant. interaction between origin and year only significant at 10% 
# interaction between origin and displacement significant
# interaction between year and weight is significant


## f. 

# displacement and horsepower look like they have a non-linear relationship with mpg. try log x and x^2

model_10 <- lm(data = auto_num, mpg ~ cylinders + log(displacement) + log(horsepower) + weight + acceleration + year + factor(origin))
summary(model_10)

model_11 <- lm(data = auto_num, mpg ~ cylinders + displacement^2 + horsepower^2 + weight + acceleration + year + factor(origin))
summary(model_11)

model_12 <- lm(data = auto_num, mpg ~ cylinders + sqrt(displacement) + sqrt(horsepower) + weight + acceleration + year + factor(origin))
summary(model_12)
# log transformation seems to make those two significant but not squaring them or finding the square root.