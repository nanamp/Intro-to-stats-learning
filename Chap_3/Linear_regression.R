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


#### 10. 
# library(ISLR)
# Carseats
# write.csv(Carseats, 'Carseats.csv')

carseats <- read.csv('Carseats.csv')
head(carseats,5)
?Carseats

## a. 
sales_model <- lm(data = carseats, Sales ~ Price + factor(Urban) + factor(US))
summary(sales_model)

## b. 
# Intercept of 13.043469 is the average value of Sales when Price is zero, store is not urban and not in the US
# Price coefficient suggests that sales reduces by 54.5 units for each unit price increase for a non urban non US store
# Urban coefficient suggests that Urban stores have 2% less sales on average
# US coefficient suggess that US stores have 2% more sales on average

## c. 
# Sales = 13.043469 * Price - 0.021916 * Urban-Yes + 1.200573 * US-Yes

## d. 
# will reject the Urban variable since it's p-value is greater than 0.05. even greater than 0.1

## e. 

sales_model_2 <- lm(data = carseats, Sales ~ Price + factor(US))
sales_model_2_sum <- summary(sales_model_2)
sales_model_2_sum

## f. 
# plot a graph to visualize the fit of the model to the data
ggplot(data = carseats, aes(x = Price, y = Sales, color = factor(US))) +
   geom_point() + 
   geom_abline(mapping = aes(intercept = sales_model_2_sum$coefficients[1,1],
                             slope = sales_model_2_sum$coefficients[2,1]),color = 'green') + # US No
   geom_abline(mapping = aes(intercept = sales_model_2_sum$coefficients[1,1] + sales_model_2_sum$coefficients[3,1],
                            slope = sales_model_2_sum$coefficients[2,1]),color = 'red')    # US Yes


plot(sales_model_2)
plot(sales_model)

# the following comments apply to both models
# plot of fitted values against residuals shows no pattern suggesting that the model is a good fit for the data.
# Q-Q plot looks good. points follow the line

## g. 

library(broom)
tidy(sales_model_2, conf.int = TRUE)

## h. 
library(olsrr)
ols_plot_resid_lev(sales_model_2)

# Observations 51, 69 and 377 are possible outliers. Possibly more outlers. Will have to investigate data
# There are several high leverage data points. 


#### 11. 

# In this problem we will investigate the t-statistic for the null hypothesis
# H0 : β = 0 in simple linear regression without an intercept. To
# begin, we generate a predictor x and a response y as follows.
set.seed (1)
x=rnorm(100)
y=2*x+rnorm(100)

## a. 
model_no_int <- lm(y ~ x + 0) # model with no intercept
summary(model_no_int)

# estimate of beta hat = 1.9939
# standard error = 0.1065
# t-statistic = 18.73
# p-value < 2x10^-16
# the p-value is less than 0.01 so we can reject the null hypothesis

## b. 
model_no_int_2 <- lm(x ~ y + 0) # model with no intercept
summary(model_no_int_2)
# estimate of beta hat = 0.39111
# standard error = 0.02368
# t-statistic = 18.73
# p-value < 2x10^-16
# the p-value is less than 0.01 so we can reject the null hypothesis


## c. 
# the t statistics are the same between the two models.
# we find out why in the next section

## d. calculate the t-statistic manually
n <- 100
t_stat <- (sqrt(n-1) * crossprod(x,y)) / 
   (sqrt(crossprod(x) * crossprod(y)- 
   (crossprod(x, y)) ^ 2))
t_stat

## f.
model_int <- lm(y ~ x)
model_int_2 <- lm(x ~ y)
summary(model_int)
summary(model_int_2)

# estimate of beta hat zero = -0.03769 for y on x and 0.03880 for x on y
# estimate of beta hat one = 1.99894 for y on x and  0.38942 for x on y
# t-statistic = 18.56 for both models


#### 12. 
## a. the coefficient of y on x will be the same as the coefficient of x on y if sum of x^2 = sum of y^2.
    
## b. 

set.seed(2)
x <- rnorm(100)
y <- 1.5 * x + rnorm(100)

model_1 <- lm(y ~ x + 0)
model_2 <- lm(x ~ y + 0)

library(broom)
tidy(model_1)
tidy(model_2)

# same t-statistic, different coefficients

## c. 
# if we have a vector x,we can take the elements in x, in a random order to find a vector y and this will satisfy the condition
set.seed(3)
x <- rnorm(100)
y = sample(x)

sum(x^2) == sum(y^2) # this should evaluate to true

model_3 <- lm(y ~ x + 0)
model_4 <- lm(x ~ y + 0)

library(broom)
tidy(model_3)
tidy(model_4)

# we end up getting two models with the same coefficient of 0.0337

#### 13.
## a. 
set.seed(1)
x <- rnorm(100)

## b. 
eps <- rnorm(100, mean = 0, sd = 0.25)

## c.
y <- -1 + 0.5 * x + eps
# y has length 100 and Beta_0 = -1. Beta_1 = 0.5

## d. 
plot(x, y) 
# plot of y against x
# positive correlation between x and y. as x increases, y increases. relationship is strong

## e. 
model_xy <- lm(y ~ x)
tidy(model_xy)


# the estimated coefficiencts are very close to the true coefficients. the intercept is slightly off. -1.01 vs -1
# the p value suggests high significance of the coefficient estimates.

## f. 
model_sum <- summary(model_xy)
int <- model_sum$coefficients[1,1]
sl <- model_sum$coefficients[2,1]

xy_dat <- tibble(x,y)
ggplot(data = xy_dat, aes(x = x , y = y, color = y)) +
   geom_point() +
   geom_abline(aes(slope = sl, intercept = int), color = 'red') +
   labs(color = 'y = -1 + 0.5x + eps')
   

## g.
model_xy2 <- lm(y~(x^2))
tidy(model_xy2)
# no evidence of improvement. the p values and t statistics are the same from before.

## h. 
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 0.1) # less noisy data
y <- -1 + 0.5 * x + eps

model_xy_ln <- lm(y~x)
tidy(model_xy_ln)

# better fit with less noisy data. p values get smaller and t - statistics get larger
# estimate of coefficients also more accurate

## i. 

set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 0.5) # more noisy data
y <- -1 + 0.5 * x + eps

model_xy_mn <- lm(y~x)
tidy(model_xy_mn)

# worse fit with more noisy data. p values get larger and t - statistics get smaller
# estimate of coefficients are less accurate

## j.
tidy(model_xy, conf.int = TRUE)
tidy(model_xy_mn, conf.int = TRUE)
tidy(model_xy_ln, conf.int = TRUE)

# original confidence intervals Beta_0 - > [-1.06, -0.961], Beta_1 -> [0.446, 0.553]
# more noisy confidence intervals Beta_0 - > [-1.12, -0.923], Beta_1 -> [0.393, 0.606]
# less noisy confidence intervals Beta_0 - > [-1.02, -0.985], Beta_1 -> [0.479, 0.521]

# conclusion: the less noisy the data, the narrower the confidence interval.


#### 14.

## a.
set.seed (1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm (100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm (100)

## b. 
cor(x1,x2) # correlation coefficient of 0.83 shows that x1 and x2 are highly correlated.

plot(x1, x2) # plot shows a clear positive correlation


## c.
mod_y_x1_x2 <- lm(y ~ x1 + x2)
tidy(mod_y_x1_x2)

# Beta_0_hat = 2.13
# Beta_1_hat = 1.44  - this is the estimate of the coefficient for x1
# Beta_2_hat = 1.01  - this is the estimate of the coefficient for x2
# estimate are quite different from the actual coefficients in the formula
# suggestion that x2 might not be significant. p value of 0.375
# we can reject the null hypothesis that Beta_1 = 0 at 95% confidence. Can reject at 99%
# we can not reject the null hypothesis that Beta_2 = 0

## d. 
mod_y_x1 <- lm(y ~ x1)
tidy(mod_y_x1)

# the estimated coefficient is now closer to the true coefficient i.e 2.11 vs 2 for intercept and 1.98 vs 2 for x1
# the p value for the coefficient for x1 is also smaller and we can reject the null hypothesis that Beta_1 = 0


## e. 
mod_y_x2 <- lm(y ~ x2)
tidy(mod_y_x2)
# estimate of coefficient for x2 is way off
# the p value is small enough that we can reject the null hypothesis that Beta_2 = 0

## f.
# there is some contradiction. fitting both variables means the impact of x2 is not significant and that we cannot
# reject the null hypothesis that Beta_2 = 0 however fitting x2 alone means that the impact of x2 is significant enough to reject that null
# hypothesis.
# this is probably due to the fact the x1 and x2 are highly correlated and that the impact of x2 can be captured in the model by including x1 alone.

## g.
x1 <- c(x1 , 0.1)
x2 <- c(x2 , 0.8)
y <- c(y, 6)

mod_y_x1_x2 <- lm(y ~ x1 + x2)
tidy(mod_y_x1_x2)

mod_y_x1 <- lm(y ~ x1)
tidy(mod_y_x1)

mod_y_x2 <- lm(y ~ x2)
tidy(mod_y_x2)

plot(x1,x2)
# the new point is an outlier, going by the plot. high x2 given low x1. 
# with the new data point, there is enough evidence to reject the hypotheses for Beta_2 = 0 but not for Beta_1
# this is opposite to what we see without the data point. This data point is high leverage
# plotting x1 and x2 separately against y shows enough evidence to reject both hypotheses.


#### 15.

## a.
library(MASS)
boston <- Boston # save dataframe
?Boston
# response = crime rate per capita by town (crim)
# predictors = proportion of land zoned (zn), proportion of non-retail business acres per town (indus), bounds charles river? (chas)
   # nitrogen oxides concentration (nox), average number of rooms (rm), proportion of houses built prior to 1940 (age)
   # distance to employment centres (dis), access to highways (rad), property tax rate (tax), pupil teacher ratio (ptratio), black population (black)
   # lower status of population (lstat), median value of owner occupied homes (medv)

model_zn <- lm(data = boston, crim ~ zn)
model_indus <- lm(data = boston, crim ~ indus)
model_chas <- lm(data = boston, crim ~ as.factor(chas))
model_nox <- lm(data = boston, crim ~ nox)
model_rm <- lm(data = boston, crim ~ rm)
model_age <- lm(data = boston, crim ~ age)
model_dis <- lm(data = boston, crim ~ dis)
model_rad <- lm(data = boston, crim ~ as.factor(rad))
model_tax <- lm(data = boston, crim ~ tax)
model_ptratio <- lm(data = boston, crim ~ ptratio)
model_lstat <- lm(data = boston, crim ~ lstat)
model_medv <- lm(data = boston, crim ~ medv)


tidy(model_zn) # statistically significant coefficient
plot(boston$zn, boston$crim) # high crime rates for towns with 0 zoning.

tidy(model_indus) # statistically significant coefficient
plot(boston$indus, boston$crim) # high industrialisation, high crime rate.

tidy(model_chas) # not statistically significant

tidy(model_nox) # statistically significant coefficient
plot(boston$nox, boston$crim) # high concentration, high crime rate

tidy(model_rm) # statistically significant coefficient
plot(boston$rm, boston$crim)

tidy(model_age) # statistically significant coefficient

tidy(model_dis) # stat significant
tidy(model_rad) # not significant
tidy(model_tax) # significant
tidy(model_ptratio)  # significant
tidy(model_lstat) # significant
tidy(model_medv) # significant


# in conclusion, most of the predictors have a relationship when plotted alone.

## b. 
model_full <- lm(data = boston, crim ~ zn + indus + factor(chas) + nox + rm + age + dis + factor(rad) + tax + ptratio + lstat + medv)
tidy(model_full)

# using a 95% level, zn, dis, medv are the only variables for which we can reject the null hypothesis that Beta_j = 0

