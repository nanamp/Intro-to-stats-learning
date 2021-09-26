library(ISLR)

###### 8.

# write.csv(College, 'College.csv') - > save college dataset

#### a.
college <- read.csv('College.csv')

# Private : Public/private indicator
# Apps : Number of applications received
# Accept : Number of applicants accepted
# Enroll : Number of new students enrolled
# Top10perc : New students from top 10% of high school class
# Top25perc : New students from top 25% of high school class
# F.Undergrad : Number of full-time undergraduates
# P.Undergrad : Number of part-time undergraduates
# Outstate : Out-of-state tuition
# Room.Board : Room and board costs
# Books : Estimated book costs
# Personal : Estimated personal spending
# PhD : Percent of faculty with Ph.D.’s
# Terminal : Percent of faculty with terminal degree
# S.F.Ratio : Student/faculty ratio
# perc.alumni : Percent of alumni who donate
# Expend : Instructional expenditure per student
# Grad.Rate : Graduation rate

#### b. 
fix(college) # view the data. allows you to see all the data in a pop-up

rownames (college )= college [,1] # assign row names to each row using the university names
college = college[,-1]
fix(college)

#### c. 
library(tidyverse)

## i. 
summary(college) # numerical summary of the variables

## ii. 
coll <- college[,1:10] %>% # subset first ten columns
  mutate(Private = ifelse(Private == "Yes",1,0)) # convert Private column to numberic
pairs(coll) # matrix of scatterplots for first ten columns

## iii. 
ggplot(data = college, aes(x = Private, y = Outstate, color = Private)) + 
  geom_boxplot()    # boxplot of Outstate vs Private - color by yes/no

# Private schools charge a higher out of state tuition on average

## iv. 
college <- mutate(college, Elite = ifelse(Top10perc > 50, "Yes", "No"))
head(college,5)

college %>%
  count(Elite) %>%
  group_by(Elite) # there are 78 Elite universities

ggplot(data = college, aes(x = Elite, y = Outstate, color = Elite)) + 
  geom_boxplot()    # boxplot of Outstate vs Elite - color by yes/no

# Elite schools charge a higher out of state tuition on average.


## v.

library(cowplot)
p1 <- ggplot(data = college) +
         geom_histogram(mapping = aes(x = Apps), binwidth = 1500) 

p2 <- ggplot(data = college) +
         geom_histogram(mapping = aes(x = Accept), binwidth = 500) 

p3 <-  ggplot(data = college) +
          geom_histogram(mapping = aes(x = Enroll), binwidth = 500) 
  
p4 <-  ggplot(data = college) +  
          geom_histogram(mapping = aes(x = F.Undergrad), binwidth = 2000)

p5 <- ggplot(data = college) +
          geom_histogram(mapping = aes(x = Outstate), binwidth = 2000)

plot_grid(p1,p2,p3,p4,p5)

p6 <- ggplot(data = college) +
  geom_histogram(mapping = aes(x = Room.Board), binwidth = 1000) 

p7 <- ggplot(data = college) +
  geom_histogram(mapping = aes(x = Personal), binwidth = 1000) 

p8 <-  ggplot(data = college) +
  geom_histogram(mapping = aes(x = perc.alumni), binwidth = 10) 

p9 <-  ggplot(data = college) +  
  geom_histogram(mapping = aes(x = Grad.Rate), binwidth = 10)

p10 <- ggplot(data = college) +
  geom_histogram(mapping = aes(x = Books), binwidth = 100)

plot_grid(p6,p7,p8,p9,p10)
  
# out of the first five variables, only Outstate seems to be centrally skewed. Rest seem to be left skewed
# out fo the second five, Grad.Rate seems to be right skewed. 


##### 9. 
# write.csv(Auto, 'Auto.csv')

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
auto <- read.csv('Auto.csv')
head(auto,5) # origin, year and name are qualitative. mpg, cylinders, displacement, horsepower, weight and acceleration are quantitative

## b.
range(auto$cylinders)
range(auto$displacement)
range(auto$horsepower)
range(auto$weight)
range(auto$acceleration)
range(auto$mpg)

## c.
summary(auto) # find the mean and sd of the variables.

## d. 
auto_filt <- filter(auto, row_number() < 10 | row_number() > 85) # remove rows 10 through 85
summary(auto_filt)

# mean of mpg increases, mean of displacement reduces. ...


## e. 

ggplot(data = auto, aes(x = weight, y = mpg, color = as.factor(year))) +
  geom_point()  # visualize miles per gallon against weight. color by year. generally, the heavier the car, the lower the mpg. newer cars also tend to have higher mpg for the same weight


ggplot(data = auto, aes(x = as.factor(cylinders), y = weight, color = as.factor(cylinders))) +
  geom_boxplot()  # seems to be a trend where the more cylinders there are, the higher the weight. trend slightly breaks between 1 and 2 cylinders


ggplot(data = auto, aes(x = as.factor(cylinders), y = acceleration, color = as.factor(cylinders))) +
  geom_boxplot() # here, would have expected the time for acceleration to 60 to decrease steadily with number of cylinders, but that is not the case


ggplot(data = auto, aes(x = weight, y = acceleration)) +
  geom_point() # there might be a negative correlation here. higher the weight, the lower the time to 60, but not very strong. also surprising

ggplot(data = auto, aes(x = horsepower, y = acceleration)) +
  geom_point() # strong correlation here. higher the horsepower, the lower the time the time to 60. as expected.

ggplot(data = auto, aes(x = displacement, y = acceleration)) +
  geom_point() # some negative correlation here. as above.

# now displacement with horsepower
ggplot(data = auto, aes(x = displacement, y = horsepower)) +
  geom_point() # as expected, the higher the displacement, the higher the horsepower.


ggplot(data = auto, aes(x = displacement, y = mpg)) +
  geom_point() # the higher the displacement, the lower the efficiency

ggplot(data = auto, aes(x = as.factor(year), y = mpg, color = as.factor(year))) +
  geom_boxplot() # generally, the newer the car, the better the efficiency, but there are some exceptions. not a strong correlation.


ggplot(data = auto, aes(x = as.factor(origin), y = mpg, color = as.factor(origin))) +
  geom_boxplot() # efficiency goes up from american to european to japanese

# origin, horsepower/displacement, year, weight can be good predictors for mpg


###### 10.
# library(MASS)
# write.csv(Boston, 'Boston.csv')

## a.
boston <- read.csv('Boston.csv')
head(boston,5)

?Boston

## b. 
pairs(boston) # pairwise scatterplots of the data

# the correlations that stand out are:
  # lower status and median value of owner-occupied homes
  # mean distance to employment centres and nitrogen oxide concentration
  # average number of rooms and  (both median value of homes and lower status of population)
  # nitrogen oxide concentration and proportion of non-retain business acres per town
# all these make real world sense

## c. check if any of the predictors are associated with per capita crime rate.
   # rad and chas are qualitative, rest are quantitative.

g1 <- ggplot(data = boston, aes(x = as.factor(rad), y = crim, color = as.factor(rad))) +
        geom_boxplot()
g2 <- ggplot(data = boston, aes(x = as.factor(chas), y = crim, color = as.factor(chas))) +
        geom_boxplot()   
g3 <- ggplot(data = boston, aes(x = zn, y = crim)) +
        geom_point()
g4 <- ggplot(data = boston, aes(x = indus, y = crim)) +
        geom_point()
g5 <- ggplot(data = boston, aes(x = nox, y = crim)) +
        geom_point()
g6 <- ggplot(data = boston, aes(x = rm, y = crim)) +
        geom_point()
g7 <- ggplot(data = boston, aes(x = age, y = crim)) +
        geom_point()
g8 <- ggplot(data = boston, aes(x = dis, y = crim)) +
        geom_point()
g9 <- ggplot(data = boston, aes(x = tax, y = crim)) +
        geom_point()
g10 <- ggplot(data = boston, aes(x = ptratio, y = crim)) +
        geom_point()
g11 <- ggplot(data = boston, aes(x = black, y = crim)) +
        geom_point()
g12 <- ggplot(data = boston, aes(x = lstat, y = crim)) +
        geom_point()
g13 <- ggplot(data = boston, aes(x = medv, y = crim)) +
        geom_point()

plot_grid(g1,g2,g3,g4)
plot_grid(g5,g6,g7,g8)
plot_grid(g9,g10,g11,g12,g13)

# there might be a relationship with age and dis. positive with age, negative with dis. not linear, some transformation required
# possible relationship with chas, no visible relationship with rm. possible relationship with lstat and medv. positive with lstat, negative with medv
# no visible relationship with black
# some towns with rad of 24 have particularly high crime rates

ggplot(data = boston, aes(x = medv, y = ptratio)) +
  geom_point() # possible relationship. pupil teacher ration drops the higher the median house value.

## d.
filter(boston, ptratio > 20) # select towns with ptratio > 20 - 

ggplot(data = boston, aes(x = black, y = ptratio)) +
  geom_point() # there are some towns with high pupil teach ratios. mostly black towns


## e.
boston_filt <- filter(boston, chas == 1) # select towns that bound Charles River - there are 35 of them
boston_filt

## f. 
summary(boston$ptratio)
summary(boston_filt$ptratio)

# median pupil teacher ratio is 19.05 for whole dataset and 17.6 for towns that bound Charles river

## g.
filter(boston, medv == min(medv)) # select towns with the lowest median house value

summary(boston)
# these have higher than median crime rates, median zn, about median levels of black pop, lower than median average rooms, higher than median tax rates

## h.
filter(boston, rm > 7) # select towns with more than 7 rooms per dwelling on average

filter(boston, rm > 8) # select towns with more than 8 rooms per dwelling on average
# very low crime rates, only two out of 13 bound the charles river, lower than overall median pupil teacher ratio
