library(ISLR)
# write.csv(College, 'College.csv') - > save college dataset

#### a.
college = read.csv('College.csv')

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




  


