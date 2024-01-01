# removes all variables stored previously
rm(list=ls()) 

# import Hmisc library
library(Hmisc)

# reading in the data, assigning it to 'data'
data <- read.csv("~/Downloads/COVID19_line_list_data.csv")

# describing data to begin the clean up process
describe(data)

# clean up death column with a dummy column saying not equal to 0 = died
data$death_dummy <- as.integer(data$death !=0) 

# calculating death rate: 0.058
sum(data$death_dummy) / nrow(data)

# claim: people who die from covid are older than people who survive
dead = subset(data, death_dummy == 1) # 63
alive = subset(data, death_dummy == 0) # 1022

mean(dead$age, na.rm = TRUE) 
mean(alive$age, na.rm = TRUE)

# dead age mean = 68,58, alive age mean = 48.07. statistically significant?

t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)
# 0 percent chance that the ages between the two are equal. p value < 0.05
# this is statistically significant

# claim: gender has no effect on people that die from COVID 19
women = subset(data, gender == "female")
male = subset(data, gender == "male")

mean(male$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

# men death rate = 8.5%, women death eate = 3.5%

t.test(women$death_dummy, male$death_dummy, alternative="two.sided", conf.level = 0.95)
# p value = 0.002 < 0.05, reject null hypothesis, statistically significant

# creating a data visualization of the data set
library(ggplot2)


  
  
