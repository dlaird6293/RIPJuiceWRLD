library(tidyverse)
library(dplyr)

# Load in data
data <- file.choose("bank.csv")
bank_data <- read.csv(data)

# Extracting out all variables from data
age = bank_data$age
job= bank_data$job
marital= bank_data$marital
education= bank_data$education
default=bank_data$default
balance= bank_data$balance
housing= bank_data$housing
loan= bank_data$loan
contact= bank_data$contact
day= bank_data$day
month= bank_data$month
duration= bank_data$duration
campaign= bank_data$campaign
previous= bank_data$previous
y= bank_data$y

# Check the statistical summaries between all variabes
null_model = lm(balance ~ 1, data = bank_data)
LR <- lm(balance~age+job+marital+education+default+balance+housing+loan+contact+day+duration+
           campaign+previous+y, data=bank_data)
summary(LR)
anova(null_model, LR)

#Plot leading predictor based on 
plot(balance ~ age, data=bank_data)
abline(LR)

#Compute variables from F test
res.ftest <- var.test(balance, age, ratio = 1,
                      alternative = ("two.sided"))
res.ftest

#Discussion 

''' The model shows statistical summaries between the variables given. Significant variables were marked "**" or "***"
after running the linear model into anova. ''''