###################################################
# Group 16
#
# SSC442 Final Project
#
# Visuzlization Code for all fiures found below
###################################################

library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(scales)
#-------------------------------------Begin Setting Up Data--------------------------------------
df <- read.csv("vgsales.csv")

SQ <- df[df$Publisher=="Square Enix",]

sl <- c("EU", "Japan", "North America", "Other_Sales")

tot <- c(32.82,49.88,48.65,13.89)

regionsperperson <- c("EU", "Japan", "North America")
EU <- 446824564 /1000000
JP <-  126543135/1000000
Na <- 368474072 /1000000
salesperperson <- c(32.82/EU,49.88/JP,48.65/Na)

per_person <- data.frame("Location"=regionsperperson, "Sales"=salesperperson)

Saletots <- data.frame("Location"=sl,"Sales"=tot)

SQ_Sales <- data.frame(SQ[sales])

Sale_totals <- data.frame(colSums(SQ_Sales))#

#EU population as of 2017 446,824,564    https://en.wikipedia.org/wiki/Demographics_of_the_European_Union
#JP Population as of 2020 126,543,135    https://www.worldometers.info/world-population/japan-population/
#NA Population as of 2020 368,474,072    https://www.worldometers.info/world-population/northern-america-population/

genre_sales = df %>% group_by(Genre) %>% summarise(GlobalSales = sum(Global_Sales),
                                                   NA_Sales = sum(NA_Sales),
                                                   EU_Sales = sum(EU_Sales),
                                                   JP_Sales = sum(JP_Sales)) 

Enix_sales = SQ %>% group_by(Genre) %>% summarise(GlobalSales = sum(Global_Sales),
                                                  NA_Sales = sum(NA_Sales),
                                                  EU_Sales = sum(EU_Sales),
                                                  JP_Sales = sum(JP_Sales)) 
Enix_sales = melt(Enix_sales)#Use For the showing of popular genres within SQ Enix
names(Enix_sales) = c('Genre','SaleType','Sale')

genre_sales = melt(genre_sales)#Use For the showing of popular genres
names(genre_sales) = c('Genre','SaleType','Sale')

#--------------------------------Finish setting up data------------------------------------------

#FIG 1
ggplot(data=Enix_sales,aes(x = reorder(Genre, -Sale), y = Sale, fill=SaleType)) + 
  geom_bar(stat='identity', position='dodge') +  
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1,angle=45),
        plot.title = element_text(hjust=0.5)) + ## center 
  ggtitle('Square Enix Sales by Genre') + 
  scale_fill_brewer(palette = 'RdYlBu')+xlab("Genre")+
  ylab('Sales')


#FIG2
ggplot(data=genre_sales,aes(x = reorder(Genre, -Sale),y = Sale,fill=SaleType)) +
  geom_bar(stat='identity', position='dodge') +  
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1,angle=45),
        plot.title = element_text(hjust=0.5)) + ## center 
  ggtitle('All Sales by Genre') + 
  scale_fill_brewer(palette='Set1')+xlab("Genre")+
  ylab('Sales')


#FIG 4
sale_plot <- Saletots%>%
  ggplot(aes(x=reorder(Location, -Sales),y=Sales))+
  geom_bar(stat="identity",fill="#b38184")+
  ggtitle("Square Enix Total Sales By Region")+
  xlab("Sales By Location")+ylab("Sales (Millions of units)")+
  theme(plot.title = element_text(hjust = 0.5))
sale_plot


#FIG5
sales_per_person_graph <- per_person%>% 
  ggplot(aes(x=reorder(Location, -Sales),y=Sales))+
  geom_bar(stat="identity",fill="#5ac18e")+
  ggtitle("Unit Sales Per Person")+
  xlab("Sales By Location")+ylab("Sales (Units Per Person)")+
  theme(plot.title = element_text(hjust = 0.5))
sales_per_person_graph



# REGRESSION MODEL #



num_obs = nrow(df)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = df[train_index, ]
test_data = df[-train_index, ]

train_data$Genre.f <- factor(train_data$Genre)
train_data$Platform.f <- factor(train_data$Platform)



fit_0 = lm(Global_Sales ~ 1, data = train_data)
fit_1=lm(Global_Sales ~ (Genre), data = train_data)
fit_2=lm(Global_Sales ~ (Platform), data = train_data)



#RMSE Function
rmse = function(actual, predicted) {
  
  sqrt(mean((actual - predicted) ^ 2))
  
}

better_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

#train RMSE
print(paste0("Fit 1 Train: ", sqrt(mean((train_data$Global_Sales - predict(fit_1, train_data)) ^ 2))))
print(paste0("Fit 2 Train: ", sqrt(mean((train_data$Global_Sales - predict(fit_2, train_data)) ^ 2))))

# test RMSE
print(paste0("Fit 1 Test: ", sqrt(mean((test_data$Global_Sales - predict(fit_1, test_data)) ^ 2))))
print(paste0("Fit 2 Test: ", sqrt(mean((test_data$Global_Sales - predict(fit_2, test_data)) ^ 2))))

# Our RMSE values are both low and close togther in value,so we can argue our model is 
#neither over fitted, or under fitted. 


model_list = list(fit_1, fit_2)
# Using sapply() to get train RMSE and test RMSE

train_rmse = sapply(model_list, better_rmse, data = train_data, response = "Global_Sales")
test_rmse = sapply(model_list, better_rmse, data = test_data, response = "Global_Sales")


#Get Complexity
get_complexity = function(model) {
  length(coef(model)) - 1
}

model_complex = sapply(model_list, get_complexity)

plot(model_complex, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)),
              max(c(train_rmse, test_rmse))),
     xlab = "Model Complexity Size",
     ylab = "RMSE")
lines(model_complex, test_rmse, type = "b", col = "red")

summary(fit_1)
summary(fit_2)
summary(fit_0)








