library(tidyverse)
library(dplyr)
#Exercise 1
ggplot(mpg, aes(x=hwy, y=displ))+
     geom_point()
   


ggplot(data=mpg, aes(x=class, y=drv))+
  geom_point()
#this plot isn't useful because all it shows is the tyoes of cars that exist in the set.
#It doesn't show any meaningful relationship between variables, just classes of car that
#exist and the ways they can drive.

#Exercise 1.b

ggplot(mpg, aes(x=hwy, y=displ, color=class))+
  geom_point()
#the cars that fall outside the linear trend are the 2 seaters.  2-seater cars may
#have higher displacement due to the fact that their engines are more powerful, as many sports cars
#are 2 seated vehicles.


#exercise 2

bank <- read.csv("bank.csv")

ggplot(bank, aes(x=balance, y=duration, color=y))+
  xlim(0,3000)+
  geom_point()
  

ggplot(bank, aes(x=campaign, y=previous, color=y))+
  xlim(0,25)+
  geom_point()
  

#These graohs can help to make this marketing campaign more efficient through examining who is
#most worth contacting for longer periods of time, as well as how many times contacting an
#individual can still be useful.  By our first plot, we can see a slight but definite negative
#correlation between duration on the phone with a representative from the bank and balance in
#the client's account.  The clients who decided to increase their long-term depositsare fairly 
#evenly spread through the data, so from this correlation that we observe, we can infer that those
#with more money in their account have a more definite idea of what they want to do with their 
#money.  It is advised to keep this in mind when contacting a client with more money, as it may not
#be worth staying on the phone with them for very long, when that time may be better spent discussing
#increasing long-term deposits with those who have less in their bank.  These individuals seem to
#be willing to stay on the phone longer, so time is better spent here, and decisions may be able
#to be more easily swayed here.  Our second plot shows the relationship between number of times a
#client was contacted during this campaign and how many times they have been contsacted in the past.
#We see that all of the "yes" answers fall in the lower left quadrant of the graph, meaning that,
#after a certain point, forther contact isn't likely to change a "no" to a "yes".  once a client reaches 
#about 16 times of total being contacted, we don't see any "yes" answers beyond this point.
#This should be paid attention to, as after approximately 16 times of contacting a client, their answer
#is unlikely to change if their answer is or has been "no" up to this point.  
#The time spent recontacting these individuals is wasted during the campaign, and those involved
#shoudl focus on those who have not been contacted as often, as most people seem to get on board
#between 3-16 times being contaced.
#

