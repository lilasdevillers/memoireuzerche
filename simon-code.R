library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)

 
load("base_1896-1906.RData")

base$year <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)


base$pollution <- 0
base$pollution[base$place=="uzerche"|base$place=="vigeois"] <- 1

#######Create variable for the places of interest#######
base$town <- c("other")
base$town[base$place=="condat-sur-ganaveix"] <- c("condat-sur-ganaveix")
base$town[base$place=="espartignac"] <- c("espartignac")
base$town[base$place=="eyburie"] <- c("eyburie")
base$town[base$place=="masseret"] <- c("masseret")
base$town[base$place=="meilhards"] <- c("meilhards")
base$town[base$place=="saint-ybard"] <- c("saint-ybard")
base$town[base$place=="salon-la-tour"] <- c("salon-la-tour")
base$town[base$place=="uzerche"] <- c("uzerche")

sort(unique(base$town))

######################seasonality###################
base$pollution <- 0
base$pollution[base$place=="uzerche"] <- 1
base$pollution[base$place=="vigeois"] <- 1
base$annee <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)

summary(lm(age~pollution+month,base))
summary(lm(age~pollution*month,base))

summary(lm(age~pollution,base))

############variation annuels###########
base$annee <- substr(x = base$date,1,4)
lm(age~pollution*annee,base)

####################Lingère########################
base$pj <- 0
base$pj[base$job == "linger"|base$job == "tanneur"|base$job == "ouvrier papetier"] <- 1
summary(lm(age~pollution*linger*cult, base))

sort(unique(base$job))
 
##################################################

#To fix the problem of ages:
base$age<-round(base$age,0)
###########Data description figures################

#####Time trend: time series first insight
t_trend <- aggregate(age~annee,base,mean)
t_trend$annee <- as.Date(t_trend$annee, format='%Y') 
ggplot(t_trend, aes(x = annee, y = age)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
labs(x='years', y='average death age') + ggtitle ("Time trend in the life time")

#########Population differences: cross-sectional data first insights##########
ggplot(base, aes(x=town, y=age))+
 geom_boxplot()

###number of death by town
ndeath<-data.frame(sort(table(base$town)))
colnames(ndeath)<-c("town","ndeath")

ggplot(ndeath, aes(x=town, y=ndeath)) +
  geom_boxplot()

########## age deaths distributions by town

#number of deaths by age and per town
agextown<-data.frame(table(base$age,base$town))
colnames(agextown)<-c("age","town","nbr")
agextown$age<-as.numeric(agextown$age)

#total number of death in each town
totald<-data.frame(table(base$town))
colnames(totald)<-c("town", "totaldeaths") 

#final table with the number of death per town and age divided by the total
age_distrib<-inner_join(agextown, totald)
age_distrib$percent<- 100*age_distrib$nbr/age_distrib$totaldeaths



ggplot(age_distrib, aes(x=age, y=percent, colour=town))+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(agextown, aes(x=age, y=nbr, colour=town))+
  geom_point()

#if needed
agextown <- filter(agextown, age>5)

##=>>>Try to cumulate the frequencies + find a better fitting model for smoothing


#####Seasonality


#### to improve

ggplot( data= aggregate(age~month,base,mean), aes(x = month, y = age)) +
  geom_point()

ggplot(data=aggregate(age~annee, base, mean), aes(x=annee, y=age))+
  geom_point()+
  geom_smooth(method=lm, y~x, se=FALSE)


