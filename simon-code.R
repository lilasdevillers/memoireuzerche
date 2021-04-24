library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)
####densities
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
###traitements de la base######
base$year <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)


base$pollution <- 0
base$pollution[base$place=="uzerche"|base$place=="vigeois"] <- 1
##we can change this later and just exclude Vigeois


###problem of names
for (town in 1:nrow(base)){
  if (base$place[town]=="condat"){base$place[town]<-"condat-sur-ganaveix"}
}

###delete the soldier
base<-slice(base, -which(base$year==1871))


#######Create variable for the places of interest
base$town <- c("other")
base$town[base$place=="condat-sur-ganaveix"] <- c("condat-sur-ganaveix")
base$town[base$place=="espartignac"] <- c("espartignac")
base$town[base$place=="eyburie"] <- c("eyburie")
base$town[base$place=="masseret"] <- c("masseret")
base$town[base$place=="meilhards"] <- c("meilhards")
base$town[base$place=="saint-ybard"] <- c("saint-ybard")
base$town[base$place=="salon-la-tour"] <- c("salon-la-tour")
base$town[base$place=="uzerche"] <- c("uzerche")
base$town[base$place=="vigeois"] <- c("vigeois")
sort(unique(base$town))





######################seasonality###################
base$pollution <- 0
base$pollution[base$place=="uzerche"|base$place=="vigeois"] <- 1
base$annee <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)

summary(lm(age~pollution+month,base))
summary(lm(age~pollution*month,base))

summary(lm(age~pollution,base))

#interpretation:
#  df<-base %>% filter(base$town!="uzerche"&base$month=="01")
#mean(df$age)
#[1] 31.85155

##################################################

###############filter seasonality#################





###############################################

##############diff-in-diff post#############
base$post<-0
base$post[base$year>1893]<-1

did<-lm(age~pollution*post,base)
summary(did)
###########################################

######illustration with a figure#######create two groups

ctrend <- aggregate(age~year*town,base,mean)
ctrend$group <- "CG"
ctrend$group[ctrend$town=="uzerche"] <- "TG"
ctrend$year <- as.Date(ctrend$year, format='%Y') 
ggplot(ctrend, aes(x = year, y = age, colour= group)) +
  geom_point() + 
  geom_vline(xintercept=ctrend$year[11], linetype="dashed", color="blue")+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Common time trend")+
  labs(y="Average death age", x="Years")+
  theme_update(plot.title=element_text(hjust=0.5))

#This is a totally wrong way to represent it, the linear regression is misspecified (we can set method free and represent se)
#It also shows that observations for the CG are really dispersed than for the TG, the choice of groups doesn't make sense


########Try to find a thiner control group ############
placebo <- aggregate(age~year*town,base,mean)
placebo$group[palcebo$town==] <- "CG"
placebo$group[placebo$town=="uzerche"] <- "TG"
placebo$year <- as.Date(placebo$year, format='%Y') 

placebo <- filter(placebo, placebo$year<1893-04-24)
ggplot(placebo, aes(x = year, y = age, colour= group)) +
  geom_point() + 
  geom_vline(xintercept=placebo$year[11], linetype="dashed", color="blue")+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Common time trend")+
  labs(y="Average death age", x="Years")+
  theme_update(plot.title=element_text(hjust=0.5))


##############################Data description figures###########################
#default theme
#centre title:  
theme_update(plot.title=element_text(hjust=0.5))



#####Time trend: time series first insight
t_trend <- aggregate(age~year,base,mean)
t_trend$year <- as.Date(t_trend$year, format='%Y') 
ggplot(t_trend, aes(x = year, y = age)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
labs(x='years', y='average death age') + ggtitle ("Average death age over time")

#########Population differences: cross-sectional data first insights##########
ggplot(base, aes(x=town, y=age))+
 geom_boxplot()+
  ggtitle("Across towns death age distribution by quartile")



###number of death by town
ndeath<-data.frame(sort(table(base$town), decreasing = TRUE))
colnames(ndeath)<-c("town","ndeath")
ndeaths<-sort()
ggplot(ndeath, aes(x=town, y=ndeath)) +
  geom_boxplot()


########## age deaths distributions by town###################################
#############Density####################
base %>% 
  ggplot(aes(x=age))+
  geom_density(fill="#69b3a2", color="#e9ecef",alpha=0.9, adjust = 0.5)+
  facet_wrap(~town)+
ggtitle("Across town death age distribution")

base %>% 
  ggplot( aes(y=town, x=age, fill=town))+
  geom_density_ridges(alpha=0.6, bandwidth=4)

###############################################


####number of deaths by age and per town########
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

ggplot(age_distrib, aes(x=age, y=percent, colour=town))+
  geom_point()
####################################




##############infant mortality by town#########
#1 years old is a child

esp <- data.frame(matrix(nrow=1, ncol=2))
esp[1,] <- c(1, mean(base$age))
names(esp) <- c("age", "esp")
for (age in (min(base$age):max(base$age))){
  esp <- rbind(esp, c(age, mean(base$age[base$age>=age])))
}
plot(esp$age, esp$esp, type="p")

quantile(base$age)



####gender proportion per town
table(base$gender, base$town)

lmgend<-lm(age~gender, base)

summary(lmgend)



#####Seasonality



