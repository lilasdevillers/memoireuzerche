library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)
library(xtable)
library(stargazer)
inta####densities
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
###traitements de la base######
base$year <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)
save(base,file = "uzerche_utlime.RData")


base$pollution <- 0
base$pollution[base$place=="uzerche"|base$place=="vigeois"] <- 1
##we can change this later and just exclude Vigeois


###problem of names
for (town in 1:nrow(base)){
  if (base$place[town]=="condat"){base$place[town]<-"condat-sur-ganaveix"}
}

###delete the soldier and other 
base<-subset(base, base$year>=1883)


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
sort(unique(base$town))





######################seasonality###################
ubase$pollution <- 0
ubase$pollution[base$town=="uzerche"] <- 1
base$annee <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)

#tacking the error into account
ourbase<-subset(base, base$year>=1894)
#
lm.season<-lm(age~pollution*month,ourbase)
rseason<-lm(age~pollution, ourbase)
stargazer(lm.season, rseason, single.row = TRUE)
summary.lm(lm.season)
summary.lm(rseason)
#uzerche only
ubase<-subset(ourbase, ourbase$pollution==1)
summary(lm(age~month,ubase))
#interpretation:
#  df<-base %>% filter(base$town!="uzerche"&base$month=="01")
#mean(df$age)
#[1] 31.85155

#a comparison 

#############Infant seasonality#######################
infant<-subset(base, base$age<=1)
summary.lm(lm(age~pollution*month,infant))

non.infant<-subset(base, base$age>1)
summary.lm(lm(age~pollution*month,non.infant))
non.infant.elder<-subset(non.infant, non.infant$age<=64)
summary.lm(lm(age~pollution*month,non.infant.elder))

###############death seasonality#################
season<-table(base$pollution, base$month)
season<-data.frame(season)
colnames(season)<-c("TG", "month", "deaths")
RowSums(season[,2:3])

stargazer()
table(base$pollution)
for (i in (1:nrow(season))){
  if (season[i,1]==0){season[i,3]<-100*season[i,3]/4838}
  else{season[i,3]<-100*season[i,3]/2035}
}
season$deaths<-round(season$deaths)
as.table(season$month, season$TG)
xtable(season, summary=FALSE, rownames=FALSE)

###############yearly analysis#########################
summary(lm(age~pollution*year, base))
#this confirm something weird happenned in 1901, and it affect more other towns
death.df<-as.data.frame(table(base$pollution, base$year))
colnames(death.df)<-c("TG", "year", "deaths")
ggplot(death.df, aes(x=year, y=deaths, colour=TG))+
  geom_point()




###error in data collection
ourbase<-subset(base, base$year>1894)
our<-table(ourbase$month)
our<-as.data.frame(our)
colnames(our)<-c("month","deaths")

theirbase<-subset(base, base$year<1894)
their<-table(theirbase$month)
their<-as.data.frame(their)
colnames(their)<-c("month","deaths")

our$their<-their$deaths

colnames(our)<-c("month", "1894-1906", "1883-1893")
stargazer(our, summary=FALSE, rownames=FALSE)

#################gauthier dernière réunion##############################
#année de naissance
base$adn<-as.numeric(base$year)-base$age
#durée d'exposition à la tan2 1896
base$expos<-as.numeric(base$year)-1896
base$expos1<-0
base$expos1[base$expos>0]<-base$expos[base$expos>0]
summary(lm(age~expos1+I(expos1^2),base))

##############diff-in-diff#############
base$post1893<-0
base$post1893[base$year>=1893]<-1

did<-lm(age~pollution*post1893,base)
summary(did)

stargazer(did, type="latex")



#############Diff-in-Diff####CT assumption doesn't hold
base$post1896<-0
base$post1896[base$year>=1896]<- 1
did2<-lm(age~pollution*post1896,base)
summary.lm(did2)

stargazer(did2, type="latex")

#############Diff-in-Diff excluding the period after 1896##############################
base$post1893<-0
base$post1893[base$year>=1893]<-1
control.base1<-subset(base,base$year<1896)
did3<-lm(age~pollution*post1893,control.base1)
summary.lm(did3)

stargazer(did3, type="latex")

base<-subset(base, base$town!="vigeois")
##########excluding the periode 1893-1896######## FINAL ONE
base$post1896<-0
base$post1896[base$year>=1896]<- 1
control.base2<-subset(base,base$year>=1896|base$year<1893)
did4<-lm(age~pollution*post1896,control.base2)
summary.lm(did4)

stargazer(did4, type="latex")


######Diff-in-Diff: Placebo test########
base$postplac<-0
base$postplac[base$year>=1888]<- 1
placebo<-subset(base, base$year<1893)
did5<-lm(age~pollution*postplac,placebo)
summary.lm(did5)


#####comparison by cutting the sample#####





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

######two treaments over time which we assume to be of the "same kind"

names(base)[names(base) == "pollution"] <- "T"

#people exposed to the pollution of the paper mill only
base$paper<-0
base$paper[base$year>=1893]<-1
base$paper[base$year>=1896]<-0
#people who were exposed in their lifetime to both pollutions
base$papertan<-0
base$papertan[base$year>=1896]<-1



cum_did<-lm(age~T*(paper + papertan), base)

summary.lm(cum_did)



#################Two diff-in-diff#######################
base$post<-0
base$post[base$year>1893]<-1

did1<-lm(age~pollution*post,base)
summary(did1)

base$post<-0
base$post[base$year>1896]<-1

did2<-lm(age~pollution*post,base)
summary(did2)
  

##############################Data description figures###########################
#default theme
#centre title:  
theme_update(plot.title=element_text(hjust=0.5))



#####Time trend: time series first insight
t_trend <- aggregate(age~year,base,mean)
t_trend$year <- as.Date(t_trend$year, format='%Y') 
t_trend$nbr<-table(base$year)
ggplot(t_trend, aes(x = year, y = age)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  geom_line(aes(y=age), color ="black")+
labs(x='Years', y='Average death age') + ggtitle ("Average death age over time")

ggplot(t_trend, aes(x=year, y=nbr))+
  geom_point()+
  geom_line(aes(y=nbr))+
  geom_smooth(se=FALSE)

####First differences to stationarity
t_trend$fd<-diff(t_trend$age, lag = 1, differences = 1)
 
t_trend$nbr<-table(base$year)
#########Population differences: cross-sectional data first insights##########
ggplot(base, aes(x=town, y=age))+
 geom_boxplot()+
  ggtitle("Across towns death age distribution by quartile")+
labs(x='Towns', y='Death age')


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
ggtitle("Across town death age distribution")+
labs(x='Death age', y='Density')

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



