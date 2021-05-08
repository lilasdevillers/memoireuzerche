#Final program
#Libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)
library(xtable)
library(stargazer)
#libraries for density
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
#library for maps
library(raster)
library(RColorBrewer)
library(classInt)
library(maptools)
library("rgdal")
library("maps")
library("rgeos")

#############Cleaning program###################
base <- read.csv("uzerche_ultime.csv", sep=";")
base <-base[,c(1:6)]
colnames(base) <- c("name","gender","job","age","place","date")
names(base) <- tolower(names(base))
base[,c(1)] <- tolower(base$name) ; base[,c(3)] <- tolower(base$job) ; base[,c(5)] <- tolower(base$place)
base$name <- iconv(base$name, to="ASCII//TRANSLIT") ; base$job <- iconv(base$job, to="ASCII//TRANSLIT") ; base$place <- iconv(base$place, to="ASCII//TRANSLIT")
base$gender <- as.numeric(base$gender)
base$age <- as.numeric(base$age)
base$date <- paste(substr(base$date, 7,11), substr(base$date, 4,5),  substr(base$date, 1,2), sep="-")
base$date <- as.Date(base$date, format="%Y-%m-%d")
base$year <- substr(x = base$date,1,4)
base$year <- as.numeric(base$year)
base$month <- substr(x = base$date,6,7)
base$month <- as.numeric(base$month)
summary(base)

#Standardizing city's names
i<-1
for(i in (1:nrow(base)) ){
  if(base[i,5]=="salon"){base[i,5] <- "salon-la-tour"}
}
i<-1
for(i in (1:nrow(base)) ){
  if(base[i,5]=="st ybard"){base[i,5] <- "saint-ybard"}
  if(base[i,5]=="saint ybard"){base[i,5] <- "saint-ybard"}
}
i<-1
for(i in (1:nrow(base)) ){
  if(base[i,5]=="condat"){base[i,5] <- "condat-sur-ganaveix"}
}

#Deleting lines with NA's
i<-1
for (i in (1:nrow(base)) ){
  for(j in (0:10)){
    if(is.na(base[i+j,2])){base <- slice(base,-(i+j))} # slice is from dplyr
    if(is.na(base[i+j,4])){base <- slice(base,-(i+j))}
    if(is.na(base[i+j,6])){base <- slice(base,-(i+j))}
  }
}

#Deleting the soldier and other lately registered deaths 
base<-subset(base, base$year>=1883)

#Deleting Vigeois
base<-subset(base, base$place!="vigeois")

#Creating categories for main towns
base$town <- c("other")
base$town[base$place=="condat-sur-ganaveix"] <- c("condat-sur-ganaveix")
base$town[base$place=="espartignac"] <- c("espartignac")
base$town[base$place=="eyburie"] <- c("eyburie")
base$town[base$place=="masseret"] <- c("masseret")
base$town[base$place=="meilhards"] <- c("meilhards")
base$town[base$place=="saint-ybard"] <- c("saint-ybard")
base$town[base$place=="salon-la-tour"] <- c("salon-la-tour")
base$town[base$place=="uzerche"] <- c("uzerche")

#Factors for characther vectors
place_factor <- factor(x=base$place)
job_factor <- factor(x=base$job)
gender_factor <- factor(x = base$gender)
levels(place_factor) ; levels(job_factor) ; levels(gender_factor)

#################Maps of Corrèze######################
correze <- readOGR(dsn="19-correze", layer="19-")
correze <- spTransform(correze, CRS("+proj=longlat"))
correze@data$id <- rownames(correze@data)

#Which area is interesting for us ?
plot(correze, col = "lightgrey",main="Map of Correze",cex=2)
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24|correze@data$CODE_CANT==22
#29 --> Canton of Vigeois, 24 --> Canton of Treignac, 28 --> Canton of Uzerche
plot(correze[canton, ], col = "turquoise", add = TRUE)
legend(x=1.039107,y=45.37074,legend=c("Cantons of Uzerche, Vigeois et Seilhac"),
       fill="turquoise", cex=1.4, bty="n")
plot(correze[correze@data$CODE_CANT==28,],col = "#8DD3C7", add=TRUE)
legend(x=2.458050,y=45.30846,legend=c("Cantons of Uzerche"),
       fill="#8DD3C7", cex=1, bty="n")
plot(correze[correze@data$CODE_CANT==29,],col = "#FFFFB3", add=TRUE)
legend(x=2.458050,y=45.21197,legend=c("Cantons of Vigeois"),
       fill="#FFFFB3", cex=1, bty="n")
plot(correze[correze@data$CODE_CANT==24,],col = "#BEBADA", add=TRUE)
legend(x=2.458050,y=45.10025,legend=c("Cantons of Treignac"),
       fill="#BEBADA", cex=1, bty="n")
points(coordinates(correze[correze@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]), 
       pch=20, col="red", cex=1)

#Focusing on this area and adding Vezere
plot(correze[canton,],main="Map of the canton of Uzerche, Vigeois and Treignac")
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]), pch=20, 
       col=c("black","black","black","black","black","black","black","black","black","black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]),c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),cex=0.5, pos=4)
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","TREIGNAC","UZERCHE"),]),c("VIGEOIS","TREIGNAC","UZERCHE"),cex=0.5, pos=4,col="red")

river <- locator(n=15) # hand drawing
lines(river,col="lightblue1",lwd=2)
legend(x=1.862834,y=45.45924,legend=c("The Vézère"),
       col ="lightblue1", lty=1:2, cex=1, bty="n")
legend(x=1.862834,y=45.40604,pch=20,legend=c("Towns not on the Vézère","Towns on the Vézère"),
       col =c("black","red"), cex=1, bty="n")

#Adding data on the map
base$place <- str_to_upper(base$place)
correze2 <- correze[canton,]
VILLE <- c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR","UZERCHE","TREIGNAC")
date <- c(1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906)
d <-1
v <- 1
i <- 1
ncol_correze2 <- ncol(correze2@data)
for(d in (1:length(date))){
  correze2@data[,ncol_correze2+d] <- "NA"
  correze2@data[,ncol_correze2+d] <- as.numeric(correze2@data[,ncol_correze2+d])
  for(v in (1:length(VILLE))){
    for(i in (1:nrow(correze2@data))){
      if(correze2@data$NOM_COMM[i]==VILLE[v]){
        correze2@data[i,ncol_correze2+d] <- nrow(base[base$place==VILLE[v]&base$annee==date[d],])
      }
    }
  }
}
names(correze2) <- c("ID_GEOFLA", "CODE_COMM", "INSEE_COM", "NOM_COMM","STATUT", "X_CHF_LIEU" ,"Y_CHF_LIEU", "X_CENTROID","Y_CENTROID","Z_MOYEN" ,  "SUPERFICIE","POPULATION","CODE_CANT", "CODE_ARR" ,"CODE_DEPT", "NOM_DEPT","CODE_REG", "NOM_REGION","id","DEAD1894", "DEAD1895", "DEAD1896", "DEAD1897", "DEAD1898", "DEAD1899", "DEAD1900", "DEAD1901", "DEAD1902", "DEAD1903", "DEAD1904", "DEAD1905", "DEAD1906")
base$place <- str_to_lower(base$place)

#Map of % of cultivators
correze2$prop_cultivateur <- "NA"
correze2$prop_cultivateur <- as.numeric(correze2$prop_cultivateur)
base$place <- str_to_upper(base$place)

prop_metier_ville <- function(x,y){
  100*sum(base$job==x&base$place==y)/sum(base$place==y)
} #function to calculate the % of cultivators in each towns

i <- 1
for(i in (1:nrow(base))){
  if(base$job[i]=="agriculteur"){base$job[i]<-"cultivateur"}
  if(base$job[i]=="cultivateur charpentier"){base$job[i]<-"cultivateur"}
  if(base$job[i]=="cultivateur macon"){base$job[i]<-"cultivateur"}
  if(base$job[i]=="proprietaire cultivateur"){base$job[i]<-"cultivateur"}
} #Harmonizing the job of cultivator

v <- 1
for(v in (1:length(VILLE))){
  correze2@data$prop_cultivateur[correze2@data$NOM_COMM==VILLE[v]] <- prop_metier_ville("cultivateur",VILLE[v])
}

nclr <- 5
plotclr <- brewer.pal(nclr,"RdPu")
plotclr <- plotclr[nclr:1] # reorder colors

colcodecult <- findColours(classIntervals(correze2@data$prop_cultivateur,nclr,style="fixed", fixedBreaks=c(10,15,25,35,45)),plotclr)
plot(correze2,col=colcodecult)
legend(x=1.906562,y=45.64311,title="% of cultivateurs",legend=names(attr(colcodecult,"table")),
       fill=attr(colcodecult, "palette"), cex=0.6, bty="n")
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("SALON-LA-TOUR","UZERCHE"),]), pch=20, 
       col=c("black","black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("SALON-LA-TOUR","UZERCHE"),]),c("SALON-LA-TOUR","UZERCHE"),cex=0.6, pos=4)


####################Descriptive stats####################
#Table of the number of people recorded in each town
dead_town <- data.frame(matrix(nrow = 0,ncol=2))
town <- c(levels(as.factor(base$town)),"treignac")

i<- 1
for(i in (1:length(town))){
  dead_town[i,1] <- town[i]
  dead_town[i,2] <- sum(base$town[]==town[i])
}
dead_town <- slice(dead_town, -6)
dead_town[11,1] <- c("Main towns")
dead_town[11,2] <- sum(base$town[]!="other")
dead_town[12,1] <- c("Other")
dead_town[12,2] <- sum(base$town[]=="other")
dead_town[13,1] <- c("Total")
dead_town[13,2] <- nrow(base)
names(dead_town) <- c("Towns","Number of deaths")
View(dead_town)
xtable(x=dead_town,caption = "Number of deaths by location")

#Table of the percentage of cultivators
cultivateur<- data.frame(matrix(nrow = length(VILLE),ncol=2))
cultivateur$X1 <- VILLE
i <- 1
for(i in (1:length(VILLE))){
  if(cultivateur$X1[i]==VILLE[i]){cultivateur$X2[i]<-correze2@data$prop_cultivateur[correze2@data$NOM_COMM==VILLE[i]]}
}

base$place <- str_to_lower(base$place)
View(cultivateur)
cultivateur <- slice(cultivateur,-9) #Deleting Treignac, might be another row number
names(cultivateur) <- c("Towns","% of cultivator")

#Population differences: cross-sectional data first insights
#Age deaths distributions by towns
#Box plots
ggplot(base, aes(x=town, y=age))+
  geom_boxplot()+
  ggtitle("Across towns death age distribution by quartile")+
  labs(x='Towns', y='Death age')

#Densities
base %>% 
  ggplot(aes(x=age))+
  geom_density(fill="#69b3a2", color="#e9ecef",alpha=0.9, adjust = 0.5)+
  facet_wrap(~town)+
  ggtitle("Across town death age distribution")+
  labs(x='Death age', y='Density')

#Time series feature
#Lifetime trend
t_trend <- aggregate(age~year,base,mean)
t_trend$year <- as.Date(t_trend$year, format='%Y') 
t_trend$nbr<-table(base$year)
ggplot(t_trend, aes(x = year, y = age)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  labs(x='Years', y='Average death age') + ggtitle ("Average death age over time")


#Average death age in each town
aggregate(age~town,base,mean)
xtable(aggregate(age~town,base,mean))

##################Regressions#########################
# Regression 1 : simple linear model, regression of death age on exposure to pollution
#Which population is exposed to pollution ?
base$pollution <- 0
base$pollution[base$place=="uzerche"] <- 1

lm.base <-lm(age~pollution,base)
summary.lm(lm.base)
stargazer(lm.base,type="latex")

#Regression 2 : cross-regression seasonnality
#investigation of the statistical biais
ourbase<-subset(base, base$year>=1894)
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

#removing statistical bias
ourbase<-subset(base, base$year>=1894)
lm.season<-lm(age~pollution*month,ourbase)
rseason<-lm(age~pollution, ourbase)
stargazer(lm.season, rseason, single.row = TRUE)
#Regression 3 : focus on tannery 2 and paper factory

#Paper factory
base$postpaper <- 0
base$postpaper[base$place=="uzerche"&base$year>=1893] <- 1
lm.postpaper <- lm(age~postpaper,base[base$place=="uzerche",])
summary(lm.postpaper)
stargazer(lm.postpaper,type="latex")

base$expospaper<-as.numeric(base$year)-1893
for(i in (1:nrow(base))){
  base$expospaper[base$expospaper<0]<- 0
}
summary(lm(age~expospaper+I(expospaper^2),base))
stargazer(lm(age~expospaper+I(expospaper^2),base[base$place=="uzerche",]),type="latex")

#Tannery 2
base$posttan2 <- 0
base$posttan2[base$place=="uzerche"&base$year>=1896] <- 1

lm.posttan2 <-lm(age~posttan2,base[base$place=="uzerche",])
summary(lm(age~posttan2,base[base$place=="uzerche",]))
stargazer(lm.posttan2,type="latex")

#Let's change the date of the exposure :
tan2table <- data.frame(matrix(ncol = 11,nrow = 1)) #recapitulative table
date <- c("1896","1897","1898","1899","1900","1901","1902","1903","1904","1905","1906")
names(tan2table) <- date

d <- 1
for(d in (1:length(date))){
  base$posttan2 <- 0
  base$posttan2[base$place=="uzerche"&base$year>=date[d]] <- 1
  lm.posttan2 <-lm(age~posttan2,base[base$place=="uzerche",])
  summary(lm(age~posttan2,base[base$place=="uzerche",]))
  stargazer(lm.posttan2,type="latex")
}

#Regression 4 : the old ones and the young ones

############Double difference approach###########
base$post1896<-0
base$post1896[base$year>=1896]<- 1
did.base<-subset(base,base$year>=1896|base$year<1893)
did<-lm(age~pollution*post1896,did.base)
stargazer(did, type="latex")


