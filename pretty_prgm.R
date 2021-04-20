####Pretty PRGM####

setwd("C:/Users/ger0n.DESKTOP-HBPHRVU/Desktop/Mageco/M?moire/memoire r")

library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)
#Open libraries

#Useful commands
base <- read.csv("uzerche_deces_1894_1906_test.csv", sep=";")
save(base,file = "uzerche_test.RData")
load("uzerche_test.RData")

View(base)

###

###Putting in form :

base <- read.csv("uzerche_deces_1874_1906_test.csv", sep=";")
base <-base[,c(1:6)]
colnames(base) <- c("name","gender","job","age","place","date")
names(base) <- tolower(names(base))
base[,c(1)] <- tolower(base$name) ; base[,c(3)] <- tolower(base$job) ; base[,c(5)] <- tolower(base$place)
base$name <- iconv(base$name, to="ASCII//TRANSLIT") ; base$job <- iconv(base$job, to="ASCII//TRANSLIT") ; base$place <- iconv(base$place, to="ASCII//TRANSLIT")
base$gender <- as.numeric(base$gender)
base$age <- as.numeric(base$age)
base$date <- paste(substr(base$date, 7,11), substr(base$date, 4,5),  substr(base$date, 1,2), sep="-")
base$date <- as.Date(base$date, format="%Y-%m-%d")
summary(base)

#standardizing city's names
i<-1
for(i in (1:nrow(base)) ){
  if(base[i,5]=="salon"){base[i,5] <- "salon-la-tour"}
}
i<-1
for(i in (1:nrow(base)) ){
  if(base[i,5]=="st ybard"){base[i,5] <- "saint-ybard"}
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

#Factors for characther vectors
place_factor <- factor(x=base$place)
job_factor <- factor(x=base$job)
gender_factor <- factor(x = base$gender)
levels(place_factor) ; levels(job_factor) ; levels(gender_factor)

###Map of Correze
#all of Correze
library("rgdal")
correze <- readOGR(dsn="19-correze", layer="19-")
correze <- spTransform(correze, CRS("+proj=longlat"))
correze@data$id <- rownames(correze@data)
#which area is interesting for us ?
plot(correze, col = "lightgrey")
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24|correze@data$CODE_CANT==22
plot(correze[canton, ], col = "turquoise", add = TRUE)
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), 
       pch=20, col="red", cex=1)
#focusing on this area and adding Vezere
plot(correze[canton,],main="Map of Correze")
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR"),]), pch=20, 
       col=c("black","black","black","black","black","black","black"), cex=1)
points(coordinates(correze[correze@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
library(raster)
x <- locator(n=20)
lines(x,col="lightblue1",lwd=2)
# hand drawing


###Regressions :

##Regression 1 : date of death
#creating data frame with data that we want
lieu_po <- data.frame(matrix(nrow=0, ncol=1))
colnames(lieu_po) <- c("pollution")
lieu_po
i <-1
for (i in 1:3536){
  if(base[i,5]=="uzerche"){
    lieu_po <- rbind(lieu_po, c(1))
  }
  else {
    lieu_po <- rbind(lieu_po, c(0))
  }
}

lifetime <- data.frame(matrix(nrow=0, ncol=1))
colnames(lifetime) <- c("lifetime")
i <-1
for (i in 1:3536){
  lifetime <- rbind(lifetime, base[i,4])
}
#useless --> base$age => Must be reviewed
lifetime <- cbind(lifetime, lieu_po)

#Linear regression + graphic
lm.lifetime <-lm(age_deces~pollution,lifetime)
summary.lm(lm.lifetime)
ggplot(data = lifetime, aes(x = pollution, y = age_deces)) +
  geom_point()+
  stat_smooth(method = "lm", level = 0.95)

##Regression 2 : date of death for kids (<= 3 years old)
#Taking values
kids <- data.frame(matrix(nrow=0, ncol=2))
kids <- cbind(lifetime)
colnames(kids) <- c("lifetime","polluated_place")
kids <-filter(lifetime,lifetime<=3)

#Linear regression
lm.kids <-lm(lifetime~polluated_place,kids)
summary.lm(lm.kids)

##Regression 3 : mortality rate (taux brut de mortalite tbm)
#creating mortality rate function. 
#x = nb of death during period T, y = nb of living people during period T, z = for 1000 residents
tbm <- function(x,y,z=1000){
  z*x/y
}

#Taking values (using data from INSEE for number of living people)
pop_insee <- read.csv("pop_insee.csv", sep=";")
pop_ville <- data.frame(matrix(nrow = 0,ncol=8))
for(i in which(pop_insee$Libell?.g?ographique=="Condat-sur-Ganaveix"|pop_insee$Libell?.g?ographique=="Espartignac"|pop_insee$Libell?.g?ographique=="Eyburie"|pop_insee$Libell?.g?ographique=="Meilhards"|pop_insee$Libell?.g?ographique=="Masseret"|pop_insee$Libell?.g?ographique=="Salon-la-Tour"|pop_insee$Libell?.g?ographique=="Saint-Ybard"|pop_insee$Libell?.g?ographique=="Uzerche")){
  pop_ville <- rbind(pop_ville,pop_insee[i,])
}
colnames(pop_ville) <- c("ville","pop_1906","pop_1901","pop_1896","pop_1891","pop_1886","pop_1881","pop_1876")
pop_ville;summary(pop_ville)
pop_ville$pop_1906 <- as.numeric(pop_ville$pop_1906)
pop_ville$pop_1901 <- as.numeric(pop_ville$pop_1901)
pop_ville$pop_1896 <- as.numeric(pop_ville$pop_1896)
pop_ville$pop_1891 <- as.numeric(pop_ville$pop_1891)
pop_ville$pop_1886 <- as.numeric(pop_ville$pop_1886)
pop_ville$pop_1881 <- as.numeric(pop_ville$pop_1881)
pop_ville$pop_1876 <- as.numeric(pop_ville$pop_1876)

annee <- c(1906,1901,1896,1891,1886,1881,1876)
tbm_ville <- data.frame(matrix(nrow = 8,ncol=7))
ville <- data.frame(x=8,y=1)
ville <- pop_ville$ville
ville <- tolower(ville)
pop_ville <- select(pop_ville,-1)
pop_ville
ville

i <- 1;j <- 1
for (i in (1:nrow(pop_ville))){
  for (j in (1:ncol(pop_ville))){
    tbm_ville[i,j] <- tbm(nrow(filter(base, base$place == ville[i], year(base$date) == annee[j])),pop_ville[i,j])
  }
}
tbm_ville <- cbind(ville,tbm_ville)
colnames(tbm_ville) <- c("ville","tbm_1906","tbm_1901","tbm_1896","tbm_1891","tbm_1886","tbm_1881","tbm_1876")
View(tbm_ville)

#Linear regressions for year 1906, 1901 and 1896 + graphics : regression 3a, 3b, 3c
tbm_ville <- cbind(tbm_ville,c(0,0,0,0,0,0,0,1)) # uzerche = polluted place
colnames(tbm_ville) <- c("ville","tbm_1906","tbm_1901","tbm_1896","tbm_1891","tbm_1886","tbm_1881","tbm_1876","pollution")
lm(tbm_1906~pollution,tbm_ville);lm(tbm_1901~pollution,tbm_ville);lm(tbm_1896~pollution,tbm_ville)
summary(lm(tbm_1906~pollution,tbm_ville));summary(lm(tbm_1901~pollution,tbm_ville));summary(lm(tbm_1906~pollution,tbm_ville))

ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1906)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1901)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1896)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)

#Linear regression all years 1906,1901 and 1896 together + graphs : regression 3d
lm_tbm <- lm(c(tbm_1906,tbm_1901,tbm_1896)~c(pollution,pollution,pollution),tbm_ville)
summary(lm_tbm)
ggplot(data = tbm_graph_tot, aes(x = pollution, y = tbm)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
tbm_graph_tot <- data.frame(matrix(nrow = 24,ncol=2))
tbm_graph_tot$X1 <-c(tbm_ville$tbm_1906,tbm_ville$tbm_1901,tbm_ville$tbm_1896)
tbm_graph_tot$X2 <- c(tbm_ville$pollution,tbm_ville$pollution,tbm_ville$pollution)
colnames(tbm_graph_tot) <- c("tbm","pollution")
