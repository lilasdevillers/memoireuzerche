####Pretty PRGM####

setwd("C:/Users/ger0n.DESKTOP-HBPHRVU/Desktop/Mageco/M?moire/memoire r")

library(xtable);library(dplyr);library(stringr);library(ggplot2);library(lubridate);library(tidyr);library(magrittr)
#Open libraries

#Useful commands
base <- read.csv("uzercheultime.csv", sep=";")
save(base,file = "uzerche_test.RData")
load("uzerche_ultime.RData")
save(river,file="river_vezere.RData")
load("river_vezere.RData")
save(correze2,file="map_correze_canton_data.RData")
load("map_correze_canton_data.RData")
load("uzercheultime.RData")
save(base,file="uzerche_ultime.RData")

###

###Putting in form :

base <- read.csv("uzerche_deces_1883_1906.csv", sep=";")
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

#standardizing city's names
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

#Factors for characther vectors
place_factor <- factor(x=base$place)
job_factor <- factor(x=base$job)
gender_factor <- factor(x = base$gender)
levels(place_factor) ; levels(job_factor) ; levels(gender_factor)

###Map of Correze
#all of Correze
library("rgdal");library("maptools");library("maps");library("rgeos")
correze <- readOGR(dsn="19-correze", layer="19-")
correze <- spTransform(correze, CRS("+proj=longlat"))
correze@data$id <- rownames(correze@data)
#which area is interesting for us ?
plot(correze, col = "lightgrey",main="Map of Correze",cex=2)
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24|correze@data$CODE_CANT==22
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
#29 --> vigeois, 24 --> Treignac, 28 --> Uzerche
points(coordinates(correze[correze@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]), 
       pch=20, col="red", cex=1)
#frame : people died in this area
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



#focusing on this area and adding Vezere
plot(correze[canton,],main="Map of the canton of Uzerche, Vigeois and Treignac")
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]), pch=20, 
       col=c("black","black","black","black","black","black","black","black","black","black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]),c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),cex=0.5, pos=4)
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","TREIGNAC","UZERCHE"),]),c("VIGEOIS","TREIGNAC","UZERCHE"),cex=0.5, pos=4,col="red")
library(raster)
river <- locator(n=15) # hand drawing
lines(river,col="lightblue1",lwd=2)
legend(x=1.862834,y=45.45924,legend=c("The Vézère"),
       col ="lightblue1", lty=1:2, cex=1, bty="n")
legend(x=1.862834,y=45.40604,pch=20,legend=c("Towns not on the Vézère","Towns on the Vézère"),
       col =c("black","red"), cex=1, bty="n")

#Adding data on the map
base$place <- str_to_upper(base$place)
correze2 <- correze[canton,]
VILLE <- c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR","UZERCHE","TREIGNAC","VIGEOIS")
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

for(v in (1:length(VILLE))){
  for(i in (1:nrow(correze2@data))){
    if(correze2@data$NOM_COMM[i]==VILLE[v]){
      correze2@data[i,ncol_correze2+1] <- nrow(base[base$place==VILLE[v],])
    }
  }
}

names(correze2) <- c("ID_GEOFLA", "CODE_COMM", "INSEE_COM", "NOM_COMM","STATUT", "X_CHF_LIEU" ,"Y_CHF_LIEU", "X_CENTROID","Y_CENTROID","Z_MOYEN" ,  "SUPERFICIE","POPULATION","CODE_CANT", "CODE_ARR" ,"CODE_DEPT", "NOM_DEPT","CODE_REG", "NOM_REGION","id","DEAD1894", "DEAD1895", "DEAD1896", "DEAD1897", "DEAD1898", "DEAD1899", "DEAD1900", "DEAD1901", "DEAD1902", "DEAD1903", "DEAD1904", "DEAD1905", "DEAD1906")
View(correze2@data)
base$place <- str_to_lower(base$place)

library(RColorBrewer)
library(classInt)
library(maptools)

nclr <- 4
plotclr <- brewer.pal(nclr,"RdPu")
plotclr <- plotclr[nclr:1] # reorder colors

colcode1906 <- findColours(classIntervals(correze2@data$DEAD1906,nclr,style="pretty"),plotclr)
colcode1905 <- findColours(classIntervals(correze2@data$DEAD1905,nclr,style="pretty"),plotclr)
colcode1904 <- findColours(classIntervals(correze2@data$DEAD1904,nclr,style="pretty"),plotclr)
colcode1903 <- findColours(classIntervals(correze2@data$DEAD1903,nclr,style="pretty"),plotclr)
colcode1902 <- findColours(classIntervals(correze2@data$DEAD1902,nclr,style="pretty"),plotclr)
colcode1901 <- findColours(classIntervals(correze2@data$DEAD1901,nclr,style="pretty"),plotclr)
colcode1900 <- findColours(classIntervals(correze2@data$DEAD1900,nclr,style="pretty"),plotclr)
colcode1899 <- findColours(classIntervals(correze2@data$DEAD1899,nclr,style="pretty"),plotclr)
colcode1898 <- findColours(classIntervals(correze2@data$DEAD1898,nclr,style="pretty"),plotclr)
colcode1897 <- findColours(classIntervals(correze2@data$DEAD1897,nclr,style="pretty"),plotclr)
colcode1896 <- findColours(classIntervals(correze2@data$DEAD1896,nclr,style="pretty"),plotclr)
colcode1895 <- findColours(classIntervals(correze2@data$DEAD1895,nclr,style="pretty"),plotclr)
colcode1894 <- findColours(classIntervals(correze2@data$DEAD1894,nclr,style="pretty"),plotclr)

#Map of 1906
plot(correze2,col=colcode1906)
locator(n=1) #helping to find where to put the legend
legend(x=0.7057949,y=45.62726,title="Number of deads in 1906",legend=names(attr(colcode1906,"table")),
       fill=attr(colcode1906, "palette"), cex=0.6, bty="n")

#Map of 1905
plot(correze2,col=colcode1905)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1905",legend=names(attr(colcode1905,"table")),
       fill=attr(colcode1905, "palette"), cex=0.6, bty="n")

#Map of 1904
plot(correze2,col=colcode1904)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1904",legend=names(attr(colcode1904,"table")),
       fill=attr(colcode1904, "palette"), cex=0.6, bty="n")

#Map of 1903
plot(correze2,col=colcode1903)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1903",legend=names(attr(colcode1903,"table")),
       fill=attr(colcode1903, "palette"), cex=0.6, bty="n")

#Map of 1902
plot(correze2,col=colcode1902)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1902",legend=names(attr(colcode1902,"table")),
       fill=attr(colcode1902, "palette"), cex=0.6, bty="n")

#Map of 1901
plot(correze2,col=colcode1901)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1901",legend=names(attr(colcode1901,"table")),
       fill=attr(colcode1901, "palette"), cex=0.6, bty="n")

#Map of 1900
plot(correze2,col=colcode1900)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1900",legend=names(attr(colcode1900,"table")),
       fill=attr(colcode1900, "palette"), cex=0.6, bty="n")

#Map of 1899
plot(correze2,col=colcode1899)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1899",legend=names(attr(colcode1899,"table")),
       fill=attr(colcode1899, "palette"), cex=0.6, bty="n")

#Map of 1898
plot(correze2,col=colcode1898)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1898",legend=names(attr(colcode1898,"table")),
       fill=attr(colcode1898, "palette"), cex=0.6, bty="n")

#Map of 1897
plot(correze2,col=colcode1897)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1897",legend=names(attr(colcode1897,"table")),
       fill=attr(colcode1897, "palette"), cex=0.6, bty="n")

#Map of 1896
plot(correze2,col=colcode1896)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1896",legend=names(attr(colcode1896,"table")),
       fill=attr(colcode1896, "palette"), cex=0.6, bty="n")

#Map of 1895
plot(correze2,col=colcode1895)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1895",legend=names(attr(colcode1895,"table")),
       fill=attr(colcode1895, "palette"), cex=0.6, bty="n")

#Map of 1894
plot(correze2,col=colcode1894)
legend(x=0.7057949,y=45.62726,title="Number of deads in 1894",legend=names(attr(colcode1894,"table")),
       fill=attr(colcode1894, "palette"), cex=0.6, bty="n")

#Map of cultivator %
correze2$prop_cultivateur <- "NA"
correze2$prop_cultivateur <- as.numeric(correze2$prop_cultivateur)
base$place <- str_to_upper(base$place)
v <- 1
for(v in (1:length(VILLE))){
  correze2@data$prop_cultivateur[correze2@data$NOM_COMM==VILLE[v]] <- prop_metier_ville("cultivateur",VILLE[v])
}

nclr <- 5
plotclr <- brewer.pal(nclr,"RdPu")
plotclr <- plotclr[nclr:1] # reorder colors

i <- 1
for(i in (1:nrow(base))){
  if(base$job[i]=="agriculteur"){base$job[i]<-"cultivateur"}
  if(base$job[i]=="cultivateur charpentier"){base$job[i]<-"cultivateur"}
  if(base$job[i]=="cultivateur macon"){base$job[i]<-"cultivateur"}
  if(base$job[i]=="proprietaire cultivateur"){base$job[i]<-"cultivateur"}
}

colcodecult <- findColours(classIntervals(correze2@data$prop_cultivateur,nclr,style="fixed", fixedBreaks=c(10,15,25,35,45,60)),plotclr)
plot(correze2,col=colcodecult)
legend(x=1.906562,y=45.74311,title="% of cultivateurs",legend=names(attr(colcodecult,"table")),
       fill=attr(colcodecult, "palette"), cex=0.6, bty="n")
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","SALON-LA-TOUR","UZERCHE"),]), pch=20, 
       col=c("black","black","black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","SALON-LA-TOUR","UZERCHE"),]),c("VIGEOIS","SALON-LA-TOUR","UZERCHE"),cex=0.6, pos=4)

prop_metier_ville <- function(x,y){
  100*sum(base$job==x&base$place==y)/sum(base$place==y)
}
cultivateur <- data.frame(matrix(nrow = length(VILLE),ncol=2))
cultivateur$X1 <- VILLE
i <- 1
for(i in (1:length(VILLE))){
  if(cultivateur$X1[i]==VILLE[i]){cultivateur$X2[i]<-correze2@data$prop_cultivateur[correze2@data$NOM_COMM==VILLE[i]]}
}
View(cultivateur)
cultivateur <- slice(cultivateur,-9)
names(cultivateur) <- c("Towns","% of cultivator")

###Population exposed to pollution
base$pollution <- 0
base$pollution[base$place=="uzerche"|base$place=="vigeois"] <- 1

<<<<<<< HEAD
### Population exposed to pollution --> cumulative effect
##opening date: tannery 1 : 1872, tannery 2 : 1896, paper mill : 1893
=======
#which individuals are exposed to pollution between 1883 and 1906 ?
>>>>>>> 9c21fb836e605a2be5028e169a17f23b0d711af6
base$tan1 <- base$pollution
base$tan2 <- 0
base$tan2[base$place=="uzerche"&base$year>=1896] <- 1
base$tan2[base$place=="vigeois"&base$year>=1896] <- 1
base$paper <- 0
base$paper[base$place=="uzerche"&base$year>=1893] <- 1
base$paper[base$place=="vigeois"&base$year>=1893] <- 1

#Regrouping places in categories
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

###Regressions :

theme_update(plot.title=element_text(hjust=0.5))
#centrer les graphs

##Regression 1 : date of death (non cumulative pollution)

#Linear regression + graphic
lm.base <-lm(age~pollution,base)
summary.lm(lm.base)
ggplot(data = base, aes(x = pollution, y = age)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95) +
  labs(y="Death age", x="Exposure to pollution (1=exposed, 0=not exposed)") +
  ggtitle("Regression of death age on pollution from 1883 to 1906")
xtable(x = summary.lm(lm.base), caption = "Regression of death age on non-cumulative pollution")

##Regression 1 bis : simple but with all the factories
base$birth <- as.numeric(base$year) - base$age

#tannery 2 --> 1896
lm.tan2 <-lm(age~tan2,base)
summary(lm(age~tan2,base))
#paper factory
lm.paper <- lm(age~paper,base)
summary(lm.paper)

##Regression 2 : cumulative pollution
lm.cum <-lm(age~tan1+tan2+paper,base)
summary.lm(lm.cum)

##Regression 3 : date of death for kids (<= 1 years old)
kids <- filter(base, base$age<=1)
View(kids)
lm.kids <-lm(age~pollution,kids)
summary.lm(lm.kids)
ggplot(data = kids, aes(x = pollution, y = age)) +
  geom_point() + stat_smooth(method = "lm", level = 0.95)+ ggtitle("Regression of death age on pollution for kids (1 years old and less)")+ labs(y="Death age", x="Exposure to pollution (1=exposed, 0=not exposed)")
qplot(seq_along(lm.kids$residuals), lm.kids$residuals) +
  xlab("") + ylab("Residus")
lm.kids2 <- lm(age~pollution+month,kids)
summary.lm(lm.kids2)
lm.kids3 <- lm(age~pollution*month,kids)
summary(lm.kids3)
xtable(summary.lm(lm.kids), caption = "Regression of age of death on pollution for kids (1 years old and less)")
xtable(summary.lm(lm.kids2), caption = "Regression of age of death on pollution and month for kids (1 years old and less)")
xtable(summary.lm(lm.kids3), caption = "Cross-regression of age of death on pollution and month for kids (1 years old and less)")

kidsa <- kids[kids$place=="condat-sur-ganaveix"|kids$place=="espartignac"|kids$place=="eyburie"|kids$place=="masseret"|kids$place=="meilhards"|kids$place=="saint-ybard"|kids$place=="salon-la-tour"|kids$place=="uzerche"|kids$place=="treignac"|kids$place=="vigeois",]
kidsb <- kids[kids$place=="condat-sur-ganaveix"|kids$place=="espartignac"|kids$place=="eyburie"|kids$place=="masseret"|kids$place=="meilhards"|kids$place=="saint-ybard"|kids$place=="salon-la-tour"|kids$place=="uzerche"|kids$place=="treignac",]

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

#truc chelou que gauthier a dit avec les villes à coté
##comparaison 1896 salon-la-tour
base$slt1896 <- 0
base$slt1896[base$place=="salon-la-tour"&base$year>=1896] <- 1
lm(age~slt1896,base[base$place=="salon-la-tour",])
summary(lm(age~slt1896,base[base$place=="salon-la-tour",]))

##condat
base$con1896 <- 0
base$con1896[base$place=="condat-sur-ganaveix"&base$year>=1896] <- 1
lm(age~con1896,base[base$place=="condat-sur-ganaveix",])
summary(lm(age~con1896,base[base$place=="condat-sur-ganaveix",]))
#**
##espartignac
base$esp1896 <- 0
base$esp1896[base$place=="espartignac"&base$year>=1896] <- 1
lm(age~esp1896,base[base$place=="espartignac",])
summary(lm(age~esp1896,base[base$place=="espartignac",]))
#eyburie
base$eyb1896 <- 0
base$eyb1896[base$place=="eyburie"&base$year>=1896] <- 1
lm(age~eyb1896,base[base$place=="eyburie",])
summary(lm(age~eyb1896,base[base$place=="eyburie",]))

#masseret
base$mas1896 <- 0
base$mas1896[base$place=="masseret"&base$year>=1896] <- 1
lm(age~mas1896,base[base$place=="masseret",])
summary(lm(age~mas1896,base[base$place=="masseret",]))

#meilhards
base$mei1896 <- 0
base$mei1896[base$place=="meilhards"&base$year>=1896] <- 1
lm(age~mei1896,base[base$place=="meilhards",])
summary(lm(age~mei1896,base[base$place=="meilhards",]))
#saint-ybard
base$sty1896 <- 0
base$sty1896[base$place=="saint-ybard"&base$year>=1896] <- 1
lm(age~sty1896,base[base$place=="saint-ybard",])
summary(lm(age~sty1896,base[base$place=="saint-ybard",]))
#*

##comparaison 1893 salon-la-tour
base$slt1893 <- 0
base$slt1893[base$place=="salon-la-tour"&base$year>=1893] <- 1
lm(age~slt1893,base[base$place=="salon-la-tour",])
summary(lm(age~slt1893,base[base$place=="salon-la-tour",]))

##condat
base$con1893 <- 0
base$con1893[base$place=="condat-sur-ganaveix"&base$year>=1893] <- 1
lm(age~con1893,base[base$place=="condat-sur(ganaveix",])
summary(lm(age~con1893,base[base$place=="condat-sur-ganaveix",]))

##espartignac
base$esp1893 <- 0
base$esp1893[base$place=="espartignac"&base$year>=1893] <- 1
lm(age~esp1893,base[base$place=="espartignac",])
summary(lm(age~esp1893,base[base$place=="espartignac",]))
#eyburie
base$eyb1893 <- 0
base$eyb1893[base$place=="eyburie"&base$year>=1893] <- 1
lm(age~eyb1893,base[base$place=="eyburie",])
summary(lm(age~eyb1893,base[base$place=="eyburie",]))
# significativé 10%
#masseret
base$mas1893 <- 0
base$mas1893[base$place=="masseret"&base$year>=1893] <- 1
lm(age~mas1893,base[base$place=="masseret",])
summary(lm(age~mas1893,base[base$place=="masseret",]))
# significativité 10%
#meilhards
base$mei1893 <- 0
base$mei1893[base$place=="meilhards"&base$year>=1893] <- 1
lm(age~mei1893,base[base$place=="meilhards",])
summary(lm(age~mei1893,base[base$place=="meilhards",]))
#saint-ybard
base$sty1893 <- 0
base$sty1893[base$place=="saint-ybard"&base$year>=1893] <- 1
lm(age~sty1893,base[base$place=="saint-ybard",])
summary(lm(age~sty1893,base[base$place=="saint-ybard",]))

