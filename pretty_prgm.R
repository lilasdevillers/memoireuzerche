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
save(river,file="river_vezere.RData")
load("river_vezere.RData")

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
points(coordinates(correze[correze@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]), 
       pch=20, col="red", cex=1)
#focusing on this area and adding Vezere
plot(correze[canton,],main="Map of Correze")
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]), pch=20, 
       col=c("black","black","black","black","black","black","black","black","black","black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]),c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),cex=0.45, pos=4)
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","TREIGNAC","UZERCHE"),]),c("VIGEOIS","TREIGNAC","UZERCHE"),cex=0.45, pos=4,col="red")
library(raster)
river <- locator(n=15) # hand drawing
lines(river,col="lightblue1",lwd=2)

#Adding data on the map
##NB dead in 1906
correze@data$DEAD1906 <- "NA"
correze@data$DEAD1906 <- as.numeric(correze@data$DEAD1906)
correze@data[213,20] <- nrow(base[base$place=="uzerche"&base$annee=="1906",])
correze@data[140,20] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1906",])
correze@data[177,20] <-nrow(base[base$place=="espartignac"&base$annee=="1906",])
correze@data[238,20] <-nrow(base[base$place=="eyburie"&base$annee=="1906",])
correze@data[17,20] <-nrow(base[base$place=="masseret"&base$annee=="1906",])
correze@data[23,20] <-nrow(base[base$place=="meilhards"&base$annee=="1906",])
correze@data[216,20] <-nrow(base[base$place=="saint-ybard"&base$annee=="1906",])
correze@data[68,20] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1906",])
correze@data[10,20] <-nrow(base[base$place=="vigeois"&base$annee=="1906",])
correze@data[179,20] <-nrow(base[base$place=="treignac"&base$annee=="1906",])

##NB dead in 1905
correze@data$DEAD1905 <- "NA"
correze@data$DEAD1905 <- as.numeric(correze@data$DEAD1905)
correze@data[213,21] <- nrow(base[base$place=="uzerche"&base$annee=="1905",])
correze@data[140,21] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1905",])
correze@data[177,21] <-nrow(base[base$place=="espartignac"&base$annee=="1905",])
correze@data[238,21] <-nrow(base[base$place=="eyburie"&base$annee=="1905",])
correze@data[17,21] <-nrow(base[base$place=="masseret"&base$annee=="1905",])
correze@data[23,21] <-nrow(base[base$place=="meilhards"&base$annee=="1905",])
correze@data[216,21] <-nrow(base[base$place=="saint-ybard"&base$annee=="1905",])
correze@data[68,21] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1905",])
correze@data[10,21] <-nrow(base[base$place=="vigeois"&base$annee=="1905",])
correze@data[179,21] <-nrow(base[base$place=="treignac"&base$annee=="1905",])

##NB dead in 1904
correze@data$DEAD1904 <- "NA"
correze@data$DEAD1904 <- as.numeric(correze@data$DEAD1904)
correze@data[213,22] <- nrow(base[base$place=="uzerche"&base$annee=="1904",])
correze@data[140,22] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1904",])
correze@data[177,22] <-nrow(base[base$place=="espartignac"&base$annee=="1904",])
correze@data[238,22] <-nrow(base[base$place=="eyburie"&base$annee=="1904",])
correze@data[17,22] <-nrow(base[base$place=="masseret"&base$annee=="1904",])
correze@data[23,22] <-nrow(base[base$place=="meilhards"&base$annee=="1904",])
correze@data[216,22] <-nrow(base[base$place=="saint-ybard"&base$annee=="1904",])
correze@data[68,22] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1904",])
correze@data[10,22] <-nrow(base[base$place=="vigeois"&base$annee=="1904",])
correze@data[179,22] <-nrow(base[base$place=="treignac"&base$annee=="1904",])

##NB dead in 1903
correze@data$DEAD1903 <- "NA"
correze@data$DEAD1903 <- as.numeric(correze@data$DEAD1903)
correze@data[213,23] <- nrow(base[base$place=="uzerche"&base$annee=="1903",])
correze@data[140,23] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1903",])
correze@data[177,23] <-nrow(base[base$place=="espartignac"&base$annee=="1903",])
correze@data[238,23] <-nrow(base[base$place=="eyburie"&base$annee=="1903",])
correze@data[17,23] <-nrow(base[base$place=="masseret"&base$annee=="1903",])
correze@data[23,23] <-nrow(base[base$place=="meilhards"&base$annee=="1903",])
correze@data[216,23] <-nrow(base[base$place=="saint-ybard"&base$annee=="1903",])
correze@data[68,23] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1903",])
correze@data[10,23] <-nrow(base[base$place=="vigeois"&base$annee=="1903",])
correze@data[179,23] <-nrow(base[base$place=="treignac"&base$annee=="1903",])

##NB dead in 1902
correze@data$DEAD1902 <- "NA"
correze@data$DEAD1902 <- as.numeric(correze@data$DEAD1902)
correze@data[213,24] <- nrow(base[base$place=="uzerche"&base$annee=="1902",])
correze@data[140,24] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1902",])
correze@data[177,24] <-nrow(base[base$place=="espartignac"&base$annee=="1902",])
correze@data[238,24] <-nrow(base[base$place=="eyburie"&base$annee=="1902",])
correze@data[17,24] <-nrow(base[base$place=="masseret"&base$annee=="1902",])
correze@data[23,24] <-nrow(base[base$place=="meilhards"&base$annee=="1902",])
correze@data[216,24] <-nrow(base[base$place=="saint-ybard"&base$annee=="1902",])
correze@data[68,24] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1902",])
correze@data[10,24] <-nrow(base[base$place=="vigeois"&base$annee=="1902",])
correze@data[179,24] <-nrow(base[base$place=="treignac"&base$annee=="1902",])

##NB dead in 1901
correze@data$DEAD1901 <- "NA"
correze@data$DEAD1901 <- as.numeric(correze@data$DEAD1901)
correze@data[213,25] <- nrow(base[base$place=="uzerche"&base$annee=="1901",])
correze@data[140,25] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1901",])
correze@data[177,25] <-nrow(base[base$place=="espartignac"&base$annee=="1901",])
correze@data[238,25] <-nrow(base[base$place=="eyburie"&base$annee=="1901",])
correze@data[17,25] <-nrow(base[base$place=="masseret"&base$annee=="1901",])
correze@data[23,25] <-nrow(base[base$place=="meilhards"&base$annee=="1901",])
correze@data[216,25] <-nrow(base[base$place=="saint-ybard"&base$annee=="1901",])
correze@data[68,25] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1901",])
correze@data[10,25] <-nrow(base[base$place=="vigeois"&base$annee=="1901",])
correze@data[179,25] <-nrow(base[base$place=="treignac"&base$annee=="1901",])

##NB dead in 1900
correze@data$DEAD1900 <- "NA"
correze@data$DEAD1900 <- as.numeric(correze@data$DEAD1900)
correze@data[213,26] <- nrow(base[base$place=="uzerche"&base$annee=="1900",])
correze@data[140,26] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1900",])
correze@data[177,26] <-nrow(base[base$place=="espartignac"&base$annee=="1900",])
correze@data[238,26] <-nrow(base[base$place=="eyburie"&base$annee=="1900",])
correze@data[17,26] <-nrow(base[base$place=="masseret"&base$annee=="1900",])
correze@data[23,26] <-nrow(base[base$place=="meilhards"&base$annee=="1900",])
correze@data[216,26] <-nrow(base[base$place=="saint-ybard"&base$annee=="1900",])
correze@data[68,26] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1900",])
correze@data[10,26] <-nrow(base[base$place=="vigeois"&base$annee=="1900",])
correze@data[179,26] <-nrow(base[base$place=="treignac"&base$annee=="1900",])

##NB dead in 1899
correze@data$DEAD1899 <- "NA"
correze@data$DEAD1899 <- as.numeric(correze@data$DEAD1899)
correze@data[213,27] <- nrow(base[base$place=="uzerche"&base$annee=="1899",])
correze@data[140,27] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1899",])
correze@data[177,27] <-nrow(base[base$place=="espartignac"&base$annee=="1899",])
correze@data[238,27] <-nrow(base[base$place=="eyburie"&base$annee=="1899",])
correze@data[17,27] <-nrow(base[base$place=="masseret"&base$annee=="1899",])
correze@data[23,27] <-nrow(base[base$place=="meilhards"&base$annee=="1899",])
correze@data[216,27] <-nrow(base[base$place=="saint-ybard"&base$annee=="1899",])
correze@data[68,27] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1899",])
correze@data[10,27] <-nrow(base[base$place=="vigeois"&base$annee=="1899",])
correze@data[179,27] <-nrow(base[base$place=="treignac"&base$annee=="1899",])

##NB dead in 1898
correze@data$DEAD1898 <- "NA"
correze@data$DEAD1898 <- as.numeric(correze@data$DEAD1898)
correze@data[213,28] <- nrow(base[base$place=="uzerche"&base$annee=="1898",])
correze@data[140,28] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1898",])
correze@data[177,28] <-nrow(base[base$place=="espartignac"&base$annee=="1898",])
correze@data[238,28] <-nrow(base[base$place=="eyburie"&base$annee=="1898",])
correze@data[17,28] <-nrow(base[base$place=="masseret"&base$annee=="1898",])
correze@data[23,28] <-nrow(base[base$place=="meilhards"&base$annee=="1898",])
correze@data[216,28] <-nrow(base[base$place=="saint-ybard"&base$annee=="1898",])
correze@data[68,28] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1898",])
correze@data[10,28] <-nrow(base[base$place=="vigeois"&base$annee=="1898",])
correze@data[179,28] <-nrow(base[base$place=="treignac"&base$annee=="1898",])

##NB dead in 1897
correze@data$DEAD1897 <- "NA"
correze@data$DEAD1897 <- as.numeric(correze@data$DEAD1897)
correze@data[213,29] <- nrow(base[base$place=="uzerche"&base$annee=="1897",])
correze@data[140,29] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1897",])
correze@data[177,29] <-nrow(base[base$place=="espartignac"&base$annee=="1897",])
correze@data[238,29] <-nrow(base[base$place=="eyburie"&base$annee=="1897",])
correze@data[17,29] <-nrow(base[base$place=="masseret"&base$annee=="1897",])
correze@data[23,29] <-nrow(base[base$place=="meilhards"&base$annee=="1897",])
correze@data[216,29] <-nrow(base[base$place=="saint-ybard"&base$annee=="1897",])
correze@data[68,29] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1897",])
correze@data[10,29] <-nrow(base[base$place=="vigeois"&base$annee=="1897",])
correze@data[179,29] <-nrow(base[base$place=="treignac"&base$annee=="1897",])

##NB dead in 1896
correze@data$DEAD1896 <- "NA"
correze@data$DEAD1896 <- as.numeric(correze@data$DEAD1896)
correze@data[213,30] <- nrow(base[base$place=="uzerche"&base$annee=="1896",])
correze@data[140,30] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1896",])
correze@data[177,30] <-nrow(base[base$place=="espartignac"&base$annee=="1896",])
correze@data[238,30] <-nrow(base[base$place=="eyburie"&base$annee=="1896",])
correze@data[17,30] <-nrow(base[base$place=="masseret"&base$annee=="1896",])
correze@data[23,30] <-nrow(base[base$place=="meilhards"&base$annee=="1896",])
correze@data[216,30] <-nrow(base[base$place=="saint-ybard"&base$annee=="1896",])
correze@data[68,30] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1896",])
correze@data[10,30] <-nrow(base[base$place=="vigeois"&base$annee=="1896",])
correze@data[179,30] <-nrow(base[base$place=="treignac"&base$annee=="1896",])

##NB dead in 1895
correze@data$DEAD1895 <- "NA"
correze@data$DEAD1895 <- as.numeric(correze@data$DEAD1895)
correze@data[213,31] <- nrow(base[base$place=="uzerche"&base$annee=="1895",])
correze@data[140,31] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1895",])
correze@data[177,31] <-nrow(base[base$place=="espartignac"&base$annee=="1895",])
correze@data[238,31] <-nrow(base[base$place=="eyburie"&base$annee=="1895",])
correze@data[17,31] <-nrow(base[base$place=="masseret"&base$annee=="1895",])
correze@data[23,31] <-nrow(base[base$place=="meilhards"&base$annee=="1895",])
correze@data[216,31] <-nrow(base[base$place=="saint-ybard"&base$annee=="1895",])
correze@data[68,31] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1895",])
correze@data[10,31] <-nrow(base[base$place=="vigeois"&base$annee=="1895",])
correze@data[179,31] <-nrow(base[base$place=="treignac"&base$annee=="1895",])

##NB dead in 1894
correze@data$DEAD1894 <- "NA"
correze@data$DEAD1894 <- as.numeric(correze@data$DEAD1894)
correze@data[213,32] <- nrow(base[base$place=="uzerche"&base$annee=="1894",])
correze@data[140,32] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1894",])
correze@data[177,32] <-nrow(base[base$place=="espartignac"&base$annee=="1894",])
correze@data[238,32] <-nrow(base[base$place=="eyburie"&base$annee=="1894",])
correze@data[17,32] <-nrow(base[base$place=="masseret"&base$annee=="1894",])
correze@data[23,32] <-nrow(base[base$place=="meilhards"&base$annee=="1894",])
correze@data[216,32] <-nrow(base[base$place=="saint-ybard"&base$annee=="1894",])
correze@data[68,32] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1894",])
correze@data[10,32] <-nrow(base[base$place=="vigeois"&base$annee=="1894",])
correze@data[179,32] <-nrow(base[base$place=="treignac"&base$annee=="1894",])


correze2 <- correze[canton,]
library(RColorBrewer)
library(classInt)
library(maptools)

nclr <- 4
plotclr <- brewer.pal(nclr,"RdPu")
plotclr <- plotclr[nclr:1] # reorder colors

colcode1906 <- findColours(classIntervals(correze2@data$DEAD1906,nclr,style="equal"),plotclr)
colcode1905 <- findColours(classIntervals(correze2@data$DEAD1905,nclr,style="equal"),plotclr)
colcode1904 <- findColours(classIntervals(correze2@data$DEAD1904,nclr,style="equal"),plotclr)
colcode1903 <- findColours(classIntervals(correze2@data$DEAD1903,nclr,style="equal"),plotclr)
colcode1902 <- findColours(classIntervals(correze2@data$DEAD1902,nclr,style="equal"),plotclr)
colcode1901 <- findColours(classIntervals(correze2@data$DEAD1901,nclr,style="equal"),plotclr)
colcode1900 <- findColours(classIntervals(correze2@data$DEAD1900,nclr,style="equal"),plotclr)
colcode1899 <- findColours(classIntervals(correze2@data$DEAD1899,nclr,style="equal"),plotclr)
colcode1898 <- findColours(classIntervals(correze2@data$DEAD1898,nclr,style="equal"),plotclr)
colcode1897 <- findColours(classIntervals(correze2@data$DEAD1897,nclr,style="equal"),plotclr)
colcode1896 <- findColours(classIntervals(correze2@data$DEAD1896,nclr,style="equal"),plotclr)
colcode1895 <- findColours(classIntervals(correze2@data$DEAD1895,nclr,style="equal"),plotclr)
colcode1894 <- findColours(classIntervals(correze2@data$DEAD1894,nclr,style="equal"),plotclr)

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
