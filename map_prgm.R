###prgm map
###Map of Correze
#all of Correze
library("rgdal")
library("maptools");library("maps");library("rgeos")
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
#river
river <- data.frame(matrix(nrow=9, ncol=2))
colnames(river) <- c("X","Y")
river$X <- c(1.468429, 1.517293, 1.538235, 1.552196, 1.569648, 1.660396, 1.758125, 1.800008, 1.838402)
river$Y <- c(45.35317, 45.38009,45.41435, 45.40946,45.42169, 45.44372,45.53916, 45.55385,45.56853)
lines(river,col="lightblue1",lwd=2)

#carte 3 : avec nos données version coulmont
#Adding data on the map
##NB dead in 1906
correze@data$DEAD <- "NA"
correze@data$DEAD <- as.numeric(correze@data$DEAD)
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

correze2 <- correze[canton,]
library(RColorBrewer)
library(classInt)
library(maptools)
plotvar <- correze2@data$DEAD
nclr <- 10
plotclr <- brewer.pal(nclr,"PuOr")
plotclr <- plotclr[nclr:1] # reorder colors
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)
plot(correze2,col=colcode)
locator(n=1) #helping to find where to put the legend
legend(x=2.016408,y=45.54895,title="Number of dead in 1906",legend=names(attr(colcode,"table")),
       fill=attr(colcode, "palette"), cex=0.6, bty="n")
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR"),]), pch=20, 
       col=c("black","black","black","black","black","black","black"), cex=1)
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)

#carte 3 bis : avec nos données version gauthier
load(file = "uzerche.Rdata")
pop$nom <- 0
pop[pop$NOM_COMM=="UZERCHE",2] <-nrow(base[base$place=="uzerche"&base$annee=="1906",])
pop[pop$NOM_COMM=="CONDAT-SUR-GANAVEIX",2] <-nrow(base[base$place=="condat-sur-ganaveix"&base$annee=="1906",])
pop[pop$NOM_COMM=="ESPARTIGNAC",2] <-nrow(base[base$place=="espartignac"&base$annee=="1906",])
pop[pop$NOM_COMM=="EYBURIE",2] <-nrow(base[base$place=="eyburie"&base$annee=="1906",])
pop[pop$NOM_COMM=="MASSERET",2] <-nrow(base[base$place=="masseret"&base$annee=="1906",])
pop[pop$NOM_COMM=="MEILHARDS",2] <-nrow(base[base$place=="meilhards"&base$annee=="1906",])
pop[pop$NOM_COMM=="SAINT-YBARD",2] <-nrow(base[base$place=="saint-ybard"&base$annee=="1906",])
pop[pop$NOM_COMM=="SALON-LA-TOUR",2] <-nrow(base[base$place=="salon-la-tour"&base$annee=="1906",])
pop[pop$NOM_COMM=="VIGEOIS",2] <-nrow(base[base$place=="vigeois"&base$annee=="1906",])
pop[pop$NOM_COMM=="TREIGNAC",2] <-nrow(base[base$place=="treignac"&base$annee=="1906",])
correze@data$pop <- pop$nom

library(classInt)
nclasse <- 3
distr <- classIntervals(correze@data$pop, nclasse, style="quantile")$brks
library(RColorBrewer)
colfunc <- colorRampPalette(c("lightpink3", "blue","lightblue4"))
colours <- colfunc(nclasse)

colMap <- colours[findInterval(correze$pop, distr, all.inside=TRUE)]
par(mar=c(6,2.5,4,2))

#tentative ajout légende et nom des villes
pointLabel(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), 
           labels = correze@data$NOM_COMM[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC")], 
           cex = 0.1, col=c("red","blue","green","yellow","brown","pink","orange","purple","cyan","black"))
legend("left",c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),
       c("red","blue","green","yellow","brown","pink","orange","purple","cyan","black"), pch="1",cex=0.2)

text(correze2@polygons,correze2@data$NOM_COMM)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]),c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),cex=0.45, adj = c(-0.2, 0.8))
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("EYBURIE"),]),c("EYBURIE"),cex=0.45, adj = c(-0.2, 0))
plot(correze2)
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR"),]), pch=20, 
       col=c("black","black","black","black","black","black","black"), cex=1)
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
#bug affiche tout dans le désordre

points(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]), pch=20, 
       col=c("black","black","black","black","black","black","black","black","black","black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),]),c("VIGEOIS","MASSERET","MEILHARDS","SALON-LA-TOUR","CONDAT-SUR-GANAVEIX","ESPARTIGNAC","TREIGNAC","UZERCHE","SAINT-YBARD","EYBURIE"),cex=0.45, pos=4)
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS","TREIGNAC","UZERCHE"),]),c("VIGEOIS","TREIGNAC","UZERCHE"),cex=0.45, pos=4,col="red")
#réussi

#pos et nom ville par ville
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("EYBURIE"),]), pch=20, 
       col=c("black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("EYBURIE"),]),c("EYBURIE"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX"),]), pch=20, 
       col=c("black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX"),]),c("CONDAT-SUR-GANAVEIX"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("ESPARTIGNAC"),]), pch=20, 
       col=c("black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("ESPARTIGNAC"),]),c("ESPARTIGNAC"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("MASSERET"),]), pch=20, 
       col=c("black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("MASSERET"),]),c("MASSERET"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("MEILHARDS"),]), pch=20, 
       col=c("black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("MEILHARDS"),]),c("MEILHARDS"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("SAINT-YBARD"),]), pch=20, 
       col=c("black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("SAINT-YBARD"),]),c("SAINT-YBARD"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("SALON-LA-TOUR"),]), pch=20, 
       col=c("black"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("SALON-LA-TOUR"),]),c("SALON-LA-TOUR"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("UZERCHE"),]), pch=20, 
       col=c("red"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("UZERCHE"),]),c("UZERCHE"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS"),]), pch=20, 
       col=c("red"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("VIGEOIS"),]),c("VIGEOIS"),cex=0.45, adj = c(0, 0))
points(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC"),]), pch=20, 
       col=c("red"), cex=1)
text(coordinates(correze2[correze2@data$NOM_COMM %in% c("TREIGNAC"),]),c("TREIGNAC"),cex=0.45, adj = c(0, 0))

#prgm auto pour faire des cartes en fct des données
correze4 <- readOGR(dsn="19-correze", layer="19-")
correze4 <- spTransform(correze4, CRS("+proj=longlat"))
base$place <- str_to_upper(base$place)
correze3 <- correze4[canton,]
rm(correze4)

d <-1
j <- 1
i <- 1
ncol_correze3 <- ncol(correze3@data)
for(d in (1:length(date))){
        correze3@data[,ncol_correze3+d] <- "NA"
        correze3@data[,ncol_correze3+d] <- as.numeric(correze3@data[,ncol_correze3+d])
        for(j in (1:length(VILLE))){
                for(i in (1:nrow(correze3@data))){
                        if(correze3@data$NOM_COMM[i]==VILLE[j]){
                                correze3@data[i,ncol_correze3+d] <- nrow(base[base$place==VILLE[j]&base$annee==date[d],])
                        }
                }
        }
}
names(correze3) <- c("ID_GEOFLA", "CODE_COMM", "INSEE_COM", "NOM_COMM","STATUT", "X_CHF_LIEU" ,"Y_CHF_LIEU", "X_CENTROID","Y_CENTROID","Z_MOYEN" ,  "SUPERFICIE","POPULATION","CODE_CANT", "CODE_ARR" ,"CODE_DEPT", "NOM_DEPT","CODE_REG", "NOM_REGION","DEAD1894", "DEAD1895", "DEAD1896", "DEAD1897", "DEAD1898", "DEAD1899", "DEAD1900", "DEAD1901", "DEAD1902", "DEAD1903", "DEAD1904", "DEAD1905", "DEAD1906")
View(correze3@data)

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

map <- map_data("world",region = c("france","germany","italy","UK","spain","poland","romania","netherlands","greece","portugal","belgium","czechia","hungary","sweden","bulgaria","denmark","slovakia","finland","ireland","croatia","lithuania","latvia","slovenia","estonia","cyprus","luxembourg","malta") )
p_map <- ggplot(data = map,aes(x = long, y = lat,group=group,fill = region)) + geom_polygon(col="grey") + coord_equal()+ scale_fill_discrete(name = "Pays")
p_map
world <- map_data("world")

#map métier --> proportion d'agri

correze2$prop_cultivateur <- "NA"
correze2$prop_cultivateur <- as.numeric(correze2$prop_cultivateur)
base$place <- str_to_upper(base$place)

prop_metier_ville <- function(x,y){
        100*sum(base$job==x&base$place==y)/sum(base$place==y)
}
v <- 1
for(v in (1:length(VILLE))){
        correze2@data$prop_cultivateur[correze2@data$NOM_COMM==VILLE[v]] <- prop_metier_ville("cultivateur",VILLE[v])
}

nclr <- 4
plotclr <- brewer.pal(nclr,"RdPu")
plotclr <- plotclr[nclr:1] # reorder colors

colcodecult <- findColours(classIntervals(correze2@data$prop_cultivateur,nclr,style="pretty"),plotclr)
plot(correze2,col=colcodecult)
legend(x=0.7057949,y=45.62726,title="% of cultivateurs",legend=names(attr(colcodecult,"table")),
       fill=attr(colcodecult, "palette"), cex=0.6, bty="n")
