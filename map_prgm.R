###prgm map
###Map of Correze
#all of Correze
library("rgdal")
correze <- readOGR(dsn="19-correze", layer="19-")
correze <- spTransform(correze, CRS("+proj=longlat"))
correze@data$id <- rownames(correze@data)
#which area is interesting for us ?
plot(correze, col = "lightgrey")
plot(correze[canton, ], col = "turquoise", add = TRUE)
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), 
       pch=20, col="red", cex=1)
#focusing on this area and adding Vezere
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24|correze@data$CODE_CANT==22
plot(correze[canton,],main="Map of Correze")
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR"),]), pch=20, 
       col=c("black","black","black","black","black","black","black"), cex=1)
points(coordinates(correze[correze@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
library(raster)
x <- locator(n=20)
lines(x,col="lightblue1",lwd=2)
# hand drawing
#carte 3 : avec nos données
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