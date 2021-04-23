load("pop.RData")

library(rgdal)
mymap <- readOGR(dsn="19-correze", layer="19-", p4s=NULL)
mypop <- data.frame(NOM_COMM=mymap@data$NOM_COMM, ORDERED_NOM_COMM=c(1:length(mymap@data$NOM_COMM)))
pop <- merge(pop, mypop, by=c("NOM_COMM"), all.y=TRUE)
pop <- pop[order(pop$ORDERED_NOM_COMM),]
mymap@data$pop <- pop$nom 
rm(pop, mypop)
head(mymap@data)

library(classInt)
nclasse <- 6
distr <- classIntervals(mymap@data$pop, nclasse, style="quantile")$brks

library(RColorBrewer)
colfunc <- colorRampPalette(c("lightpink3", "lightpink", "white", "lightblue", "lightblue4"))
colours <- colfunc(nclasse)
rm(colfunc)
colMap <- colours[findInterval(mymap$pop, distr, all.inside=TRUE)]
par(mar=c(6,2.5,4,2))
plot(mymap, col=colMap, main="Population in Corrèze", sub="1867-1901 cohorts")

points(coordinates(mymap[mymap@data$NOM_COMM %in% c("USSEL", "TULLE", "BRIVE-LA-GAILLARDE"),]), 
       pch=20, col="red", cex=1)

library(maptools)
pointLabel(coordinates(mymap[mymap@data$NOM_COMM %in% c("USSEL", "TULLE", "BRIVE-LA-GAILLARDE"),]), 
           labels = mymap@data$NOM_COMM[mymap@data$NOM_COMM %in% c("USSEL", "TULLE", "BRIVE-LA-GAILLARDE")], 
           offset = 0, cex = 0.6, col="red")


