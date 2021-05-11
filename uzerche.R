#PRGM DATA ANALYSIS UZERCHE
setwd("~/Bureau/Magistère - L3/mémoire/programmes/memoireuzerche")

install.packages("ggplot2")
install.packages("lubridate")

library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)

base <- read.csv("uzerche_deces_1894_1906.csv", sep=";")
save(base,file = "uzerche.RData")
load("uzerche.RData")
View(base)
summary(base)
ls()

# Putting in form
base <- read.csv("uzerche_deces_1874_1906.csv", sep=";")
base <- base[,c(1:6)]
colnames(base) <- c("name","gender","job","age","place","date")
names(base) <- tolower(names(base))
base[,c(1)] <- tolower(base$name) ; base[,c(3)] <- tolower(base$job) ; base[,c(5)] <- tolower(base$place)
base$name <- iconv(base$name, to="ASCII//TRANSLIT") ; base$job <- iconv(base$job, to="ASCII//TRANSLIT") ; base$place <- iconv(base$place, to="ASCII//TRANSLIT")
base$gender <- as.numeric(base$gender) # male=0, female=1
base$age <- as.numeric(base$age)
base$date <- paste(substr(base$date, 7,11), substr(base$date, 4,5),  substr(base$date, 1,2), sep="-")
base$date <- as.Date(base$date, format="%Y-%m-%d") # date : year-month-day
summary(base)

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

i<-1
for (i in (1:nrow(base)) ){
  for(j in (0:10)){
    if(is.na(base[i+j,2])){base <- slice(base,-(i+j))} # slice is from dplyr
    if(is.na(base[i+j,4])){base <- slice(base,-(i+j))}
    if(is.na(base[i+j,6])){base <- slice(base,-(i+j))}
  }
}

# Factors for characther vectors
place_factor <- factor(x=base$place)
job_factor <- factor(x=base$job)
gender_factor <- factor(x = base$gender)
levels(place_factor) ; levels(job_factor) ; levels(gender_factor)

ggplot(data=base, aes(x = year(base$date),y = ))+geom_bar()
summary(base)

# test corr?lation
lieu_po <- data.frame(matrix(nrow=0, ncol=3))
colnames(lieu_po) <- c("const","pollution","i")
lieu_po
i <-1
for (i in 1:nrow(base)){
  if(base[i,5]=="uzerche"){
    lieu_po <- rbind(lieu_po, c(1, 1,i))
  }
  else {
    lieu_po <- rbind(lieu_po, c(1, 0,i))
  }
}

lieu_po <- data.frame(matrix(nrow=0, ncol=1))
colnames(lieu_po) <- c("pollution")
lieu_po
i <-1
for (i in 1:nrow(base)){
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
for (i in 1:nrow(base)){
  lifetime <- rbind(lifetime, base[i,4])
}
#inutile --> base$age
lifetime <- cbind(lifetime, lieu_po)

cor(x = lieu_po,y=lifetime)
cor(x=lifetime,y=lieu_po)
lm.lifetime <-lm(age_deces~pollution,lifetime)
summary.lm(lm.lifetime)
qplot(seq_along(lm.lifetime$residuals), lm.lifetime$residuals) +
  xlab("") + ylab("R?sidus")

colnames(lifetime) <- c("lifetime","polluated_place")

qplot(data = lifetime,polluated_place , fill = I("dodger blue"))
qplot(data = lifetime, seq_along(polluated_place), polluated_place) + xlab("Index")

qplot(data = lifetime,lifetime , fill = I("dodger blue"))
qplot(data = lifetime, seq_along(lifetime), lifetime) + xlab("Index")

#verification des hyp en economo manuellement
lifetime <- cbind(lifetime,lieu_po)
crossprod(lifetime$polluated_place,lifetime$polluated_place)
rep(1, nrow(lifetime)) 
X <- matrix(nrow = 0,ncol = 2)
X <- cbind(rep(1, nrow(lifetime)) ,lifetime$polluated_place)
colnames(X) <-c("constante","polluated_place")
crossprod(X,X)
det(crossprod(X,X))
solve(crossprod(X,X))
crossprod(X,lifetime$lifetime)
b <- solve(crossprod(X,X))%*%crossprod(X,lifetime$lifetime)

colnames(lifetime) <- c("age_deces","pollution")
ggplot(data = lifetime, aes(x = pollution, y = age_deces)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)

###Corrélation mortalité infantile

kids <- data.frame(matrix(nrow=0, ncol=2))
kids <- filter(base, base$age<=1)
View(kids)
lm.kids <-lm(age~pollution,kids)
summary.lm(lm.kids)
ggplot(data = kids, aes(x = pollution, y = age)) +
  geom_point() + stat_smooth(method = "lm", level = 0.95)+ ggtitle("Regression of age of death on pollution for kids (1 years old and less)")+ labs(y="Death age", x="Exposition to pollution (1=exposed, 0=not exposed)")
qplot(seq_along(lm.kids$residuals), lm.kids$residuals) +
  xlab("") + ylab("Residus")
lm.kids2 <- lm(age~pollution+month,kids)
lm.kids3 <- lm(age~pollution*month,kids)
xtable(summary.lm(lm.kids), caption = "Regression of age of death on pollution for kids (1 years old and less)")
xtable(summary.lm(lm.kids2), caption = "Regression of age of death on pollution and month for kids (1 years old and less)")
xtable(summary.lm(lm.kids3), caption = "Cross-regression of age of death on pollution and month for kids (1 years old and less)")

nrow(base)
esp <- data.frame(matrix(nrow=1, ncol=2))
esp[1,] <- c(1, mean(base$age[base$age>=1]))
names(esp) <- c("age", "esp")
for (age in (min(base$age):max(base$age))){
  esp <- rbind(esp, c(age, mean(base$age[base$age>=age])))
}

#test carte
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
# Importer les polygones
correze <- readOGR(dsn="19-correze", layer="19-")
# ?tape pour changer la projection de la carte
correze <- spTransform(correze, CRS("+proj=longlat"))
# Pour permettre la jointure des objets g?om?triques
correze@data$id <- rownames(correze@data)
# Transformer en data frame pour fournir ? ggplot()
correze_points <- fortify(correze,data="id")
# Permet d'?viter des trous ?ventuels
correze_df <- join(correze_points, correze@data, by="id")


p_map_correze <- ggplot(data = correze_df,
                       aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_equal()
p_map_correze

correze_ville<- data.frame(NOM_COMM=mymap@data$NOM_COMM, ORDERED_NOM_COMM=c(1:length(mymap@data$NOM_COMM)))

#test taux de mortalit? et corr?lation

#importer la base de donn?es de l'INSEE sur le nb de d?c?s par ann?e
pop_insee <- read.csv("pop_insee.csv", sep=";")
filter(pop_insee$Libell?.g?ographique=="Uzerche")
which(pop_insee$Libell?.g?ographique=="Uzerche"|pop_insee$Libell?.g?ographique=="Eyburie")
mode(pop_insee$Libell?.g?ographique)
#tentative
i <-1
for(i in (1:nrow(pop_insee))){
  if(pop_insee$Libell?.g?ographique=="Uzerche"){
    pop_uzerche <- slice(pop_insee,i)
  }
}
pop_uzerche <- slice(pop_insee, 6571)
pop_uzerche
pop_ville$pop_1906 <- as.numeric(pop_ville$pop_1906)
pop_ville$pop_1901 <- as.numeric(pop_ville$pop_1901)
pop_ville$pop_1896 <- as.numeric(pop_ville$pop_1896)
pop_ville$pop_1891 <- as.numeric(pop_ville$pop_1891)
pop_ville$pop_1886 <- as.numeric(pop_ville$pop_1886)
pop_ville$pop_1881 <- as.numeric(pop_ville$pop_1881)
pop_ville$pop_1876 <- as.numeric(pop_ville$pop_1876)
colnames(pop_uzerche) <- c("ville","pop_1906","pop_1901","pop_1896","pop_1891","pop_1886","pop_1881","pop_1876")
pop_uzerche2 <- data.frame(matrix(nrow = 7,ncol=2))
pop_uzerche2[,2] <- c(pop_uzerche$pop_1876,pop_uzerche$pop_1881,pop_uzerche$pop_1886,pop_uzerche$pop_1891,pop_uzerche$pop_1896,pop_uzerche$pop_1901,pop_uzerche$pop_1906)
pop_uzerche2[,1] <- c(1876,1881,1886,1891,1896,1901,1906)
colnames(pop_uzerche2) <- c("year","pop")
pop_uzerche3 <- data.frame(matrix(nrow = 7,ncol=1))
pop_uzerche3[,1] <- c(pop_uzerche$pop_1876,pop_uzerche$pop_1881,pop_uzerche$pop_1886,pop_uzerche$pop_1891,pop_uzerche$pop_1896,pop_uzerche$pop_1901,pop_uzerche$pop_1906)
colnames(pop_uzerche3) <- c("pop")

pop_uzerche2
mode(pop_uzerche2$ann?e)
pop_uzerche2$ann?e <- as.numeric(pop_uzerche2$ann?e)
pop_uzerche2$pop <- as.numeric(pop_uzerche2$pop)

#taux brut de mortalit?
tbm <- function(x,y,z=1000){
  z*x/y
}
tbm(sum(year(base$date)==1906),pop_uzerche$pop_1906)
tbm(sum(year(base$date)==i),pop_uzerche$pop[1,j])
tbm(sum(year(base$date)==i),slice(pop_uzerche2[,1],1901))
filter(pop_uzerche2, pop_uzerche2$year == 1901)
select(pop_uzerche2,pop_uzerche2$year == 1901)

class(pop_uzerche2$ann?e)
slice(pop_uzerche2,1901)

## Version rapide et qui marche
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
tbm_ville

#truc de lilas
nrow(base %>% filter (base$place == "meilhards", year(base$date) == "1906"))/ MSM[2,"1906"]*1000
nrow(filter(base, base$place == ville[1], year(base$date) == annee[j]))
date <- as.numeric(date)
filter(year(base$date) == 1906)

nrow(filter (base,base$place == "meilhards", year(base$date) == annee[1]))
     
nrow(filter (base,base$place == "meilhards", year(base$date) == 1906))
nrow(filter (base,base$place == ville[5], year(base$date) == 1906))

mode(year(base$date))
mode(date)

##Corrélation mortalité infantile



#corr?lation avec le taux de mortalit?
tbm_ville <- cbind(tbm_ville,c(0,0,0,0,0,0,0,1))
colnames(tbm_ville) <- c("ville","tbm_1906","tbm_1901","tbm_1896","tbm_1891","tbm_1886","tbm_1881","tbm_1876","pollution")
lm(tbm_1906~pollution,tbm_ville);lm(tbm_1901~pollution,tbm_ville);lm(tbm_1896~pollution,tbm_ville);lm(tbm_1881~pollution,tbm_ville);lm(tbm_1876~pollution,tbm_ville)
summary(lm(tbm_1906~pollution,tbm_ville));summary(lm(tbm_1901~pollution,tbm_ville));summary(lm(tbm_1896~pollution,tbm_ville));summary(lm(tbm_1881~pollution,tbm_ville));summary(lm(tbm_1876~pollution,tbm_ville))
ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1906)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1901)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1896)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1881)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
ggplot(data = tbm_ville, aes(x = pollution, y = tbm_1876)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
#pb graph
lm_tbm <- lm(c(tbm_1906,tbm_1901,tbm_1896,tbm_1881,tbm_1876)~c(pollution,pollution,pollution,pollution,pollution),tbm_ville)
summary(lm_tbm)
ggplot(data = tbm_graph_tot, aes(x = pollution, y = tbm)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
tbm_graph_tot <- data.frame(matrix(nrow = 24,ncol=2))
tbm_graph_tot$X1 <-c(tbm_ville$tbm_1906,tbm_ville$tbm_1901,tbm_ville$tbm_1896)
tbm_graph_tot$X2 <- c(tbm_ville$pollution,tbm_ville$pollution,tbm_ville$pollution)
colnames(tbm_graph_tot) <- c("tbm","pollution")

tbm_ville <- mutate(pop_1906 = tbm(sum(year(base$date)==1906),pop_ville$pop_1906))
#marche pas

for(i in c(1876,1881,1886,1891,1896,1901,1906)){
  
  base_tbm <- rbind(base_tbm,c(i,tbm(sum(year(base$date)==i),pop_uzerche$pop[,2])))
}

base_tbm <- rbind(base_tbm,c(1876,tbm(sum(year(base$date)==1876),pop_uzerche$pop_1876)))
base_tbm <- rbind(base_tbm,c(1886,tbm(sum(year(base$date)==1886),pop_uzerche$pop_1886)))
base_tbm <- rbind(base_tbm,c(1881,tbm(sum(year(base$date)==1881),pop_uzerche$pop_1881)))
base_tbm <- rbind(base_tbm,c(1891,tbm(sum(year(base$date)==1891),pop_uzerche$pop_1891)))
base_tbm <- rbind(base_tbm,c(1896,tbm(sum(year(base$date)==1896),pop_uzerche$pop_1896)))
base_tbm <- rbind(base_tbm,c(1901,tbm(sum(year(base$date)==1901),pop_uzerche$pop_1901)))
base_tbm <- rbind(base_tbm,c(1906,tbm(sum(year(base$date)==1906),pop_uzerche$pop_1906)))


base_tbm
base_tbm <- data.frame(matrix(nrow=0, ncol=2))
colnames(base_tbm) <- c("année","taux")
year(base$date)
filter(year(base$date)==1906)
year(base$date)==1906

sum(year(base$date)==1906)

#gautier :
base$annee <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)
sort(unique(base$annee))
sort(unique(base$month))
base$pollution <- 0
base$pollution[base$place=="uzerche"|base$place=="vigeois"] <- 1
lm2 <- lm(age~pollution+month,base)
lm3 <- lm(age~pollution*month,base)
aggregate(age~town,base,mean)
aggregate(age~annee,base,mean)
summary(lm(age~month,base))


base$town <- c("other")
base$town[base$place=="condat-sur-ganaveix"] <- c("condat-sur-ganaveix")
base$town[base$place=="espartignac"] <- c("espartignac")
base$town[base$place=="eyburie"] <- c("eyburie")
base$town[base$place=="masseret"] <- c("masseret")
base$town[base$place=="meilhards"] <- c("meilhards")
base$town[base$place=="saint-ybard"] <- c("saint-ybard")
base$town[base$place=="salon-la-tour"] <- c("salon-la-tour")
base$town[base$place=="uzerche"] <- c("uzerche")

sort(unique(base$code_place))

#stats descriptives :
aggregate(age~month+annee,base,mean)
aggregate(age~annee,base,mean)
ggplot( data= aggregate(age~month,base,mean), aes(x = month, y = age)) +
  geom_point()
ggplot( data= aggregate(age~annee,base,mean), aes(x = annee, y = age)) +
  geom_point()

base$dead <- 1
aggregate(age~town,base,mean)
aggregate(dead~place,base,sum)
ggplot( data= aggregate(dead~code_place,base,sum), aes(x = code_place, y = dead)) +
  geom_bar(stat='identity')
ggplot( data= aggregate(dead~place,base,sum), aes(x = place, y = dead)) +
  geom_bar(stat='identity')
aggregate(dead~pollution,base,sum)
ggplot( data= aggregate(dead~pollution,base,sum), aes(x = pollution, y = dead)) +
  geom_bar(stat='identity')
aggregate(dead~pollution+code_place,base,sum)
histo <-base %>%
  group_by(annee, code_place) %>%
  summarise(age = mean(age))
head(histo)
ggplot(data = histo,
       aes(x = annee, y= age,
           fill = code_place))+
  geom_histogram(stat='identity',fill=rep(c("paleturquoise","brown","coral","palevioletred","palegreen","gray","sandybrown","orange","pink"), 23)) 
histo <- slice(histo, which(code_place!="8"))
histo2 <-base %>%
  group_by(annee, town) %>%
  summarise(age = mean(age))
histo2 <- slice(histo2, which(annee!="1893"))

ggplot(data = histo,
       aes(x = annee, y= age,
           fill = code_place))+
  geom_histogram(stat='identity')+
  scale_fill_distiller(type = "seq", palette = "Accent",direction = 9,values = NULL, space = "Lab", na.value = "grey50",guide = "colourbar",aesthetics = "fill")
ggplot(data = histo2,
       aes(x = annee, y= age,
           fill = town))+
  geom_histogram(stat='identity')+
  scale_fill_brewer(type = "seq", palette = "Set1",direction = 9,aesthetics = "fill")

kids_dead <- base %>%group_by(annee,age<=2) %>%summarise(nb = sum(dead))
kids_dead$`age <= 2`<- as.numeric(kids_dead$`age <= 2`)
ggplot(data = kids_dead,
       aes(x = annee, y= nb,fill = 'age <= 2'))+
  geom_histogram(stat='identity',fill=rep(c("red","blue"),23))
kids_dead <- slice(kids_dead, which(annee!="1893"))

kids_dead2 <- base %>%group_by(annee,town,age<=2) %>%summarise(nb = sum(dead))
kids_dead2$`age <= 2`<- as.numeric(kids_dead2$`age <= 2`)
ggplot(data = kids_dead2,
       aes(x = annee, y= nb,colour = 'age <= 2',fill=town))+
  geom_histogram(stat='identity',colour=rep(c("white","red"),161))+
  scale_fill_brewer(type = "seq", palette = "Pastel1",direction = 7,aesthetics = "fill")
kids_dead2 <- slice(kids_dead2, which(annee!="1893"))
kids_dead2 <- slice(kids_dead2, which(town!="other"))
which(kids_dead2$town=="espartignac")
kids_dead2 <- slice(kids_dead2, which(town!="espartignac"))

#stats descriptives au propre :

##1: moy et mediane et ecart type d'age de d?c?s par ville
aggregate(age~town,base,mean)
aggregate(age~town,base,median)
aggregate(age~town,base,sd)
moy_median <- cbind(aggregate(age~town,base,mean), aggregate(age~town,base,median))
colnames(moy_median) <- c("lieu","moy_age","lieu2","median_age")
moy_median <- select(moy_median, -3)
ggplot(data = aggregate(age~town,base,mean),aes(x = town, y= age,group=town,col=town))+
  geom_point() + ggtitle("moy age de deces en fct ville")
ggplot(data = moy_median,aes(x = lieu, y= c("moy_age","median_age"),group=c("moy_age","median_age"),col=c("moy_age","median_age")))+
  geom_point() + ggtitle("moy age de deces en fct ville")

##2: moy et mediane et ecart type d'age de d?c?s par ville et par annee + graph
base %>%group_by(annee, town) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))

#histogramme
histo2 <-base %>%
  group_by(annee, town) %>%
  summarise(age = mean(age))
histo2 <- slice(histo2, which(annee!="1893"))
ggplot(data = histo2,
       aes(x = annee, y= age,
           fill = town))+
  geom_histogram(stat='identity')+
  scale_fill_brewer(type = "seq", palette = "Set1",direction = 9,aesthetics = "fill")

#courbes
ggplot(data = histo2,aes(x = annee, y= age,group = town,colour=town))+
  geom_line() + ggtitle("moy age de deces par ville en fct de l'année")

histo3 <-slice(histo2, which(town!="other"))
histo3 <-slice(histo3, which(town!="espartignac"))
ggplot(data = histo3,aes(x = annee, y= age,group = town,colour=town))+
  geom_line() + ggtitle("moy age de deces par ville en fct de l'année")

#3: moy, mediam, ecart type deces par annee et par mois :
base %>%group_by(annee, month) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))

ggplot(data = base%>%group_by(annee, month)%>%summarise(moy_age = mean(age)),aes(x = annee, y= moy_age,group=month,col=month))+
  geom_point() + ggtitle("moy age de deces par mois en fct de l'année")

#4 : moy, median, ecart type deces par mois et par ville :
base %>%group_by(month) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))
aggregate(age~month,base,mean)
ggplot(data = base%>%group_by(month)%>%summarise(moy_age = mean(age)),aes(x = month, y= moy_age))+
  geom_point() + ggtitle("moy age de deces en fct du mois")
base %>%group_by(month,town) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))
ggplot(data = base %>%group_by(month,town) %>%summarise(moy_age = mean(age)),aes(x = month, y= moy_age,group=town,col=town))+
  geom_point() + ggtitle("moy age de deces par ville en fct du mois")


## gautier carte
load("pop.RData")

library(rgdal)
mymap <- readOGR(dsn="19-correze", layer="19-", p4s=NULL)
mypop <- data.frame(NOM_COMM=mymap@data$NOM_COMM, ORDERED_NOM_COMM=c(1:length(mymap@data$NOM_COMM)))
pop <- merge(pop, mypop, by=c("NOM_COMM"), all.y=TRUE)
pop <- pop[order(pop$ORDERED_NOM_COMM),]
mymap@data$pop <- pop$nom 
rm(pop, mypop)
head(mymap@data)
plot(mymap)
plot(correze)
library(classInt)
nclasse <- 6
distr <- classIntervals(mymap@data$pop, nclasse, style="quantile")$brks

library(RColorBrewer)
colfunc <- colorRampPalette(c("lightpink3", "lightpink", "white", "lightblue", "lightblue4"))
colours <- colfunc(nclasse)
rm(colfunc)
colMap <- colours[findInterval(mymap$pop, distr, all.inside=TRUE)]
par(mar=c(6,2.5,4,2))
plot(mymap[canton,], col=colMap, main="Population in Correze", sub="1867-1901 cohorts")

points(coordinates(mymap[mymap@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE", "TREIGNAC"),]), 
       pch=20, col="red", cex=1)

library(maptools)
pointLabel(coordinates(mymap[mymap@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE", "TREIGNAC"),]), 
           labels = mymap@data$NOM_COMM[mymap@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE", "TREIGNAC")], 
           offset = 0, cex = 0.6, col="red")

#carte qui marche
library("rgdal")
correze <- readOGR(dsn="19-correze", layer="19-")
correze <- spTransform(correze, CRS("+proj=longlat"))
correze@data$id <- rownames(correze@data)
#carte 1 : toute la correze
plot(correze, col = "lightgrey")
plot(correze[canton, ], col = "turquoise", add = TRUE)
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), 
       pch=20, col="red", cex=1)
#carte 2 : seulement la partie qui nous interesse
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24
plot(correze[canton,],main="Population in Correze", sub="1874-1906 cohorts")
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), pch=20, 
       col=c("red","blue","green","yellow","brown","pink","orange","purple","cyan","black"), cex=1)
#carte 3 : avec nos données
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
pop <- pop[order(pop$ORDERED_NOM_COMM),]

mypop <- data.frame(NOM_COMM=correze@data$NOM_COMM, ORDERED_NOM_COMM=c(1:length(correze@data$NOM_COMM)))
pop <- merge(pop, mypop, by=c("NOM_COMM"), all.y=TRUE)
pop <- pop[order(pop$ORDERED_NOM_COMM),]
correze@data$pop <- pop$nom 

library(classInt)
nclasse <- 2
distr <- classIntervals(correze@data$pop, nclasse, style="quantile")$brks
library(RColorBrewer)
colfunc <- colorRampPalette(c("lightpink3", "lightblue"))
colours <- colfunc(nclasse)

colMap <- colours[findInterval(correze$pop, distr, all.inside=TRUE)]
par(mar=c(6,2.5,4,2))

#carte 3 bis : comme la 2 mais avec la méthode de gautier et les quantiles du nb pop
load("pop.RData")

library(rgdal)
mymap <- readOGR(dsn="19-correze", layer="19-", p4s=NULL)
mypop <- data.frame(NOM_COMM=mymap@data$NOM_COMM, ORDERED_NOM_COMM=c(1:length(mymap@data$NOM_COMM)))
pop <- merge(pop, mypop, by=c("NOM_COMM"), all.y=TRUE)
pop <- pop[order(pop$ORDERED_NOM_COMM),]
mymap@data$pop <- pop$nom 
rm(pop, mypop)
head(mymap@data)
plot(mymap)
plot(correze)
library(classInt)
nclasse <- 6
distr <- classIntervals(mymap@data$pop, nclasse, style="quantile")$brks

library(RColorBrewer)
colfunc <- colorRampPalette(c("lightpink3", "lightpink", "white", "lightblue", "lightblue4"))
colours <- colfunc(nclasse)
rm(colfunc)
colMap <- colours[findInterval(mymap$pop, distr, all.inside=TRUE)]
par(mar=c(6,2.5,4,2))
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24
plot(mymap[canton,], col=colMap, main="Population in Correze")

points(coordinates(mymap[mymap@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), pch=20, 
       col=c("red","blue","green","yellow","brown","pink","orange","purple","cyan","black"), cex=1)

#carte 4 : la notre sans pop + vezère
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24|correze@data$CODE_CANT==22
plot(correze[canton,],main="Map of Correze")
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR"),]), pch=20, 
       col=c("black","black","black","black","black","black","black"), cex=1)
points(coordinates(correze[correze@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
x <- locator(n=20)
lines(x,col="lightblue1",lwd=2)
# a tracer à la main

#marche pas : tentative rajouter legende et nom des villes
pointLabel(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), 
           labels = correze@data$NOM_COMM[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC")], 
           cex = 0.1, col=c("red","blue","green","yellow","brown","pink","orange","purple","cyan","black"))
legend("left",c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),
       c("red","blue","green","yellow","brown","pink","orange","purple","cyan","black"), pch="1",cex=0.2)

#truc pour tracer
library(raster)
adm_fr <- getData('GADM', country='FRA', level=2)
plot(mymap[canton,])
x <- locator(n=2) 
# Là, je clique deux fois sur la carte pour faire un segment
lines(x,col="red",lwd=3)
# Là, je clique 5 fois sur la carte pour faire la ligne bleue
x <- locator(n=10)
lines(x,col="lightblue1",lwd=2)

#carte au propre
library("rgdal")
correze <- readOGR(dsn="19-correze", layer="19-")
correze <- spTransform(correze, CRS("+proj=longlat"))
correze@data$id <- rownames(correze@data)
#determination de la zone qui nous interesse
plot(correze, col = "lightgrey")
plot(correze[canton, ], col = "turquoise", add = TRUE)
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR" ,"UZERCHE","VIGEOIS", "TREIGNAC"),]), 
       pch=20, col="red", cex=1)
#zoom sur cette zone + tracage vezere
canton <- correze@data$CODE_CANT==28|correze@data$CODE_CANT==29|correze@data$CODE_CANT==24|correze@data$CODE_CANT==22
plot(correze[canton,],main="Map of Correze")
points(coordinates(correze[correze@data$NOM_COMM %in% c("CONDAT-SUR-GANAVEIX","ESPARTIGNAC","EYBURIE","MASSERET","MEILHARDS","SAINT-YBARD","SALON-LA-TOUR"),]), pch=20, 
       col=c("black","black","black","black","black","black","black"), cex=1)
points(coordinates(correze[correze@data$NOM_COMM %in% c("TREIGNAC" ,"UZERCHE","VIGEOIS"),]), pch=20, 
       col=c("red","red","red"), cex=1)
library(raster)
x <- locator(n=20)
lines(x,col="lightblue1",lwd=2)
# a tracer à la main


library(RColorBrewer)
library(classInt)
library(maptools)
plotvar <- correze2@data$DEAD
nclr <- 10
plotclr <- brewer.pal(nclr,"PuOr")
plotclr <- plotclr[nclr:1] # r?eordonne les couleurs
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)
plot(correze2,col=colcode)
locator(n=1) #sert `a trouver les coordonn?ees du point o`u vous souhaitez placer la l?egende
legend(x=2.008351,y=45.74843,title="Number of dead in 1906",legend=names(attr(colcode,"table")),
       fill=attr(colcode, "palette"), cex=0.6, bty="n")

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
plot(correze2)
View(correze2@data)

#carte avec les routes
routeslimou<-readShapeLines("limousin/roads.shp",proj4string=CRS("+proj=longlat"))

#Exporter en Latex
library(xtable)

tab<-xtable(s, caption= "summary statistics of air pollution data", 
            align=c("|c","|c","|c","|c","|c|"))

