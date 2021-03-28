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

?kids <- data.frame(matrix(nrow=0, ncol=2))
kids <- cbind(lifetime)
colnames(kids) <- c("age_deces_enfant","pollution")
filter(lifetime, lifetime<=3)
kids <-filter(lifetime,lifetime<=3)
lm.kids <-lm(age_deces_enfant~pollution,kids)
summary.lm(lm.kids)
ggplot(data = kids, aes(x = pollution, y = age_deces_enfant)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.95)
qplot(seq_along(lm.kids$residuals), lm.kids$residuals) +
  xlab("") + ylab("R?sidus")

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
correze <- readOGR(dsn="./N_SECTEUR_CC_019.shp", layer="N_SECTEUR_CC_019")
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
colnames(base_tbm) <- c("ann?e","taux")
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
base$pollution[base$place=="uzerche"] <- 1
lm2 <- lm(age~pollution+month,base)
lm3 <- lm(age~pollution*month,base)
aggregate(age~month,base,mean)
aggregate(age~annee,base,mean)
summary(lm(age~month,base))

base$code_place <-0
base$code_place[base$place=="condat-sur-ganaveix"] <- 1
base$code_place[base$place=="espartignac"] <- 2
base$code_place[base$place=="eyburie"] <- 3
base$code_place[base$place=="masseret"] <- 4
base$code_place[base$place=="meilhards"] <- 5
base$code_place[base$place=="saint-ybard"] <- 6
base$code_place[base$place=="salon-la-tour"] <- 7
base$code_place[base$place=="vigeois"] <- 8
base$code_place[base$place=="uzerche"] <- 9

base$code_place2 <- c("other")
base$code_place2[base$place=="condat-sur-ganaveix"] <- c("condat-sur-ganaveix")
base$code_place2[base$place=="espartignac"] <- c("espartignac")
base$code_place2[base$place=="eyburie"] <- c("eyburie")
base$code_place2[base$place=="masseret"] <- c("masseret")
base$code_place2[base$place=="meilhards"] <- c("meilhards")
base$code_place2[base$place=="saint-ybard"] <- c("saint-ybard")
base$code_place2[base$place=="salon-la-tour"] <- c("salon-la-tour")
base$code_place2[base$place=="uzerche"] <- c("uzerche")

sort(unique(base$code_place))

#stats descriptives :
aggregate(age~month+annee,base,mean)
aggregate(age~annee,base,mean)
ggplot( data= aggregate(age~month,base,mean), aes(x = month, y = age)) +
  geom_point()
ggplot( data= aggregate(age~annee,base,mean), aes(x = annee, y = age)) +
  geom_point()

base$dead <- 1
aggregate(age~code_place2,base,mean)
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
  group_by(annee, code_place2) %>%
  summarise(age = mean(age))
histo2 <- slice(histo2, which(annee!="1893"))

ggplot(data = histo,
       aes(x = annee, y= age,
           fill = code_place))+
  geom_histogram(stat='identity')+
  scale_fill_distiller(type = "seq", palette = "Accent",direction = 9,values = NULL, space = "Lab", na.value = "grey50",guide = "colourbar",aesthetics = "fill")
ggplot(data = histo2,
       aes(x = annee, y= age,
           fill = code_place2))+
  geom_histogram(stat='identity')+
  scale_fill_brewer(type = "seq", palette = "Set1",direction = 9,aesthetics = "fill")

kids_dead <- base %>%group_by(annee,age<=2) %>%summarise(nb = sum(dead))
kids_dead$`age <= 2`<- as.numeric(kids_dead$`age <= 2`)
ggplot(data = kids_dead,
       aes(x = annee, y= nb,fill = 'age <= 2'))+
  geom_histogram(stat='identity',fill=rep(c("red","blue"),23))
kids_dead <- slice(kids_dead, which(annee!="1893"))

kids_dead2 <- base %>%group_by(annee,code_place2,age<=2) %>%summarise(nb = sum(dead))
kids_dead2$`age <= 2`<- as.numeric(kids_dead2$`age <= 2`)
ggplot(data = kids_dead2,
       aes(x = annee, y= nb,colour = 'age <= 2',fill=code_place2))+
  geom_histogram(stat='identity',colour=rep(c("white","red"),161))+
  scale_fill_brewer(type = "seq", palette = "Pastel1",direction = 7,aesthetics = "fill")
kids_dead2 <- slice(kids_dead2, which(annee!="1893"))
kids_dead2 <- slice(kids_dead2, which(code_place2!="other"))
which(kids_dead2$code_place2=="espartignac")
kids_dead2 <- slice(kids_dead2, which(code_place2!="espartignac"))

#stats descriptives au propre :

##1: moy et mediane et ecart type d'age de d?c?s par ville
aggregate(age~code_place2,base,mean)
aggregate(age~code_place2,base,median)
aggregate(age~code_place2,base,sd)

##2: moy et mediane et ecart type d'age de d?c?s par ville et par annee + graph
base %>%group_by(annee, code_place2) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))

#histogramme
histo2 <-base %>%
  group_by(annee, code_place2) %>%
  summarise(age = mean(age))
histo2 <- slice(histo2, which(annee!="1893"))
ggplot(data = histo2,
       aes(x = annee, y= age,
           fill = code_place2))+
  geom_histogram(stat='identity')+
  scale_fill_brewer(type = "seq", palette = "Set1",direction = 9,aesthetics = "fill")

#courbes
ggplot(data = histo2,aes(x = annee, y= age,group = code_place2,colour=code_place2))+
  geom_line()

histo3 <-slice(histo2, which(code_place2!="other"))
histo3 <-slice(histo3, which(code_place2!="espartignac"))
ggplot(data = histo3,aes(x = annee, y= age,group = code_place2,colour=code_place2))+
  geom_line()

#3: moy, mediam, ecart type deces par annee et par mois :
base %>%group_by(annee, month) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))

ggplot(data = base%>%group_by(annee, month)%>%summarise(moy_age = mean(age)),aes(x = annee, y= moy_age,group=month,col=month))+
  geom_point()

#4 : moy, median, ecart type deces par mois :
base %>%group_by(month) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))
aggregate(age~month,base,mean)
ggplot(data = base%>%group_by(month)%>%summarise(moy_age = mean(age)),aes(x = month, y= moy_age))+
  geom_point()
base %>%group_by(month,code_place2) %>%summarise(moy_age = mean(age),med_age=median(age),sd_age=sd(age))
ggplot(data = base %>%group_by(month,code_place2) %>%summarise(moy_age = mean(age)),aes(x = month, y= moy_age,group=code_place2,col=code_place2))+
  geom_point()

#je fais juste un test pour voir comment ça marche

