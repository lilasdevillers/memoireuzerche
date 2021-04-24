library(dplyr)

base<-read.csv("uzerche_deces_1883_1906.csv", sep=";")
save(base,file = "uzerche_ultime.RData")
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
  if(base[i,5]=="st ybard"){base[i,5] <- "saint-ybard"}
  if(base[i,5]=="condat"){base[i,5] <- "condat-sur-ganaveix"}
  if(base[i,5]=="st jal"){base[i,5] <- "st_jal"}
}

i<-1
for (i in (1:nrow(base)) ){
  for(j in (0:10)){
    if(is.na(base[i+j,2])){base <- slice(base,-(i+j))} # slice is from dplyr
    if(is.na(base[i+j,4])){base <- slice(base,-(i+j))}
    if(is.na(base[i+j,6])){base <- slice(base,-(i+j))}
  }
}


###whitespace####
i<-1
for(i in (1:nrow(base)) ){
  
  if(base[i,3]==" cultivateur"){
    print(base[i,3])
    base[i,3] <- "cultivateur"}
}

levels(as.factor(base$job))

sort(unique(base$job))
#Doesn't work

###
base$year <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)


base$pollution <- 0
base$pollution[base$place=="uzerche"|base$place=="vigeois"] <- 1
##we can change this later and just exclude Vigeois


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
base$town[base$place=="vigeois"] <- c("vigeois")

#####delete data before 1883:
base<-slice(base, -which(base$year<1883))

table(base$place)

