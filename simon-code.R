library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(magrittr)
 
load("base_1896-1906.RData")

#######Create variable for the places of interest#######

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

sort(unique(base$code_place2))

######################seasonality###################
base$pollution <- 0
base$pollution[base$place=="uzerche"] <- 1
base$pollution[base$place=="vigeois"] <- 1
base$annee <- substr(x = base$date,1,4)
base$month <- substr(x = base$date,6,7)

summary(lm(age~pollution+month,base))
summary(lm(age~pollution*month,base))

summary(lm(age~pollution,base))

############variation annuels###########
base$annee <- substr(x = base$date,1,4)
lm(age~pollution*annee,base)

####################Lingère########################
base$pj <- 0
base$pj[base$job == "linger"|base$job == "tanneur"|base$job == "ouvrier papetier"] <- 1
summary(lm(age~pollution*linger*cult, base))

sort(unique(base$job))
 

