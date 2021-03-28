setwd("~/Bureau/Magistère - L3/mémoire/programmes/memoireuzerche")

# Lire les donnees

# Fichier Excel
library("readxl")
df <- read_excel("deces_uzerche_1874_1883.xls")
df <- read_excel("deces_uzerche_1874_1883.xlsx")

# Fichier txt
df <- read.table("deces_uzerche_1874_1883.txt", header=TRUE, sep=";")

# Fichier csv
df <- read.csv("deces_uzerche_1874_1883.csv", sep=";")
head(df)
df <- read.csv("deces_uzerche_1874_1883.csv", sep=";", header=TRUE, colClasses=c(rep("character",4), "numeric", rep("character",4)))
df[c(1:3),c(1,3,4)]
df <- df[,c(1:7)]
head(df)
# Premiere mise en forme
names(df)
names(df) <- tolower(names(df))
df[,c(1,3,4)] <- lapply(df[,c(1,3,4)], tolower)
for (j in c(1,3,4)){
  df[,j] <- iconv(df[,j], to="ASCII//TRANSLIT")
}
rm(j)

df$date <- paste(substr(df$date, 7,11), substr(df$date, 4,5),  substr(df$date, 1,2), sep="-")
# df$date <- as.Date(df$date, format="%Y-%m-%d")
# as.numeric(difftime(as.Date(df$date[1], format="%Y-%m-%d"), as.Date(df$date[2], format="%Y-%m-%d"), units = c("days")))

# Un premier regard

head(df); tail(df); nrow(df); ncol(df)

head(df[,1]); head(df$nom); df[c(1,2,3,4,5),1]; df[c(1:5),1]; df[1:5,1]

df[1,]; names(df)[3]

summary(df$age); mean(df$age); sum(df$age)/length(df$age)
mean(df$age[df$age>5]); mean(df$age[df$age>10]); mean(df$age[df$age>20])

colSums(df[, c(2,5)])/nrow(df)
df$genre <- as.numeric(df$genre)

summary(df)

# Figure
esp <- data.frame(matrix(nrow=1, ncol=2))
esp[1,] <- c(1, mean(df$age[df$age>=1]))
names(esp) <- c("age", "esp")
for (age in (min(df$age):max(df$age))){
  esp <- rbind(esp, c(age, mean(df$age[df$age>=age])))
}

# esp <- data.frame("age"=numeric(), "esp"=numeric())
# esp[1,] <- c(1, mean(df$age[df$age>=1]))
# i <- 2
# for (age in (min(df$age):max(df$age))){
#   esp[i,] <- c(age, mean(df$age[df$age>=age]))
#   i <- i+1
# }
# rm(age, i)

plot(esp$age, esp$esp, type="p")

nmorts <- aggregate(nom ~ age, df, length)
names(nmorts) <- c("age", "n")
esp <- merge(esp, nmorts, by=c("age"), all.x=TRUE)
points(esp$age, esp$n, col="red")

plot(esp$age, esp$esp, type="p", ylim=c(min(esp$n), max(esp$n)))
esp$n
plot(esp$age, esp$esp, type="p", ylim=c(min(esp$n, na.rm=TRUE), max(esp$n, na.rm=TRUE)),
     xlab="Age au d?c?s", ylab="Esp?rance de vie")
points(esp$age, esp$n, col="red")
points(esp$age, esp$n, col="red", type="b", lty="dotted", pch=15)
legend(60, 140, legend=c("Nombre de d?c?s", "Esp?rance de vie"),
       col=c("red", "black"), text.col=c("red", "black"), 
       pch=c(15, 1), cex=1, box.lty=0)

# R?gressions

lm1 <- lm(esp ~ age, esp)
summary(lm1)
summary(lm1)$coef; summary(lm1)$coef[,1]; summary(lm1)$coef[2,1]

lm2 <- lm(log(esp) ~ age, esp); summary(lm2)

lm3 <- lm(log(esp) ~ log(age + 1e-06), esp); summary(lm3)

lm4 <- lm(esp ~ age + I(age^2), esp); summary(lm4)
lm4 <- lm(esp ~ age + I(age^2) + I(age^3), esp); summary(lm4)

# Mortalite infantile

esp1 <- aggregate(nom ~ lieu, df[df$age <= 10,], length)
esp2 <- aggregate(nom ~ lieu, df, length)
esp1 <- merge(esp1, esp2, by=c("lieu"), all.x=TRUE)
esp1$prop <- esp1$nom.x / esp1$nom.y
esp1 <- esp1[esp1$nom.x >= 50,]
esp1[order(esp1$prop),]

# Sauver ses fichiers

save(df, file="uzerche.RData")
# load("uzerche.RData")