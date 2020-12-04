##### BLOOD LAB #####

getwd()
setwd("/Users/elenaforbath/Downloads/Fall 2020/Ornithology/lab")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tiff")
install.packages("rtiff")
install.packages("raster") 
install.packages("sp")
install.packages("rgdal")
install.packages("tidyr")
install.packages("stringr")
install.packages("lme4")
install.packages("cowplot")
install.packages("lsmeans")
install.packages("DHARMa")

library(tiff)
library(rtiff)
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)
library(stringr)
library(lme4)
library(cowplot)
library(lme4)
library(lsmeans)
library(DHARMa)

data <- read.csv("blood_lab.csv")

data2 <- aggregate(data$Time, by = list(day=data$day.1), FUN = sum)

## number of visits
## need to calculate number of visits per tag per day 
length(which(data$Tag=="01103F9126"))
length(data$Tag)

install.packages("COUNT")
library(COUNT)

count(data, c("Tag", "day.1"))
data2<- data %>% count(Tag, day.1, sort = TRUE)

bands <- read.csv("bands.csv")

data3 <- merge(data2, bands, by = "Tag")

## regular plots
plot(n ~ day.1, data = data3, col = data3$Blood)

X<-split(data3, data3$Blood)
blood <- X$Blood
sham <- X$Sham
con <- X$Control

bl <- ggplot(blood, aes(x=day.1, y=n, group=Tag)) +
  geom_line(color = "red") +
  ylim(c(1, 6))+
  xlim(c(0,40)) +
  title("BLOOD")
sh <- ggplot(sham, aes(x=day.1, y=n, group=Tag)) +
  geom_line(color = "darkgreen")+
  ylim(c(1, 6))+
  xlim(c(0,40)) +
  title("SHAM")
co <- ggplot(con, aes(x=day.1, y=n, group=Tag)) +
  geom_line(color = "blue")+
  ylim(c(1, 6)) +
  xlim(c(0,40)) +
  title("CONTROL")

install.packages("ggpubr")
library(ggpubr)

ggarrange(bl, sh, co, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)

ggplot(data3, aes(x=day.1, y=n, group=Tag, color = Blood)) +
  geom_line() +
  xlab("days since capture") +
  ylab("number of visits")

## statistical analyses
aov <- aov(n ~ Blood, data = data3)
summary(aov)
aov








