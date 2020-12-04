##### BLOOD LAB #####

getwd()
setwd("/Users/elenaforbath/Downloads/Fall 2020/Ornithology/lab")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("sp")
install.packages("rgdal")
install.packages("tidyr")
install.packages("stringr")
install.packages("lme4")
install.packages("cowplot")
install.packages("lsmeans")
install.packages("DHARMa")

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

## number of visits
## need to calculate number of visits per tag per day 
install.packages("COUNT")
library(COUNT)

data2<- data %>% count(Tag, day.1, sort = TRUE)

data_sum<- aggregate(data3$n, by=list(Blood=data3$Blood), FUN=sum)
data_sum
#Blood   x
#1   Blood 139
#2 Control 146
#3    Sham 168
 

bands <- read.csv("bands.csv")

data3 <- merge(data2, bands, by = "Tag")

data3 <- subset(data3, day.1 <=4 )
data3 <- subset(data3, day.1 >=0)

## regular plots
plot(n ~ day.1, data = data3, col = data3$Blood)

X<-split(data3, data3$Blood)
blood <- X$Blood
sham <- X$Sham
con <- X$Control


###### plot by TAGS (mutiple lines) #####

lm1 <- lm(n ~ day.1, data = blood)
summary(lm1)
coef(lm1)
bl <- ggplot(blood, aes(x=day.1, y=n, group=Tag)) +
  geom_point(color = "red", size = 5)+
  ylim(c(1, 6))+
  xlim(c(0,3)) +
  title("BLOOD") +
  xlab("") +
  ylab("# of visits") +
  geom_abline(slope =  0.2142857, intercept =  1.5, 
              col = "black", 
              linetype = "dashed")
bl


lm2 <- lm(n ~ day.1, data = sham)
summary(lm2)
coef(lm2)
sh <- ggplot(sham, aes(x=day.1, y=n)) +
  geom_point(color = "darkgreen", size = 5)+
  ylim(c(1, 6))+
  xlim(c(0,3)) +
  title("SHAM") +
  xlab("") +
  ylab("# of visits") +
 geom_abline(intercept =  1.6105263 , slope = 0.2421053 , 
              col = "black", 
              linetype = "dashed")
sh


lm3 <- lm(n ~ day.1, data = con)
summary(lm3)
coef(lm3)
co <- ggplot(con, aes(x=day.1, y=n, group=Tag)) +
  geom_point(color = "blue", size = 5)+
  ylim(c(1, 6)) +
  xlim(c(0,3)) +
  title("CONTROL") +
  xlab("days since capture") +
  ylab("# of visits") +
  geom_abline(intercept =  1.5854922 , slope = 0.1943005 , 
              col = "black",
              linetype = "dashed")
co

install.packages("ggpubr")
library(ggpubr)

ggarrange(bl, sh, co, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)

ggplot(data3, aes(x=day.1, y=n, group=Tag, color = Blood)) +
  geom_line() +
  xlab("days since capture") +
  ylab("# of visits")



##### plot by TOTAL VISITS (not by tag) #####
blood_ag <- aggregate(blood$n, by=list(day.1=blood$day.1), FUN=sum)
blood_ag

bl <- ggplot(blood_ag, aes(x=day.1, y=x)) +
  stat_smooth(method = 'loess', 
              col = "black", 
              se= FALSE, 
              linetype = "dashed") +
  geom_line(color = "red")+
  ylim(c(1, 13))+
  xlim(c(0,40)) +
  title("BLOOD") +
  xlab("") +
  ylab("sum # of visits") 
bl


sham_ag <- aggregate(sham$n, by=list(day.1=sham$day.1), FUN=sum)
sham_ag

sh <- ggplot(sham_ag, aes(x=day.1, y=x)) +
  geom_line(color = "darkgreen")+
  stat_smooth(method = 'loess',
              col = "black", 
              se= FALSE, 
              linetype = "dashed") +
  ylim(c(1, 13))+
  xlim(c(0,40)) +
  title("SHAM") +
  xlab("") +
  ylab("sum # of visits")
sh



con_ag <- aggregate(con$n, by=list(day.1=con$day.1), FUN=sum)
con_ag

co <- ggplot(con_ag, aes(x=day.1, y=x)) +
  stat_smooth(method = 'loess', 
              col = "black", 
              se= FALSE, 
              linetype = "dashed") +
  geom_line(color = "blue")+
  ylim(c(1, 13)) +
  xlim(c(0,40)) +
  title("CONTROL") +
  xlab("days since capture") +
  ylab("sum # of visits") 
co

ggarrange(bl, sh, co, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)

##### boxplots #####


ggplot(data3, aes(x=Blood, y=n)) +
  geom_boxplot() +
  xlab("Treatment") +
  ylab("# of visits")

data3$Blood <- factor(data3$Blood ,
                       levels = c('Control','Sham', 'Blood'),
                      ordered = TRUE)

ggplot(data3, aes(x=Blood, y=n)) +
  geom_boxplot() +
  labs(y="Treatment", x="#of visits")


aov <- aov(n ~ Blood, data=data3)
TukeyHSD(aov)


##### statistical analyses ???? #####
aov <- aov(n ~ Blood, data = data3)
summary(aov)
aov

lm(n ~ day.1, data = blood)

manova(n ~ Blood, data = data3)
kt <- kruskal.test(n ~ Blood, data = data3)
summary(kt)


test <- pairwise.wilcox.test(cbind(data3$n, data3$day.1), data3$Blood, p.adjust.method = "BH")
test
summary(test)
