SasData <- read_excel("C:/Users/crandall/Desktop/Saskia/YEAR1_allreefs.xlsx")
View(SasData)

library(readxl)
library(ggplot2)
library(dismo)
library(dplyr)
library(gbm)
colnames(SasData)
sum(is.na(SasData$SurvDev))
SasData$Reef<-as.factor(SasData$Reef)
SasData$Site<-as.factor(SasData$Site)


SasData %>%
  ggplot(aes(y=SurvDev, x=median_speed))+
  facet_grid(Reef~.)+
  geom_violin()

mod<- gbm.step(data=data.frame(SasData), gbm.x = c(6,7,8,9,10,11,12,13,16, 18, 19, 20, 
                                                   21, 22, 23, 24, 25, 26, 27, 28, 29), 
               gbm.y = 17, family = "bernoulli", tree.complexity = 5,
               learning.rate = 0.0005, bag.fraction = 0.75)
mod
plot(mod)
summary(mod)
gbm.plot(mod, n.plots=10, plot.layout=c(3,3), write.title = FALSE)


Davies<-SasData %>%
  filter(Reef=="Davies")

Heron<-SasData %>%
  filter(Reef=="Heron")

Moore<-SasData %>%
  filter(Reef=="Moore")

modDavies<- gbm.step(data=data.frame(Davies), gbm.x = c(6,7,8,9,10,11,12,13,16, 18, 19, 20, 
                                                        21, 22, 23, 24, 25, 26, 27, 28, 29), 
                     gbm.y = 17, family = "bernoulli", tree.complexity = 5,
                     learning.rate = 0.00005, bag.fraction = 0.75)
modDavies
plot(modDavies)
summary(modDavies)
gbm.plot(mod, n.plots=10, plot.layout=c(3,3), write.title = FALSE)


modHeron<- gbm.step(data=data.frame(Heron), gbm.x = c(6,7,8,9,10,11,12,13,16, 18, 19, 20, 
                                                      21, 22, 23, 24, 25, 26, 27, 28, 29), 
                    gbm.y = 17, family = "bernoulli", tree.complexity = 5,
                    learning.rate = 0.0001, bag.fraction = 0.75)
modHeron
plot(modHeron)
summary(modHeron)
gbm.plot(mod, n.plots=10, plot.layout=c(3,3), write.title = FALSE)


modMoore<- gbm.step(data=data.frame(Moore), gbm.x = c(2,4,5,6,7,8,9,10,11,12,13,16), gbm.y = 17, family = "bernoulli", tree.complexity = 5,
                    learning.rate = 0.001, bag.fraction = 0.75)
mod
summary(modMoore)
plot(modMoore)
gbm.plot(mod, n.plots=10, plot.layout=c(3,3), write.title = FALSE)


SasData %>%
  ggplot(aes(y=percentile_10, x=percentile_90))+
  geom_point()
