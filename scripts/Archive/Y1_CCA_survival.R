install.packages("tidyverse")
install.packages("sjPlot")
install.packages("svglite")

library (tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(readxl)
library(vegan)
library(GGally)
library(corrplot)
library(car)
library(mvabund)
library(scales)
library(ggrepel)
library(readxl)
library(FactoMineR)
library(factoextra)
library(Hmisc)
library(corrplot)
library(stats)
install.packages("corrr")
library('corrr')
library(ggcorrplot)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(glmm)
library(glmmTMB)
library(emmeans)
library(performance) #for residuals diagnostics
library(DHARMa)   #for residuals and diagnostics
library(broom)
library(reshape2)
install.packages("ggeffects")
library(ggeffects)
install.packages("pdp")
install.packages("DALEX")
library(pdp)
library(DALEX)
library(GGally)
library(sjPlot)

# All sites

#import data by choosing file
TransSurv <- read.csv(file.choose(), header = TRUE)

# check data
head (TransSurv)

#Edit data to remove NAs & 0s
TransSurv <- filter(TransSurv, Tfinal_surv !="NA")

#Mutate data to indicate if factor or numeric
TransSurv <- TransSurv %>%
  mutate(Reef=factor(Reef), T0_CCA= as.numeric(T0_CCA),T0_predeployment_surv= as.numeric(T0_predeployment_surv), T1_vessel_surv= as.numeric(T1_vessel_surv), T2_vessel_surv= as.numeric(T2_vessel_surv), Tfinal_surv= as.numeric(Tfinal_surv))

str(TransSurv)

#GLM
allsite <- glm(Tfinal_surv ~ T0_CCA, data = TransSurv, family = "binomial")
summary(allsite)

#Davies

#import data by choosing file
DaviesSurv <- read.csv(file.choose(), header = TRUE)

# check data
head (DaviesSurv)

#Edit data to remove NAs & 0s
DaviesSurv <- filter(DaviesSurv, Tfinal_surv !="NA")

#Mutate data to indicate if factor or numeric
DaviesSurv <- DaviesSurv %>%
  mutate(Reef=factor(Reef), T0_CCA= as.numeric(T0_CCA),T0_predeployment_surv= as.numeric(T0_predeployment_surv), T1_vessel_surv= as.numeric(T1_vessel_surv), T2_vessel_surv= as.numeric(T2_vessel_surv), Tfinal_surv= as.numeric(Tfinal_surv))

str(DaviesSurv)

#GLM
Davies <- glm(Tfinal_surv ~ T0_CCA, data = DaviesSurv, family = "binomial")
summary(Davies)

#Moore

#import data by choosing file
MooreSurv <- read.csv(file.choose(), header = TRUE)

# check data
head (MooreSurv)

#Edit data to remove NAs & 0s
MooreSurv <- filter(MooreSurv, Tfinal_surv !="NA")

#Mutate data to indicate if factor or numeric
MooreSurv <- MooreSurv %>%
  mutate(Reef=factor(Reef), T0_CCA= as.numeric(T0_CCA),T0_predeployment_surv= as.numeric(T0_predeployment_surv), T1_vessel_surv= as.numeric(T1_vessel_surv), T2_vessel_surv= as.numeric(T2_vessel_surv), Tfinal_surv= as.numeric(Tfinal_surv))

str(MooreSurv)

#GLM
Moore <- glm(Tfinal_surv ~ T0_CCA, data = MooreSurv, family = "binomial")
summary(Moore)

#Heron

#import data by choosing file
HeronSurv <- read.csv(file.choose(), header = TRUE)

# check data
head (HeronSurv)

#Edit data to remove NAs & 0s
HeronSurv <- filter(HeronSurv, Tfinal_surv !="NA")

#Mutate data to indicate if factor or numeric
HeronSurv <- HeronSurv %>%
  mutate(Reef=factor(Reef), Site=factor(Site), T0_CCA= as.numeric(T0_CCA),T0_predeployment_surv= as.numeric(T0_predeployment_surv), T1_vessel_surv= as.numeric(T1_vessel_surv), T2_vessel_surv= as.numeric(T2_vessel_surv), Tfinal_surv= as.numeric(Tfinal_surv))

str(HeronSurv)

#GLMM
Heron <- glmmTMB(Tfinal_surv ~ T0_CCA + (1 | Site), data = HeronSurv, family = "binomial")
summary(Heron)

#Plotting results
ggplot(DaviesSurv, aes(x = T0_CCA, y = Tfinal_surv)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_bw() +
  ylab("Survival") +
  theme(axis.text = element_text(angle = 90, vjust = 0.5))

ggplot(MooreSurv, aes(x = T0_CCA, y = Tfinal_surv)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_bw() +
  ylab("Survival") +
  theme(axis.text = element_text(angle = 90, vjust = 0.5))

ggplot(HeronSurv, aes(x = T0_CCA, y = Tfinal_surv)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_bw() +
  ylab("Survival") +
  theme(axis.text = element_text(angle = 90, vjust = 0.5))
