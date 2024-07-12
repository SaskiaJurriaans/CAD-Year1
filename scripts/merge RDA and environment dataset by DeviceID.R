# Saskia, April 19, 2024

#Merge Benthic data with Environment dataset so that both datasets have equal number of rows (representing ReefDev). This is necessary because some devices were not imaged/analysed using the benthic quadrats. Consequently, the RDA output with PC1 and PC2 does not fit in with the Environment dataset.
#Steps to take:
#1. load data
#2. In benthic dataset, create Device_ID column based on ReefDev column
#3. In RDA dataset, create Device_ID column based on label column ("sit1, sit2, etc).
#4. Merge benthic, environment and RDA datasets


#load data --> FinalMetrics_Summary.xlsx
#Y1Environment<-FinalMetrics_Summary <- read_excel("data/FinalMetrics_Summary.xlsx")
#name df: Y1Environment

#load data --> YEAR1Benthic.xlsx
#Y1Benthic <- read_excel("data/YEAR1_Benthic_data.xlsx")
#name df: Y1Benthic

#load data RDA Davies
#RDA_Davies <- read_excel("output/Community Composition/RDA sites Davies.xlsx")
#name df: RDA_Davies

#load data RDA Heron
#RDA_Heron <- read_excel("output/Community Composition/RDA sites Heron.xlsx")
#name df: RDA_Heron

#load data RDA Moore
#RDA_Moore <- read_excel("output/Community Composition/RDA sites Moore.xlsx")
#name df: RDA_Moore

library(dplyr)
library(tidyr)

#Create new column with Device_ID which is just the number instead of ReefDev ("Davies_1") in Y1Benthic
Y1Benthic <- Y1Benthic |>
  mutate(Device_ID = sub(".*_", "", ReefDev))

# Merge the two data frames by Site
BenthicEnv <- merge(Y1Benthic, Y1Environment, by = c("Reef", "Site"), all = TRUE) 
BenthicEnv <- BenthicEnv |>
  select(-c(4:24))

#create net column with Device_ID instead of site label (which corresponds to ReefDev number)
RDA_Davies <- RDA_Davies |>
  mutate(Device_ID = as.numeric(sub("sit", "", label)),
         Reef = "Davies")

RDA_Moore <- RDA_Moore |>
  mutate(Device_ID = as.numeric(sub("sit", "", label)),
         Reef = "Moore")

RDA_Heron <- RDA_Heron |>
  mutate(Device_ID = as.numeric(sub("sit", "", label)),
         Reef = "Heron")

RDA <-rbind(RDA_Davies,RDA_Heron,RDA_Moore)


#combine RDA and BenticEnv
Y1_Benthic_Environment_DevID <- merge(RDA, BenthicEnv, by = c("Reef", "Device_ID"), all = TRUE)

Y1_Benthic_Environment_DevID <- Y1_Benthic_Environment_DevID |>
  arrange(Reef, Site, Device_ID)

#save dataframe
library(openxlsx)
write.xlsx(Y1_Benthic_Environment_DevID, "YEAR1 Benthic Environment DeviceID.xlsx", rowName = FALSE)
