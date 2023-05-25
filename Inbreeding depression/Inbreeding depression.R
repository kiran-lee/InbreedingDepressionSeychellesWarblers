library(readr)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library("data.table") 

#Link BirdID from warbler database to sequenced samples ----
##Read files
SequencedIndividuals<-read_excel("FROH_SW.xlsx")
SequencedIndividuals$SeqID<-SequencedIndividuals$ID
SequencedIndividuals<-SequencedIndividuals %>%
  rename("ManualBirdID"="BirdID") 

Identifiers<-read_excel("SheffieldSubmissions.xlsx")
Identifiers26076<-read_excel("ID 26076_Sample information table.xlsx")
PilotIdentifiers<-read_excel("SamplesForPilotTargetCapture_290119_sortBTN_Qubit.xlsx")

##Clean ID numbers
PilotSequences<-subset(SequencedIndividuals, SequencedIndividuals$Plate=="LIMS24675"|SequencedIndividuals$Plate=="LIMS25133")
PilotSequences$ID<-trimws(sapply(strsplit(PilotSequences$ID, "_"), `[[`, 2))
MissingBloodIDSequences<-subset(SequencedIndividuals, SequencedIndividuals$Plate == "LIMS26076p4"|SequencedIndividuals$Plate == "LIMS26076raw")
MissingBloodIDSequences$ID<-as.numeric(sapply(strsplit(MissingBloodIDSequences$ID, "_"), "[[", 1))
BloodIDSequences<-subset(SequencedIndividuals, SequencedIndividuals$Plate!="LIMS24675"& SequencedIndividuals$Plate!="LIMS25133"&SequencedIndividuals$Plate != "LIMS26076p4"&SequencedIndividuals$Plate != "LIMS26076raw" )
BloodIDSequences$ID<-BloodIDSequences$ID <- sub('_repeat','',BloodIDSequences$ID)
BloodIDSequences$ID<-BloodIDSequences$ID <- sub('-repeat','',BloodIDSequences$ID)
BloodIDSequences$ID<-trimws(sapply(strsplit(BloodIDSequences$ID, "_"), `[[`, 1))
BloodIDSequences$ID<-trimws(sapply(strsplit(BloodIDSequences$ID, "-"), `[[`, 2))
BloodIDSequences$ID<-sub('.+-(.+)', '\\1', BloodIDSequences$ID)

##Pilot sequences use Blood Tube Number, the rest use BloodID
PilotSequences$Identifier<-paste("BloodTubeNumber")
MissingBloodIDSequences$Identifier<-paste("BloodID")
BloodIDSequences$Identifier<-paste("BloodID")

##Get BirdIDs from IDs
PilotSequences$ID<-as.numeric(PilotSequences$ID)
PilotSequences <- PilotSequences %>% 
  left_join(select(PilotIdentifiers, BirdID, BloodTubeNumber), by = c("ID" = "BloodTubeNumber"))

BloodIDSequences$ID<-as.numeric(BloodIDSequences$ID)
BloodIDSequences <- BloodIDSequences %>% 
  left_join(select(Identifiers, BirdID, BloodID), by = c("ID" = "BloodID"))

MissingBloodIDSequences$ID<-as.numeric(MissingBloodIDSequences$ID)
Identifiers26076$Sample_number<-as.numeric(Identifiers26076$`Sample number`)
Identifiers26076$BloodID<-as.numeric(Identifiers26076$`Sample name`)
MissingBloodIDSequences <- MissingBloodIDSequences %>% 
  left_join(select(Identifiers26076, Sample_number, BloodID), by = c("ID" = "Sample_number"))
MissingBloodIDSequences <- MissingBloodIDSequences %>% 
  left_join(select(Identifiers, BirdID, BloodID), by = c("BloodID" = "BloodID"))



#Lifespan ----
##Read files
BirthDate <- read_csv("BirthDate27032023.csv", col_types = cols(BirthDate = col_date(format = "%d/%m/%Y"))) #In query table, this is BirdID
LastSeenYear <- read_csv("CurrentBTOextended27032023.csv") #In query table, this is CurrentBTOextended
##Make terms
###BirthYear from BirthDate
BirthDate <- BirthDate %>% 
  mutate(BirthYear = format(BirthDate, "%Y")) %>%
  mutate(BirthYear = as.numeric(BirthYear))
###Lifespan
Lifespan <- merge(BirthDate,LastSeenYear,by="BirdID", all = TRUE) %>% 
  mutate(LastSeenYea = as.numeric(LastSeenYea)) %>%
  mutate(Lifespan = LastSeenYea - BirthYear) %>%
  filter(LastSeenYea < 2022)  %>%
  select(BirdID,Lifespan,LastSeenYea) 
#filter set at 2022, because the definition of alive/dead is an individual has not been observed for two consecutive fieldseasons. Hence, next year, can change this to 2023.
Sequenced<-read_excel("FROH_SW.xlsx")
Sequenced<- merge(Sequenced, Lifespan, by="BirdID", all = FALSE)


ROHxLifespan<-ggplot(Sequenced, aes(x=F, y=Lifespan)) + geom_point()
ROHxLifespan
