## CC Tutorial - Making a CC Tutorial
# 04/12-21
# s1865014@ed.ac.uk @loulillelund) - Lucas L.

#Introduction ----

#Firstly, we need to load packagages for access to the functions in operation
#and the installation of new/un-installed packages
install.packages("rgbif")
install.packages("RColorbrewer")
install.packages("ggExtra")
install.packages("terra")

library(rgbif)
library(bannerCommenter)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggmap)
library(ggthemes)
library(ggExtra)
library(broom)
library(readr)
library(terra)


#Load Data

#load population density data
#load coordinate (Laittudinal,latitudinal coord data (World_map))
#get zoomed in geography on e.g. the Pacific Ocean

getwd()

KillerWhaleData <- read.csv("~/Desktop/Uni/Year_4_(Honours)/DataScience/Tutorial_loulillelund/KillerWhalePods(NOAA.2017).csv", sep = ";")
view(KillerWhaleData)
str(KillerWhaleData)


### Manipulating Dataset ----


KillerWhaleData$X1999.2000.Estimate <- as.numeric(KillerWhaleData$X1999.2000.Estimate)
class(KillerWhaleData$X1999.2000.Estimate)
KillerWhaleData$X2001.04.Estimate <- as.numeric(KillerWhaleData$X2001.04.Estimate)
KillerWhaleData$X2005.12.Estimate <- as.numeric(KillerWhaleData$X2005.12.Estimate)

#Taking mean of all 3 estimates
KillerWhaleData$Mean.Estimate <- rowMeans(KillerWhaleData[, c(3, 4, 5)])

#Load World Coordinate Data
WorldCoords <- map_data("world") # Create dataframe for coordinates of all countries
str(WorldCoords)
WorldCoords$lat <- as.numeric(WorldCoords$lat)
WorldCoords$long <- as.numeric(WorldCoords$long)

#Filtering for coordinates only of Alaska, USA
AlaskaCoords <- filter(WorldCoords, WorldCoords$subregion == "Alaska", preserve = TRUE) 
  group_by(lat = 57.58, long = -135.48) %>%
  transmute(subregion = "Southeast Alaska") %>%
  ungroup() %>%
  group_by(lat = 60.63, long = -147.32) %>%
  mutate(subregion = "Prince William Sound") 

AlaskaCoords$lat <- as.integer(AlaskaCoords$lat)
AlaskaCoords$long <- as.integer(AlaskaCoords$long)
  
#removing outlier coordintes i.e. ones with highly positive long and very low positive lat
AlaskaCoords1 <- slice(AlaskaCoords, 13:3133, preserve = TRUE)
AlaskaCoords2 <- slice(AlaskaCoords1, 50:3122, preserve = TRUE)
AlaskaCoords3 <- AlaskaCoords %>% slice(-c(1:12))

AlaskaCoords.Clean <- AlaskaCoords[!(AlaskaCoords$long>0)] 

AlaskaCoordsCln <-subset(AlaskaCoords, long<0)

## Subsetting two dataframes to only 'Prince William Sound' and only 'Southeast Alaska' coordinates

#Adding location variable to both datasets, for merging
class(AlaskaCoords$lat)

#Firstly, for Prince William Sound (PWS)
AlaskaCoordsPWS <- mutate(AlaskaCoords$lat ~ 60.63 & long ~ -147.3, subregion = "Prince William Sound")
AlaskaCoords$subregion[AlaskaCoords$lat~60]AlaskaCoords$long~-147][<-"Prince William Sound"
                                                                   
#Nextly, for Southeast Alaska (SEA)
AlaskaCoordsSEA <- group_by(AlaskaCoords, lat = "57.58", long = "-135.48") %>%
  transmute(subregion = "Southeast Alaska") %>%
  distinct(subregion)

#Now, to add a verticle column (not row as it is now) referring to the sub-region in which a pod is located

#class(KillerWhaleData$X1999.2000.Estimate)
#KillerWhaleData$X1999.2000.Estimate <- as.numeric(KillerWhaleData$X1999.2000.Estimate)
#class(KillerWhaleData$Pod.ID)
#KillerWhaleData$Pod.ID <- as.character(KillerWhaleData$Pod.ID)
#KillerWhaleData <- KillerWhaleData %>%
  #mutate(KillerWhaleData$subregion == "Southeast Alaska")

class(KillerWhaleData$Mean.Estimate)
  
#Creating subregion column for Killer Whale pods
KillerWhaleData <- transform(KillerWhaleData, subregion= ifelse(Pod.ID==c52, "Southeast Alaska", "Prince William Sound"))

KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AA1"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AA30"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AB"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AB25"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AD05"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AD16"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AE"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AH01"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AH20"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AI"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AJ"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AK"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AL"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AN10"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AN20"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AS2"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AS30"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AW"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AX01"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AX27"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AX32"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AX40"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AX48"]<-"Prince William Sound"
KillerWhaleData$subregion[KillerWhaleData$Pod.ID=="AY"]<-"Prince William Sound"

Alaska_SEAOrcas_map <- left_join(AlaskaCoordsSEA, KillerWhaleData, by = "subregion")
Alaska_Map <- left_join(AlaskaCoords, KillerWhaleData, by = "subregion")
                               
(AlaskaOrcas_Map1 <- ggplot(AlaskaCoordsCln, aes(x = long, y = lat, group = group)) + 
geom_polygon(aes(fill = "group"), colour = 'gray45', size = 0.1) + 
    labs(title = "Map of Alaska") +
    theme(plot.title = element_text(size = 25 , face = "bold.italic", hjust = 0.5),
          legend.position = c(0.88, 0.5))) + theme(panel.background = element_rect(fill = 'white'),
axis.line = element_line(color = NA), 
axis.text = element_blank(), axis.ticks = element_blank(), 
axis.title = element_blank())
  


(AlaskaSEAOrcas_Map <- leftjoin

#coord_map("ortho", orientation = c(90, 0, 0), ylim = c(0, 90) + 
#fill = "#5bbcd6" colour = "black", shape = 21, size = 4, alpha = 0.6)
                               
  