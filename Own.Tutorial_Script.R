           # Data Science Tutorial - Basic Statistics and Cartography in R
                                  # 04/12-21
                          # s1865014@ed.ac.uk (@loulillelund)
                              # Lucas Lillelund


##################################################################
##                         Introduction                         ##
##                      Manipulating Dataset                     ##
##                          Histograms                          ##
##                              MAP                             ##
##                     Statistical Analysis                     ##
##################################################################


## The purpose of this script is to import South Alaskan Orca abundance data,                  ##
## obtained from https://www.fisheries.noaa.gov/species/killer-whale, tidy/manipulate          ##
## the data such that an average of the various abundance estimates for each pod is generated  ##
## and to make it clear which subregion each Orca pod corresponds to. Still, this script also  ##
## serves to understand the distribution of this average pod size data, make a basic map of    ##
## Alaska where these two subregions are highlighted differently to communicate different      ##
## average Orca pod size present in each.                                                      ##


### Introduction ----

#Firstly, we need to load packages for access to the functions in operation
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

KillerWhaleData <- read.csv("~/Desktop/Uni/Year_4_(Honours)/DataScience/Tutorial_loulillelund/Data/KillerWhalePods(NOAA.2017).csv", sep = ";")
view(KillerWhaleData)
str(KillerWhaleData)


### Manipulating Dataset ----


KillerWhaleData$X1999.2000.Estimate <- as.numeric(KillerWhaleData$X1999.2000.Estimate)
class(KillerWhaleData$X1999.2000.Estimate)
KillerWhaleData$X2001.04.Estimate <- as.numeric(KillerWhaleData$X2001.04.Estimate)
KillerWhaleData$X2005.12.Estimate <- as.numeric(KillerWhaleData$X2005.12.Estimate)

#Taking mean of all 3 estimates
KillerWhaleData$Mean.Estimate <- rowMeans(KillerWhaleData[, c(3, 4, 5)])

#Renaming X column to subregion
KillerWhaleData <- rename(KillerWhaleData, "subregion" = "X")

#Remove sums and 'Unassigned to pods' Pod ID
KillerWhaleData <- slice(KillerWhaleData, -c(4, 29, 30))

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

length(KillerWhaleData$Mean.Estimate)


### Histograms ----

hist(KillerWhaleData$Mean.Estimate) #base R

(SouthAlaskanOrcaPods_histogram <- ggplot(KillerWhaleData, aes(x = Mean.Estimate, fill = subregion)) +  
    geom_histogram(stat = "count") +
    geom_vline(aes(xintercept = mean(Mean.Estimate)),            
               colour = "red", linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("#97F7C5", "#4ED973", "#08873D")) +          
    # Adding custom colours    
    labs(x = "\n Mean Estimate", y = "Frequency \n",                   
         # Adding x and y axis labels.
         # "\n" adds space before x and after y axis text
         caption = "\n Fig.1 The response variable, mean estimate of Orca pod abundance reveals a right-skewed normal distribution with a mean at around 18 Orcas (n=27).") +   
    # Adding informative figure caption
    # caption = "\n Fig.1") +  # Adding caption for figure in panel
    Theme.Tutorial() +  # Adding our personalised theme
    guides(fill = guide_legend(title = "subregion")))
# Adding an informative legend title

#Creating my own theme for the tutorial...based on challenge2

Theme.Tutorial <- function(){            # Creating a tutorial custom theme function
  # defining font, font sizes, alignment and text angle
  theme(plot.title    = element_text(size = 20, 
                                     face = "bold"),
        plot.subtitle = element_text(size = 16, 
                                     face = "plain"),
        axis.title    = element_text(size = 15,
                                     face = "bold"),
        axis.text.x   = element_text(size = 12,
                                     angle = 45,
                                     vjust = 1,
                                     hjust = 1, 
                                     face = "bold"), 
        axis.text.y   = element_text(size = 12, 
                                     face = "bold"),
        legend.position = c(0.88, 0.8),                                # Legend removal
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Plot margins
        panel.grid = element_blank())
}


### MAP ----

#Load World Coordinate Data
WorldCoords <- map_data("world") # Create dataframe for coordinates of all countries
str(WorldCoords)
WorldCoords$lat <- as.numeric(WorldCoords$lat)
WorldCoords$long <- as.numeric(WorldCoords$long)

#Filtering for coordinates only of Alaska, USA
AlaskaCoords <- filter(WorldCoords, WorldCoords$subregion == "Alaska", preserve = TRUE)

AlaskaCoords$lat <- as.integer(AlaskaCoords$lat)
AlaskaCoords$long <- as.integer(AlaskaCoords$long)
  
#removing outlier coordintes i.e. ones with highly positive long and very low positive lat
AlaskaCoordsCln <-subset(AlaskaCoords, long<0)

## Subsetting two dataframes to only 'Prince William Sound' and only 'Southeast Alaska' coordinates

#Adding location variable to both datasets, for merging
class(AlaskaCoords$lat)

#Firstly, for Southeast Alaska (SEA)
AlaskaCoordsSEA <- group_by(AlaskaCoords, lat = "57.58", long = "-135.48") %>%
  transmute(subregion = "Southeast Alaska") %>%
  distinct(subregion)

#Secondly, for Prince William Sound (PWS)
AlaskaCoordsPWS <- group_by(AlaskaCoords, lat = "60.63", long = "-147.3") %>%
  transmute(subregion = "Prince William Sound") %>%
  distinct(subregion) 

mean(KillerWhaleData$Mean.Estimate[1:3]) #Mean pod size in Southeastern Alaska subregion
mean(KillerWhaleData$Mean.Estimate[4:27]) #Mean pod size in Prince William Sound subregion

## Add a mean pod size estimate for Alaska as 0
AlaskaCoordsCln$Mean.Estimate <- c(0)
AlaskaCoordsPWS$Mean.Estimate <- c(15.31) #doing the same for the PWS coordinates df
AlaskaCoordsSEA$Mean.Estimate <- c(33.78) #likewise for the SEA coordinates df

#Giving SEA and PWS a group number
AlaskaCoordsPWS$group <- c(1) 
AlaskaCoordsSEA$group <- c(2)

#Adding two extra set of coordinates for PWS for mapping purposes
PWSextracoordA <- data.frame(-146.3, 60.63, "Prince William Sound", 15.31, 1)
names(PWSextracoordA) <- c("long", "lat", "subregion", "Mean.Estimate", "group")

PWSextracoordB <- data.frame(-147.3, 61.63, "Prince William Sound", 15.31, 1)
names(PWSextracoordB) <- c("long", "lat", "subregion", "Mean.Estimate", "group")

PWSextraCoords <- rbind(PWSextracoordA, PWSextracoordB)
AlaskaCoordsPWS <- rbind(PWSextraCoords, AlaskaCoordsPWS)


#Adding two extra set of coordinates for SEA for mapping purposes
SEAextracoordA <- data.frame(-134.48, 57.58, "Southeast Alaska", 33.78, 2)
names(SEAextracoordA) <- c("long", "lat", "subregion", "Mean.Estimate", "group")

SEAextracoordB <- data.frame(-135.48, 56.58, "Southeast Alaska", 33.78, 2)
names(SEAextracoordB) <- c("long", "lat", "subregion", "Mean.Estimate", "group")


SEAextraCoords <- rbind(SEAextracoordA, SEAextracoordB)
SEAextraCoords <- select(SEAextraCoords, long, lat, group, subregion, Mean.Estimate)

AlaskaCoordsSEA <- rbind(SEAextraCoords, AlaskaCoordsSEA)

SEAandPWS_Coords <- rbind(AlaskaCoordsPWS, AlaskaCoordsSEA) #merging the two subregion coordinate dataframes

AlaskaCoordsCln <- subset(AlaskaCoordsCln, select = -c(order, region)) #removing order and region columns from Alaska coords df

#Arrange columns in same order

SEAandPWS_Coords <- select(SEAandPWS_Coords, long, lat, group, subregion, Mean.Estimate)
str(SEAandPWS_Coords)
SEAandPWS_Coords$long <- as.numeric(SEAandPWS_Coords$long)
SEAandPWS_Coords$lat <- as.numeric(SEAandPWS_Coords$lat)


Tutorial.Map <- rbind(SEAandPWS_Coords, AlaskaCoordsCln) 


(Tut.Map <- ggplot(Tutorial.Map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = subregion), color = "black") +
    labs(title = "Map of Alaska and Two Subregions") +
    theme(plot.title = element_text(size = 25 , face = "bold.italic", hjust = 0.5)) +
    Theme.Tutorial())


### Statistical Analysis ----
  
OrcaPodAbundancevsID.ANOVA <- aov(Mean.Estimate~Pod.ID, data = KillerWhaleData)
summary(OrcaPodAbundancevsID.ANOVA)


str(KillerWhaleData)

KillerWhaleData$Pod.ID <- as.factor(KillerWhaleData$Pod.ID)
AvgOrcaPodSizeVSsubregion.ANOVA <- aov(KillerWhaleData$Mean.Estimate~KillerWhaleData$subregion, data = KillerWhaleData)
summary(AvgOrcaPodSizeVSsubregion.ANOVA)

mean(KillerWhaleData)

#Testing if residuals are normally distributed
AvgOrcaPodSizeVSsubregion.resids <- resid(AvgOrcaPodSizeVSsubregion.ANOVA)
shapiro.test(AvgOrcaPodSizeVSsubregion.resids)

#Bartlett test for homoscedastic variances
bartlett.test(Mean.Estimate~subregion,data=KillerWhaleData)
