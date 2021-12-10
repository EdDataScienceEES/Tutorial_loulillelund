_Tutorial by Lucas Lillelund_

![](Orcinusorca(NOAA).png)


# Basic Statistics and Cartography in R Tutorial

**Tutorial Objectives:**
1) Access and use environmental/ecological data
2) Manipulate an ecological dataset
  a. Propose a research question
3) Understand the data
  a. Describing the distribution
  b. Consider ANOVA assumptions
4) Make a map!
5) Conduct a 1-way ANOVA, a Shapiro-wilk normality test and a Bartlett test of equal variances


---

Hello, clever coders!

Today, I want to briefly teach ya'll about how about some of the basic ways to understand and visualise ecological data. In this tutorial, I want to explore, with you, how to import a dataset of species population abundance, make a basic map depicting depicting the distribution of population abundances and conduct a 1-way ANalysis Of VAriance (ANOVA) to determine whether population abundances have signficantly changed over the course of a monitoring period.

To set the scene, as the Ocean's top-level predator, Killer Whales (Orcinus orca) have become the most widely distributed cetacean, in the world. Orcas forage for food in pods, some of which occur in the Northern Pacific Ocean along the coastline of Alaska. Over the past century or so, this beastly species has become increasingly threatened by human activity and environmental degradation which has led to their protection under the Marine Mammal Protection Act as well as scientific monitoring of their populations by organisations like the National Oceanic and Atmospheric Administration (NOAA), headquartered in Maryland, USA. NOAA fisheries produces regular stock assessment reports for various keystone marine species for various purposes; however, for today we will make use of their 2017 Alaska resident report.


## Getting Started

*NB: The following tutorial instructions are based on a mac with the software macOS Big Sur (Version 11.3.1), so specifics may be different for a windows device, yet the fundamentals still apply.*

Fear not, if you are new to the R language or coding in general, as the tutorial is meant for all levels. RStudio, the GUI which allows us to use R can be dowloaded from: https://cran.r-project.org/

Now, open 'RStudio', create a new project under 'File/New Project...'. From here, you'll need to create an R script file to write your code, you can do this via 'File/New File/R Script'. Now, it is good practice to structure and organize your script file, this can be done using simple headings like:

```r
# CC Tutorial - Own Tutorial
# 04/12-21
# s1865014@ed.ac.uk (@loulillelund)
# Lucas Lillelund
```

You should, of course, replace my personal information wiht your own and otherwise modify to your liking. Next, for an coding endeavour, it is key to understand which 'packages' your are going to be working with. The '#' sign prior to the text facilitates annotation in your script, whereas without this sign, R will understand the text to be a line of code. For those unaware, packages are used to supplement base R functions to enhance the coding process, in some way or another, for example, by computing an output more efficiently. You can use the following packages for this tutorial.

```r
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
```

You can download the modified data from the repository with the name "KillerWhalePods(NOAA.2017).csv". Using this population abundance data, we will associate these magnitudes with location along the Alaskan coast, in order to produce a map, locating the multiple pod of various sizes along the coast. Exemplar code for how you may import the datafile with your absolute file path after downloading it to your local machine (replacing my path with yours):

```r
KillerWhaleData <- read.csv("~/Desktop/Uni/Year_4_(Honours)/DataScience/Tutorial_loulillelund/KillerWhalePods(NOAA.2017).csv", sep = ";") #Import datafile
view(KillerWhaleData)
str(KillerWhaleData) #Examine structure of data
```

This data file, obtained from NOAA (2021), provides a list of Killer Whale abundance estimates associated with certain recognized pods along the southern coast of Alaska, USA. There are two general regions which the 27 pods are considered local to: "Southeast Alaska" and "Price William Sound", the latter region here being in the southwest of Alaska.

The third line of code describes structural aspects of the data, including the nature of the variables in each column, namely that qualitative categorical variables such as the 'Pod.ID' and the 'subregion' are what's known as "character" variables versus the quantitative population abundance estimates which are "numeric" variables.


## Manipulating an Ecological Dataset

Now that we have the data and understanding its basic features, we can begin to manipulate it to our fancy. For instance, we can compute the mean values of the different year abundance estimates, rename a variable and remove unwanted datapoints using following lines of code:

```r
KillerWhaleData$Mean.Estimate <- rowMeans(KillerWhaleData[, c(3, 4, 5)]) #Generating mean values
KillerWhaleData <- rename(KillerWhaleData, "subregion" = "X") #Renaming variable/column
KillerWhaleData <- slice(KillerWhaleData, -c(4, 29, 30)) #Removing subregion sums and 'Unassigned to pods' Pod ID
```

Hopefully, the above code makes clear sense, as the goal of it is to help with functions and outputs we want to produce later on, for example, the 3 datapoints removed are not classified according to a specific individual Orca pod which would interfere in investigating the following research question.


### Devising a Research Q

With the dataset looking more orderly, we can now decide on a research question to investigate with statistical functions in R! Let's say, we are interested in the differences in the average Orca pod sizes between the two subregions, so as to determine whether these locations have a signficant impact on the average pod size. We might hypothesize that there is a significant difference in average Orca pod size across Orca pods located in either Prince William Sound and Southeast Alaska.

For this question, a common statistical test used is a 1-way ANOVA to determine whether or not there is a significant amount of variance between data points in a dataset given certain assumptions. We will come back to the actual test later on in the tutorial, but for now let's explore the distribution so as to better understand how the mean abundance estimate data are distributed.

```r
hist(KillerWhaleData$Mean.Estimate)
```

![~/Desktop/Uni/Year_4_(Honours)/DataScience/Tutorial_loulillelund/Outputs/BasicAvgOrcaPodSize_Hist.png]("BasicAvgOrcaPodSize_Hist.png")

This histogram here visualises the distribution of the mean estimated Orca abundances for the 27 Orca pods located along the South Alaskan coastline. As the figure depicts, the data seem right-skewed with a frequency peak between 10 and 20 Orcas as the most common pod size. We might presume to distribution to be Gaussian or normal, still. You might beautify this histogram with the following code, using the ggplot2 package:


```r
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
        legend.position = "none",                                # Legend removal
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Plot margins
        panel.grid = element_blank())
}
```

Firstly, you should run the above in your 'Rstudio' to define a customised theme function for the purpose of this tutorial, this can be used for future visualisations in 'ggplot2' and simplifies the process of having to iteratively improve certain aspects of a figure. Below, lies the code for producing your nice histogram of the mean abundance estimates distribution for the Orca Pods belonging to the Prince William Sound and Southeastern Alaska regions.

```r
(SouthAlaskanOrcaPods_histogram <- ggplot(KillerWhaleData, aes(x = Mean.Estimate, fill = subregion)) +  
    geom_histogram(stat = "count") +
    geom_vline(aes(xintercept = mean(Mean.Estimate)),            
               colour = "red", linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("#97F7C5", "#4ED973", "#08873D")) +          
    # Adding custom colours    
    labs(x = "\n Mean Estimate", y = "Frequency \n",                   
         # Adding axis labels.
         # "\n" adds space before between the axes text and the figure
         caption = "\n Fig.1 The response variable, mean estimate of Orca pod abundance reveals a right-skewed normal distribution with a mean at around 18 Orcas (n=27).") +   
    # Adding an informative figure caption
    # caption = "\n Fig.1") +  
    Theme.Tutorial() +  # Adding the tutorial custom theme
    guides(fill = guide_legend(title = "subregion")))
```

![~/Desktop/Uni/Year_4_(Honours)/DataScience/Tutorial_loulillelund/Outputs/OrcaPod.MeanAbundances_Hist.png]("OrcaPod.MeanAbundances_Hist.png")


The nicer histogram figure adds an element of aesthetic and visual appeal which can be nice in communicating science. Also there is an additional layter of complexity in that there is a dashed line depicting where the mean of the distribution lies, still pods belong to the two subregions are differentiated and the text is more fitting.


### Considering Assumptions

... Now, looking back at our ANOVA hypothetical from earlier, in order to in essence test whether Orca pod ID significantly affects the mean estimate of the orca pod size, a linear model is made to describe this functional relationship and in doing so assumes three major things! That is...

1) The model residuals are normally distributed.
2) The variances assume homoscedasticity.
3) Each observation is independent from one another.

We will come back to how we can assess the first two, with visual plots using code in R and also run code to perform certain statistical tests like the shapiro-wilk test for the normal distribution of model residuals and the bartlett test to test for homoscedasticy in model variances.

As for the third, this relates to how the observations were made by NOAA in the first place, are monitored species observations/counts over the years related to one another over time, for instance were the observations made in the same season e.g. breeding season? This would clearly affect population size from one year to another. Alternatively, does the ability to conduct species counts, accurately, depend on the surveyor (perhaps not for Orcas, as they are quite easy to identify). However, there might be several retrospective questions to be asked about how the data relates to itself, how much overall constancy there is and how other temporal and spatial factors (e.g. less habitable space for certain pods?) may vary between individual population (pod) abundance estimates.

## Making a Map!

... Have you ever made your own map? ... You HAVEN'T?!?!?! Well, let me tell you! you... have come to just the right place ;) For this part of the tutorial, I would like to illustrate how you can create a simple map in R, a map depicting Alaska along with the two subregions, Prince William Sound and Southeast Alaska, which are different colors from one another. The two colors, theoretically could, represent different average pod sizes for the two subregions, with southeast Alaska having a greater average pod size. The average pod size for these two subregions can be determined using the code:

```r
mean(KillerWhaleData$Mean.Estimate[1:3]) #Mean pod size in Southeastern Alaska subregion
mean(KillerWhaleData$Mean.Estimate[4:27]) #Mean pod size in Prince William Sound subregion
```

From this we can say the average pod size in Southeast Alaska is around 34 Orcas whereas that of Prince William Sound is approximately 15 Orcas. Now, for the map...


First, you can run this code:

```r
WorldCoords <- map_data("world") # Create dataframe for coordinates of all countries
str(WorldCoords)
WorldCoords$lat <- as.numeric(WorldCoords$lat)
WorldCoords$long <- as.numeric(WorldCoords$long)
```

In order to load the coordinates of the world dataset, check its structure and render the latitude and longitude variables as numeric variables. Then, by running:

```r
AlaskaCoords <- filter(WorldCoords, WorldCoords$subregion == "Alaska", preserve = TRUE)
AlaskaCoordsCln <-subset(AlaskaCoords, long<0)
```

just the latitudinal and longitudinal coordinates of Alaska are kept. Then, we can create a dataframe of coordinates to depict the two subregions:

```r
#Firstly, for Southeast Alaska (SEA)
AlaskaCoordsSEA <- group_by(AlaskaCoords, lat = "57.58", long = "-135.48") %>%
  transmute(subregion = "Southeast Alaska") %>%
  distinct(subregion)

#Secondly, for Prince William Sound (PWS)
AlaskaCoordsPWS <- group_by(AlaskaCoords, lat = "60.63", long = "-147.3") %>%
  transmute(subregion = "Prince William Sound") %>%
  distinct(subregion)

#Adding the Mean pod size values to the coordinates dataframes for both subregions
AlaskaCoordsPWS$Mean.Estimate <- c(15.31) #doing the same for the PWS coordinates df
AlaskaCoordsSEA$Mean.Estimate <- c(33.78) #likewise for the SEA coordinates df

#Giving SEA and PWS a group number
AlaskaCoordsPWS$group <- c(1)
AlaskaCoordsSEA$group <- c(2)

## Adding two extra sets of coordinates for PWS, for mapping purposes
PWSextracoordA <- data.frame(-146.3, 60.63, "Prince William Sound", 15.31, 1)
names(PWSextracoordA) <- c("long", "lat", "subregion", "Mean.Estimate", "group")

PWSextracoordB <- data.frame(-147.3, 61.63, "Prince William Sound", 15.31, 1)
names(PWSextracoordB) <- c("long", "lat", "subregion", "Mean.Estimate", "group")

## Merging individual dataframes
PWSextraCoords <- rbind(PWSextracoordA, PWSextracoordB) #merging extra coordinates for PWS
AlaskaCoordsPWS <- rbind(PWSextraCoords, AlaskaCoordsPWS) #merging all coordinates for PWS

## Adding two extra set of coordinates for SEA, for mapping purposes
SEAextracoordA <- data.frame(-134.48, 57.58, "Southeast Alaska", 33.78, 2)
names(SEAextracoordA) <- c("long", "lat", "subregion", "Mean.Estimate", "group")

SEAextracoordB <- data.frame(-135.48, 56.58, "Southeast Alaska", 33.78, 2)
names(SEAextracoordB) <- c("long", "lat", "subregion", "Mean.Estimate", "group")

## Merging individual dataframes
SEAextraCoords <- rbind(SEAextracoordA, SEAextracoordB) #merging the 2 extra coordinates into one dataframe
SEAextraCoords <- select(SEAextraCoords, long, lat, group, subregion, Mean.Estimate) #arranging the columns

AlaskaCoordsSEA <- rbind(SEAextraCoords, AlaskaCoordsSEA) #merging the three coordinate sets into one dataframe

SEAandPWS_Coords <- rbind(AlaskaCoordsPWS, AlaskaCoordsSEA) #merging the two subregion coordinate dataframes
SEAandPWS_Coords <- select(SEAandPWS_Coords, long, lat, group, subregion, Mean.Estimate)
str(SEAandPWS_Coords) #Investigating the structure of the dataframe
SEAandPWS_Coords$long <- as.numeric(SEAandPWS_Coords$long) #making the longitudinal variable numeric
SEAandPWS_Coords$lat <- as.numeric(SEAandPWS_Coords$lat) #making the latitudinal variable numeric
```

Apologies, for the several lines of code here, don't worry about understanding everything, the main takeaway is that we created individual dataframes of a total of 3 coordinate sets per subregion which were merged together, after ensuring the dataframes had the same structure, that is the variables: longitude, latitude, subregion, Mean.Estimate (the average pod size per subregion, in this case) and group. Now that we have one dataframe with three sets of coordinates for each of the two subregions, we can combine the dataframe containing the coordinates for mapping Alaska with the dataframe containing the coordinates for our two subregions, having ensured the two dataframes have identical layouts.


```r
AlaskaCoordsCln$Mean.Estimate <- c(0)
AlaskaCoordsCln <- subset(AlaskaCoordsCln, select = -c(order, region)) #removing order and region columns

## Create the 'master' dataframe with coordinates for Alaska, as a whole, and the subregions
Tutorial.Map <- rbind(SEAandPWS_Coords, AlaskaCoordsCln)
```

Alas! We have obtained the dataframe we wanted with coordinates of the entire state of Alaska combined with the three sets of coordinates for highlight the subregions, Prince William Sound and Southeast Alaska, where our various Orca pods are found. To visualise the map in 'ggplot2' using this map dataframe, feel free to run the following code:

```r
(Tut.Map <- ggplot(Tutorial.Map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = subregion), color = "black") +
    labs(title = "Map of Alaska and Two Subregions") +
    theme(plot.title = element_text(size = 25 , face = "bold.italic", hjust = 0.5)) +
    Theme.Tutorial())
```

![~/Desktop/Uni/Year_4_(Honours)/DataScience/Tutorial_loulillelund/Outputs/Alaskawith2subregions_Map.png]("Alaskawith2subregions_Map.png")


Brilliant! We've created a basic map of Alaska with the two subregions, Southeastern Alaska and Prince William Sound, highlighted (based on their estimated total Orca abundance). In this sense, the green area on the map represents an area where the average Orca pod size is about 15 Orcas and around the blue area, Orca or Killer Whale pods are larger, on average, at about 34 Orcas per pod. As noted, this is quite a simple map, so one might want additional features or layers of complexity such as other areas with Orca pod statistics or even the average pod size per subregion written on the map, rather than represented by a color. However, I'll leave it to you to explore how this can be done in your own time, as we still have a statistical test to conduct!


### Statistical Analysis

The time has come... to make use of the wonderful statistical functions & test which R gives access to! If you recall from earlier, we discussed devising a scientific research question after having had a look at our dataset. This is a crucial step in the scientific process as it sets the bounds for your subsequent analyses and should always be done prior to analysis of the data. In reference to the "seed planted earlier", if you will, for our dataset, we could investigate the relationship between subregion and mean Orca pod size. To conduct a 1-way ANOVA of this function, try running the following code in your script:

```r
AvgOrcaPodSizeVSsubregion.ANOVA <- aov(KillerWhaleData$Mean.Estimate~KillerWhaleData$subregion, data = KillerWhaleData)
summary(AvgOrcaPodSizeVSsubregion.ANOVA)
```

Here, we are assigning the ANOVA test of the linear model describing how subregion relates to the mean abundance estimate data, to an object and the summarising the outputs of this ANOVA object which produces the following table:

```r
                          Df Sum Sq Mean Sq F value  Pr(>F)   
KillerWhaleData$subregion  1  909.9   909.9   8.658 0.00693 **
Residuals                 25 2627.4   105.1                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

Congratulations! You've just run a 1-way ANOVA! In brief, this summary table informs us about the degrees of freedom of both the independent variable (subregion) and the dependent variable (mean pod abundance), the sum of squares and the mean of the squares (the square being the difference between an individual datapoint and the mean, squared), used for calculating the variance and the F-statistic,.

Also produced is the F-statistic which describes a ratio of the variation between the subregion groups compared to variation within the subregion groups and this facilitates the generation of our p-value which is, in this case, 0.00693. In general, in the traditional frequentist framework, the p-value is compared to an alpha value of either 0.05 or 0.01 whereby if the p-value is less than the alpha value, the null hypothesis can be confidently rejected. Therefore, we can say that the subregion in which the an south Alaskan Orca pod is located, significantly influences its average size.

Critically, though there is a large difference in sample sizes (i.e. number of Orca pods) between the two subregions, which may indeed bias results as, for instance, as one large pod size in the Southeast Alaska grouping would have a larger affect on the group mean due to the smaller sample size than would the equivalent value in the Prince William Sound grouping due to the larger sample size. Nonetheless, if we assume these two be all of the Orca pods found in these two regions and the data to be accurate, then we might argue Orca pod size is on average larger in the Southeast Alaskan subregion compared to that of Prince William Sound, perhaps due to less competition from fewer pods, more habitable space in this geographical area and/or more resource availability.

As mentioned earlier, there are a few assumptions made when conducting a 1-way ANOVA. The first of which is that the residuals of the model, values quantifying how much is not described by the linear function, are normally distributed. Run the following code to test for this.

```r
#Testing if residuals are normally distributed
AvgOrcaPodSizeVSsubregion.resids <- resid(AvgOrcaPodSizeVSsubregion.ANOVA)
shapiro.test(AvgOrcaPodSizeVSsubregion.resids)
```

From this Shapiro-Wilk normality test, we obtain the following output:

```r
data:  AvgOrcaPodSizeVSsubregion.resids
W = 0.92448, p-value = 0.05066
```

Here, W is the test-statistic, generated by dividing the square of the the sum of a constant (different for each sample value) multiplied by the sample values by the sum of squares (as described earlier). This is used to ultimatetely generate the p-value, which as it is greater than an alpha value of 0.05, we would be inclined to accept the null hypothesis, that the residuals are indeed normally distributed.

Lastly, to test whether the variances (how far the individual data points are from the mean) across the two subregions assume homoscedastictiy (i.e. they are all equal), we will conduct a Bartlett test.

```r
#Bartlett test for homoscedastic variances
bartlett.test(Mean.Estimate~subregion,data=KillerWhaleData)
```

This generates the following output:

```r
data:  Mean.Estimate by subregion
Bartlett's K-squared = 2.1875, df = 1, p-value = 0.1391
```

The null hypothesis of this test is such that the variances for the treatment groups, Prince William Sound and Southeast Alaska, are the same. Thus, as the p-value obtained is greater than 0.05, we conclude the variances of our linear model do indeed assume homoscedasticity.

# Conclusion

A genuine thank you, if you have managed to make it this far. I hope you have enjoyed my tutorial where we learned the following:

1) How to access and use an environmental/ecological dataset
2) How to transform an ecological dataset and begin framing a scientific research Q having transformed the data
3) How to understand the distribution of ecological data with histograms as well as the assumptions that are be made in making a linear model between a categorical independent variable and a continuous response ecological data (NB: we could have converted our response variable form a continuous to a discrete variable which might be sensible given that it is abundance data)
4) How to make a basic map using 'ggplot2' highlighting the two subregions included in our dataset
5) How to analyse for a statistically significant difference between groupings in an independent variable


If you have any questions regarding this tutorial, please do not hesitate to reach out to me via email: s1864014@ed.ac.uk and for more information on map-making in R, check out: https://towardsdatascience.com/making-interactive-maps-in-r-with-less-than-15-lines-of-code-bfd81f587e12#:~:text=%20Making%20Interactive%20Maps%20in%20R%20with%20Less,the%20leaflet%20function%2C%20using%20the%20pipe.%20More%20
and/or https://geocompr.robinlovelace.net/adv-map.html


Happy coding,
Lucas Lillelund


#### <a href="INSERT_SURVEY_LINK" target="_blank">We would love to hear your feedback on the tutorial, whether you did it in the classroom or online!</a>

<ul class="social-icons">
	<li>
		<h3>
			<a href="https://twitter.com/our_codingclub" target="_blank">&nbsp;Follow our coding adventures on Twitter! <i class="fa fa-twitter"></i></a>
		</h3>
	</li>
</ul>

### &nbsp;&nbsp;Subscribe to our mailing list:
<div class="container">
	<div class="block">
        <!-- subscribe form start -->
		<div class="form-group">
			<form action="https://getsimpleform.com/messages?form_api_token=de1ba2f2f947822946fb6e835437ec78" method="post">
			<div class="form-group">
				<input type='text' class="form-control" name='Email' placeholder="Email" required/>
			</div>
			<div>
                        	<button class="btn btn-default" type='submit'>Subscribe</button>
                    	</div>
                	</form>
		</div>
	</div>
</div>
