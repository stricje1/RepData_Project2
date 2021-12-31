---
title: "Affects of Severe Weather on People and Property"
author: "Jeffrey Strickland"
date: "12/29/2021"
output:
  word_document: default
  html_document:
    fig_caption: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(message=FALSE, warning=FALSE, 
                      error=FALSE,scipen=999)
options(scipen=999)
library(captioner)
fig_nums <- captioner::captioner()
fig_nums("first", "Storm events in severe consequences to public health - fatalities (1950-2011)")
fig_nums("second", "Storm events in severe consequences to public health - injuries (1950-2011)")
fig_nums("third", "Storm events which have most severe consequences to the economy (1950-2011)")
fig_nums("fourth", "Storm events in severe consequences in terms of crop damage (1950-2011)")
```

## <span style="color: blue;"> Synopsis <span>

### *Introduction*
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many insurance companies use weather data for understanding the impacts of severe weather events in terms of fatalities, injuries, and property damage, and rates are based on their findings. For instance, some insurance companies will no longer cover property damage in Colorado due to the impact of wildfires and hail on property damage. While this analysis is focus on the overall storm affects across the United States, similar studies are regularly performed for states and regions.

### *Analysis Questions and Outcomes* [5]
For this study, we have two analysis questions we want to answer:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

**Tornado** events have the greatest impacts on population health, both fatalities and injuries, and exceed other events at least three-fold.

2. Across the United States, which types of events have the greatest economic consequences?

For property damage, **flood** events (widespread) is overwhelmingly the leader, while the major cause of crop damage is drought.

## <span style="color: blue;"> Data Processing </span>

The Data acquired for this study comes from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This data is available on NOAA's website at this link.

### *Install and Load Packages*
The code for installing packages not shown. The required packages are `captioner`, `readr`, `dplyr`, `knitr`, `kableExtra`, `ggplot2`, `grid`, `gridExtra`, and `ggplotify.`

```{r eval=FALSE, include=FALSE}
if(!require(readr)) install.packages("readr") # does not work with markdown
if(!require(dplyr)) install.packages("dplyr")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")
```

```{r include=FALSE}
library(readr)
library(knitr)
library(plyr)
library(tidyr)
library(kableExtra)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggplotify)
```

### *Load the Weather Data*

Here, we load the dataset, using a conditional statement. The data used for the analysis is drawn from the U.S. NOAA storm database, as mentioned. The data for the analysis covers the period from 1950 to September 2011. The data can be downloaded from this link. The code we use first checks to see if the data has already been downloaded and unzipped to the working directory. If it has not, we generate a temporary file to hold the data that we download and unzip, thus saving disk space.

```{r echo=TRUE}
storm <- tempfile() # Create the temporary file
if (!file.exists(storm)){
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", storm)
storm_data <-read.csv(storm)
}
unlink(storm) # Release the temporary file
```

### *Exploratory Data Analysis (EDA)*
Here we perform EDA, as a part of our data processing or "wrangling." First, we display the names of the data fields (variables) and compare them with reference [1] for definitions and completeness, to ensure we have the necessary data and to better understand the content.

```{r echo=TRUE}
names(storm_data) 
```
### *Populate Initial Analysis Dataset*
The column names in storm_data are clear enought to generate an initial analysis dataframe. 

```{r echo=TRUE}
storm_data <- storm_data[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG",    "CROPDMGEXP", "COUNTYNAME", "STATE")]
```

Seeing that the data possesses the required fields, we examine the structure of those data. This will inform us as to the structure of the dataset (i.e., list, vector, dataframe, etc.), as well as reveal the type of variable we have, i.e., characters, integers, floating-point number, etc. It will also show us examples of the data in each field.

```{r echo=TRUE}
str(storm_data)
```

The structure table shows the weather event-type, `EVTYPE`, recognizable by the Tornado events shown. This is the response variable for all our analysis. FATALITIES and INJURIES will help answer the Population heath question. Finally, the fields for property damage (PROPDMG and PROPDMGEXP) and crop damage (CROPDMG and CROPDMGEXP). For instance, property damage (PROPDMG), a numerical field, and its associated exponent (PROPDMGEXP), a character field, provide the data for property damage effects. These fields are explained in Reference [1].

Now that we have a "feel" for the data, we generate a subset from storm_data, comprised of the variables we need for our analysis. Subsets example computational efficiency and removes detractors.

### *Prepare the Storm Data for Analysis*

**Fatalities and Injuries**
Recall our first analysis question: *Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?*
  
Here we form a subset the contains the `FATALITIES` and `INJURY` fields with the Storm event type, `EVTYPE.` Although the larger set is not computationally constraining, it is good analysis practice to generate data subset for each analysis question. We name this subset, storm1, to indicate it is the storm data we need to answer analysis question 1.

```{r echo=TRUE}
storm1 <- storm_data[,c("EVTYPE", "FATALITIES", "INJURIES")]
head(storm1,15)%>%
  kbl(col.names = c("Event Type", "Fatalities", "Injuries"), align = "c",
      caption = "Table 1. Severe Weather Data Affecting Population Health") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "10em") %>%
  column_spec(3, width = "10em")
```

```{r echo=TRUE}
print(str(storm1))
print(summary(storm1))
```

To ensure we have what we asked form, we check the structure of this subset. Seeing no issues, we proceed with preparing the data for analysis question 2: *Across the United States, which types of events have the greatest economic consequences?* We generate this subset and name it Storm 2. Again, we examine the structure and data summary.

```{r echo=TRUE}
storm2 <- storm_data[,c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
head(storm2,15)%>%
  kbl(col.names = c("Event Type", "Property Damage", "Property Exponent", 
                    "Crop Damage", "Crop Exponent"), align = "c", 
      caption = "Table 2. Severe Weather Data Affecting Population Health") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") 
```

```{r echo=TRUE}
print(str(storm2))
print(summary(storm2))
```

We see that there is no missing value, so no need of inputting missing values. Next, let's calculate deaths and injuries by event type to determine which storms and other weather events are most harmful to public health in the nation.

### *Compile the Data for Fatalities and Injuries Analysis*
Recall that we are looking for the top seven storm event types, so we'll get the representing the top seven for fatalities, and the top seven for injuries.

First, we sum the the fatalities by storm event type, followed by the summed injuries for each storm event type.

```{r echo=TRUE}
fatalities<-aggregate(FATALITIES~EVTYPE,storm1,sum)
injuries<-aggregate(INJURIES~EVTYPE,storm1,sum)
```

Now, we get the top seven storm types for fatalities, and the for injuries.

```{r echo=TRUE}
top_7_fatality<-arrange(fatalities, desc(fatalities$FATALITIES))[1:7,]
top_7_injury<-arrange(injuries, desc(injuries$INJURIES))[1:7,]
```

### *Compile the Data for Property and Crop damage Analysis*
Next, let's similarly extract the seven storms and weather events that cause the most severe propery and crop damage. Here, we need to investigate the "meaning" of the values in the property damage exponent field, PROPDMGEXP, and crop damage field, CROPDMGEXP. We need to convert the given "codes" into their numerical values to use them in calculating total damages, i.e., multiplying damage values by exponent values.

First, we extract the unique values contained in the PROPDMGEXP and CROPDMGEXP. field.

```{r echo=TRUE}
unique(storm2$PROPDMGEXP)
unique(storm2$CROPDMGEXP)
```

Given these unique letter and integer "codes", we identify the following patterns, and will use them to convert them to their respective exponent values. 
* The number 1 places 1 zero to the right, yielding 10. 
* We consider 2 as adding 2 zeros behind 1, so h, H, and 2 are 100. 
* Also, we take 3 as adding 3 zeros after 1, So we take k and 3 as 1000. 
* Likewise, 6 adds 6 zeros to 1, so M, m and 6 are 1000000. 
* Then 9 adds 9 zeros to 1, so 'B' and 9 are 1 billion or 1000000000. 
* The number 4 becomes 10000; 
* 5 becomes 10000; 
* 7 becomes 10000000; 
* 8 becomes 100000000. 
* Finally, we take 0, ?, and + as 1, adding no digits to 1. 

Therefore, we can multiply the property and crop damage values by their respective exponents. 

### *Build Lookup Tables*
To facilitate this task, we build lookup tables for matching EXP and their Values. Then we use the lookup table and the storm dataset to get the value of property and crop damages.  We should not that  there are some inconsistencies in the crop and property exponents, i.e., the m, M and h, H entries. However, for the purpose of this analysis take them as the same.

```{r echo=TRUE}
PROPDMGEXP  <- sort(unique(storm2$PROPDMGEXP))                          # property damage levels
propMult <- c(1,1,1,1,1,10,100,1000,10000,100000,1000000,10000000,   # property damage multipliers
                 100000000,1000000000,100,100,1000,1000000,1000000)
propLookup  <- data.frame(cbind(PROPDMGEXP,propMult))                # propLookup table
propLookup$propMult <- as.numeric(as.character(propLookup$propMult))     # convert to numeric
CROPDMGEXP  <- sort(unique(storm2$CROPDMGEXP))                          # crop damage levels
cropMult <- c(1,1,1,100,1000000000,1000,1000,1000000,1000000)        # crop damage multipliers
cropLookup  <- data.frame(cbind(CROPDMGEXP, cropMult))                # cropLookup table
cropLookup$cropMult <- as.numeric(as.character(cropLookup$cropMult))     # convert to numeric
```
### *Merge Lookup Tables into Storm Data*
with the next code chunk, we merge property damage multiplier, `propMult`, and crop damage multiplier, `cropMult`, with sever weather (`storm2`) data set.

```{r echo=TRUE}
storm2 <- merge(storm2,propLookup)                          # merge propMult into data set
storm2 <- merge(storm2,cropLookup)                          # merge cMultiplier into data set
```

For reference, we generate kable tables to represent the lookup tables.

```{r echo=TRUE}
propLookup%>%
  kbl(col.names = c("EXPONENT", "VALUE"), caption = "Table 3. Prpoerty Damage Exponent Codes with Values") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "18em")
```
```{r echo=TRUE}
cropLookup%>%
  kbl(col.names = c("EXPONENT", "VALUE"), 
      caption = "Table 4. Crop Damage Exponent Codes with Values") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "18em")
```

Now, we aggregate and sum the property damage damage by event type. Then, we generate the top seven most severe storms with the highest property damage values.


```{r echo=TRUE}
storm2$TOTALPROP <- storm2$PROPDMG * as.numeric(storm2$propMult)
storm_prop <- aggregate(TOTALPROP ~ EVTYPE, data = storm2, sum)
top_property <- storm_prop[order(-storm_prop$TOTALPROP), ]
top_7_property<-top_property[1:7, ]

```

Next, we aggregate and sum the crop damage damage by event type. Then, we generate the top seven most severe stroms with the highest crop damage values.

```{r echo=TRUE}
storm2$TOTALCROP <- storm2$CROPDMG * as.numeric(storm2$cropMult)
storm_crop <- aggregate(TOTALCROP ~ EVTYPE, data = storm2, sum)
top_crop <- storm_crop[order(-storm_crop$TOTALCROP), ]
top_7_crop<-top_crop[1:7, ]
```

## <span style="color: blue;"> Results </span>

We are now ready to present and discuss the results of our analysis questions.

### *Public Health - Fatalities*
First, we generate a barplot with Fatality results, as seen in Figure 1. 

```{r fatalities, fig.cap=fig_nums("first"), fig.height = 3, fig.width = 8}
f1<-ggplot(top_7_fatality, aes(x=reorder(EVTYPE, FATALITIES), 
                               y=FATALITIES,fill=FATALITIES)) 
f1 +geom_bar(stat='identity',color='white')+ 
  scale_fill_gradient(low = "red",high = "red4")+
  ggtitle('Top 7 Storm Events by Fatality')+
  xlab('Type of Event')+
  coord_flip()+
  ylab('Total number of Deaths')
```


```{r echo=TRUE}
top_7_fatality %>%
  kbl(col.names = c("Event Type", "Fatalities"), align = "c", 
      caption = "Table 5. Top Seven Storm Type Affecting Public Health (Fatalities)") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "18em")
```

Figure 1 indicates that tornadoes are responsible for the most fatalities, over three thousand more that those due to heat injuries (excessive heat). it is interesting to note the distinction between `Excessive Heat` and `Heat`. According to Reference [2], the big distinction is "well above normal" versus "above normal" heat and high humidity:

"**Excessive Heat (Z)**. Excessive Heat results from a combination of high temperatures (well above normal) and high humidity. An Excessive Heat event occurs and is reported in Storm Data whenever heat index values meet or exceed locally/regionally established excessive heat warning thresholds. Fatalities (directly-related) or major impacts to human health that occur during excessive heat warning conditions are reported using this event category."

"**Heat (Z).** A period of heat resulting from the combination of high temperatures (above normal) and relative humidity. A Heat event occurs and is reported in Storm Data whenever heat index values meet or exceed locally/regionally established advisory thresholds. Fatalities or major impacts on human health occurring when ambient weather conditions meet heat advisory criteria are reported using the Heat event."

Lightning related deaths are well-understood by Coloradans, particularly at the higher altitudes. I've had a few close calls during high country hiking and horseback riding.

Flash floods are also common, especially with snow-melt and other natural phenomena. On July 31, 1976, the skies opened up over the Big Thompson Canyon, setting off the deadliest natural disaster in Colorado history that claimed 144 lives and caused $35 million of damages. There is is a dry wadi a little west of my house that is frequented by flash floods in the summer. The powerful effect of water is demonstrated by the manner it physical changes the terrain during a flash flood.

TSTM Winds are Thunderstorm Winds, which is defined as (see Reference [2]):

"**Thunderstorm Wind (C)**. Winds, arising from convection (occurring within 30 minutes of lightning being observed or detected), with speeds of at least 50 knots (58 mph), or winds of any speed (non-severe thunderstorm winds below 50 knots) producing a fatality, injury, or damage."

These type of winds are commonplace where I live (on the Colorado prairie) with or without thunderstorms.

These results are summarized in Table 5.

```{r echo=TRUE}
top_7_fatality %>%
  kbl(col.names = c("Event Type", "Fatalities"), align = "c", 
      caption = "Table 5. Top Seven Storm Type Affecting Public Health (Fatalities)") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "18em")
```

### *Public Health - Injuries*
Next, we deal with injuries caused by severe whether events. As before, we summarize the effects with a table, followed by a horizontal barplot.

```{r echo=TRUE}
top_7_injury %>%
  kbl(col.names = c("Event Type", "Injuries"), align = "c", 
      caption = "Table 6.Top Seven Storm Type Affecting Public Health (Injuries)") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "20em")
```

The outcome is very similar to fatality results. One exception is the shear scale of tornado related injuries.There are more than 75,000 more injuries caused by tornado than any other event.

Added to the injury results are ice storms, defined as:

"**Ice Storm (Z)**. Ice accretion meeting or exceeding locally/regionally defined warning criteria (typical value is 1/4 or 1/2 inch or more)."

The barplot summaries these results visually.

```{r injuries, fig.cap=fig_nums("second"), fig.height = 3, fig.width = 8}
f2<-ggplot(top_7_injury, aes(x=reorder(EVTYPE, INJURIES), 
                             y=INJURIES,fill=INJURIES)) 
f2 + geom_bar(stat='identity',color='white')+
  scale_fill_gradient(low = "orangered", high = "orangered4")+
  ggtitle('Top 7 Storm Events by Injuries')+
  xlab('Type of Event')+
  coord_flip()+
  ylab('Total number of Injuries')+
  theme(plot.title = element_text(size = 12))  
```

### **Propery Damage**
As we did with public health effects, we generate a table with the property damage resulting from severe weather, with an accompanying horizontal barchart.

```{r echo=TRUE}
top_7_property%>%
  kbl(col.names = c("Event Type", "Property Damage"), align = "c", 
      caption = "Table 7. Top Seven Storm Type Affecting Property Damage") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "20em")
```

Property damage follows a similar pattern as fatalities, but the leader is different, with total costs for property damage due to flood coming in at $144,657,709,807 or one hundred forty four billion, six hundred fifty seven million, seven hundred nine thousand eight hundred and seven U.S. dollars. or one trillion five hundred billion. Floods dominate property damage like tornado dominates fatalities. This is not surprising, since floods are generally more widespread as tornado. It is more common to hear that the Mississippi River went over its banks (an dikes) everywhere, jut the other day, than it is to hear about a massive tornado. The most destructive tornado, or series of tornadoes, carved an approximately 250-mile path through northeast Arkansas, southeast Missouri, northwest Tennessee and western Kentucky on Dec 11, 2021, but was the first of such intensity in December since 1957.

How do we separate hurricanes from flooding and tornado, both of which can occur near simultaneously. The guidelines for reporting (see Reference[2]) are clear: 

"Wind damage is the only individual hazard to be encoded in Hurricane/Typhoon, Tropical Storm, and Tropical Depression. This restriction prevents a “double-count” from occurring in the national report entitled “A Summary of Natural Hazard Statistics for [Year] in the United States,” which is based upon the header strips of Storm Data events....Include all other impacts as separate events (e.g., storm surge/tide, freshwater flooding, tornadoes, debris flow, rip currents, etc.)."

"Flooding along the coast, even if it is from distant swells, will be entered as Storm Surge/Tide, not Coastal Flood. Rip Currents and High Surf can be entered in addition to Storm Surge/Tide, if applicable."

It is surprising to see Hurricane as a separate event (from Hurricane/Typhoon), since the NATIONAL WEATHER SERVICE INSTRUCTION 10-1605, JULY 26, 2021, does not single it out. It is possible that earlier years did not combine hurricanes and typhoons, but the literature does not reveal it.

```{r property, fig.cap=fig_nums("third"), fig.height = 6, fig.width = 8}
# Propery Damage

names(top_7_property)<-c('EVTYPE', 'PropDamage')
d1<- ggplot(top_7_property, aes(x=reorder(EVTYPE, PropDamage), 
                                y=PropDamage,fill=PropDamage))+ 
  geom_bar(stat='identity',colour='white')+
  scale_fill_gradient(low = "green4",high = "brown")+
  ggtitle('Top 7 Storm Events by property damage')+
  xlab('Type of Event')+
  coord_flip()+
  ylab('Total Property Damage Cost(USD)')


# Crop Damage

names(top_7_crop)<-c('EVTYPE', 'CropDamage')
d2<-  ggplot(top_7_crop, aes(x=reorder(EVTYPE, CropDamage), 
                             y=CropDamage,fill=CropDamage))+ 
  geom_bar(stat='identity',colour='white')+
  scale_fill_gradient(low = "green3",high = "brown")+
  ggtitle('Top 7 Storm Events by crop damage')+
  xlab('Type of Event')+
  coord_flip()+
  ylab('Total CROP Damage Cost(USD)')

grid.arrange(d1, d2)
```

### **Crop Damage**
Here again, we generate a similar table with the crop damage resulting from severe weather, with an accompanying horizontal barchart. The bottom barplot in Figure 3 conveys this information visually.

```{r echo=TRUE}
top_7_crop%>%
  kbl(col.names = c("Event Type", "Crop Damage"), align = "c", 
      caption = "Table 8. Top Seven Storm Type Affecting Crop Damage") %>%
  kable_classic(full_width = F, font = "Cambria", "striped") %>%
  column_spec(2, width = "20em")
```

For this category, drought rules the day as is intuitive. The difference between flash floods and floods is clear, but what about river flooding. Reference [1] hase no separate category for river floods and states:

"River flooding may be included as a Flood event. However, such entries should be confined to the effects of the river flooding, such as roads and bridges washed out, homes and businesses damaged, and the dollar estimates of such damage. The Water Resources Services Branch at National Weather Service Headquarters will maintain the official records of river stages, flood stages, and crests. Therefore, river stages need not be included in Storm Data."

Like hurricanes, river flooding may be a legacy record, since this data goes back to 1950.

## <span style="color: blue;"> Conclusion </span>
Here, we recall our analysis questions and summarize the results:

Analysis Question 1. *Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?*

With respect to fatalities, we found that tornado events dominate bot aspects of population health, fatalities and injuries. Deaths due to tornado events total 5,633 from 1950 through 2011.

Analysis Question 2. *Across the United States, which types of events have the greatest economic consequences?*

In terms of property damage, flood events are the major contribute, resulting in nearly 145 billion US Dollars. For crop damage, drought is the leading cause, resulting in nearly 14 billion US dollars.

## <span style="color: blue;"> References </span>

[1] NOAA. (2021). Storm Data Bulk Data Format. https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Bulk-csv-Format.pdf

[2] NATIONAL WEATHER SERVICE INSTRUCTION 10-1605: STORM DATA PREPARATION, JULY 26, 2021. http://www.nws.noaa.gov/directives/.

[3] Pohl, J. (Jul 29, 2016). 40 years later: Scores killed in Big Thompson Flood. Coloradoan. https://www.coloradoan.com/story/news/2016/07/29/big-thompson-flood-killed-scores/87524858/

[4] Samenow, J., Feuerstein, J., and Livingston, I.(Dec, 11 2021). How Friday night’s rare and deadly December tornado outbreak unfolded. https://www.washingtonpost.com/weather/2021/12/11/tornado-path-mayfield-kentucky-deaths/

[5] Strickland, J. (2020). Data Science Applications using R. Lulu.com. ISBN 978-0-359-81042-0. https://www.lulu.com/spotlight/strickland_jeffrey
