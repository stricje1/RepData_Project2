# title: "Affects of Severe Weather on People and Property"
# author: "Jeffrey Strickland"
# date: "12/29/2021"

options(scipen=999)
library(captioner)
fig_nums <- captioner::captioner()
fig_nums("first", "Storm events in severe consequences to public health - fatalities (1950-2011)")
fig_nums("second", "Storm events in severe consequences to public health - injuries (1950-2011)")
fig_nums("third", "Storm events which have most severe consequences to the economy (1950-2011)")
fig_nums("fourth", "Storm events in severe consequences in terms of crop damage (1950-2011)")

### *Analysis Questions and Outcomes*
#  1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

#  2. Across the United States, which types of events have the greatest economic consequences?

# The Data acquired for this study comes from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. 

### *Install and Load Packages*
if(!require(readr)) install.packages("readr") # does not work with markdown
if(!require(dplyr)) install.packages("dplyr")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")

library(readr)
library(knitr)
library(plyr)
library(tidyr)
library(kableExtra)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggplotify)

### *Load the Weather Data*

storm <- tempfile() # Create the temporary file
if (!file.exists(storm)){
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", storm)
  storm_data <-read.csv(storm)
}
unlink(storm) # Release the temporary file

names(storm_data) 

### *Populate Initial Analysis Dataset*
storm_data <- storm_data[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG",    "CROPDMGEXP", "COUNTYNAME", "STATE")]

str(storm_data)

### *Prepare the Storm Data for Analysis*
storm1 <- storm_data[,c("EVTYPE", "FATALITIES", "INJURIES")]
head(storm1,15)%>%
  kbl(col.names = c("Event Type", "Fatalities", "Injuries"), align = "c",
      caption = "Table 1. Severe Weather Data Affecting Population Health") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") %>%
  column_spec(2, width = "10em") %>%
  column_spec(3, width = "10em")

print(str(storm1))
print(summary(storm1))

storm2 <- storm_data[,c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
head(storm2,15)%>%
  kbl(col.names = c("Event Type", "Property Damage", "Property Exponent", 
                    "Crop Damage", "Crop Exponent"), align = "c", 
      caption = "Table 2. Severe Weather Data Affecting Population Health") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") 

print(str(storm2))
print(summary(storm2))

### *Compile the Data for Fatalities and Injuries Analysis*

fatalities<-aggregate(FATALITIES~EVTYPE,storm1,sum)
injuries<-aggregate(INJURIES~EVTYPE,storm1,sum)

top_7_fatality<-arrange(fatalities, desc(fatalities$FATALITIES))[1:7,]
top_7_injury<-arrange(injuries, desc(injuries$INJURIES))[1:7,]

### *Compile the Data for Property and Crop damage Analysis*

unique(storm2$PROPDMGEXP)
unique(storm2$CROPDMGEXP)

#Given these unique letter and integer "codes", we identify the following patterns, and will use them to convert them to their respective exponent values. 
# The number 1 places 1 zero to the right, yielding 10. 
# We consider 2 as adding 2 zeros behind 1, so h, H, and 2 are 100. 
# Also, we take 3 as adding 3 zeros after 1, So we take k and 3 as 1000. 
# Likewise, 6 adds 6 zeros to 1, so M, m and 6 are 1000000. 
# Then 9 adds 9 zeros to 1, so 'B' and 9 are 1 billion or 1000000000. 
# The number 4 becomes 10000; 
# 5 becomes 10000; 
# 7 becomes 10000000; 
# 8 becomes 100000000. 
# Finally, we take 0, ?, and + as 1, adding no digits to 1. 

### *Build Lookup Tables*

PROPDMGEXP  <- sort(unique(storm2$PROPDMGEXP))                          # property damage levels
propMult <- c(1,1,1,1,1,10,100,1000,10000,100000,1000000,10000000,   # property damage multipliers
              100000000,1000000000,100,100,1000,1000000,1000000)
propLookup  <- data.frame(cbind(PROPDMGEXP,propMult))                # propLookup table
propLookup$propMult <- as.numeric(as.character(propLookup$propMult))     # convert to numeric
CROPDMGEXP  <- sort(unique(storm2$CROPDMGEXP))                          # crop damage levels
cropMult <- c(1,1,1,100,1000000000,1000,1000,1000000,1000000)        # crop damage multipliers
cropLookup  <- data.frame(cbind(CROPDMGEXP, cropMult))                # cropLookup table
cropLookup$cropMult <- as.numeric(as.character(cropLookup$cropMult))     # convert to numeric

### *Merge Lookup Tables into Storm Data*
# with the next code chunk, we merge property damage multiplier, `propMult`, and crop damage multiplier, `cropMult`, with sever weather (`storm2`) data set.
storm2 <- merge(storm2,propLookup)                          # merge propMult into data set
storm2 <- merge(storm2,cropLookup)                          # merge cMultiplier into data set

propLookup%>%
  kbl(col.names = c("EXPONENT", "VALUE"), caption = "Table 3. Prpoerty Damage Exponent Codes with Values") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") %>%
  column_spec(2, width = "18em")

cropLookup%>%
  kbl(col.names = c("EXPONENT", "VALUE"), 
      caption = "Table 4. Crop Damage Exponent Codes with Values") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") %>%
  column_spec(2, width = "18em")

# Aggregate and sum the property damage damage by event type. 
# generate the top seven most severe storms with the highest property damage values.

storm2$TOTALPROP <- storm2$PROPDMG * as.numeric(storm2$propMult)
storm_prop <- aggregate(TOTALPROP ~ EVTYPE, data = storm2, sum)
top_property <- storm_prop[order(-storm_prop$TOTALPROP), ]
top_7_property<-top_property[1:7, ]

storm2$TOTALCROP <- storm2$CROPDMG * as.numeric(storm2$cropMult)
storm_crop <- aggregate(TOTALCROP ~ EVTYPE, data = storm2, sum)
top_crop <- storm_crop[order(-storm_crop$TOTALCROP), ]
top_7_crop<-top_crop[1:7, ]

### *Public Health - Fatalities*


f1<-ggplot(top_7_fatality, aes(x=reorder(EVTYPE, FATALITIES), 
                               y=FATALITIES,fill=FATALITIES)) 
f1 +geom_bar(stat='identity',color='white')+ 
  scale_fill_gradient(low = "red",high = "red4")+
  ggtitle('Top 7 Storm Events by Fatality')+
  xlab('Type of Event')+
  coord_flip()+
  ylab('Total number of Deaths')

top_7_fatality %>%
  kbl(col.names = c("Event Type", "Fatalities"), align = "c", 
      caption = "Table 5. Top Seven Storm Type Affecting Public Health (Fatalities)") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") %>%
  column_spec(2, width = "18em")

### *Public Health - Injuries*

f2<-ggplot(top_7_injury, aes(x=reorder(EVTYPE, INJURIES), 
                             y=INJURIES,fill=INJURIES)) 
f2 + geom_bar(stat='identity',color='white')+
  scale_fill_gradient(low = "orangered", high = "orangered4")+
  ggtitle('Top 7 Storm Events by Injuries')+
  xlab('Type of Event')+
  coord_flip()+
  ylab('Total number of Injuries')+
  theme(plot.title = element_text(size = 12))  

top_7_injury %>%
  kbl(col.names = c("Event Type", "Injuries"), align = "c", 
      caption = "Table 6.Top Seven Storm Type Affecting Public Health (Injuries)") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") %>%
  column_spec(2, width = "20em")


### *Propery Damage*

top_7_property%>%
  kbl(col.names = c("Event Type", "Property Damage"), align = "c", 
      caption = "Table 7. Top Seven Storm Type Affecting Property Damage") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") %>%
  column_spec(2, width = "20em")

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


### *Crop Damage*

top_7_crop%>%
  kbl(col.names = c("Event Type", "Crop Damage"), align = "c", 
      caption = "Table 8. Top Seven Storm Type Affecting Crop Damage") %>%
  kable_classic(full_width = F, html_font = "Cambria", "striped") %>%
  column_spec(2, width = "20em")
