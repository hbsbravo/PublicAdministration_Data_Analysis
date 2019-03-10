#Data Wrangling in R
#We will be using the Coal Consumption data here

library(tidyverse)

coalcons<-read_csv('http://594442.youcanlearnit.net/coal.csv')

glimpse(coalcons)

str(coalcons)

View(coalcons)

#Export the data to see why its hasn't loaded correctly as a tidy dataset

library(xlsx)
write.xlsx(coalcons, "F://R Work//Data Wrangling in R//coalcons.xlsx")


#Loading the data again and skipping the first two rows during the process

coalcons<-read_csv('http://594442.youcanlearnit.net/coal.csv',skip=2)

glimpse(coalcons)

View(coalcons)

colnames(coalcons)[1]<-'region'

summary(coalcons)

#We find that most of the variables are characters and that we are dealing with a wide dataset
#Therefore we decide to change it from a Wide dataset to a long dataset
#Secondly we want to ensure that there is one observation per row rather than the 30 different observations per row as in the current coal consumption dataset
#The gather function will help convert a wide dataset to a long one
#gather() does the reverse of spread(). gather() collects a set of column names and places them into a single "key" column. It also collects the cells of those columns and places them into a single value column.


coal_long<-gather(coalcons,'year','coal_consumption',-region)
coal_long

glimpse(coal_long)

coal_long$year<-as.integer(coal_long$year)
summary(coal_long)

View(coal_long)

summary(coal_long)

# The regions variable has a combination of countries and continents in the same columns and we don't want them together that way. Combining countries and continents combines observational units and this will cause issues in our analysis
#To get started lets look at all the unique variables in the region column

unique(coal_long$region)

# To start off we need to know where all the non country variables are

noncountries<-c("North America","Central & South America","Antarctica","Europe","Euroasia","Middle East","Africa","Asia & Oceania","World")

matches<- which(!is.na(match(coal_long$region, noncountries)))

coal_country<-coal_long[-matches,]

coal_region<-coal_long[matches,]

unique(coal_country$region)

unique(coal_region$region)

View(coal_region)

#Now lets change to the appropriate data types

coal_region$region<-as.character(coal_region$region)

coal_region$coal_consumption<-as.integer(coal_region$coal_consumption)

summary(coal_region)

#Now we will remove the variable world from the coal_region dataset

coal_region<-coal_region[- grep("World", coal_region$region),]
coal_region

?grep
#Lets maps some relationships regarding coal consumption

ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption))+ geom_point()

ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption))+ geom_line(mapping=aes(color=region))
