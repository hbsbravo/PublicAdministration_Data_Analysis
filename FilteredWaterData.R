#Austin Water Quality Dataset

library(lubridate)
library(tidyverse)
library(stringr)

#Uploading the dataset
water<-read_csv("http://594442.youcanlearnit.net/austinwater.csv")

str(water)

glimpse(water)


#Creating a separate tibble for further calculations
water<-tibble('siteName'=water$SITE_NAME,
              'siteType'=water$SITE_TYPE,
              'sampleTime'=water$SAMPLE_DATE,
              'parameterType'=water$PARAM_TYPE,
              'paramter'=water$PARAMETER,
              'result'=water$RESULT,
              'unit'=water$UNIT)


glimpse(water)         

colnames(water)[5]<-'parameter'

unique(water$parameter)

#Lets try to find parameters with teh string 'PH'
unique(water[which(str_detect(water$parameter,'PH')),]$parameter)

#However now we get all the variables that have the letters 'ph' in them
#So lets identify the unique parameter types and parameters first

unique(water$parameterType)


unique(water$parameter)

#Lets filter the valyes by the parameter types that may contain the values we are looking for


filtered_water<-subset(water,(parameterType=='Alkalinity/Hardness/pH')|parameterType=='Conventionals')
glimpse(filtered_water)

unique(filtered_water$parameter)

filtered_water<- subset(filtered_water,(parameter=='PH')|(parameter=="WATER TEMPERATURE"))


#So we've filtered the dataset to the variables that are of most relevance to our analysis
#Now lets set the datapoints to the most appropriate data types

summary(filtered_water)

# We have plenty of character variables that would be better represented by factor variables (which is itself a character) allowing us to do additional analysis
#Site type jumps as a variable that is a good candidate to convert to factors since it allows us to analyze based on categories

filtered_water$siteType<-as.factor(filtered_water$siteType)

summary(filtered_water)
summary(filtered_water$siteType)

#Now lets correct the other datatypes as well

filtered_water$parameterType<-as.factor(filtered_water$parameterType)

summary(filtered_water$parameterType)

filtered_water$parameter<-as.factor(filtered_water$parameter)

summary(filtered_water$parameter)

filtered_water$unit<-as.factor(filtered_water$unit)

summary(filtered_water$unit)

filtered_water$sampleTime<-mdy_hms(filtered_water$sampleTime)

summary(filtered_water$sampleTime)

#Now lets move onto the other correcting/cleaning some data
#First, lets look at the units of measurement

summary(filtered_water)

subset(filtered_water,unit=="Feet")

#Lets store the row number using the unit Feet so that we can fix it

convert<-which(filtered_water$unit=="Feet")

filtered_water$unit[convert]<-'Deg. Fahrenheit'

subset(filtered_water,unit=='MG/L')

# We don't know how they got calculated as MG/L but we will try to fix them

glimpse(subset(filtered_water,unit=='MG/L' ))

glimpse(subset(filtered_water,unit=='MG/L' & parameter=='PH'))

convert<-which(filtered_water$unit=='MG/L' & filtered_water$parameter=='PH')

filtered_water$unit[convert]<-'Standard units'

glimpse(subset(filtered_water,unit=='MG/L' ))

#So I'll reduce this to those cases where the temperature is probably farenheight i.e. greater than 70

glimpse(subset(filtered_water,unit=='MG/L' & result>70))

convert<-which(filtered_water$unit=='MG/L' & filtered_water$result>70)

filtered_water$unit[convert]<-'Deg. Celsius'

#So now the only things left are the Fahrenheit temperatures
summary(filtered_water)

filtered_water$unit[convert]<-'Deg. Fahrenheit'

#Now we assume that the three values left are cell

glimpse(subset(filtered_water,unit=='MG/L' ))


convert<-which(filtered_water$unit=='MG/L')


filtered_water$unit[convert]<-'Deg. Celsius'


library(tidyverse)

#Checking for the last time to see if all our variables are in the right spot
summary(filtered_water)

#So we've made a lot of changes to the data format to correct the data entry errors , we will now move oto identify and remove the outliers
#We will start by doing a scatter plot to get a general view of the dataset
ggplot(filtered_water, mapping=aes(x=sampleTime, y=result))+ geom_point()


#There is one outlier to the extreme right  which does not seem to be possible.
#Now lets try to isolate that outlier

glimpse(subset(filtered_water,result>100000))

remove<-which(filtered_water$result>100000 | is.na(filtered_water$result))

filtered_water<-filtered_water[-remove]

summary(filtered_water)

#When you look at the result value you find another outlier where the value is 7104
#I can't think of a logical way to resolve this so I'm going to create the remove vector again, and remove anything where the result is greater than 1000

remove <- which(filtered_water$result>1000 | is.na(filtered_water$result))
filtered_water <- filtered_water[-remove,]

summary(filtered_water)
library("tidyverse")

ggplot(data=filtered_water, mapping=aes(x=unit,y=result))+ geom_boxplot()

# so when we look at our box plot we see that there are numerous values in the results tables that have values above 60 degrees celsius which is way above what the water temperature should be
# So we will try to convert those two values to celsius
convert <- which(filtered_water$result>60 & filtered_water$unit=='Deg. Celsius')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'


convert<-which(filtered_water$result>60 & filtered_water$unit=='Deg. Celsius')
filtered_water$unit[convert]<-'Deg. Fahrenheit'


ggplot(data=filtered_water, mapping=aes(x=unit,y=result))+ geom_boxplot()


#There is still some messy data here which is primarily because we have farenheight and celsius both as units here
# So lets start with those values that have farenheit as a unit


fahrenheit<-which(filtered_water$unit=='Deg. Fahrenheit')

filtered_water$result[fahrenheit]<-(filtered_water$result[fahrenheit]-32)*(5.0/9.0)

ggplot(data=filtered_water, mapping=aes(x=unit,y=result))+ geom_boxplot()

# While we havent changed the naming conventions in the column from fahrenheit to celsius we still find that the temparature ranges are what they usually should be for the celsius metric while looking at the box plot


filtered_water$unit[fahrenheit]<-'Deg. Celsius'

summary(filtered_water)

# We just need to fix up the unit values
filtered_water$unit[fahrenheit] <- 'Deg. Celsius'

# And check the plots again
ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()
summary(filtered_water)


# There are some empty factor levels in there, let's get rid of them
filtered_water$unit <- droplevels(filtered_water$unit)
summary(filtered_water)


# Getting rid of parameterType and unit since they are not of any particular use to us
filtered_water <- filtered_water[,-c(4,7)]

filtered_water

#Now we're down to 5 columns

# Try a spread
filtered_water_wide <- spread(filtered_water,parameter,result)
#This gives us an error message for duplicate identifiers which means that when the spread tried to bring together records
# What are those duplicate rows all about?
# Let's look at a few
filtered_water[c(49274, 49342,49219,49284),]

# It looks like there are indeed duplicate measurements in the dataset
# Let's find all of them.  I'll start by building a tibble that excludes the result data
dupe_check<-filtered_water[,-5]

# And now lets find which records are duplicates
dupes <- which(duplicated(dupe_check))

# And remove those
filtered_water <- filtered_water[-dupes,]

# and then retry the spread
filtered_water_wide <- spread(filtered_water,paramter,result)
filtered_water_wide

# and i'll just clean up those column names
colnames(filtered_water_wide)[4] <- 'pH'
colnames(filtered_water_wide)[4] <- 'temperature'


