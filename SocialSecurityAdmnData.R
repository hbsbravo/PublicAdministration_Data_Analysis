#Data wrangling in R
#Social Security Disability Dataset


#Lets load some relevant libraries
library(tidyverse)
library(lubridate)
library(stringr)


#Downloading the dataset
ssdata<-read_csv("http://www.594442.youcanlearnit.net/ssadisability.csv")

#Looking at the dataset
glimpse(ssdata)

#We find that the dataset tells us about the number of social security applications recieved each year starting with the ones recieved at the start of the federal fiscal year FY08 to FY17
#We also notice that most of the variables are doubles

#One problem with this data is that we have many different observations from one row. 
#So we're going to start lengthening the dataset where we have a single data element in each row
#We'll us the gather function to lengthen the dataset

ssd_long<-gather(ssdata,month,applications,-Fiscal_Year)
#"-Fiscal_Year" is used because we don't want to gather that data about FY
print(ssd_long,n=20)

#Lets start working with the date information first
#How many months does the ssd_long tibble have
unique(ssd_long$month)

#We have data by month for all the application sent and those sent through the internet
#Lets use the 'separate'function to clean this up a bit
#We will also ask the function to use "_" to split these columns as it divides the name of the month and the application method
ssd_long<-separate(ssd_long,month,c("month","application_method"),sep="_")

print(ssd_long,n=20)
View(ssd_long)
unique(ssd_long$month)

#So September to May we see the use of 3 character abbreviations but for other months like June,July and August we see longer names
#We would like to shorten these month names because that way we will be able to create dates if we wanted to work with them later
#Lets use the substring function to remove everything bu the first three letters from the month values
ssd_long$month<-substr(ssd_long$month,1,3)

#Now lets look at the FY variables
unique(ssd_long$Fiscal_Year)
#These will also cause issues since they are not real years


#Lets try to wwitch these to a standard year format
#We can do this by replacing the FY's by the '20's
#Lets use the 'String Replace" function from the stringr library to do that.
ssd_long$Fiscal_Year<-str_replace(ssd_long$Fiscal_Year,"FY","20")
unique(ssd_long$Fiscal_Year)

#Now we have the month and year in separate fields
#Lets use them to create a date value
#By first creating a new string
#The paste function concaternates several string variables into a new string
paste("01",ssd_long$month,ssd_long$Fiscal_Year)
#Here "01" is supposed to be the first day of the month
#Now lets use lubridates dmy function to convert them into day, month, and year form
ssd_long$Date<-dmy(paste("01",ssd_long$month,ssd_long$Fiscal_Year))
unique(ssd_long$Date)
#Now  we have dates in their standard formats by using the lubridate function
glimpse(ssd_long$Date)

#Now lets take the dates
#We should be mindful that fiscal years are not the same as standard format years.
#For instance the month of October FY08 will actually be in Year 2017 
#Since Fed Govt Fiscal years start in October.
#So we will have to make corrections for the month Oct,Nov, and Dec

#Lets find the rows that have those months 
advanced_dates<-which(month(ssd_long$Date)>=10)
advanced_dates

#Lets use lubridates year function to extract the year, and subtract one from it
#Lets start by writing the code for the year subtraction that over-writes the existing year in those locations
year(ssd_long$Date[advanced_dates])<-year(ssd_long$Date[advanced_dates])-1
#Now everything in our data set is in calender years\
unique(ssd_long$Date)

#Now lets do a few more transformations 
summary(ssd_long)
#Lets get rid of data regarding the month and fiscal year since we already have dates
ssd_long$Fiscal_Year<-NULL
ssd_long$month<-NULL

#Lets convert application variable to a factor since it only has two values
ssd_long$application_method<-as.factor(ssd_long$application_method)
summary(ssd_long)

print(ssd_long,n=20)
#We have the number of internet application and total applications in separate rows
#Lets tidy up this dataset by cleaning it up a bit more ,and widening it.
#By using the separate function
ssa<-spread(ssd_long,application_method,applications)

print(ssa,n=20)

#Lets see if the efforts of the social security admn to move applications online been successfull
#So lets create a new variable pertaining to teh %age of applications online
#To do this we divide the internet application number by the total application number
ssa$online_percentage<-ssa$Internet/ssa$Total*100

#Lets use ggplot to plot the change of percentage in time by using a scatterplot
ggplot(data=ssa,mapping=aes(x=Date,y=online_percentage))+geom_point()

#There are 8rows that are absent that are future months that weren't actually recorded in the data 
#Its seems that the social security admns efforts to increase the number of online applications was successful by year
#In 2007 that number was less than 10 % but it steadily started to increase before levelling off in 2015
