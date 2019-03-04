#DEFINE A DATA SCIENCE PROCESS

#deleting everything in the environment
#rm(list = ls(all = TRUE))

#installing packages

#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("cellranger")
#install.packages("scales")
#install.packages("shiny")
#install.packages("varhandle")
#install.packages("fracdiff")
#install.packages("uroot")
#install.packages("imputeTS")
#install.packages("hms")
#install.packages("mice")
#install.packages("Hmisc")
#install.packages("VIM")
#install.packages("rio")
#install.packages("mi")
#install.packages("RcppArmadillo")
#install.packages("forecast")
#install.packages("stats")
#install.packages("shinydashboard")
#install.packages("shiny")
#install.packages("timeDate")
#install.packages("timeSeries")
#install.packages("ggfortify")
#install.packages("changepoint")
#install.packages("strucchange")
#install.packages("ggpmisc")
#install.packages("magrittr")
#install.packages("fUnitRoots")
#install.packages("urca")
#install.packages("modelr")

#calling libraries

library("lubridate")
library("dplyr")
library("tidyr")
library("tidyverse")
library("cellranger")
library("scales")
library("shiny")
library("varhandle")
library("fracdiff")
library("uroot")
library("imputeTS")
library("hms")
library("mice")
library("Hmisc")
library("rio")
library("VIM")
library("mi")
library("RcppArmadillo")
library("forecast")
library("stats")
library("shinydashboard")
library("shiny")
library("timeDate")
library("timeSeries")
library("fUnitRoots")
library("urca")
library("ggfortify")
library("changepoint")
library("strucchange")
library("ggpmisc")
library("magrittr")
library("modelr")


#setting the working directory
setwd("C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/4. DEEP ANALYTICS AND VISUALIZATION/TASK_1_Define a Data Science Process")
getwd()

#importing the dataset (txt file)
energyds <- read.delim("household_power_consumption.txt",header = TRUE, sep=";")
head(energyds)

#describing the dataset
summary(energyds)
str(energyds)

#checking the type of the attributes and changing it

energyds$Global_active_power <- as.numeric(as.character(energyds$Global_active_power))

energyds$Global_reactive_power <- as.numeric(as.character(energyds$Global_reactive_power))

energyds$Voltage <- as.numeric(as.character(energyds$Voltage))

energyds$Global_intensity <- as.numeric(as.character(energyds$Global_intensity))

energyds$Sub_metering_1 <- as.numeric(as.character(energyds$Sub_metering_1))

energyds$Sub_metering_2 <- as.numeric(as.character(energyds$Sub_metering_2))


#I converted all the numeric attributes in numerical;
#now we have two factors (Date and Time) and seven numerical variables.


#Combining Data and Time in one column called "DateTime"
energyds <-cbind(energyds,paste(energyds$Date,energyds$Time), stringsAsFactors=FALSE)
colnames(energyds)[10] <-"DateTime"

#moving the last just created column at the start of the dataset
energyds <- energyds[,c(ncol(energyds), 1:(ncol(energyds)-1))]

#N.B. so far, DateTime is a character; Date and Time are factors

#converting date and time format
energyds$DateTime <- strptime(energyds$DateTime, "%d/%m/%Y %H:%M:%S", tz ="GMT")

#now DateTime is an object of class POSIXlt, format:"2006-12-16 17:24:00"
#from the summary we can see that the first date is 16-12-2006 17:24:00 
#and the last date is 26-11-2010 21:02:00

energyds$Date <- as.Date(energyds$Date, "%d/%m/%Y", tz = "GMT")

#TO DO: Time is still of class Factor! -> to be changed


#changing unit of measure of GAP and GRP (from KWatt to WattHour) -> power
#in order to have it in the same unit of SM1-2-3 -> energy. 
#therefore, GAP and GRP BECOME ENERGY after the conversion.
#converting GAP and GRP to ENERGY (from Kwatts to WattHour)

energyds$Global_active_power <- (energyds$Global_active_power*1000)/60
energyds$Global_reactive_power <- (energyds$Global_reactive_power*1000)/60

#plotting all numerical variables (after conversion)

par(mfrow = c(2, 4))
hist(energyds$Global_active_power, main = "GAP Frequency", xlab = "Global Active Power (WattHour)")
hist(energyds$Global_reactive_power, main = "GRP Frequency", xlab = "Global Reactive Power (WattHour)")
hist(energyds$Voltage, main = "Voltage Frequency", xlab = "Voltage (Volt)")
hist(energyds$Global_intensity, main = "GI Frequency", xlab = "Global Intensity (Ampere)")
hist(energyds$Sub_metering_1, main = "SM 1 Energy Frequency", xlab = "Active Energy (WattHour)")
hist(energyds$Sub_metering_2, main = "SM 2 Energy Frequency", xlab = "Active Energy (WattHour)")
hist(energyds$Sub_metering_3, main = "SM 3 Energy Frequency", xlab = "Active Energy (WattHour)")

#creating two datasets corresponding to the dates of time changes in Paris in 2010  
#(28March2010 and 31October2010)
#march282010 <- subset(energyds, Date=="2010-03-28")
#october312010 <- subset(energyds, Date=="2010-10-31")

#checking 28 april 2007 (day with a lot of NA)
#april282007 <- subset(energyds, Date=="2007-04-28" )
#from 00:21 of 28th April 2007 to 14:23 of 30th April 2007 there are all NAs
#it was saturday, sunday, monday -> maybe holiday!


#NAs analysis
#create a new dataset without missing data 
energy_wo_NA <- na.omit(energyds)

#creating a new dataset of NAs in gap
missing_gap <- subset(energyds, is.na(Global_active_power))
#N.B. All NAs has NAs in gap and, consequently, NAs in the rest of the variables

#missing_2006 <- subset(missing_gap, year(Date)=="2006")
#missing_2007 <- subset(missing_gap, year(Date)=="2007")
#missing_2008 <- subset(missing_gap, year(Date)=="2008")
#missing_2009 <- subset(missing_gap, year(Date)=="2009")
#missing_2010 <- subset(missing_gap, year(Date)=="2010")


#plotting the NAs
par(mfrow = c(1, 1))
tabNA <- table(missing_gap$Date)
tabNA
plot(tabNA)

#accessing value of the element 7 and of the element "2007-01-14" in the table
#tabNA[[7]]
#tabNA[["2007-01-14"]]

#IDEA: if number of minutes > 178 -> impute missing values with zero (holiday)
#otherwise -> impute missing values with mean (black out, general meter out of order)

#plotting the NAs of every year

#par(mfrow = c(2, 3))
#tabNA2006 <- table(missing_2006$Date)
#tabNA2007 <- table(missing_2007$Date)
#tabNA2008 <- table(missing_2008$Date)
#tabNA2009 <- table(missing_2009$Date)
#tabNA2010 <- table(missing_2010$Date)
#plot(tabNA2006)
#plot(tabNA2007)
#plot(tabNA2008)
#plot(tabNA2009)
#plot(tabNA2010)

#for every day we can see how many minutes have NA value. 
#N.B. 1400 (=60*24) NAs in one day meaning that consumption was equal to zero for the whole day!
#we can conclude when the family went on holiday, and when the general meter was out of order, or maybe some black out moments due to the utility.

#plotting GLOBAL ACTIVE ENERGY CONSUMPTION
ggplot(data=energyds, aes(energyds$Global_active_power))+
  geom_histogram(binwidth = 1)+
  xlab("Global Active Energy (WattHour)")+
  ggtitle("Global Active Energy")+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))
#we have a large number of observations for 0 to 10 WattHour, 
#far fewer between 10 WattHour and 25 WattHour
#and then a higher but steadily decreasing number of observations of over 25 WattHour.
#This represents two different types of power consumption: 
#the first when nobody is at home or occupants are asleep, 
#and the second when household appliances are being used.


##looking for ENERGY WASTES, based on the variable GLOBAL REACTIVE POWER
#N.B. the third quartile (3.233); the maximum (23.167)

#hist(energyds$Global_reactive_power)
#high_grp_values <- subset(energyds, Global_reactive_power > 3.233)
#very_high_grp_values <- subset(energyds, Global_reactive_power > 5)

#looking for outliers
#outlier_values_grp <- boxplot.stats(energyds$Global_reactive_power)$out
#outlier_values_grp


#ACTIVE AND REACTIVE POWER
#The active power is the real power consumes by the load. 
#Whereas, the reactive power is the useless power.
#The active power does the useful work in the circuit. 
#And the reactive power merely flows in the circuit without doing any useful work.

#Extracting day, month and year from DateTime (POSIXlt format)

#day(energyds[1,1])
#month(energyds[1,1])
#year(energyds[1,1])


#creating a new dataset called energyds_no_datetime without DateTime column
energyds_no_datetime <- energyds[,-1]
#avg and sum of GAP and GRP for every date
by_date <- group_by(energyds_no_datetime, Date, add = TRUE)
by_date %>% summarise(gap = mean(Global_active_power),grp = mean(Global_reactive_power))
by_date %>% summarise(gap = sum(Global_active_power),grp = sum(Global_reactive_power))

#selecting year 2007, 2008, 2009, 2010
year2007 <- subset(energyds_no_datetime, year(Date) == 2007)
year2008 <- subset(energyds_no_datetime, year(Date) == 2008)
year2009 <- subset(energyds_no_datetime, year(Date) == 2009)
year2010 <- subset(energyds_no_datetime, year(Date) == 2010)


#deleting column Time from year2007, ..., year2010
year2007 <- year2007[, -2]
year2008 <- year2008[, -2]
year2009 <- year2009[, -2]
year2010 <- year2010[, -2]

by_month2007 <- group_by(year2007, month(Date), add = TRUE)
by_month2007
summary(by_month2007)
#there are NAs in these dataset!

by_month2008 <- group_by(year2008, month(Date), add = TRUE)

by_month2009 <- group_by(year2009, month(Date), add = TRUE)

by_month2010 <- group_by(year2010, month(Date), add = TRUE)

#avg and sum of GAP and GRP for every month of 2007, ... , 2010
tabmonths2007mean <- by_month2007 %>% summarise(gap = mean(Global_active_power, na.rm=TRUE),grp = mean(Global_reactive_power, na.rm=TRUE))
tabmonths2007sum <- by_month2007 %>% summarise(gap = sum(Global_active_power, na.rm=TRUE),grp = sum(Global_reactive_power, na.rm=TRUE))
tabmonths2007mean
tabmonths2007sum

tabmonths2008mean <- by_month2008 %>% summarise(gap = mean(Global_active_power, na.rm=TRUE),grp = mean(Global_reactive_power, na.rm=TRUE))
tabmonths2008sum <- by_month2008 %>% summarise(gap = sum(Global_active_power, na.rm=TRUE),grp = sum(Global_reactive_power, na.rm=TRUE))
tabmonths2008mean
tabmonths2008sum

tabmonths2009mean <- by_month2009 %>% summarise(gap = mean(Global_active_power, na.rm=TRUE),grp = mean(Global_reactive_power, na.rm=TRUE))
tabmonths2009sum <- by_month2009 %>% summarise(gap = sum(Global_active_power, na.rm=TRUE),grp = sum(Global_reactive_power, na.rm=TRUE))
tabmonths2009mean
tabmonths2009sum

tabmonths2010mean <- by_month2010 %>% summarise(gap = mean(Global_active_power, na.rm=TRUE),grp = mean(Global_reactive_power, na.rm=TRUE))
tabmonths2010sum <- by_month2010 %>% summarise(gap = sum(Global_active_power, na.rm=TRUE),grp = sum(Global_reactive_power, na.rm=TRUE))
tabmonths2010mean
tabmonths2010sum

#GRAPH PER MONTH in 2007, ..., 2010 (AVG GAP and GRP -> TO DO GRP) to see the trend above

par(mfrow = c(2,2))
plot(tabmonths2007mean$`month(Date)`, tabmonths2007mean$gap, main = "AVG GAP per month in 2007",xlab = "Month", ylab = "AVG GAP",type = "b", lwd = 2, cex = .5)
plot(tabmonths2008mean$`month(Date)`, tabmonths2008mean$gap, main = "AVG GAP per month in 2008",xlab = "Month", ylab = "AVG GAP",type = "b", lwd = 2, cex = .5)
plot(tabmonths2009mean$`month(Date)`, tabmonths2009mean$gap, main = "AVG GAP per month in 2009",xlab = "Month", ylab = "AVG GAP",type = "b", lwd = 2, cex = .5)
plot(tabmonths2010mean$`month(Date)`, tabmonths2010mean$gap, main = "AVG GAP per month in 2010",xlab = "Month", ylab = "AVG GAP",type = "b", lwd = 2, cex = .5)


#AVG GAE in 2007
par(mfrow = c(1,1))
plot(tabmonths2007mean$`month(Date)`, tabmonths2007mean$gap, main = "AVG GAE per month in 2007",xaxt = "n", xlab = "Month", ylab = "AVG GAE",type = "b", lwd = 2, cex = .5)
axis(1, at=1:12, labels = c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#nicer AVG GAE in 2007 with ggplot
p <- ggplot(tabmonths2007mean, aes(`month(Date)`, gap)) + geom_line() 
p <- p + ggtitle("AVG Global Active Energy Consumption per month in 2007")
p <- p + scale_x_discrete(name ="month", 
                     limits=c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
p <- p + scale_y_continuous(name="AVG GAE (Watt-hour)", breaks = seq(0,40,2))
p


#inspecting missing values in august 2008:
#TO DO: CREATE THE SUBSET OF AUGUST 2008
par(mfrow = c(1,1))
inspect.na(missing_august_2008)
inspect.na(missing_august_2008, hist=TRUE, barplot=FALSE)

#GRAPH PER MONTH in 2007, ..., 2010 (TOTAL GAP) to see the trend above

par(mfrow = c(2,2))
plot(tabmonths2007sum$`month(Date)`, tabmonths2007sum$gap, main = "Total GAP per month in 2007",xlab = "Month", ylab = "Total GAP",type = "b", lwd = 2, cex = .5)
plot(tabmonths2008sum$`month(Date)`, tabmonths2008sum$gap, main = "Total GAP per month in 2008",xlab = "Month", ylab = "Total GAP",type = "b", lwd = 2, cex = .5)
plot(tabmonths2009sum$`month(Date)`, tabmonths2009sum$gap, main = "Total GAP per month in 2009",xlab = "Month", ylab = "Total GAP",type = "b", lwd = 2, cex = .5)
plot(tabmonths2010sum$`month(Date)`, tabmonths2010sum$gap, main = "Total GAP per month in 2010",xlab = "Month", ylab = "Total GAP",type = "b", lwd = 2, cex = .5)
 

#MAKE IT NICER WITH GGPLOT
#http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/


#CHECKING the consumption of GAP in one week of february 2009
week2009 <- energyds[energyds$Date>="2009-02-23" & energyds$Date<="2009-03-01",]

head(week2009)

#converting DateTime of week2009 from POSIXlt to POSIXct because ggplot requires an x POSIXct
week2009$DateTime=as.POSIXct(week2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")
class(week2009$DateTime)

#plotting the GAP in that week
ggplot(data=week2009, aes(x=week2009$DateTime, y=week2009$Global_active_power))+
  geom_line()+
  xlab("Day/Time")+
  ylab("GAE (Watt-hour)")+
  ggtitle("Global Active Energy Consumption in a ordinary week (2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))


plotweek <- ggplot(data=week2009, aes(x=week2009$DateTime, y=week2009$Global_active_power))+
  geom_line()+
  #xlab("Day/Time")+
  ylab("GAE (Watt-hour)")+
  ggtitle("Global Active Energy Consumption in a ordinary week (2009)")+
 # scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))

plotweek <- plotweek + scale_x_discrete(name ="day/time", 
                          limits=c("Mon 23/02 00:00","Tue 24/02 00:00", "Wed 25/02 00:00", "Thu 26/02 00:00", "Fri 27/02 00:00", "Sat 28/02 00:00", "Sun 01/03 00:00", "Mon 02/03 00:00"))

plotweek

#If we look closely at the graph, we can see that power usage seems to roughly peak in the mornings
#and the evenings on weekdays, with higher usage on Friday evening, and all of Sunday.

#TO DO: TAKE ANOTHER ORDINARY WEEK AND REPEAT THE ANALYSIS
#TO DO: CATCH SEASONALITY (ONE SUMMER WEEK AND ONE WINTER WEEK)

##SUBMETER ANALYSIS
#The dataset includes measures about specific energy uses, and it’d be interesting to see exactly what appliances are being used at different times. 
#we’re going to just look at a subsection of the data, the readings for Thursday to Sunday of the week we examined in the previous plot.


halfweek2009 <- energyds[energyds$Date>="2009-02-26" & energyds$Date<="2009-03-01",]
head(halfweek2009)

#converting DateTime from POSIXlt to POSIXct 
halfweek2009$DateTime=as.POSIXct(halfweek2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")
class(halfweek2009$DateTime)


ggplot(data=halfweek2009, aes(halfweek2009$DateTime))+
  geom_line(aes(y = halfweek2009$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = halfweek2009$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = halfweek2009$Sub_metering_3, color="Sub metering 3")) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in 4 ordinary days (2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255)),guide='legend') 


#Here, we can see that kitchen electricity usage is pretty low on Thursdays 
#with higher usage on Friday evening, Saturday evening, and Sunday from the afternoon until the evening. 
#By contrast, people seem to use their laundry rooms on Saturday morning, Saturday evening, and on Sunday around noon. 
#Electric water heating and air-conditioning appear to have highest usage during Friday evening, Saturday daytime, and Sunday day time.


#FIXING TIME CHANGE (N.B. working on column DateTime of the NEW DATASET "energy"!)
energy <- energyds

index_change <- which((energy$DateTime >= as.POSIXct("2007-03-25 02:00:00", tz = "GMT") & energy$DateTime <= as.POSIXct("2007-10-28 01:59:00", tz="GMT")) | (energy$DateTime >= as.POSIXct("2008-03-30 02:00:00", tz = "GMT") & energy$DateTime <= as.POSIXct("2008-10-26 01:59:00", tz="GMT")) | (energy$DateTime >= as.POSIXct("2009-03-29 02:00:00", tz = "GMT") & energy$DateTime <= as.POSIXct("2009-10-25 01:59:00", tz="GMT")) | (energy$DateTime >= as.POSIXct("2010-03-28 02:00:00", tz = "GMT") & energy$DateTime <= as.POSIXct("2010-10-31 01:59:00", tz="GMT")))
index_change

#first index: 141637
#energy[141637,]
#last index: 2036676
#index_change[length(index_change)]
#energy[2036676,]

energy$DateTime[index_change] <- energy$DateTime[index_change] + 3600
energy$DateTime[index_change]

#ANALYSIS OF MISSING VALUES with mice / imputeTS packages

statsNA(energy$Global_active_power)

md.pattern(energy)

aggr_plot <- aggr(energy, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#all imputation methods available in mice package
#methods(mice)

#mean(energy$Global_active_power, na.rm = T)
#median(energy$Global_active_power, na.rm = T)


#DATASET FOR DESCRIPTIVE
#FOR LOOP TO FIX THE NAs of the dataset energy (for descriptive analysis)
#NAs > 178 in one day -> holiday -> replaced with ZERO

l = c()
tabNA

for (i in 1:length(row.names(tabNA))) {
  if (tabNA[i] >= 178 & tabNA[i] != 891)
  {l = c(l, row.names(tabNA)[i])}
}


a = which((as.character(energy$Date) %in% l) & is.na(energy$Global_active_power))

energy[a,4:10] = c(0,0,0,0,0,0,0)

summary(energy)


#FOR LOOP for imputing the rest of 1332 NAs (<178 in one day -> black out/meter out of order) in the energy datset (descriptive analysis)
#with the function impute from Hmisc package

for (i in 4:10) {
  energy[i] <- impute(energy[i], mean)
  
}

summary(energy)

#impute(energy, mean)  # replace with mean
#impute(BostonHousing$ptratio, median)  # median
#impute(BostonHousing$ptratio, 20)  # replace specific number


#create the column day
energy$day <- weekdays(energy$DateTime)

#create the column week
energy$week <- week(energy$DateTime)


#function which returns the Season
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}




#check
#getSeason(energy[1,1])
#getSeason(energy[1877700,1])
#getSeason(energy[2033758,1])

#creating the season column
energy$season <- getSeason(energy$DateTime)

head(energy)


##DATASET FOR FORECASTS

energy_forecast <- energyds

#fixing time change
index_change_forecast <- which((energy_forecast$DateTime >= as.POSIXct("2007-03-25 02:00:00", tz = "GMT") & energy_forecast$DateTime <= as.POSIXct("2007-10-28 01:59:00", tz="GMT")) | (energy_forecast$DateTime >= as.POSIXct("2008-03-30 02:00:00", tz = "GMT") & energy_forecast$DateTime <= as.POSIXct("2008-10-26 01:59:00", tz="GMT")) | (energy$DateTime >= as.POSIXct("2009-03-29 02:00:00", tz = "GMT") & energy_forecast$DateTime <= as.POSIXct("2009-10-25 01:59:00", tz="GMT")) | (energy_forecast$DateTime >= as.POSIXct("2010-03-28 02:00:00", tz = "GMT") & energy_forecast$DateTime <= as.POSIXct("2010-10-31 01:59:00", tz="GMT")))
index_change_forecast

energy_forecast$DateTime[index_change_forecast] <- energy_forecast$DateTime[index_change_forecast] + 3600
energy_forecast$DateTime[index_change_forecast]

#DEALING WITH NAs for FORECAST
#FOR LOOP for IMPUTING LESS THAN 178 MINUTES IN ONE DAY (forecast dataset) with locf
m = c()

for (i in 1:length(row.names(tabNA))) {
  if (tabNA[i] < 178 | tabNA[i] == 891)
  {m = c(m, row.names(tabNA)[i])}
}
b = which((as.character(energy_forecast$Date) %in% m))

#in b there are all the MINUTES of the 61 days appearing in m (61*1440 = 87840)
b

#dec212006 <- subset(energy_forecast, Date == "2006-12-21")
#which(is.na(dec212006$Global_active_power))
#dec212006[685,]

#in m there are all the DAYS with less than 178 NAs in one day
m

for (i in 4:10) {
  energy_forecast[b,i] <- na.locf(energy_forecast[b,i], option = "locf", na.remaining = "rev")
  
}

summary(energy_forecast)

#energy_forecast[6834:6846,]

#creating columns day, season and week
energy_forecast$day <- weekdays(energy_forecast$DateTime)
energy_forecast$season <- getSeason(energy_forecast$DateTime)
energy_forecast$week <- week(energy_forecast$DateTime)


#imputing the rest of NAs (holidays) of the forecast dataset with random values picked from 
#the same hour of the same weekday of the same season

ll= unique(energy_forecast$season)
l = unique(energy_forecast$day)

for (i in ll){
  for (j in 0:23){
    for (k in 1:length(l))
    {
      index = which( hour(energy_forecast$DateTime) == j & as.character(energy_forecast$day)== l[k] & as.character(energy_forecast$season)==i  )
      for (m in 4:10)
      {energy_forecast[index,m] = as.integer(impute(energy_forecast[index,m],'random'))}
    }}}


summary(energy_forecast)


#creating Other column in energy_forecast dataset 
energy_forecast$Other <- energy$Global_active_power - (energy$Sub_metering_1 + energy$Sub_metering_2 + energy$Sub_metering_3)
head(energy_forecast)


#ENERGY DATASET FOR DESCRIPTIVE

#creating Other column in energy dataset 
energy$Other <- energy$Global_active_power - (energy$Sub_metering_1 + energy$Sub_metering_2 + energy$Sub_metering_3)

#SAMPLING DATA every 15 minutes and creating new dataset called "energy15"
energy15 = energy[seq(1, nrow(energy), 15), ]

#from now on: working on the dataset "energy15" for DESCRIPTIVE

#energy BY DAY 

#picking the date from DateTime
energy15$Date_tz <- as.Date(energy15$DateTime)

#deleting the column DateTime and creating the new dataset "energy_by_day_desc"
energy_by_day_desc <- energy15[, -1]

#grouping by day and making the avg and the sum of the interesting columns
energy_by_day_desc <- energy_by_day_desc %>% 
  group_by(Date_tz) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other))

energy_by_day_desc


#N.B. energy_by_day_desc is composed of 1442 days!


#N.B. dataset origine per descriptive: energy15. da questo si dipartono i vari livelli di aggregazione
#(giorno, mese, stagione)

#energy BY MONTH

#deleting unuseful columns
energy_by_month_desc <- energy15[, -c(1,6,7,11,12)]

#grouping by month and making the avg and the sum of the interesting columns
energy_by_month_desc <- energy_by_month_desc %>% 
  group_by(month(Date_tz),year(Date_tz)) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other)) %>% 
  arrange(`year(Date_tz)`, `month(Date_tz)`)

energy_by_month_desc

#energy BY SEASON

energy_by_season_desc <- energy15[, -c(1,6,7,11)]
energy_by_season_desc$season <- as.factor(energy_by_season_desc$season)
levels(energy_by_season_desc$season)
energy_by_season_desc$season = factor(energy_by_season_desc$season,levels(energy_by_season_desc$season)[c(4,2,3,1)])



#energy BY YEAR

head(energy15)
#deleting unuseful columns
energy_by_year_desc <- energy15[, -c(1,6,7,11,12)]

#grouping by year and making the avg and the sum of the interesting columns
energy_by_year_desc <- energy_by_year_desc %>% 
  group_by(year(Date_tz)) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other))
 
energy_by_year_desc


# Create test data.
dat = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))

# Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

# Make the plot
p1 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "My Ring plot !") +
  labs(title="")
p1


#grouping by SEASON and making the avg and the sum of the interesting columns
energy_by_season_desc <- energy_by_season_desc %>% 
  group_by(season, year(Date_tz)) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other))
 
energy_by_season_desc <- arrange(energy_by_season_desc, `year(Date_tz)`, season)
energy_by_season_desc


###GROUPING IN THE SAME WAY THE FORECASTS DATASET (where NAs were imputed in a different way)

energy_forecast$Date_tz <- as.Date(energy_forecast$DateTime)

#deleting the column DateTime
energy_by_day_forecast <- energy_forecast[, -1]

#ENERGY BY DAY

#grouping by day and making the avg and the sum of the interesting columns
energy_by_day_forecast <- energy_by_day_forecast %>% 
  group_by(Date_tz) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other))
energy_by_day_forecast


#N.B. energy_by_day_forecast is composed of 1442 days!


#energy BY WEEK
head(energy_forecast)
energy_by_week_forecast <- energy_forecast[, -1]

#grouping by week and making the avg and the sum of the interesting columns
energy_by_week_forecast <- energy_by_week_forecast %>% 
  group_by(week(Date_tz), year(Date_tz)) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other)) %>% 
  arrange(`year(Date_tz)`, `week(Date_tz)`)
energy_by_week_forecast


#energy BY MONTH

#deleting unuseful columns
energy_by_month_forecast <- energy_forecast[, -c(1,6,7,11,12)]

#grouping by month and making the avg and the sum of the interesting columns
energy_by_month_forecast <- energy_by_month_forecast %>% 
  group_by(month(Date_tz), year(Date_tz)) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other)) %>% 
  arrange(`year(Date_tz)`, `month(Date_tz)`)
energy_by_month_forecast


#energy BY SEASON

energy_by_season_forecast <- energy_forecast[, -c(1,6,7,11)]
energy_by_season_forecast$season <- as.factor(energy_by_season_forecast$season)
energy_by_season_forecast$season = factor(energy_by_season_forecast$season,levels(energy_by_season_forecast$season)[c(4,2,3,1)])
#levels(energy_by_season_forecast$season)

#grouping by SEASON and making the avg and the sum of the interesting columns
energy_by_season_forecast <- energy_by_season_forecast %>% 
  group_by(season, year(Date_tz)) %>%
  summarise(AVG_gap = mean(Global_active_power), AVG_grp = mean(Global_reactive_power), AVG_SM1 = mean(Sub_metering_1), AVG_SM2 = mean(Sub_metering_2), AVG_SM3 = mean(Sub_metering_3), AVG_Other = mean(Other), Tot_gap = sum(Global_active_power), Tot_grp = sum(Global_reactive_power), Tot_SM1 = sum(Sub_metering_1), Tot_SM2 = sum(Sub_metering_2), Tot_SM3 = sum(Sub_metering_3), Tot_Other = sum(Other))

energy_by_season_forecast <- arrange(energy_by_season_forecast, `year(Date_tz)`, season)

energy_by_season_forecast


#N.B. winter aggregates in one class both january and december of the same year... FIX THIS problem!!!

#working on FORECAST DATASETS to make PREDICTIONS

#selecting data from 1st January 2007 and creating the new datasets

#N.B. for the daily version I deleted the 29th february 2008 (leap year) in order to have 365 days for each year
energy_by_day_forecast_from2007 <- energy_by_day_forecast %>% 
  dplyr::filter(Date_tz >= "2007-01-01" & Date_tz != "2008-02-29")

energy_by_week_forecast_from2007 <- energy_by_week_forecast %>% 
  dplyr::filter(`year(Date_tz)` >= 2007)

energy_by_month_forecast_from2007 <- energy_by_month_forecast %>% 
  dplyr::filter(`year(Date_tz)` >= 2007)

energy_by_season_forecast_from2007 <- energy_by_season_forecast %>% 
  dplyr::filter(`year(Date_tz)` >= 2007)



#PLOT TOTAL GAE BY MONTH - for each year ON THE MONTHLY DESCRIPTIVE DATASET:

energy_by_day_desc_from2007 <- energy_by_day_desc %>% 
  dplyr::filter(Date_tz >= "2007-01-01" & Date_tz != "2008-02-29")

#N.B. NO weekly dataset because it only exists for forecasts

energy_by_month_desc_from2007 <- energy_by_month_desc %>% 
  dplyr::filter(`year(Date_tz)` >= 2007)

energy_by_season_desc_from2007 <- energy_by_season_desc %>% 
  dplyr::filter(`year(Date_tz)` >= 2007)

energy_by_month_desc_from2007 %>%
  ggplot(aes(x = `month(Date_tz)`, y = Tot_gap)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap(~ `year(Date_tz)`, ncol = 2) +
  labs(title = "Total Monthly Consumption",
       subtitle = "Data plotted by year",
       y = "Monthly GAE (WattHour)",
       x = "Month") + theme_bw(base_size = 15)


#DAILY FORECASTS: converting it to a Time series
energy_by_day_forecast_from2007_ts <- ts(energy_by_day_forecast_from2007, start =c(2007,1), frequency = 365)
#str(energy_by_day_forecast_from2007_ts)
#1425 days from 1st of jan 2007

#WEEKLY FORECASTS: converting it to a Time series
energy_by_week_forecast_from2007_ts <- ts(energy_by_week_forecast_from2007, start = c(2007,1), frequency = 53)
head(energy_by_week_forecast_from2007_ts)

#MONTHLY FORECASTS : converting it to a Time series
energy_by_month_forecast_from2007_ts <- ts(energy_by_month_forecast_from2007, start = c(2007,1), frequency = 12)


#SEASONLY FORECASTS: converting it to a Time series
energy_by_season_forecast_from2007_ts <- ts(energy_by_season_forecast_from2007, start = c(2007,1), frequency = 4)



#INDEX OF COLUMN "TOT GAP" to predict in the different TS:

#colnames(energy_by_day_forecast_from2007_ts)
#colnames(energy_by_week_forecast_from2007_ts)
#colnames(energy_by_month_forecast_from2007_ts)
#colnames(energy_by_season_forecast_from2007_ts)


#energy_by_day_forecast_from2007_ts -> 8
#energy_by_week_forecast_from2007_ts -> 9
#energy_by_month_forecast_from2007_ts -> 9
#energy_by_season_forecast_from2007_ts -> 9


plot.ts(energy_by_season_forecast_from2007_ts[,9])
plot.ts(energy_by_week_forecast_from2007_ts[,9])


###-------------------------------------------------------------------
#WORKING ON DAILY FORECASTS: predict the next 35 days (TOT GAP)
#?tslm
plot.ts(energy_by_day_forecast_from2007_ts[,8])
#it seems that an additive model is appropriate to describe this time series, as the seasonal fluctuations are roughly 
#constant in size over time and do not seem to depend on
#the level of the time series, and the random fluctuations also seem to be roughly constant in size over time.
#there seems to be seasonal variation in the consumption of energy: there is a
#peak every winter, and a trough every summer.
#it seems a PERIODIC FUNCTION (a function that repeats its values in regular intervals or periods (1 year))



#DECOMPOSING SEASONAL DATA
##A seasonal time series consists of a trend component, a seasonal component and an irregular component. Decomposing
#the time series means separating the time series into these three components: that is, estimating these three
#components.
#The decompose function estimates the trend, seasonal, and irregular components of a time series 
#that can be described using an additive model.


#decompose the GAP in daily TS
dec_daily <- decompose(energy_by_day_forecast_from2007_ts[,8])
dec_daily
plot(dec_daily)

#The plot shows the original time series (top), the estimated trend component (second from top), the estimated
#seasonal component (third from top), and the estimated irregular component (bottom). We see that the estimated trend
#component shows a decrease from 2008 to 2009, followed by a steady increase from
#2009 to 2010. take into account CRISIS of 2008 for this downwards tendency.

plot(dec_daily$x)
plot(dec_daily$seasonal)
plot(dec_daily$trend)
plot(dec_daily$random)


#NICER PLOTS 
#plotting the 3 components of the daily TS
autoplot(dec_daily)
autoplot(dec_daily$trend)


#Identify change points in mean and variance #THIS DOES NOT WORK because of NAs
#dec_daily$trend %>%
 # changepoint:: cpt.meanvar() %>%  # Identify change points
  #autoplot()
# Detect jump in a data
#strucchange::breakpoints(dec_daily$trend) %>% #DOES NOT WORK becayse NOT APPLICABLE TO TS OBJECTS
 # autoplot()



#ORIGINAL DAILY DATA BEAUTIFUL
p <- ggplot(data = energy_by_day_forecast_from2007, aes(x = Date_tz, y = Tot_gap)) + 
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle("Seasonality in Energy Consumption")+
  xlab("Time")+
  ylab("Daily Global Active Energy")
p
p + scale_x_date(date_labels = "%b/%Y")
p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "auto"
)



#ggplot(lynx, as.numeric = FALSE) + geom_line() + 
 # stat_peaks(colour = "red") +
  #stat_peaks(geom = "text", colour = "red", 
   #          vjust = -0.5, x.label.fmt = "%Y") +
  #stat_valleys(colour = "blue") +
  #stat_valleys(geom = "text", colour = "blue", angle = 45,
   #            vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")


#ADJUSTMENT FOR SEASONALITY
#If you have a seasonal time series that can be described using an additive model, you can seasonally adjust the time
#series by estimating the seasonal component, and subtracting the estimated seasonal component from the original time
#series.

daily_seasonally_adjusted <- energy_by_day_forecast_from2007_ts[,8] - dec_daily$seasonal
plot(daily_seasonally_adjusted)

#dec_daily_season_adj <- decompose(daily_seasonally_adjusted)
#plot(dec_daily_season_adj)

#the seasonal variation has been removed from the seasonally adjusted time series. The seasonally
#adjusted time series now just contains the trend component and an irregular component.

#If you have a time series that can be described using an additive model with CONSTANT LEVEL AND NO SEASONALITY, you can
#use simple exponential smoothing to make short-term forecasts -> To make forecasts using simple exponential smoothing in R, 
#we can fit a simple exponential smoothing predictive model using the “HoltWinters()” function in R.




####SIMPLE EXPONENTIAL SMOOTHING
plot(daily_seasonally_adjusted)
fcHW_daily <- HoltWinters(daily_seasonally_adjusted, beta=FALSE, gamma=FALSE)
fcHW_daily
#N.B. alpha = 0.14
#N.B.Values of alpha that are close to 0 mean that little weight is placed on the most recent observations
#when making forecasts of future values.

fcHW_daily$fitted
plot(fcHW_daily)

#As a measure of the accuracy of the forecasts, we can calculate the sum of squared errors for the in-sample forecast
#errors, that is, the forecast errors for the time period covered by our original time series.
fcHW_daily$SSE
#SSE = 62475323761


#N.B. by default HoltWinters() just makes forecasts for the time period covered by the original data.
# We can make forecasts for further time points by using the “forecast.HoltWinters()” 
#function in the R “forecast” package.
#You specify how many further time points you want to make forecasts for by using the “h” parameter in forecast.HoltWinters().
#could not find this function -> I have used the forecast function

#prediction for the next 35 days (willing to complete year 2010)
fcHW_daily_future <- forecast(fcHW_daily, h=35)
fcHW_daily_future
#The forecast function gives you the forecast for a day, a 80% prediction interval for the forecast, and
#a 95% prediction interval for the forecast.

#plotting the predictions made by HolWinters - daily
plot(fcHW_daily_future)
#Here the forecasts for the next 35 days are plotted as a blue line, 
#the 80% prediction interval as a dark grey shaded area, and
#the 95% prediction interval as a light grey shaded area.

#If the predictive model cannot be improved upon, there should be no correlations between forecast
#errors for successive predictions. In other words, if there are correlations between forecast errors for successive predictions,
#it is likely that the simple exponential smoothing forecasts could be improved upon by another forecasting technique.


checkresiduals(fcHW_daily_future, lag = 20)


#To figure out whether this is the case, we can obtain a correlogram of the in-sample forecast errors for lags 1-20. We
#can calculate a correlogram of the forecast errors using the “Acf()” function in R.
Acf(fcHW_daily_future$residuals, lag.max = 20)
#To test whether there is significant evidence for non-zero correlations at lags 1-20, 
#we can carry out a Ljung-Box test.
Box.test(fcHW_daily_future$residuals, lag=20, type="Ljung-Box")
#p-value < 2.2e-16 -> below 0.05 -> significant autocorrelations (there is evidence of non-zero autocorrelations
#in the in-sample forecast errors at lags 1-20.)



qqnorm(fcHW_daily_future$residuals)
qqline(fcHW_daily_future$residuals)
#The values are normal as they rest on a line and aren’t all over the place.


#To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors
#are normally distributed with mean zero and constant variance. To check whether the forecast errors have constant
#variance, we can make a time plot of the in-sample forecast errors:
plot.ts(fcHW_daily_future$residuals)
hist(fcHW_daily_future$residuals)


#The plot shows that the distribution of forecast errors is roughly centred on zero, and is more or less normally
#distributed

#To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of the forecast
#errors, with an overlaid normal curve that has mean zero and the same standard deviation as the distribution of forecast
#errors. To do this, we can define an R function “plotForecastErrors()”, below:
#(returns ERROR)

####--------------
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }

  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#You can then use plotForecastErrors() to plot a
#histogram (with overlaid normal curve) of the forecast errors for the rainfall predictions:

plotForecastErrors(fcHW_daily_future$residuals)

###-------------

#If the Ljung-Box test shows that there is little evidence of non-zero autocorrelations in the in-sample forecast errors,
#and if the distribution of forecast errors seems to be normally distributed with mean zero, this would suggest that the simple
#exponential smoothing method provides an adequate predictive model.
#Furthermore, the assumptions that the 80% and 95% predictions intervals were based upon (that
#there are no autocorrelations in the forecast errors, and the forecast errors are normally distributed with mean zero and
#constant variance) would be probably valid.

accuracy(fcHW_daily_future)



####HOLT'S EXPONENTIAL SMOOTHING

#If you have a time series that can be described using an additive model with increasing or decreasing trend and no
#seasonality, you can use Holt’s exponential smoothing to make short-term forecasts.
plot(daily_seasonally_adjusted)
plot(dec_daily_season_adj)

fcHW_daily_1 <- HoltWinters(daily_seasonally_adjusted, gamma=FALSE)
fcHW_daily_1

#alpha:  0.4755332
#beta : 0.1570963

#in this case alpha is 0.47 (relatively low, indicating that the estimate of the level at the current time point is based upon both recent observations
#and some observations in the more distant past.) and beta is 0.15 (very low -> meaning that the estimate of the slope of the trend component 
#is based mostly upon old observations)


#N.B. If they were high, telling us that both the estimate of the
#current value of the level, and of the slope b of the trend component, were based mostly upon 
#very recent observations in the time series.

fcHW_daily_1$SSE
#SSE = 85035432646

plot(fcHW_daily_1)

#prediction for the next 35 days (willing to complete year 2010)
fcHW_daily_1_future <- forecast(fcHW_daily_1, h=35)
fcHW_daily_1_future
plot(fcHW_daily_1_future)

#PROBLEM: PREDICTIONS TAKE ALSO NEGATIVE VALUES. Why?
#TO DO: read tutorial example

checkresiduals(fcHW_daily_1_future$residuals)

Acf(fcHW_daily_1_future$residuals, lag.max = 20)
Box.test(fcHW_daily_1_future$residuals, lag=20, type="Ljung-Box")

#the correlogram shows that the sample autocorrelation for the in-sample forecast errors exceeds the
#significance bounds at several lags.
#the p-value of the Ljung-Box test is less than 0.05
#indicating that there is evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.
#therefore, the model can be improved upon!



plot(fcHW_daily_1_future$residuals)
#The time plot of forecast errors shows that the forecast errors have roughly constant variance over time (apart in the initial part)
hist(fcHW_daily_1_future$residuals)
#The histogram of forecast errors show that it is plausible that the forecast errors are normally distributed with mean zero and constant variance.


accuracy(fcHW_daily_1_future)


####HOLT - WINTERS EXPONENTIAL'S SMOOTHING

#If you have a time series that can be described using an additive model with increasing or decreasing 
#trend and seasonality, you can use Holt-Winters exponential smoothing to make short-term forecasts.

#Holt-Winters exponential smoothing estimates the level, slope and seasonal component at the current time point.
#Smoothing is controlled by three parameters: alpha, beta, and gamma, for the estimates of the level, slope b of the
#trend component, and the seasonal component, respectively, at the current time point.
#The parameters alpha, beta and gamma all have values between 0 and 1, and values that are close to 0 mean that relatively 
#little weight is placed on the most recent observations when making forecasts of future values.

daily_seasonal <- energy_by_day_forecast_from2007_ts[,8]
plot(daily_seasonal)

fcHW_daily_2 <- HoltWinters(daily_seasonal)
fcHW_daily_2

#Smoothing parameters:
 # alpha: 0.1290861
#beta : 0
#gamma: 0.486647



#The value of alpha 0.12 is low, indicating that the estimate of the level at the current time point is based upon 
#observations in the more distant past.
#The value of beta is 0.00, indicating that the estimate of the slope b
#of the trend component is not updated over the time series, and instead is set equal to its initial value.
#The value of gamma (0.48) indicates that the estimate of the seasonal component at the current time point 
#is based upon both recent and past observations.

fcHW_daily_2$SSE
#SSE = 63724715311

plot(fcHW_daily_2)

#making forecasts for future times (next 35 days) not included in the original time series
fcHW_daily_2_future <- forecast(fcHW_daily_2, h=35)
fcHW_daily_2_future
plot(fcHW_daily_2_future)

checkresiduals(fcHW_daily_2_future)

#We can investigate whether the predictive model can be improved upon by checking whether the in-sample forecast
#errors show non-zero autocorrelations at lags 1-20, by making a correlogram and carrying out the Ljung-Box test
Acf(fcHW_daily_2_future$residuals, lag.max = 20)
Box.test(fcHW_daily_2_future$residuals, lag=20, type="Ljung-Box")
#The correlogram shows that the autocorrelations for the in-sample forecast errors exceed the significance bounds
#for lags 1-20.
#Furthermore, the p-value for Ljung-Box test is < 2.2e-16, indicating that there is evidence of non-zero
#autocorrelations at lags 1-20.

plot(fcHW_daily_2_future$residuals)
#the forecast errors don't have constant variance over time...
hist(fcHW_daily_2_future$residuals)
#... but they are normally distributed around zero


accuracy(fcHW_daily_2_future)

#the model can be improved!


#SO FAR, HW ARE NOT APPROPRIATE MODELS FOR THE DAILY TS




#ARIMA MODELS

#Exponential smoothing methods are useful for making forecasts, and make no assumptions about the correlations
#between successive values of the time series. However, if you want to make prediction intervals for forecasts made
#using exponential smoothing methods, the prediction intervals require that the forecast errors are uncorrelated and are
#normally distributed with mean zero and constant variance.
#While exponential smoothing methods do not make any assumptions about correlations between successive values of
#the time series, in some cases you can make a better predictive model by taking correlations in the data into account.
#Autoregressive Integrated Moving Average (ARIMA) models include an explicit statistical model for the irregular
#component of a time series, that allows for non-zero autocorrelations in the irregular component.


#ARIMA models are defined for stationary time series. Therefore, if you start off with a non-stationary time series, you
#will first need to ‘difference’ the time series until you obtain a stationary time series. If you have to difference the 
#time series d times to obtain a stationary series, then you have an ARIMA(p,d,q) model, where d is the order of
#differencing used.
#A time series is stationary in mean and variance if the level of the series stays roughly constant over time, 
#and the variance of the series appears roughly constant over time.

plot(daily_seasonal)

#test for stationarity of the time series KPSS Unit Root Tests
#N.B. are used for testing a null hypothesis that an observable time series is stationary around a deterministic trend 
#(i.e. trend-stationary) against the alternative of a unit root.


#TEST FOR STATIONARITY from the package urca
KPSS_urca <- ur.kpss(daily_seasonal)
summary(KPSS_urca) #clear results!!

#Test is of type: mu with 7 lags. 

#Value of test-statistic is: 0.3406 

#Critical value for a significance level of: 
#  10pct  5pct 2.5pct  1pct
#critical values 0.347 0.463  0.574 0.739

#We can accept the null hypothesis of stationarity as the value of the test statistic is less than the 
#10%, 5% and 1% critical values (0.3406 < 0.347, 0.3406 <0.463, 0.3406 < 0.574, 0.3406 <0.739).

####CODE for differencing the time series and repeating the KPSS test:
#daily_seasonal_diff <- diff(daily_seasonal, differences = 1)
#plot(daily_seasonal_diff)
#summary(ur.kpss(daily_seasonal_diff))

#Test is of type: mu with 7 lags. 

#Value of test-statistic is: 0.0106  

#Critical value for a significance level of: 
 # 10pct  5pct 2.5pct  1pct
#critical values 0.347 0.463  0.574 0.739


#Now we are pretty sure that the time series is stationary, thus we can apply the ARIMA model with order of
#differencing (d) 1 (ARIMA (p,1,q) model).
#By taking the time series of first differences, we have removed the trend component of the time series 
#and are left with an irregular component.
#We can now examine whether there are correlations between successive terms of this
#irregular component; if so, this could help us to make a predictive model


#SELECT THE BEST ARIMA MODEL
#The next step is to figure out the values of p and q for the ARIMA model.

Acf(daily_seasonal, lag.max=500)

#To plot the partial correlogram for lags 1-20 for the once differenced time series 
#and get the values of the partial autocorrelations, we use the “Pacf()” function, by typing:
Pacf(daily_seasonal, lag.max=500)

#getting the values of partial autocorrelations
Pacf(daily_seasonal_diff, lag.max=500, plot = F)


#the correlogram is zero after lag (?), and the partial correlogram tails off to zero after lag (?)

#read pag 55-56 for the choice of the model

#AUTO ARIMA FUNCTION to find the appropriate ARIMA model
#auto.arima(daily_seasonal)

#Series: daily_seasonal 
#ARIMA(5,0,2) with non-zero mean 

#Coefficients:
#  ar1     ar2     ar3     ar4      ar5      ma1      ma2       mean
#0.8072  0.2207  0.0419  0.0809  -0.1644  -0.4456  -0.4038  25301.570
#s.e.  0.1159  0.1374  0.0501  0.0393   0.0318   0.1144   0.0957   1964.169

#sigma^2 estimated as 48691389:  log likelihood=-14630.56
#AIC=29279.11   AICc=29279.24   BIC=29326.47

#the original time series can be modelled using an ARIMA(5,0,2) model 
#(with p=5, d=0, q=2), where d is the order of differencing required.
#R has estimated several models and calculated the Akaike information criterion for each model. 
#Then it picked the model with the lowest AIC.

#If we use the “bic” criterion, which penalises the number of parameters, we get ARIMA(1,0,3):
daily_seasonal_arima <- auto.arima(daily_seasonal, ic = "bic")
#Series: daily_seasonal 
#ARIMA(1,0,3) with non-zero mean 

#Coefficients:
#  ar1      ma1      ma2     ma3       mean
#0.9758  -0.6131  -0.2346  0.0733  25287.210
#s.e.  0.0082   0.0273   0.0295  0.0261   1691.339

#sigma^2 estimated as 49511480:  log likelihood=-14643.92
#AIC=29299.83   AICc=29299.89   BIC=29331.4

#FORECASTING WITH ARIMA MODEL

#Once you have selected the best candidate ARIMA(p,d,q) model for your time series data, you can estimate the
#parameters of that ARIMA model and use that as a predictive model for making forecasts for future values of your
#time series.
#You can estimate the parameters of an ARIMA(p,d,q) model using the “arima()” function in R.

#following deleted:
#daily_seasonal_arima <- arima(daily_seasonal, order = c(1,0,3)) #fit an ARIMA (1,0,3) model
#daily_seasonal_arima

#predicting the next 35 days (specifying the confidence level for prediction intervals to be 95%)
daily_seasonal_arima_future <- forecast(daily_seasonal_arima, h=35, level = 95)
daily_seasonal_arima_future
plot(daily_seasonal_arima_future)

checkresiduals(daily_seasonal_arima_future)

#acf and box test
Acf(daily_seasonal_arima_future$residuals, lag.max = 20)
Box.test(daily_seasonal_arima_future$residuals, lag=20, type="Ljung-Box")

plot(daily_seasonal_arima_future$residuals) #time plot of forecast errors
plotForecastErrors(daily_seasonal_arima_future$residuals) #histogram of forecast errors
mean(daily_seasonal_arima_future$residuals) #mean is negative!


accuracy(daily_seasonal_arima)
accuracy(daily_seasonal_arima_future)




###LINEAR MODEL to DAILY time series including trend and seasonality components
plot(daily_seasonal)
lm_daily <- tslm(daily_seasonal~ trend + season)
plot(lm_daily)
summary(lm_daily)
lm_daily_future <- forecast(lm_daily, h = 35)
summary(lm_daily_future)
plot(lm_daily_future)
#NICER PLOT
autoplot(lm_daily_future)+
  ggtitle("Forecasts of Energy Consumption in the next 35 days (Linear Regression)") +
  xlab("Time") + ylab("WattHour")


checkresiduals(lm_daily_future)
#IMPORTANT: ACCURACY
accuracy(lm_daily_future)

#                       ME     RMSE      MAE     MPE     MAPE      MASE      ACF1
#Training set 3.985887e-13 6260.528 4799.966 -9.8622 24.97963 0.6143406 0.2876951





#STL with multiple seasonal periods

daily_seasonal %>% mstl() %>%
  autoplot() + xlab("Day")

#The mstl() function is a variation on stl() designed to deal with multiple seasonality. 
#It will return multiple seasonal components, as well as a trend and remainder component.




#ERROR METRICS:
#rmse(actual, predicted) #from Metrics package
# daily_seasonal #actual
# lm_daily$fitted.values #predicted

#rmse(model, data) #from modelr
rmse(lm_daily, daily_seasonal) 


plot(dec_daily)
plot(dec_monthly)
plot(dec_seasonly)


###-----------------------------------
#investigate about the VALLEY IN AUGUST 2008 (does it depend on the imputation of NAs or just low consumption?)

min_values <- subset(energyds, Date >= "2008-08-01" & Date <= "2008-08-31")
summary(min_values)
missing_gap_min <- subset(min_values, is.na(Global_active_power))
missing_gap_min
#2 isolated NAS in here (2008-08-04 17:03:00 and 2008-08-31 18:41:00)-> they were days with very low consumption!
mean(min_values$Global_active_power, na.rm = T) #4.608137
sum(min_values$Global_active_power, na.rm = T) #205698

#comparing with the consumption of other augusts from the other years:

min_values_cfr1 <- subset(energyds, Date >= "2007-08-01" & Date <= "2007-08-31")
summary(min_values_cfr1)
mean(min_values_cfr1$Global_active_power, na.rm = T) #12.73644
sum(min_values_cfr1$Global_active_power, na.rm = T) #568274.3

min_values_cfr <- subset(energyds, Date >= "2009-08-01" & Date <= "2009-08-31")
summary(min_values_cfr)
mean(min_values_cfr$Global_active_power, na.rm = T) #11.07698
sum(min_values_cfr$Global_active_power, na.rm = T) #484606.7

min_values_cfr2 <- subset(energyds, Date >= "2010-08-01" & Date <= "2010-08-31")
summary(min_values_cfr2)
mean(min_values_cfr2$Global_active_power, na.rm = T) #9.846303
sum(min_values_cfr2$Global_active_power, na.rm = T) #368389.6

#-> august 2008 was the lowest august ever!



####----------
#decompose the GAP in monthly TS

dec_monthly <- decompose(energy_by_month_forecast_from2007_ts[,9])
dec_monthly
plot(dec_monthly$seasonal)
plot(dec_monthly$trend)
plot(dec_monthly$random)
autoplot(dec_monthly)

#decompose the GAP in seasonly TS

dec_seasonly <- decompose(energy_by_season_forecast_from2007_ts[,9])
dec_seasonly
plot(decompose(energy_by_season_forecast_from2007_ts[,9]))
plot(dec_seasonly$seasonal)
plot(dec_seasonly$trend)
plot(dec_seasonly$random)


forecast::findfrequency(energy_by_season_forecast_from2007_ts[,9])


