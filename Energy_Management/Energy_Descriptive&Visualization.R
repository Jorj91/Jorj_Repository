#PLOTS FOR DESCRIPTIVE


#COMPARISON WINTER - SUMMER WEEK

#FEBRUARY 2009

week_feb_2009 <- energy15[energy15$Date_tz>="2009-02-23" & energy15$Date_tz<="2009-03-01",]
head(week_feb_2009)

class(week_feb_2009$Date_tz)

#converting DateTime from POSIXlt to POSIXct 
week_feb_2009$DateTime=as.POSIXct(week_feb_2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")
class(week_feb_2009$DateTime)

#THIS WORKS
ggplot(data=week_feb_2009, aes(week_feb_2009$DateTime))+
  geom_line(aes(y = week_feb_2009$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_feb_2009$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_feb_2009$Sub_metering_3, color="Sub metering 3")) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Winter week (February 2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255)),guide='legend') 



####
tabNA
#7-13 Jan 2008
#JANUARY 2008

week_jan_2008 <- energy15[energy15$Date_tz>="2008-01-07" & energy15$Date_tz<="2008-01-13",]
head(week_jan_2008)

#converting DateTime from POSIXlt to POSIXct 
week_jan_2008$DateTime=as.POSIXct(week_jan_2008$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")

#THIS WORKS
ggplot(data=week_jan_2008, aes(week_jan_2008$DateTime))+
  geom_line(aes(y = week_jan_2008$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_jan_2008$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_jan_2008$Sub_metering_3, color="Sub metering 3")) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Winter week (January 7 -13, 2008)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255)),guide='legend') 


plot(monthly_seasonal_noout)
###
#14-20 Jan 2008

#JANUARY 2008

week_jan1_2008 <- energy15[energy15$Date_tz>="2008-01-14" & energy15$Date_tz<="2008-01-20",]
head(week_jan1_2008)

#converting DateTime from POSIXlt to POSIXct 
week_jan1_2008$DateTime=as.POSIXct(week_jan1_2008$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")

#THIS WORKS
ggplot(data=week_jan1_2008, aes(week_jan1_2008$DateTime))+
  geom_line(aes(y = week_jan1_2008$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_jan1_2008$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_jan1_2008$Sub_metering_3, color="Sub metering 3")) + 
  geom_line(aes(y = week_jan1_2008$Other, color="Other")) +
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Winter week (January 14 - 20, 2008)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255), 'Other'=rgb(40, 20, 150, maxColorValue = 255)),guide='legend') 


###THIS for WINTER
#12-18 Jan 2009

#JANUARY 2008

week_jan1_2009 <- energy15[energy15$Date_tz>="2009-01-12" & energy15$Date_tz<="2009-01-18",]
head(week_jan1_2009)

#converting DateTime from POSIXlt to POSIXct 
week_jan1_2009$DateTime=as.POSIXct(week_jan1_2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")

#THIS WORKS
ggplot(data=week_jan1_2009, aes(week_jan1_2009$DateTime))+
  geom_line(aes(y = week_jan1_2009$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_jan1_2009$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_jan1_2009$Sub_metering_3, color="Sub metering 3")) + 
  geom_line(aes(y = week_jan1_2009$Other, color="Other")) +
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Winter week (January 12 - 18, 2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255), 'Other'=rgb(40, 20, 150, maxColorValue = 255)),guide='legend') 







#df filtering the week and the interesting columns
week_feb_2009_df <- energy_by_day_desc %>% 
  dplyr::filter(Date_tz >= "2009-02-23" & Date_tz <= "2009-03-01") %>% 
  dplyr::select(Date_tz, Tot_gap, Tot_SM1, Tot_SM2, Tot_SM3, Tot_Other)
week_feb_2009_df

#reshaping the dataset in order to do the stacked bar
Day_of_week_feb = sort(rep(week_feb_2009_df$Date_tz, 4))
Day_of_week_feb <- paste(Day_of_week_feb, weekdays(Day_of_week_feb))
Day_of_week_feb

energy_consumption_feb=rep(c("SM1" , "SM2" , "SM3", "Other") , 7)
energy_consumption_feb

value_feb=c(81,203,831,1868,0,101,967,915,46,20,473,712,0,14,455,807,83,14,1001,1180,53,371,514,826,640,211,585,1244)
value_feb  

data_feb=data.frame(Day_of_week_feb,energy_consumption_feb,value_feb)



# Grouped
ggplot(data_feb, aes(fill=energy_consumption_feb, y=value_feb, x=Day_of_week_feb)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked (Level -> to make comparison with summer)
ggplot(data_feb, aes(fill=energy_consumption_feb, y=value_feb, x=Day_of_week_feb)) + 
  geom_bar( stat="identity")


## Stacked Percent (Share of SMs and Other)
ggplot(data_feb, aes(fill=energy_consumption_feb, y=value_feb, x=Day_of_week_feb)) + 
  geom_bar( stat="identity", position="fill")



#JULY 2009


tabNA
week_jul_2009 <- energy15[energy15$Date_tz>="2009-07-13" & energy15$Date_tz<="2009-07-19",]
head(week_jul_2009)

#converting DateTime from POSIXlt to POSIXct 
week_jul_2009$DateTime=as.POSIXct(week_jul_2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")
class(week_jul_2009$DateTime)

#THIS WORKS
ggplot(data=week_jul_2009, aes(week_jul_2009$DateTime))+
  geom_line(aes(y = week_jul_2009$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_jul_2009$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_jul_2009$Sub_metering_3, color="Sub metering 3")) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Summer week (July 2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255)),guide='legend') 


###
#JULY 2007
week_jul_2007 <- energy15[energy15$Date_tz>="2007-07-16" & energy15$Date_tz<="2007-07-22",]
head(week_jul_2007)

#converting DateTime from POSIXlt to POSIXct 
week_jul_2007$DateTime=as.POSIXct(week_jul_2007$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")
class(week_jul_2007$DateTime)

#THIS WORKS
ggplot(data=week_jul_2007, aes(week_jul_2007$DateTime))+
  geom_line(aes(y = week_jul_2007$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_jul_2007$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_jul_2007$Sub_metering_3, color="Sub metering 3")) + 
  geom_line(aes(y = week_jul_2007$Other, color="Other")) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Summer week (July 2007)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255),'Other'=rgb(40, 20, 150, maxColorValue = 255)),guide='legend') 

###
tabNA




###THIS FOR SUMMER
#AUGUST 2009
week_aug_2009 <- energy15[energy15$Date_tz>="2009-08-17" & energy15$Date_tz<="2009-08-23",]
head(week_aug_2009)

#converting DateTime from POSIXlt to POSIXct 
week_aug_2009$DateTime=as.POSIXct(week_aug_2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")


#THIS WORKS
ggplot(data=week_aug_2009, aes(week_aug_2009$DateTime))+
  geom_line(aes(y = week_aug_2009$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_aug_2009$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_aug_2009$Sub_metering_3, color="Sub metering 3")) + 
  geom_line(aes(y = week_aug_2009$Other, color="Other")) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Summer week (August 2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255), 'Other'=rgb(40, 20, 150, maxColorValue = 255)),guide='legend') 

###




###
#AUGUST 2009 FERRAGOSTO WEEK
week_fer_2009 <- energy15[energy15$Date_tz>="2009-08-10" & energy15$Date_tz<="2009-08-16",]
head(week_fer_2009)

summary(energy15)
#converting DateTime from POSIXlt to POSIXct 
week_fer_2009$DateTime=as.POSIXct(week_fer_2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")

plot(monthly_seasonal_noout)
tabNA
#THIS WORKS
ggplot(data=week_fer_2009, aes(week_fer_2009$DateTime))+
  geom_line(aes(y = week_fer_2009$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = week_fer_2009$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = week_fer_2009$Sub_metering_3, color="Sub metering 3")) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Summer week (August 2009 - Ferragosto)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255)),guide='legend') 

###


#df filtering the week and the interesting columns
week_jul_2009_df <- energy_by_day_desc %>% 
  dplyr::filter(Date_tz >= "2009-07-13" & Date_tz <= "2009-07-19") %>% 
  dplyr::select(Date_tz, Tot_gap, Tot_SM1, Tot_SM2, Tot_SM3, Tot_Other)
week_jul_2009_df

#reshaping the dataset in order to do the stacked bar
Day_of_week_jul = sort(rep(week_jul_2009_df$Date_tz, 4))

Day_of_week_jul <- paste(Day_of_week_jul, weekdays(Day_of_week_jul))
Day_of_week_jul

energy_consumption_jul=rep(c("SM1" , "SM2" , "SM3", "Other") , 7)
energy_consumption_jul

value_jul=c(0,41,242,265,182,41,421,393,9,65,379,290,0,121,293,287,0,44,460,319,0,49,370,580,0,39,305,988)
value_jul

data_jul=data.frame(Day_of_week_jul,energy_consumption_jul,value_jul)



# Grouped
ggplot(data_jul, aes(fill=energy_consumption_jul, y=value_jul, x=Day_of_week_jul)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked (Level -> to make comparison with winter)
ggplot(data_jul, aes(fill=energy_consumption_jul, y=value_jul, x=Day_of_week_jul)) + 
  geom_bar( stat="identity")

# Stacked Percent (Share of SMs and Other)
ggplot(data_jul, aes(fill=energy_consumption_jul, y=value_jul, x=Day_of_week_jul)) + 
  geom_bar( stat="identity", position="fill")


require(gridExtra)

#STACKED winter vs summer
# Stacked WINTER
plot1 <- ggplot(data_feb, aes(fill=energy_consumption_feb, y=value_feb, x=Day_of_week_feb)) + 
  geom_bar( stat="identity")
# Stacked SUMMER
plot2 <- ggplot(data_jul, aes(fill=energy_consumption_jul, y=value_jul, x=Day_of_week_jul)) + 
  geom_bar( stat="identity")
plot1
plot2
#to plot them side by side:
grid.arrange(plot1, plot2, nrow=2)


#STACKED PERCENT winter vs summer
# Stacked Percent WINTER
plot3 <- ggplot(data_feb, aes(fill=energy_consumption_feb, y=value_feb, x=Day_of_week_feb)) + 
  geom_bar( stat="identity", position="fill")
# Stacked Percent SUMMER
plot4 <- ggplot(data_jul, aes(fill=energy_consumption_jul, y=value_jul, x=Day_of_week_jul)) + 
  geom_bar( stat="identity", position="fill")
grid.arrange(plot3, plot4, nrow=2)



###THIS for WINTER
#12-18 Jan 2009

#JANUARY 2008

week_jan1_2009 <- energy15[energy15$Date_tz>="2009-01-12" & energy15$Date_tz<="2009-01-18",]

head(week_jan1_2009) #sampling every 15 minutes

#converting DateTime from POSIXlt to POSIXct 
week_jan1_2009$DateTime=as.POSIXct(week_jan1_2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")

#THIS WORKS
ggplot(data=week_jan1_2009, aes(week_jan1_2009$DateTime))+
  geom_line(aes(y = week_jan1_2009$Sub_metering_1, color="Sub metering 1"), size = 0.8) + 
  geom_line(aes(y = week_jan1_2009$Sub_metering_2, color="Sub metering 2"), size = 0.8) + 
  geom_line(aes(y = week_jan1_2009$Sub_metering_3, color="Sub metering 3"), size = 0.8) + 
  geom_line(aes(y = week_jan1_2009$Other, color="Other"), size = 0.8) +
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Winter week (12-18 January, 2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255), 'Other'=rgb(51, 153, 255, maxColorValue = 255)),guide='legend') 




###THIS FOR SUMMER
#AUGUST 2009
week_aug_2009 <- energy15[energy15$Date_tz>="2009-08-17" & energy15$Date_tz<="2009-08-23",]

head(week_aug_2009) #every 15 minutes

#converting DateTime from POSIXlt to POSIXct 
week_aug_2009$DateTime=as.POSIXct(week_aug_2009$DateTime,format="%d/%m/%Y T %H:%M:%S",tz="GMT")


#THIS WORKS
ggplot(data=week_aug_2009, aes(week_aug_2009$DateTime))+
  geom_line(aes(y = week_aug_2009$Sub_metering_1, color="Sub metering 1"), size = 0.8) + 
  geom_line(aes(y = week_aug_2009$Sub_metering_2, color="Sub metering 2"), size = 0.8) + 
  geom_line(aes(y = week_aug_2009$Sub_metering_3, color="Sub metering 3"), size = 0.8) + 
  geom_line(aes(y = week_aug_2009$Other, color="Other"), size = 0.8) + 
  xlab("Day/Time")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("Submeters Active Energy in a Summer week (17-23 August 2009)")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(236, 97, 119, maxColorValue = 255), 'Sub metering 2'=rgb(59,55,80, maxColorValue = 255), 'Sub metering 3'=rgb(117, 165, 138, maxColorValue = 255), 'Other'=rgb(51, 153, 255, maxColorValue = 255)),guide='legend') 



#WINTER

#df filtering the week and the interesting columns
week_jan_2009_df <- energy_by_day_desc %>% 
  dplyr::filter(Date_tz >= "2009-01-12" & Date_tz <= "2009-01-18") %>% 
  dplyr::select(Date_tz, Tot_gap, Tot_SM1, Tot_SM2, Tot_SM3, Tot_Other)
week_jan_2009_df

#reshaping the dataset in order to do the stacked bar
Day_of_week_jan = sort(rep(week_jan_2009_df$Date_tz, 4))
Day_of_week_jan <- paste(Day_of_week_jan, weekdays(Day_of_week_jan))
Day_of_week_jan

energy_consumption_jan=rep(c("SM1" , "SM2" , "SM3", "Other") , 7)
energy_consumption_jan


value_jan=c(0,15,825,979,82,19,1097,1215,164,22,665,1136,172,25,632,943,413,228,1035,1415,198,216,996,2325,339,458,872,1894)
value_jan 

data_jan=data.frame(Day_of_week_jan,energy_consumption_jan,value_jan)



# Grouped
ggplot(data_jan, aes(fill=energy_consumption_jan, y=value_jan, x=Day_of_week_jan)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked (Level -> to make comparison with summer)
ggplot(data_jan, aes(fill=energy_consumption_jan, y=value_jan, x=Day_of_week_jan)) + 
  geom_bar( stat="identity")


## Stacked Percent (Share of SMs and Other)
ggplot(data_jan, aes(fill=energy_consumption_jan, y=value_jan, x=Day_of_week_jan)) + 
  geom_bar( stat="identity", position="fill")



#SUMMER

#df filtering the week and the interesting columns
week_aug_2009_df <- energy_by_day_desc %>% 
  dplyr::filter(Date_tz >= "2009-08-17" & Date_tz <= "2009-08-23") %>% 
  dplyr::select(Date_tz, Tot_gap, Tot_SM1, Tot_SM2, Tot_SM3, Tot_Other)
week_aug_2009_df

#reshaping the dataset in order to do the stacked bar
Day_of_week_aug = sort(rep(week_aug_2009_df$Date_tz, 4))

Day_of_week_aug <- paste(Day_of_week_aug, weekdays(Day_of_week_aug))
Day_of_week_aug

energy_consumption_aug=rep(c("SM1" , "SM2" , "SM3", "Other") , 7)
energy_consumption_aug

week_aug_2009_df

value_aug=c(41,54,349,392,193,100,373,515,65,56,360,458,0,60,390,380,137,60,488,471,369,217,556,734,78,48,457,669)
value_aug

data_aug=data.frame(Day_of_week_aug,energy_consumption_aug,value_aug)

# Grouped
ggplot(data_aug, aes(fill=energy_consumption_aug, y=value_aug, x=Day_of_week_aug)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked (Level -> to make comparison with winter)
ggplot(data_aug, aes(fill=energy_consumption_aug, y=value_aug, x=Day_of_week_aug)) + 
  geom_bar( stat="identity")

# Stacked Percent (Share of SMs and Other)
ggplot(data_aug, aes(fill=energy_consumption_aug, y=value_aug, x=Day_of_week_aug)) + 
  geom_bar( stat="identity", position="fill")


require(gridExtra)

#STACKED winter vs summer
# Stacked WINTER
plot1 <- ggplot(data_jan, aes(fill=energy_consumption_jan, y=value_jan, x=Day_of_week_jan)) +
  xlab("Day")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("12-18 January 2009")+
  geom_bar( stat="identity")
# Stacked SUMMER
plot2 <- ggplot(data_aug, aes(fill=energy_consumption_aug, y=value_aug, x=Day_of_week_aug)) + 
  xlab("Day")+
  ylab("Energy (Watt-Hour)")+
  ggtitle("17-23 August 2009")+
  geom_bar( stat="identity")
plot1
plot2
#to plot them side by side:
grid.arrange(plot1, plot2, nrow=2, top = textGrob("Winter week vs Summer week: Absolute Comparison",gp=gpar(fontsize=20,font=3)))
?grid.arrange


#STACKED PERCENT winter vs summer
# Stacked Percent WINTER
plot3 <- ggplot(data_jan, aes(fill=energy_consumption_jan, y=value_jan, x=Day_of_week_jan)) + 
  xlab("Day")+
  ylab("% of Energy")+
  ggtitle("12-18 January 2009")+
  geom_bar( stat="identity", position="fill")
# Stacked Percent SUMMER
plot4 <- ggplot(data_aug, aes(fill=energy_consumption_aug, y=value_aug, x=Day_of_week_aug)) + 
  xlab("Day")+
  ylab("% of Energy")+
  ggtitle("17-23 August 2009")+
  geom_bar( stat="identity", position="fill")
grid.arrange(plot3, plot4, nrow=2, top = textGrob("Winter week vs Summer week: Relative Comparison",gp=gpar(fontsize=20,font=3)))




# The doughnut function permits to draw a donut plot
doughnut <-
  function (x, labels = names(x), edges = 200, outer.radius = 0.8, 
            inner.radius=0.6, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL, 
            angle = 45, col = NULL, border = FALSE, lty = NULL, 
            main = NULL, ...)
  {
    if (!is.numeric(x) || any(is.na(x) | x < 0))
      stop("'x' values must be positive.")
    if (is.null(labels))
      labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
      col <- if (is.null(density))
        palette()
    else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p), 
           y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                outer.radius)
      polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
              angle = angle[i], border = border[i], 
              col = col[i], lty = lty[i])
      Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
        text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], 
             xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
             ...)
      }
      ## Add white disc          
      Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
      polygon(Pin$x, Pin$y, density = density[i], 
              angle = angle[i], border = border[i], 
              col = "white", lty = lty[i])
    }
    
    title(main = main, ...)
    invisible(NULL)
  }


# Let's use the function, it works like PiePlot !
# inner.radius controls the width of the ring!

tot_by_year <- energy_by_year_desc[,c(1,8:13)]
tot_by_year <- tot_by_year[,-3]
tot_by_year

par(mfrow = c(1, 1))
donut2007 <- doughnut( c(43278,56350,201652,346874) , inner.radius=0.5, col = c("indianred", "gray0", "seagreen" , "steelblue1"), main = "2007" )
donut2008 <- doughnut( c(39622,44177,211708,332354) , inner.radius=0.5, col = c("indianred", "gray0", "seagreen" , "steelblue1"), main = "2008")
donut2009 <- doughnut( c(40386,39307,236858,309376) , inner.radius=0.5, col = c("indianred", "gray0", "seagreen" , "steelblue1"), main = "2009")
donut2010 <- doughnut( c(30769,33856,220627,254061) , inner.radius=0.5, col = c("indianred", "gray0", "seagreen" , "steelblue1"), main = "2010")

