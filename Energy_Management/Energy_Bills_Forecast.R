#FORECASTS WITH LINEAR MODEL for the next 13 months (dec 10-dec11)

#prediction of total energy per month in WattHour (dec 10-dec11)
lm_monthly_future_13$mean

#YEARLY total energy consumption prediction for year 2011 in kWH
sum(lm_monthly_future_13$mean[2:13]/1000)
#9008.025 kWH

#TO DO: YEARLY power consumption from 2007 to 2010 in kWH




#electricity price in France: 14.72 euro cents per kWh
#With the average household in France consuming about 4670 kWh electricity (in French) in 2015, 
#the average annual electricity cost in France ranged from 773.95€ - 731.38€, 
#depending on the electricity provider and plan chosen. This works out to an average monthly power bill of about 
#66.50€ - 60€ per month.


#prediction of future electricity bill amounts (in €) (dec 10-dec11)
bills_euro_13 <- round((lm_monthly_future_13$mean/1000)*0.1472)
bills_euro_13

sum(bills_euro_13[2:13])

#joining the past (real) and the future (estimated) total gap (WattHour)
#N.B. original monthly_seasonal series considered (with the outlier in august 08)
comb_totgap_07_11 <- ts.union(monthly_seasonal, lm_monthly_future_13$mean)
comb_totgap_07_11 <- pmin(comb_totgap_07_11[,1], comb_totgap_07_11[,2], na.rm = TRUE)
comb_totgap_07_11

#electricity bill amounts for past and future (07 - 11)
bills_euro_07_11 <- round((comb_totgap_07_11/1000)*0.1472)
autoplot(bills_euro_07_11)


#info about the household
#The data comes from a house in Sceaux (92330) which is 10 kilometres south of Paris.
#The house uses gas-based heating system, has three floors and seven rooms and
#is inhabited by a family of four or five: two parents working full time and two or three children.


# Library
library(tidyverse)

#plotting the future bills 
bills_euro_13

tstodf <- data.frame(x=as.yearmon(time(bills_euro_13)), y=melt(bills_euro_13)$value)

class(as.yearmon(time(bills_euro_13)))
tstodf$x

month(tstodf$x)

ggplot(tstodf, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=5, color="darkgreen", fill=alpha("lightgreen", 0.3), alpha=0.7, shape=21, stroke=2) +
  xlab("Month")+
  ylab("Euro") +
  ggtitle("Future Energy Monthly Bills (Estimation)")


ggplot(tstodf, aes(x=x, y=y)) +
  geom_segment(aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")
  
  
#USE THIS (with names of the months on the x axis)

require(zoo)
  
ggplot(tstodf, aes(x=x, y=y)) +
    scale_x_yearmon(format="%b %Y", n=6)+
    geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
    geom_point( size=5, color="darkgreen", fill=alpha("lightgreen", 0.3), alpha=0.7, shape=21, stroke=2) +
    xlab("Month")+
    ylab("€ (Euro)") +
    ggtitle("Future Energy Monthly Bills (Estimation)")
  

  
