getwd()
setwd("C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/4. DEEP ANALYTICS AND VISUALIZATION/TASK3_Evaluate Techniques for WiFi Locationing")  

rm(list=ls())

library(tidyr)
library(data.table)
library(plyr)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(caTools)
library(gridExtra)
library(ranger)
library(e1071)
library(scatterplot3d)
library(FactoMineR)
library(kknn)
library(randomForest)
library(e1071)
library(pROC)
library(multiROC)
library(ROCR)
library(data.table)
library(dplyr)
library(formattable)
library(cowplot)



#importing the training and the validation sets (csv file)
wifi_train <- read.csv("trainingData.csv",header = TRUE, sep=",")
wifi_valid <- read.csv("validationData.csv",header = TRUE, sep=",")

#dim(wifi_train)
#dim(wifi_valid)
#summary(wifi_train)
#str(wifi_train)


#creating dataset with only waps (columns 1:520)
onlywaps <- wifi_train[,1:520]
#creating column sum of waps
onlywaps$sum_waps <- apply(onlywaps, 1, sum)

#indexes of rows with all waps = 100 (they are 76 rows)
nosignalrows <- which(onlywaps$sum_waps == 52000)
#76 rows in the original training set
wifi_train_nosignal <- wifi_train[nosignalrows,]

#removing these rows from the training -> I have now 19861 rows
wifi_train <- wifi_train[-nosignalrows,]

#check that these point are spread quite everywhere and not in a specific area
wifi_train_nosignal$BUILDINGID <- as.factor(wifi_train_nosignal$BUILDINGID)
wifi_train_nosignal$FLOOR <- as.factor(wifi_train_nosignal$FLOOR)
plot(wifi_train_nosignal$BUILDINGID, wifi_train_nosignal$FLOOR)
hist(wifi_train_nosignal$BUILDINGID)
hist(wifi_train_nosignal$FLOOR)
plot(wifi_train_nosignal$LONGITUDE, wifi_train_nosignal$LATITUDE)
plot(wifi_train$LONGITUDE, wifi_train$LATITUDE)


#finding and deleting all the columns with variance == 0:

#training set
apply(wifi_train, 2, var)
traincol_var0 <- which(apply(wifi_train, 2, var) == 0)
#traincol_var0 
length(traincol_var0)#55 columns in the training with zero variance, all of them WAPs


#validation set
validcol_var0 <- which(apply(wifi_valid, 2, var) == 0)
#N.B. in the validation the user id is always equal to zero
#validcol_var0[156]
#unique(wifi_valid$USERID)
length(validcol_var0[-156])

list <- append(traincol_var0, validcol_var0[-156])
length(list) #210
#length(validcol_var0)#156 columns in the validation with zero variance, most of them WAPs and also SPACEID, RELATIVE POSITION AND USERID
#length(validcol_var0[-156]) #155 columns

wifi_train <- wifi_train[, - list]
wifi_valid <- wifi_valid[, -list]


#plotting all the floors and all the buildings of the training

#for (i in c(0,1,2,3,4)){
 # g = ggplot( subset(wifi_train, FLOOR == i),aes(x=LONGITUDE, y =LATITUDE))+geom_point()+ggtitle(paste('Floor',i)) 
  #print(g)
#}

#coercing floor and building in training and test to factor
wifi_train$FLOOR <- as.factor(wifi_train$FLOOR)
wifi_valid$FLOOR <- as.factor(wifi_valid$FLOOR)
#levels(wifi_train$FLOOR)
#levels(wifi_valid$FLOOR)
wifi_train$BUILDINGID <- as.factor(wifi_train$BUILDINGID)
wifi_valid$BUILDINGID <- as.factor(wifi_valid$BUILDINGID)
#levels(wifi_train$BUILDINGID)
#levels(wifi_valid$BUILDINGID)



#renaming the levels of building and floor in train and valid
levels(wifi_train$BUILDINGID) <- c("B.one", "B.two", "B.three")
levels(wifi_valid$BUILDINGID) <- c("B.one", "B.two", "B.three")
levels(wifi_train$FLOOR) <- c("F.zero", "F.one", "F.two", "F.three", "F.four")
levels(wifi_valid$FLOOR) <- c("F.zero", "F.one", "F.two", "F.three", "F.four")

#concatenating BUILDING and FLOOR in training and validation
wifi_train$BF <- paste(wifi_train$BUILDINGID, wifi_train$FLOOR, sep = " - ")
wifi_train$BF <- as.factor(wifi_train$BF)
wifi_valid$BF <- paste(wifi_valid$BUILDINGID, wifi_valid$FLOOR, sep = " - ")
wifi_valid$BF <- as.factor(wifi_valid$BF)


require(gridExtra) #side by side plots with ggplot

plotzero <- ggplot( subset(wifi_train, FLOOR == "F.zero"),aes(x=LONGITUDE, y =LATITUDE))+geom_point()+ggtitle(paste("FLOOR ZERO")) 
plotone <- ggplot( subset(wifi_train, FLOOR == "F.one"),aes(x=LONGITUDE, y =LATITUDE))+geom_point()+ggtitle(paste("FLOOR ONE")) 
plottwo <- ggplot( subset(wifi_train, FLOOR == "F.two"),aes(x=LONGITUDE, y =LATITUDE))+geom_point()+ggtitle(paste("FLOOR TWO")) 
plotthree <- ggplot( subset(wifi_train, FLOOR == "F.three"),aes(x=LONGITUDE, y =LATITUDE))+geom_point()+ggtitle(paste("FLOOR THREE")) 
plotfour <- ggplot( subset(wifi_train, FLOOR == "F.four"),aes(x=LONGITUDE, y =LATITUDE))+geom_point()+ggtitle(paste("FLOOR FOUR")) 
grid.arrange(plotzero, plotone, plottwo, plotthree, plotfour, ncol=2)

#misclassification errors of floor 1 of validation set (TO RUN AFTER MODELING!!!)
ggplot(subset(rows_misclassified_floor_valid, FLOOR == "F.one"), aes(x=LONGITUDE, y =LATITUDE))+geom_point()
grid.arrange(plotone, ggplot(subset(rows_misclassified_floor_valid, FLOOR == "F.one"), aes(x=LONGITUDE, y =LATITUDE))+geom_point())

#plot(wifi_train$BUILDINGID,wifi_train$FLOOR) 

#building one has 4 floors
#building two has 4 floors
#building three has 5 floors

#now training and validation have both 320 columns

#replace values = 100 with -105 in the training
waps <- wifi_train[,1:312]
lastcolumns <- wifi_train[,313:320]
waps[waps == 100 ] <- -105
wifi_train <- cbind(waps, lastcolumns)


#replace values = 100 with -105 in the validation
waps_v <- wifi_valid[,1:312]
lastcolumns_v <- wifi_valid[,313:320]
waps_v[waps_v == 100 ] <- -105
wifi_valid <- cbind(waps_v, lastcolumns_v)

#removing duplicates
wifi_train <- unique(wifi_train) #now the training has 19224 rows

#find the values between -30 to 0 and check if they belong to the same phone id and user id
#wifi_train$PHONEID <- as.integer(wifi_train$PHONEID)
#by_phone <- wifi_train %>% group_by(PHONEID)
#by_phone %>% summarise(
 # maxwap = max(wifi_train[,1:312])
#)


#all the phoneID have at least one zero in the values of the WAPs
#unique(wifi_train$PHONEID)
#unique(wifi_valid$PHONEID)
#unique(wifi_train$USERID)
#unique(wifi_valid$USERID)


#indexes of waps which have a maximum higher than -30
waps_too_strong <- which(apply(wifi_train[,1:312], 2, max) > -30)
#max(wifi_train$WAP011)
length(which(apply(wifi_train[,1:312], 2, max) > -30)) #50 WAPs have a maximum > -30

Problem_rows <- wifi_train[which(apply(wifi_train[,1:312],1, max)>-30),]
Problem_cols <- wifi_train[,which(apply(wifi_train[,1:312],2, max)>-30)]

Problem_rows$PHONEID <- as.factor(Problem_rows$PHONEID)
Problem_rows$USERID <- as.factor(Problem_rows$USERID)

Problem_by_phone_user <- Problem_rows %>% 
  group_by(USERID, PHONEID) %>% 
  summarise(n = n())

Problem_by_phone_user %>% arrange(desc(n))


#problems with user ID 6, phone ID 19 -> count =  397
phone_19 <- wifi_train%>%filter(PHONEID==19)
#phone id has 980 obs in total, 397 of them with problems.... not reliable phone.

#glimpse(wifi_train[,300:320])

#3D image of reference point locations in data set

scatterplot3d(wifi_train$LONGITUDE, wifi_train$LATITUDE, wifi_train$FLOOR,
              type='p',
              highlight.3d = FALSE,
              color='lightpink4',
              angle=155,
              pch=16,
              box=FALSE,
              main = "Locations Across Three Buildings",
              cex.lab = 1,
              cex.main=1,
              cex.sub=1,
              col.sub='blue',
              xlab='Longitude', ylab='Latitude',zlab = 'Floor')

#EXPLORATORY ANALYSIS ON ---USERS--- FOR EVERY BUILDING AND EVERY FLOOR

wifi_train$USERID <- as.factor(wifi_train$USERID) 
wifi_train$PHONEID <- as.factor(wifi_train$PHONEID) 


#NUMBER OF CAPTURES MADE BY EVERY USER AND PHONE in TRAINING
u <- wifi_train %>% 
  group_by(USERID, PHONEID) %>% 
  summarise(n = n())
u %>% arrange(desc(n))

formattable(u %>% arrange(desc(n)), align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `n` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))))

#NUMBER OF CAPTURES MADE BY EVERY USER AND PHONE in VALIDATION
v <- wifi_valid %>% 
  group_by(USERID, PHONEID) %>% 
  summarise(n = n())
v %>% arrange(desc(n))

formattable(v %>% arrange(desc(n)), align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `n` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))))

#Building 1 : Users in floors
b1 <- wifi_train%>%filter(BUILDINGID=="B.one")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 1 : USER ID in floors")
#building 1 is bad representated by users (only USERID 1 and USERID 11)


#Building 2: Users in floors
b2 <- wifi_train%>%filter(BUILDINGID=="B.two")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 2 : USER ID in floors")
#building 2 is well representated by users


#Building 3: Users in floors
b3 <- wifi_train%>%filter(BUILDINGID=="B.three")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 3 : USER ID in floors")
#building 3 is well representated by users

grid.arrange(b1, b2, b3, ncol=3)


#EXPLORATORY ANALYSIS ON ---PHONES--- FOR EVERY BUILDING AND EVERY FLOOR

#TRAINING
#Building 1: PHONE ID in floors
b11 <- wifi_train%>%
  filter(BUILDINGID=="B.one")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 1 : PHONEID in floors") 
#building 1 bad represented by Phone ID (only 13 and 14)

#Building 2: PHONE ID in floors
b22 <- wifi_train%>%
  filter(BUILDINGID=="B.two")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 2 : PHONEID in floors") 
#building 2 well represented by PHONE ID


#Building 3: PHONE ID in floors
b33 <- wifi_train%>%
  filter(BUILDINGID=="B.three")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 3 : PHONEID in floors") 
#building 3 well represented by PHONE ID


grid.arrange(b11, b22, b33, ncol=3)


#VALIDATION
#Building 1: PHONE ID in floors
b11v <- wifi_valid%>%
  filter(BUILDINGID=="B.one")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 1 : PHONEID in floors") 


#Building 2: PHONE ID in floors
b22v <- wifi_valid%>%
  filter(BUILDINGID=="B.two")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 2 : PHONEID in floors") 
#building 2 well represented by PHONE ID


#Building 3: PHONE ID in floors
b33v <- wifi_valid%>%
  filter(BUILDINGID=="B.three")%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title="Building 3 : PHONEID in floors") 
#building 3 well represented by PHONE ID


grid.arrange(b11v, b22v, b33v, ncol=3)



#ORIGINAL DS TRAIN
original_train <- read.csv("trainingData.csv",header = TRUE, sep=",")

#ORIGINAL DS VALID
original_valid <- read.csv("trainingData.csv",header = TRUE, sep=",")

#ANOMALY: the same device, hold by the same user, in the same place, detected different signal strengths for the same WAPs.
different_RSSI_sameWAP_samePLACE <- original_train[c(628,846),]


#NUMBER OF LOCATIONS BY USER

#TRAINING
plot(wifi_train$USERID,
xlab="USER NUMBER", ylab="frequency",
main="Number of locations by User - Training",
col="darkgreen")
#the majority of captures were made by USER 11 (to which corresponds PHONE 13)

#VALIDATION
wifi_valid$USERID <- as.factor(wifi_valid$USERID)

plot(wifi_valid$USERID,
     xlab="USER NUMBER", ylab="frequency",
     main="Number of locations by User - Validation",
     col="darkgreen")


#NUMBER OF LOCATIONS BY PHONE

#TRAINING
plot(wifi_train$PHONEID,
     xlab="PHONE NUMBER", ylab="frequency",
     main="Number of locations by Phone",
     col="lightgreen")
#the majority of captures were made from PHONE 14 (which belongs to USERS 1, 9, 16) and 13 (which belongs to USER 11)

wifi_train %>% 
  group_by(USERID, PHONEID) %>% 
  summarise(n = n())


#VALIDATION
wifi_valid$PHONEID <- as.factor(wifi_valid$PHONEID)

plot(wifi_valid$PHONEID,
     xlab="PHONE NUMBER", ylab="frequency",
     main="Number of locations by Phone - Validation",
     col="lightgreen")



#NUMBER OF LOCATIONS IN TRAINING AND VALIDATION
#TRAIN
loc_train <- wifi_train%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
loc_train
#933 different locations in the training 

#Same locations in Training Data have above 20 rows (captures). 
#The user has recorded his position several times on the same location. 
#lack of representativity in the training set

#VALID
loc_valid <- wifi_valid%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
loc_valid
#1074 different location in the validation

#we have just one row by location in the Validation Data. 
#Users were moving randomly. 
#better representativity in the validation set.


#NUMBER OF LOCATION BY BUILDING AND FLOOR

#TRAINING

#building 1

unique(wifi_train$FLOOR)

loc_train_B1 <- wifi_train%>%
  filter(BUILDINGID == "B.three" & FLOOR == "F.four")%>%
  group_by(LONGITUDE, LATITUDE)%>%
  summarize(count=n())  
loc_train_B1



#TRAIN
#building1
wifi_train%>%
  filter(BUILDINGID == "B.one")%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
#259 locations in building1

#building2
wifi_train%>%
  filter(BUILDINGID == "B.two")%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
#265 locations in building 2 

#building3
wifi_train%>%
  filter(BUILDINGID == "B.three")%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
#409 locations in building 3



#VALIDATION
#building1
wifi_valid%>%
  filter(BUILDINGID == "B.one")%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
#514 locations in building 1

#building2
wifi_valid%>%
  filter(BUILDINGID == "B.two")%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
#301 locations in building 2

#building3
wifi_valid%>%
  filter(BUILDINGID == "B.three")%>%
  group_by(LONGITUDE, LATITUDE, FLOOR)%>%
  summarize(count=n())  
#259 locations in building 3

#number of captures for every floor in every building

wifi_train%>%
  filter(BUILDINGID == "B.one")%>%
  group_by(FLOOR) %>%
  summarize(count=n()) 


wifi_train%>%
  filter(BUILDINGID == "B.two")%>%
  group_by(FLOOR) %>%
  summarize(count=n())


wifi_train%>%
  filter(BUILDINGID == "B.three")%>%
  group_by(FLOOR) %>%
  summarize(count=n())


#FROM FLOOR ERRORS:
#most frequent errors: in building 2, actual floor one misclassified for floor two (34 times) -> maybe B2F1 has weaker signals than B2F2!
#in building 1, actual floor one misclassified for floor zero (24 times)

#focus on B2F1 to see the intensity of signals
B2F1_train <- wifi_train%>%
  filter(BUILDINGID == "B.two" & FLOOR == "F.one")

all_waps_B2F1_train <- as.vector(B2F1_train[,1:312])
all_waps_B2F1_train <- data.matrix(all_waps_B2F1_train)
hist(all_waps_B2F1_train, breaks = 60)


trial <- all_waps_B2F1_train[all_waps_B2F1_train[,] != -105]
trial
hist(trial)

dd <- density(trial)
plot(dd, main="Density of WAPs in B2F1 - Training")
polygon(dd, col="lightgreen", border="black")

#focus on B1F1 to see the intensity of signals
B1F1_train <- wifi_train%>%
  filter(BUILDINGID == "B.one" & FLOOR == "F.one")

all_waps_B1F1_train <- as.vector(B1F1_train[,1:312])
all_waps_B1F1_train <- data.matrix(all_waps_B1F1_train)
hist(all_waps_B1F1_train, breaks = 60)

trial1 <- all_waps_B1F1_train[all_waps_B1F1_train[,] != -105]
trial1
hist(trial1)

dd1 <- density(trial1)
plot(dd1, main="Density of WAPs in B1F1 - Training")
polygon(dd1, col="lightgreen", border="black")



#focus on B2F2 to see the intensity of signals
B2F2_train <- wifi_train%>%
  filter(BUILDINGID == "B.two" & FLOOR == "F.two")

all_waps_B2F2_train <- as.vector(B2F2_train[,1:312])
all_waps_B2F2_train <- data.matrix(all_waps_B2F2_train)
hist(all_waps_B2F2_train, breaks = 60)


trial2 <- all_waps_B2F2_train[all_waps_B2F2_train[,] != -105]
trial2
hist(trial2)

dd2 <- density(trial2)
plot(dd2, main="Density of WAPs in B2F2 - Training")
polygon(dd2, col="lightgreen", border="black")



#B1F1
B1F1_train <- wifi_train%>%
  filter(BUILDINGID == "B.one" & FLOOR == "F.one")

all_waps_B1F1_train <- as.vector(B1F1_train[,1:312])
all_waps_B1F1_train <- data.matrix(all_waps_B1F1_train)
hist(all_waps_B1F1_train, breaks = 60)

trial3 <- all_waps_B1F1_train[all_waps_B1F1_train[,] != -105]
trial3
hist(trial3)

dd3 <- density(trial3)
plot(dd3, main="Density of WAPs in B1F1 - Training")
polygon(dd3, col="lightgreen", border="black")

#B1F0
B1F0_train <- wifi_train%>%
  filter(BUILDINGID == "B.one" & FLOOR == "F.zero")

all_waps_B1F0_train <- as.vector(B1F0_train[,1:312])
all_waps_B1F0_train <- data.matrix(all_waps_B1F0_train)
hist(all_waps_B1F0_train, breaks = 60)

trial4 <- all_waps_B1F0_train[all_waps_B1F0_train[,] != -105]
trial4
hist(trial4)

dd4 <- density(trial4)
plot(dd4, main="Density of WAPs in B1F0 - Training")
polygon(dd4, col="lightgreen", border="black")





#LOCATIONS

sort(loc_train$count, decreasing = T)
sort(loc_valid$count, decreasing = T)

#The instance count for locations ranges from  2 to 80, being 20 the mode. 


#visualizing it:
#TRAINING
ggplot(loc_train, aes(x = count)) +
  geom_histogram(fill='seagreen', binwidth = 2, color='black')+
  scale_x_continuous(breaks=seq(0,100,10)) +
  ggtitle('Frequency of Count of Locations - Training') +
  xlab('Count of Locations') +
  ylab('Frequency') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA))

#VALIDATION
ggplot(loc_valid, aes(x = count)) +
  geom_histogram(fill='seagreen', binwidth = 1, color='black')+
  scale_x_continuous(breaks=seq(0,10,1)) +
  ggtitle('Frequency of Count of Locations - Validation') +
  xlab('Count of Location') +
  ylab('Frequency') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA))

#Number of WAPs detected across the 3 buildings:

#-Add count of WAP's detected as feature
wifi_train_copy <- wifi_train
wifi_valid_copy <- wifi_valid
wifi_train_copy$WAP_num <- apply(wifi_train_copy[,1:312], 1, function (x) length(x[x != -105]))


#-Distribution of WAP count by building- boxplot
ggplot(wifi_train_copy, aes(x=BUILDINGID, y=WAP_num)) + 
  geom_boxplot(fill='lightgreen') +
  theme(text = element_text(size=14)) +
  ggtitle('Distribution of Number of Detected WAPs by Building - Training') +
  labs(x="Building", y= 'Number of WAPs' ) +
  theme(panel.border=element_rect(colour='black', fill=NA))

#Building 3 has the highest median detected WAPs whereas Building 1 and 2 have similar medians.

#-Distribution of WAP count by BUILDING AND FLOOR
ggplot(wifi_train_copy, aes(x=BF, y=WAP_num)) + 
  geom_boxplot(fill='lightgreen') +
  theme(text = element_text(size=14)) +
  ggtitle('Distribution of Number of Detected WAPs by Building and Floor- Training') +
  labs(x="Building", y= 'Number of WAPs' ) +
  theme(panel.border=element_rect(colour='black', fill=NA))



#Feature Reduction (with PCA)

wifi_train_copy$WAP_num <- NULL

normalize <- function(x) { return( (x +105) / (0 + 105) ) }


pmatrix1 <- as.data.frame(apply(wifi_train_copy[,1:312],
                                2,
                                normalize))

pmatrix2 <- as.data.frame(apply(wifi_valid_copy[,1:312],
                                2, normalize))


#applying the PCA to the numerical variables of wifi_train_copy (WAPs)
princ <- stats::prcomp(pmatrix1, center = F, scale = F)
princ

screeplot(princ,npcs = 80)
plot(princ, xlab = "var")

plot(princ, type = "l")

variance <- princ$sdev^2/sum(princ$sdev^2)*100
plot(variance, type = "line", col = "red")
summary(princ)

plot(cumsum(variance), xlab = "Principal Component",
     main = "Cumulative Proportion of Variance Explained",
     type = "b")
abline(v=29, col="seagreen", lwd=3)

#the first 29 PCs accounts for more than 80% of the variance of the data. 
#It means that if you project all your data, you will reconstruct the points with 80% of accuracy.

pmatrix1 <- as.matrix(pmatrix1)
pmatrix2 <- as.matrix(pmatrix2)

rotation <- princ$rotation
new_train <- pmatrix1 %*% rotation
new_valid <- pmatrix2 %*% rotation

all.equal(new_train, princ$x)

#we need the first 29 PCs to explain 80% of our data
comp <- 29
new_trainingSet <-as.data.frame(cbind(new_train[,1:comp],wifi_train_copy[,313:320]))
#37 variables in new_trainingSet
new_validationSet <- as.data.frame(cbind(new_valid[,1:comp], wifi_valid_copy[,313:320]))
#37 variables in new_validationSet

#Saving the new data so I can use later without running all the script.

write.csv(new_trainingSet, "new_training.csv",
          row.names = FALSE)
write.csv(new_validationSet, "new_validation.csv",
          row.names = FALSE)

#check that all the columns in TRAINING and VALIDATION are equal 
all.equal(colnames(new_trainingSet), colnames(new_validationSet))

#reading the files
training <- read.csv("new_training.csv",
                     sep=",", as.is = TRUE)
validation <- read.csv("new_validation.csv",
                       sep=",", as.is = TRUE)


#removing the columns that we don't need for predicting

training$USERID <- NULL
training$PHONEID <- NULL
training$TIMESTAMP <- NULL
training$BF <- NULL

validation$USERID <- NULL
validation$PHONEID <- NULL
validation$TIMESTAMP <- NULL
validation$BF <- NULL
#so now we have 33 variables in the training and validation. 

training$BUILDINGID <- as.factor(training$BUILDINGID)
validation$BUILDINGID <- as.factor(validation$BUILDINGID)

training$FLOOR <- as.factor(training$FLOOR)
validation$FLOOR <- as.factor(validation$FLOOR)

training_copy <- training
validation_copy <- validation



###----
#PCA DATASET

#predicting BUILDING on dataset training (WITH PCA)

#deleting unuseful columns

training$LONGITUDE <- NULL
training$LATITUDE <- NULL
training$FLOOR <- NULL

validation$LONGITUDE <- NULL
validation$LATITUDE <- NULL
validation$FLOOR <- NULL


#Control for Training
#Create Control Cross Validation

set.seed(333)
ctrl <- trainControl(method = "repeatedcv", number = 10)


#KNN to predict the building (with PCA)
set.seed(333)
KK_BU_ACP <- train.kknn(BUILDINGID~.,
                    data = training,trControl = ctrl,
                    kmax = 9)

KK_BU_predic_ACP <- predict(KK_BU_ACP, validation)

confusionMatrix(KK_BU_predic_ACP, validation$BUILDINGID)
#Accuracy and Kappa = 1


#RF to predict the building (with PCA) (BEST MODEL)
set.seed(333)
RF_BU_ACP <- randomForest(BUILDINGID~.,
                      data = training,trControl = ctrl,
                      ntree= 500)

#confusion matrix on the training
RF_BU_ACP$confusion
#fitted on the training
length(RF_BU_ACP$predicted)  #19224
class(RF_BU_ACP$predicted) #factor


RF_BU_predic_ACP <- predict(RF_BU_ACP, validation)
#predictions on the validation
length(RF_BU_predic_ACP) #1111
class(RF_BU_predic_ACP) #factor

#confusion matrix on the validation
confusionMatrix(RF_BU_predic_ACP, validation$BUILDINGID)
#Accuracy and Kappa = 1


#AUC and ROC of the RF predicting BUILDING (with PCA)

roc_rf_BU_ACP <- multiclass.roc(validation_copy$BUILDINGID, as.numeric(RF_BU_predic_ACP), levels=levels(validation_copy$BUILDINGID), percent = T)
roc_rf_BU_ACP
#Multi-class area under the curve: 100%

rs_BU_ACP <- roc_rf_BU_ACP[['rocs']]
plot.roc(rs_BU_ACP[[1]])

rocobj_RF_BU <- plot.roc(rs_BU_ACP[[1]], col = "seagreen", lwd = 2, identity.col="black", identity.lwd=1, auc.polygon = T, legacy.axes = T, print.auc = T, main = "ROC Curve of Random Forest (BUILDING)")
rocobj_RF_BU$auc #100%


#SVM to predict the building (with PCA)
set.seed(333)
SVM_BU_ACP <- svm(BUILDINGID ~ ., data = training, kernel = "radial",
              trControl = ctrl)



#fitted on the training
length(SVM_BU_ACP$fitted)

SVM_BU_predic_ACP <- predict(SVM_BU_ACP, validation)

#confusion matrix on the validation
confusionMatrix(SVM_BU_predic_ACP, validation$BUILDINGID)
#accuracy = 0.999, kappa = 0.998 (1 misclassification)




#PREDICTING FLOOR (with the predictions of BUILDING) (with PCA)


#adding the column floor to training
training$FLOOR <- training_copy$FLOOR

#adding the column Predicted Building ID in the training (from the random forest) and deleting the BUILDING ID
training$P.BUILDINGID <- RF_BU_ACP$predicted
training$BUILDINGID <- NULL

#adding the column FLOOR to validation
validation$FLOOR <- validation_copy$FLOOR

#adding the column Predicted Building ID in the validation (from the random forest) and deleting the BUILDING ID
validation$P.BUILDINGID <- RF_BU_predic_ACP
validation$BUILDINGID <- NULL


#KNN to predict the FLOOR (with ACP)
set.seed(333)
KK_FL_ACP <- train.kknn(FLOOR~.,
                    data = training,trControl = ctrl,
                    kmax = 9)

KK_FL_predic_ACP <- predict(KK_FL_ACP, validation)

confusionMatrix(KK_FL_predic_ACP, validation$FLOOR)
#Accuracy : 0.8677, Kappa : 0.8166


#AUC and ROC of the KK predicting BUILDING (with ACP)
roc_kk_FL_ACP <- multiclass.roc(validation_copy$FLOOR, as.numeric(KK_FL_predic_ACP), levels=levels(validation_copy$FLOOR), percent = T)
#Multi-class area under the curve: 89.86%
#A multiclass AUC is a mean of AUC and cannot be plotted.

rs_FL_kk_ACP <- roc_kk_FL_ACP[['rocs']]
rocobj_kk_FL <- plot.roc(rs_FL_kk_ACP[[1]])


#Random Forest to predict the FLOOR (with ACP) (BEST MODEL)
set.seed(333)
RF_FL_ACP <- randomForest(FLOOR~.,
                      data = training,trControl = ctrl,
                      ntree= 500)

#confusion matrix on the training
RF_FL_ACP$confusion
confusionMatrix(RF_FL_ACP$predicted, training$FLOOR)
#fitted on the training
length(RF_FL_ACP$predicted)  #19224
class(RF_FL_ACP$predicted) #factor


RF_FL_predic_ACP <- predict(RF_FL_ACP, validation)
#predictions on the validation
length(RF_FL_predic_ACP) #1111
class(RF_FL_predic_ACP) #factor

#confusion matrix on the validation
confusionMatrix(RF_FL_predic_ACP, validation$FLOOR)
#Accuracy : 0.8713; Kappa : 0.821

RF_FL_pred_actual_valid <- data.frame(cbind(RF_FL_predic_ACP,validation$FLOOR))
class(RF_FL_pred_actual_valid)

colnames(RF_FL_pred_actual_valid) = c("RF_FL_pred","FL_actual")

RF_FL_pred_actual_valid %>%
  group_by(RF_FL_pred, FL_actual) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


index_well_classified_FL <- which(RF_FL_pred_actual_valid$RF_FL_pred == RF_FL_pred_actual_valid$FL_actual)
index_well_classified_FL #968 rows out of 1111
index_misclassified_floor <- which(RF_FL_pred_actual_valid$RF_FL_pred != RF_FL_pred_actual_valid$FL_actual) #143 rows

rows_wellclassified_floor_valid <- wifi_valid[index_well_classified_FL,] #968 records. taking all the columns from the validation
rows_misclassified_floor_valid <- wifi_valid[index_misclassified_floor,] #143 records. taking all the columns from the validation

errors_floor <- RF_FL_pred_actual_valid[which(RF_FL_pred_actual_valid$RF_FL_pred != RF_FL_pred_actual_valid$FL_actual),]
errors_floor <- cbind(rows_misclassified_floor_valid, errors_floor)

errors_floor %>%
  group_by(RF_FL_pred, FL_actual, BUILDINGID) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#LEGEND:
#1 -> Floor four
#2 -> Floor one
#3 -> Floor three
#4 -> Floor two
#5-> Floor zero

#most frequent errors: in building 2, actual floor one misclassified for floor two (34 times)
#in building 1, actual floor one misclassified for floor zero (24 times)

right_floor <- RF_FL_pred_actual_valid[which(RF_FL_pred_actual_valid$RF_FL_pred == RF_FL_pred_actual_valid$FL_actual),]
right_floor <- cbind(rows_wellclassified_floor_valid, right_floor)

right_floor %>%
  group_by(RF_FL_pred, FL_actual, BUILDINGID) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#LEGEND:
#1 -> Floor four
#2 -> Floor one
#3 -> Floor three
#4 -> Floor two
#5-> Floor zero

#building one, floor one is the best guessed (183 times), followed by building 1, floor two. 


#most important error: real F4 (1), predicted F0 (5). 
index_REAL4_PRED0 <- which(RF_FL_pred_actual_valid$RF_FL_pred == 5 & RF_FL_pred_actual_valid$FL_actual == 1)
#index 882

row_REAL4_PRED0 <- wifi_valid[index_REAL4_PRED0,]
row_REAL4_PRED0_W <- row_REAL4_PRED0[, 1:312]

#row_REAL4_PRED0_waps <- wifi_valid[index_REAL4_PRED0,1:312]
as.numeric(row_REAL4_PRED0_W[row_REAL4_PRED0_W [,]!= -105])
#length(as.numeric(row_REAL4_PRED0_W[row_REAL4_PRED0_W [,]!= -105])) #31 WAPS detected
hist(as.numeric(row_REAL4_PRED0_W[row_REAL4_PRED0_W [,]!= -105]), breaks = 80) #waps signals in the validation for that OBS
##LONGITUDE == -7381.326 & LATITUDE == 4864845


#same area in the training (B3F4 REAL)
B3F4_LO_LA <- wifi_train %>% 
 filter(BUILDINGID == "B.three" & FLOOR == "F.four" & (LONGITUDE >= -7391 & LONGITUDE <= -7371) & (LATITUDE >= 4864800 & LATITUDE <= 4864853)) 
#122 obs corresponding

B3F4_LO_LA_W <- as.vector(B3F4_LO_LA[, 1:312])
B3F4_LO_LA_W <- data.matrix(B3F4_LO_LA_W)

B3F4_LO_LA_W[B3F4_LO_LA_W[,] != -105]
hist(B3F4_LO_LA_W[B3F4_LO_LA_W[,] != -105], breaks = 100)


#same area in the training (B3F0 PREDICTED)
B3F0_LO_LA <- wifi_train %>% 
  filter(BUILDINGID == "B.three" & FLOOR == "F.zero" & (LONGITUDE >= -7391 & LONGITUDE <= -7371) & (LATITUDE >= 4864800 & LATITUDE <= 4864853)) 
#268 obs corresponding

B3F0_LO_LA_W <- as.vector(B3F0_LO_LA[, 1:312])
B3F0_LO_LA_W <- data.matrix(B3F0_LO_LA_W)

B3F0_LO_LA_W[B3F0_LO_LA_W[,] != -105]
hist(B3F0_LO_LA_W[B3F0_LO_LA_W[,] != -105], breaks = 60)


par(mfrow=c(1,3))


hist(as.numeric(row_REAL4_PRED0_W[row_REAL4_PRED0_W [,]!= -105]), breaks = 80)
hist(B3F4_LO_LA_W[B3F4_LO_LA_W[,] != -105], breaks = 100)
hist(B3F0_LO_LA_W[B3F0_LO_LA_W[,] != -105], breaks = 60)

par(mfrow = c(1,1))


#DENSITY of WAPs in misclassified floor - Validation
all_waps_misc_FL_valid <- as.vector(rows_misclassified_floor_valid[,1:312])
all_waps_misc_FL_valid <- data.matrix(all_waps_misc_FL_valid)

c <- density(all_waps_misc_FL_valid)
plot(c, main="Density of WAPs in errors on Floor - Validation")
polygon(c, col="lightgreen", border="black")

#DENSITY of WAPs in wellclassified floor - Validation
all_waps_wellc_FL_valid <- as.vector(rows_wellclassified_floor_valid[,1:312])
all_waps_wellc_FL_valid <- data.matrix(all_waps_wellc_FL_valid)

b <- density(all_waps_wellc_FL_valid)
plot(b, main="Density of WAPs in well classified instances - Validation")
polygon(b, col="lightgreen", border="black")


rows_misclassified_floor_valid$WAP_num <- apply(rows_misclassified_floor_valid[,1:312], 1, function (x) length(x[x != -105]))
hist(rows_misclassified_floor_valid$WAP_num, breaks = 35, main = "Number of WAPs in Misclassified Floor")
rows_wellclassified_floor_valid$WAP_num <- apply(rows_wellclassified_floor_valid[,1:312], 1, function (x) length(x[x != -105]))
hist(rows_wellclassified_floor_valid$WAP_num, breaks = 30, main = "Number of WAPs in well classified Floor")


plot(rows_misclassified_floor_valid$PHONEID, main="Frequency of PHONE in misclassified instances of Floor - Validation", col = "lightgreen", ylim = c(0, 35)) #phone 0 (Celkon A27, ver 4.0.4(6577)), phone 13 (HTC Wildfire S, ver 2.3.5)'s fault
plot(rows_misclassified_floor1_valid$PHONEID, main = "Frequency of PHONE in misclassified instances of Floor 1 - Validation", col = "lightgreen", ylim = c(0,30)) #phone 0, phone 13's fault

#misclassification errors in FLOOR 1
rows_misclassified_floor1_valid <- rows_misclassified_floor_valid %>% 
  filter(FLOOR == "F.one")

rows_misclassified_floor_valid %>% 
  group_by(BF) %>% 
  summarize( count = n())


plot(rows_misclassified_floor_valid$BUILDINGID, col = "seagreen", main = "Distribution of errors in Floor across Buildings", ylim = c(0, 100))
plot(rows_wellclassified_floor_valid$BUILDINGID, col = "lightgreen", main = "Distribution of instances well classified across Buildings", ylim = c(0, 500))
#building1 Floor1 was well classified !!

#most of the misclassification errors for the floor1 (misclassified 29 times as floor zero and 36 times 
#as floor two) are in building 2!


unique(wifi_train$PHONEID) #1 3 6 7 8 10 11 13 14 16 17 18 19 22 23 24
unique(wifi_valid$PHONEID) # 0 2 4 5 9 12 13 14 15 20 21


#FOCUS ON PHONE 0
phone_0_valid_misc_FL <- rows_misclassified_floor_valid %>% filter(PHONEID == 0)
all_waps_0_valid <- as.vector(phone_0_valid_misc_FL[,1:312])
all_waps_0_valid <- data.matrix(all_waps_0_valid)
hist(all_waps_0_valid)

d <- density(all_waps_0_valid)
plot(d, main="Density of WAPs from PHONE ID 0 in errors on floor - Validation")
polygon(d, col="lightgreen", border="black")


#no comparison possible with the training since PHONE ID 0 is not there. 

phone_13_valid <- wifi_valid %>% filter(PHONEID == 13)
phone_20_valid <- wifi_valid %>% filter(PHONEID == 20)

#phone_0_train <- wifi_train %>% filter(PHONEID == 0) #PHONE 0 not in the training!
phone_13_train <- wifi_train %>% filter(PHONEID == 13)
#phone_20_train <- wifi_train %>% filter(PHONEID == 20) #PHONE 20 not in the training!

#FOCUS ON PHONE ID 13
#validation
phone_13_valid_misc_FL <- rows_misclassified_floor_valid %>% filter(PHONEID == 13)
all_waps_13_valid <- as.vector(phone_13_valid_misc_FL[,1:312])
all_waps_13_valid <- data.matrix(all_waps_13_valid)

e <- density(all_waps_13_valid)
plot(e, main="Density of WAPs from PHONE ID 13 in errors on floor - Validation")
polygon(e, col="lightgreen", border="black")


#training
#phone_13_train <- wifi_train %>% filter(PHONEID == 13)
all_waps_13_train <- as.vector(phone_13_train[,1:312])
all_waps_13_train <- data.matrix(all_waps_13_train)

e_t <- density(all_waps_13_train)
plot(e_t, main="Density of WAPs from PHONE ID 13 - Training")
polygon(e_t, col="lightgreen", border="black")



#FOCUS ON PHONE 20
phone_20_valid_misc_FL <- rows_misclassified_floor_valid %>% filter(PHONEID == 20)
all_waps_20_valid <- as.vector(phone_20_valid_misc_FL[,1:312])
all_waps_20_valid <- data.matrix(all_waps_20_valid)
hist(all_waps_20_valid)

f <- density(all_waps_20_valid)
plot(f, main="Density of WAPs from PHONE ID 20 in errors on floor - Validation")
polygon(f, col="lightgreen", border="black")


#no comparison possible with the training since PHONE ID 20 is not there. 


#AUC and ROC of the RF predicting FLOOR (with ACP)
roc_rf_FL_ACP <- multiclass.roc(validation_copy$FLOOR, as.numeric(RF_FL_predic_ACP), levels=levels(validation_copy$FLOOR), percent = T)
#Multi-class area under the curve: 89.86%
#A multiclass AUC is a mean of AUC and cannot be plotted.

rs_FL_ACP <- roc_rf_FL_ACP[['rocs']]
#rocs stores individual roc curve info for each classes.
rocobj_RF_FL <- plot.roc(rs_FL_ACP[[1]], col = "seagreen", lwd = 2, identity.col="black", identity.lwd=1, auc.polygon = T, legacy.axes = T, print.auc = F, main = "ROC Curve of Random Forest (FLOOR) - AUC = 0.89")
rocobj_RF_FL$auc #why is different from Multi-class area under the curve: 89.86%?
#N.B. it could be that this depends on the fact that we are dealing with a multiclass classification...
#to visualize all of them:
sapply(2:length(rs_FL_ACP),function(i) lines.roc(rs_FL_ACP[[i]],col=i))


#SOURCE: https://stackoverflow.com/questions/34169774/plot-roc-for-multiclass-roc-in-proc-package

#SVM to predict the FLOOR (with ACP)
set.seed(333)
SVM_FL_ACP <- svm(FLOOR ~ ., data = training,kernel = "radial",
              trControl = ctrl)

#fitted on the training
length(SVM_FL_ACP$fitted)

SVM_FL_predic_ACP <- predict(SVM_FL_ACP, validation)

#confusion matrix on the validation
confusionMatrix(SVM_FL_predic_ACP, validation$FLOOR)
#Accuracy : 0.8632, Kappa : 0.8111







#PREDICTING LONGITUDE (with PCA)


#adding the column longitude to training
training$LONGITUDE <- training_copy$LONGITUDE

#adding the column Predicted FLOOR in the training (from the random forest) and deleting the FLOOR
training$P.FLOOR <- RF_FL_ACP$predicted
training$FLOOR <- NULL

#adding the column LONGITUDE to validation
validation$LONGITUDE <- validation_copy$LONGITUDE

#adding the column Predicted FLOOR in the validation (from the random forest) and deleting the FLOOR
validation$P.FLOOR <- RF_FL_predic_ACP
validation$FLOOR <- NULL


#KNN to predict the LONGITUDE (with PCA)

set.seed(333)
KK_LO_ACP <- train.kknn(LONGITUDE~.,
                    data = training,trControl = ctrl,
                    kmax = 9)

KK_LO_predic_ACP <- predict(KK_LO_ACP, validation)

postResample(KK_LO_predic_ACP, validation$LONGITUDE)
#     RMSE  Rsquared       MAE 
#9.4782941 0.9938027 5.6856583 



#Random Forest to predict the LONGITUDE (with PCA) (BEST MODEL)
##IT TAKES 5/10 MINUTES
set.seed(333)
RF_LO_ACP <- randomForest(LONGITUDE~.,
                      data = training,trControl = ctrl,
                      ntree= 500)


#fitted on the training
length(RF_LO_ACP$predicted)  #19224
RF_LO_predic_ACP <- predict(RF_LO_ACP, validation)

postResample(RF_LO_predic_ACP, validation$LONGITUDE)
#   RMSE    Rsquared       MAE 
#8.0446559 0.9956922 5.3307850 

#length(validation$LONGITUDE[index_well_classified_FL])


#postResample of LONGITUDE for well_classified_FLOOR instances
postResample(RF_LO_predic_ACP[index_well_classified_FL], validation$LONGITUDE[index_well_classified_FL])
#     RMSE Rsquared      MAE 
# 7.465219 0.996512 4.945310 


#RF Residual Analysis
plot(RF_LO_predic_ACP, validation$LONGITUDE)
residual_RF_LO_ACP <- validation$LONGITUDE - RF_LO_predic_ACP
i <- hist(residual_RF_LO_ACP, breaks = 40, main = "Histogram of Residuals from Random Forest - LONGITUDE", xlab = "Residuals")




#SVM to predict the LONGITUDE (with PCA)
set.seed(333)
SVM_LO_ACP <- svm(LONGITUDE ~ ., data = training,kernel = "radial",
              trControl = ctrl)

#fitted on the training
length(SVM_LO_ACP$fitted)

SVM_LO_predic_ACP <- predict(SVM_LO_ACP, validation)

postResample(SVM_LO_predic_ACP, validation$LONGITUDE)
#     RMSE   Rsquared        MAE 
#12.1055545  0.9902037  8.4621676

#RESIDUALS FOR LONGITUDE IN THE 3 MODELS
par(mfrow = c(1,3))
#knn
hist(validation$LONGITUDE - KK_LO_predic_ACP, breaks = 40, main = "Residuals from KNN - LONGITUDE", xlab = "Residuals")
#rf
hist(validation$LONGITUDE - RF_LO_predic_ACP, breaks = 40, main = "Residuals from RF - LONGITUDE", xlab = "Residuals", border = "midnightblue", col = "darkturquoise")
#svm
hist(validation$LONGITUDE - SVM_LO_predic_ACP, breaks = 40, main = "Residuals from SVM - LONGITUDE", xlab = "Residuals")


#plotting the normal curve with the residuals of the RF -LONGITUDE
res_RF_LON = validation$LONGITUDE - RF_LO_predic_ACP
m<-mean(res_RF_LON)
std<-sqrt(var(res_RF_LON))
hist(res_RF_LON, breaks=40, freq = F,
     xlab="Residuals",
     main="Residuals from RF - LONGITUDE")

#lines(density(res_RF_LON), col="red")
lines(seq(-40, 40, by=.5), dnorm(seq(-40, 40, by=.5),
                                mean(res_RF_LON), sd(res_RF_LON)), col="blue")

#PREDICTING LATITUDE (with PCA)


#adding the column latitude to training
training$LATITUDE <- training_copy$LATITUDE

#adding the column Predicted FLOOR and Predicted LONGITUDE in the training and deleting the LONGITUDE
training$P.FLOOR <- RF_FL_ACP$predicted
training$P.LONGITUDE <- RF_LO_ACP$predicted
training$LONGITUDE <- NULL

#
validation$LATITUDE <- validation_copy$LATITUDE
validation$P.LONGITUDE <- RF_LO_predic_ACP

#
validation$P.FLOOR <- RF_FL_predic_ACP
validation$FLOOR <- NULL
validation$LONGITUDE <- NULL


#KNN to predict the LATITUDE (with PCA)

set.seed(333)
KK_LA_ACP <- train.kknn(LATITUDE~.,
                        data = training,trControl = ctrl,
                        kmax = 9)

KK_LA_predic_ACP <- predict(KK_LA_ACP, validation)

postResample(KK_LA_predic_ACP, validation$LATITUDE)
#  RMSE   Rsquared      MAE 
#9.419045 0.982250 5.777010 




#Random Forest to predict the LATITUDE (with PCA) (BEST MODEL)
##IT TAKES 5/10 MINUTES
set.seed(333)
RF_LA_ACP <- randomForest(LATITUDE~.,
                          data = training,trControl = ctrl,
                          ntree= 500)


#fitted on the training
length(RF_LA_ACP$predicted)  #19224
RF_LA_predic_ACP <- predict(RF_LA_ACP, validation)

postResample(RF_LA_predic_ACP, validation$LATITUDE)
#  RMSE  Rsquared       MAE 
#8.3843661 0.9859653 5.5111584 





#SVM to predict the LATITUDE (with PCA)
set.seed(333)
SVM_LA_ACP <- svm(LATITUDE ~ ., data = training,kernel = "radial",
                  trControl = ctrl)

summary(SVM_LA_ACP)
#fitted on the training
length(SVM_LA_ACP$fitted)

SVM_LA_predic_ACP <- predict(SVM_LA_ACP, validation)

postResample(SVM_LA_predic_ACP, validation$LATITUDE)
#    RMSE   Rsquared       MAE 
#9.0326070 0.9837527 6.5814910  



#ENSEMBLE RF and SVM for LATITUDE (with ACP)

#Ensemble learning involves combining multiple predictions 
#derived by different techniques in order to create a stronger overall prediction.

RF_SVM_LA_pred_ensemble_ACP <-(RF_LA_predic_ACP + SVM_LA_predic_ACP)/2

postResample(RF_SVM_LA_pred_ensemble_ACP, validation$LATITUDE)
#RMSE       Rsquared       MAE 
#8.2352774 0.9865461 5.7395870 


#postResample of LATITUDE for well_classified_FLOOR instances
postResample(RF_SVM_LA_pred_ensemble_ACP[index_well_classified_FL], validation$LATITUDE[index_well_classified_FL])
#   RMSE  Rsquared       MAE 
#7.5680475 0.9888877 5.2946562 


#When we equally combine the svm predictions from the single model with the random forest predictions,
#we get an error rate which is superior to either the svm model alone, or the random forest model alone.
#As we have seen, ensemble learning can outperform any one single model when used properly. 

#ENSEMBLE Residual Analysis
plot(RF_SVM_LA_pred_ensemble_ACP, validation$LATITUDE)
residual_RF_SVM_LAT_ACP <- validation$LATITUDE - RF_SVM_LA_pred_ensemble_ACP
h <- hist(residual_RF_SVM_LAT_ACP, breaks = 40, main = "Histogram of Residuals from Ensemble - LATITUDE", xlab = "Residuals")

#RESIDUALS FROM THE THREE MODELS
par(mfrow = c(2,2))
hist(validation$LATITUDE - KK_LA_predic_ACP, breaks = 40, main = "Residuals from KNN - LATITUDE", xlab = "Residuals")
hist(validation$LATITUDE - RF_LA_predic_ACP, breaks = 40, main = "Residuals from RF - LATITUDE", xlab = "Residuals")
hist(validation$LATITUDE - SVM_LA_predic_ACP, breaks = 40, main = "Residuals from SVM - LATITUDE", xlab = "Residuals")
hist(validation$LATITUDE - RF_SVM_LA_pred_ensemble_ACP, breaks = 40, main = "Residuals from Ensemble - LATITUDE", xlab = "Residuals", border = "midnightblue", col = "darkturquoise")


#COMPUTING ERRORS FOR EACH OBS, COMBINING LONG AND LAT
RF_LO_predic_ACP #predicted long from RF
validation_copy$LONGITUDE #actual long

RF_SVM_LA_pred_ensemble_ACP #predicted lat from ENS
validation_copy$LATITUDE #actual lat

long_lat_pred_act <- data.frame(RF_LO_predic_ACP, validation_copy$LONGITUDE, RF_SVM_LA_pred_ensemble_ACP, validation_copy$LATITUDE)
long_lat_pred_act$ERROR <- sqrt((long_lat_pred_act$validation_copy.LONGITUDE - long_lat_pred_act$RF_LO_predic_ACP)^2 + (long_lat_pred_act$validation_copy.LATITUDE - long_lat_pred_act$RF_SVM_LA_pred_ensemble_ACP)^2)
long_lat_pred_act


mean(long_lat_pred_act$ERROR) #8.652052
median(long_lat_pred_act$ERROR) #6.451526

long_lat_pred_act$FLOOR <- validation_copy$FLOOR

long_lat_pred_act

colnames(long_lat_pred_act) <- c("P.LONGITUDE", "LONGITUDE", "P.LATITUDE", "LATITUDE", "ERROR", "FLOOR")

summary(long_lat_pred_act)


## ALL BUILDINGS Giorgia
aaaa <- ggplot(data=long_lat_pred_act, aes(x = LONGITUDE, y = LATITUDE, colour = ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") +
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = P.LONGITUDE, yend = P.LATITUDE),
               arrow = arrow(length = unit(0.01, "npc")))+
  scale_color_gradient(low="springgreen1", high="red", space ="Lab", limit=c(0,20), na.value = "darkviolet")

bbbb <- ggplot(data=long_lat_pred_act, aes(x = LONGITUDE, y = LATITUDE,colour = ERROR)) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE),size = 2,shape = 4,color="black") +
  geom_segment(aes(x = LONGITUDE, y = LATITUDE, xend = P.LONGITUDE, yend = P.LATITUDE),
               arrow = arrow(length = unit(0.05, "npc")))+
  scale_color_gradient(low="springgreen1", high="red", space ="Lab", limit=c(0,20), na.value = "darkviolet") +
  facet_wrap("FLOOR",dir="v",ncol=1)

plot_grid(aaaa,bbbb)


#RADIUS of error (LONGITUDE AND LATITUDE)
radius_overall <- sqrt((5.3)^2 + (5.7)^2)
radius_overall #7.7 mt
radius_right_FL <- sqrt((4.9)^2 + (5.2)^2)
radius_right_FL #7.1 mt


summary(wifi_train$LONGITUDE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7691   -7597   -7423   -7466   -7359   -7301 
summary(wifi_train$LATITUDE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4864746 4864821 4864853 4864871 4864930 4865017

z <- wifi_train %>%
  group_by(BUILDINGID) %>%
  summarize(max_long = max(LONGITUDE),
  min_long = min(LONGITUDE),
  max_lat = max(LATITUDE),
  min_lat = min(LATITUDE))

z

formattable(z, align =c("l","c","c","c","c", "c", "c", "c", "r"))
