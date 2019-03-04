#PREDICT SENTIMENT

rm(list=ls())

setwd("C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/3. BIG DATA WEB MINING/Task 3 - Predict the sentiment")  
getwd()


# Required packages
library(doParallel)
library(corrplot)
library(ggplot2)
library(mlbench)
library(caret)
library(plyr)
library(dplyr)
library(arules)
require(psych)

#Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6 
#I have 8 cores

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 


#original data
iphone_original <- read.csv("iphone_smallmatrix_labeled_8d.csv", header = TRUE)

#sentiment_iphone with robocop
iphone_sentiment_robo = read.csv("sentiment.csv", header = TRUE)

iphone_original$iphonesentimentrobo <- iphone_sentiment_robo$iphoneSentiment

hist(iphone_original$iphonesentiment)
hist(iphone_original$iphonesentimentrobo)

summary(iphone_original$iphonesentimentrobo)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-399.000    0.000    0.000    3.931    0.000 2301.000 

prop.table(table(iphone_original$iphonesentimentrobo))

summary(iphone_original$iphonesentiment)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.000   5.000   3.725   5.000   5.000 

class(iphone_original$iphonesentimentrobo) #integer
hist(iphone_original$iphonesentimentrobo, breaks = 200) #majority concentrated in zero

unique(iphone_original$iphonesentimentrobo)

#dicretizing the dependent variable in 7 classes
disfixed7 <- arules::discretize(iphone_original$iphonesentimentrobo, method = "fixed", breaks= c(-Inf, -50, -10, -1, 1, 10, 50, Inf))

summary(disfixed7)
#[-Inf,-50)  [-50,-10)   [-10,-1)     [-1,1)     [1,10)    [10,50)  [50, Inf] 
#206        633        429       8768        504       1923        510 

#replacing the numerical dependent with the new factor dependent
iphone_original$iphonesentimentrobo <- disfixed7
unique(iphone_original$iphonesentimentrobo)

#hist(iphone_untouched$iphonesentiment)
#sort(unique(iphone_untouched$iphonesentiment))

#plot of the dependent iphonesentimentrobo
ggplot(iphone_original, aes(x=iphonesentimentrobo)) + geom_bar() #majority are -1,1 -> unbalance in dependent variable
#plot of the old dependent iphonesentiment
ggplot(iphone_original, aes(x=iphonesentiment)) + geom_bar() #majority were 5

levels(iphone_original$iphonesentimentrobo)

iphone_original$iphonesentimentrobo <- ordered(iphone_original$iphonesentimentrobo, levels = c("[-Inf,-50)","[-50,-10)","[-10,-1)","[-1,1)","[1,10)","[10,50)","[50, Inf]"), labels = c("Very Negative", "Negative", "Somewhat Negative", "Neutral", "Somewhat Positive", "Positive", "Very Positive"))

unique(iphone_original$iphonesentimentrobo)

levels(iphone_original$iphonesentimentrobo)

#plot of the dependent iphonesentimentrobo with lables
ggplot(iphone_original, aes(x=iphonesentimentrobo)) + geom_bar()

#iphone_original will remain the original dataset with 12973 records and 60 variables 
#(it has both old iphone sentiment and new iphonesentimentrobo)




#importing the iphone matrix
iphone = read.csv("iphone_smallmatrix_labeled_8d.csv", header = TRUE)

#change with the right iphone sentiment from robolabel
names(iphone)
iphone$iphonesentiment <- iphone_original$iphonesentimentrobo
#plot of the dependent
ggplot(iphone, aes(x=iphonesentiment)) + geom_bar()


#looking for all zeros rows
onlyattributes <- iphone[,1:58]
#creating column sum of attributes
onlyattributes$sum_attributes <- apply(onlyattributes, 1, sum)

#indexes of rows with all attributes = 0
noattributerows <- which(onlyattributes$sum_attributes == 0) #0 rows

#dataset of rows having iphone = 0 (971 rows)
iphonezero <- iphone %>% 
  filter(iphone == 0)

ggplot(iphonezero, aes(x=iphonesentiment)) + geom_bar()
summary(iphonezero$iphonesentiment)

#indexes of rows having iphone = 0 (971 indexes)
iphonezero <- which(iphone$iphone == 0)


#iphone <- iphone[-iphonezero,]  #for deleting rows with iphone = 0
#i did not delete them because they have values i other columns related to iphone


#COLUMNS RELATED TO IPHONE (14)
#iphone$iphone
#iphone$ios
#iphone$iphonecampos
#iphone$iphonecamneg
#iphone$iphonecamunc
#iphone$iphonedispos
#iphone$iphonedisneg
#iphone$iphonedisunc
#iphone$iphoneperpos
#iphone$iphoneperneg
#iphone$iphoneperunc
#iphone$iosperpos
#iphone$iosperneg
#iphone$iosperunc


#creating dataset with rows having all columns related to iphone = 0. they did not talk at all about iphone.
#829 observations
iphonecommentzero <- iphone %>% 
  filter(iphone == 0 & ios == 0 & iphonecampos==0 & iphonecamneg==0 & iphonecamunc==0 & iphonedispos == 0 & iphonedisneg == 0 & iphonedisunc == 0 & iphoneperpos == 0 & iphoneperneg == 0 & iphoneperunc == 0 & iosperpos == 0 & iosperneg == 0 & iosperunc == 0)

summary(iphonecommentzero$iphonesentiment)
#they have been all classified as Neutral by the function but this is not correct, since they didn't talk at all about iphone
#we cannot conclude that they are neutral because they didn't take any position
#I will delete these rows 

iphonecommentzeroindex <- which(iphone$iphone == 0 & iphone$ios == 0 & iphone$iphonecampos==0 & iphone$iphonecamneg==0 & iphone$iphonecamunc==0 & iphone$iphonedispos == 0 & iphone$iphonedisneg == 0 & iphone$iphonedisunc == 0 & iphone$iphoneperpos == 0 & iphone$iphoneperneg == 0 & iphone$iphoneperunc == 0 & iphone$iosperpos == 0 & iphone$iosperneg == 0 & iphone$iosperunc == 0)
iphone <- iphone[-iphonecommentzeroindex,] 
#so now iphone dataset is composed of 12.144 rows and 59 columns 

names(iphone)

#deleting all columns related to iphone (14 columns)
iphone <- iphone[,-c(1, 6, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 55, 57)]
names(iphone)
#now we have 45 variables

correlationMatrix <- cor(iphone[,1:44])
print(correlationMatrix)

#TO DO: treat the unbalance in the dependent variable with down-sampling or up-sampling from caret

#FEATURE SELECTION WITH 3 METHODS: 

#starting dataset for all the methods; iphone (12144 * 45)

summary(iphone)

#1. REMOVING REDUNDANT VARIABLES THROUGH CORRELATION (will create iphone_relevant dataset)

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names = F)
print(highlyCorrelated)
length(highlyCorrelated) #26
names(iphone[,highlyCorrelated])

#new dataset just with relevant attributes (19)
iphone_relevant <- iphone[,-highlyCorrelated]
names(iphone_relevant)

#correlation between attributes and dependent variable
#reload the dataset and take the dependent as numerical
iphone_untouched <- read.csv("iphone_smallmatrix_labeled_8d.csv", header = TRUE)
iphone_untouched$iphonesentiment <- iphone_sentiment_robo$iphoneSentiment
cor(iphone_untouched[,1:58], iphone_untouched$iphonesentiment)
#non significant linear relationships (apart from htccphone with 0.5 correlation)

#computing sparsity of the dataset (= count zero elements / total elements)
sum(iphone == 0)/(dim(iphone)[1]*dim(iphone)[2])
#0.9661891

dim(iphone)

#2. RFE (will create iphone_rfe dataset)

# Let's sample the data before using RFE
set.seed(123)

#creating the new dataset iphone_rfe
iphone_rfe = iphone

iphoneSample <- iphone_rfe[sample(1:nrow(iphone_rfe), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 45 iphonesentiment) 
rfeResults <- rfe(iphoneSample[,1:44], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:44), 
                  rfeControl=ctrl)

# Get results
rfeResults
#The top 5 variables (out of 25):
#samsungcampos, htcdisunc, htcperneg, htcperpos, googleperpos

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
iphone_rfe <- iphone_rfe[,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
iphone_rfe$iphonesentiment <- iphone$iphonesentiment

# review outcome
str(iphone_rfe)


#2. NEAR ZERO VARIANCE
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, percentage unique, zero variance and near zero variance 
nzvMetrics <- nearZeroVar(iphone, saveMetrics = TRUE)
nzvMetrics

#nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(iphone, saveMetrics = FALSE)
nzv

##create a new data set and remove near zero variance features (4 attributes remain: iphone, samsunggalaxy, htcphone, iphonesentiment)
iphoneNZV <- iphone[,-nzv]
str(iphoneNZV)



#columns = unname(apply(iphone_original,2,function(x){return(length(which(x != 0 )))}))
#column= c()
#for (i in 1:length(columns)){
 # if (columns[i]<= 12){ column = c(column,i )}
#}



#DATASETS CREATED SO FAR:
#iphone 45 vars
#iphone_relevant 19 vars
#iphone_rfe 28 vars
#iphoneNZV 3 vars


####----
#OUT OF THE BOX 
#MODELING on the iphone dataset with 45 var

set.seed(998)

#define an 70%/30% train/test split of the dataset

inTraining <- createDataPartition(iphone$iphonesentiment, p = .70, list = FALSE)

training <- iphone[inTraining,] #8505 obs in the training
testing <- iphone[-inTraining,] #3639 obs in the testing

#10 fold cross validation (10 folds, 3 repetitions, upsampling to resolve unbalanced dependent variable)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, sampling = "up")

#train C5.0 model
set.seed(999)
C5.0 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl, metric = "Accuracy")

summary(C5.0)


#predictor variables
predictors(C5.0) #returns which predictors were used in the model 

#make predictions
testPredC5.0 <- predict(C5.0, testing)

#performace measurment
metricC5.0 <- postResample(testPredC5.0, testing$iphonesentiment)
metricC5.0

#with upsampling:
#Accuracy      Kappa 
#0.10964551 0.04695961 

#N.B. without upsampling:
#Accuracy     Kappa 
#0.6782083 0.1189873 


#Create a confusion matrix from C5.0 predictions 
cmC5.0 <- confusionMatrix(testPredC5.0, testing$iphonesentiment) 
cmC5.0


#N.B. running the following algorithms with sampling = "up" argument within the cross validation is computationally too expensive (more than 1hour each).
#therefore, I will create upsample and downsample dataset from the training and I will run all the algo on the downsample

up_sample <- upSample(training[,1:44], training$iphonesentiment) #upSample samples with replacement to make the class distributions equal
down_sample <- downSample(training[,1:44], training$iphonesentiment) #downSample will randomly sample a data set so that all classes have the same frequency as the minority class.

#N.B. Simple random sampling is used to down-sample for the majority class(es). 
#N.B. Note that the minority class data are left intact and that the samples will be re-ordered in the down-sampled version.
#N.B. For up-sampling, all the original data are left intact and additional samples are added to the minority classes with replacement.

ggplot(up_sample, aes(x=Class)) + geom_bar() 
ggplot(down_sample, aes(x=Class)) + geom_bar() 
#renames the dependent variable
names(up_sample)[names(up_sample) == 'Class'] <- 'iphonesentiment'
names(down_sample)[names(down_sample) == 'Class'] <- 'iphonesentiment'


#train a RANDOM FOREST using down_sample dataset (1015 * 45)

#removing sampling = up from the cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(999)
rf_down <- train(iphonesentiment~., data = down_sample, method = "rf", trainControl = fitControl)

testPredrf_down <- predict(rf_down, testing)

#performace measurment
metricrf_down <- postResample(testPredrf_down, testing$iphonesentiment)
metricrf_down
#Accuracy      Kappa 
#0.21406980 0.03922841 

#Create a confusion matrix from rf predictions 
cmrf_down <- confusionMatrix(testPredrf_down, testing$iphonesentiment) 
cmrf_down


#train a SVM RADIAL using down_sample
set.seed(999)
svmradial_down <- train(iphonesentiment~., data = down_sample, method = "svmRadial", trControl=fitControl)

summary(svmradial)

#predictor variables
predictors(svmradial) #returns which predictors were used in the model 

#make predictions
testPredsvmradial_down <- predict(svmradial_down, testing)

#performace measurment
metricsvmradial_down <- postResample(testPredsvmradial_down, testing$iphonesentiment)
metricsvmradial_down


#Create a confusion matrix from SVM RADIAL predictions 
cmsvmradial_down <- confusionMatrix(testPredsvmradial_down, testing$iphonesentiment) 
cmsvmradial_down


#train knn model usind down_sample
set.seed(999)
knn_down <- train(iphonesentiment~., data = down_sample, method = "kknn", trControl=fitControl)

summary(knn)

#predictor variables
predictors(knn) #returns which predictors were used in the model 

#make predictions
testPredknn_down <- predict(knn_down, testing)

#performace measurment
metricknn_down <- postResample(testPredknn_down, testing$iphonesentiment)
metricknn_down

#Create a confusion matrix from KNN predictions 
cmknn_down <- confusionMatrix(testPredknn_down, testing$iphonesentiment) 
cmknn_down


#THE BEST MODEL SO FAR IS RANDOM FOREST


#proportion of values in every variable

#unbalance in the dependent variable
prop.table(table(training$iphonesentiment)) #neutral: 65%; #positive: 15%
#all zero in the predictors
prop.table(table(training$samsunggalaxy))
prop.table(table(training$sonyxperia))
prop.table(table(training$nokialumina))
prop.table(table(training$htcphone))
prop.table(table(training$googleandroid))
prop.table(table(training$samsungcampos))

#plotting all variables of training to show that there are almost all zero in the predictors

multi.hist(training[,1:44])


####----
#IPHONE RELEVANT 

#MODELING on the iphone relevant dataset with rf

training_relevant <- iphone_relevant[inTraining,] #8505 obs in the training
testing_relevant <- iphone_relevant[-inTraining,] #3639 obs in the testing

down_sample_relevant <- downSample(training_relevant[,1:18], training_relevant$iphonesentiment) #downSample will randomly sample a data set so that all classes have the same frequency as the minority class.
names(down_sample_relevant)[names(down_sample_relevant) == 'Class'] <- 'iphonesentiment'

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(999)
rf_down_relevant <- train(iphonesentiment~., data = down_sample_relevant, method = "rf", trainControl = fitControl)

testPredrf_down_relevant <- predict(rf_down_relevant, testing)

#performace measurment
metricrf_down_relevant <- postResample(testPredrf_down_relevant, testing$iphonesentiment)
metricrf_down_relevant


#Create a confusion matrix from rf predictions 
cmrf_down_relevant <- confusionMatrix(testPredrf_down_relevant, testing$iphonesentiment) 
cmrf_down_relevant


####----
#IPHONE RFE

#MODELING on the iphone rfe dataset with rf

training_rfe <- iphone_rfe[inTraining,] #8505 obs in the training
testing_rfe <- iphone_rfe[-inTraining,] #3639 obs in the testing

down_sample_rfe <- downSample(training_rfe[,1:25], training_rfe$iphonesentiment) #downSample will randomly sample a data set so that all classes have the same frequency as the minority class.
names(down_sample_rfe)[names(down_sample_rfe) == 'Class'] <- 'iphonesentiment'

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(999)
rf_down_rfe <- train(iphonesentiment~., data = down_sample_rfe, method = "rf", trainControl = fitControl)

testPredrf_down_rfe <- predict(rf_down_rfe, testing)

#performace measurment
metricrf_down_rfe <- postResample(testPredrf_down_rfe, testing$iphonesentiment)
metricrf_down_rfe


#Create a confusion matrix from rf predictions 
cmrf_down_rfe <- confusionMatrix(testPredrf_down_rfe, testing$iphonesentiment) 
cmrf_down_rfe


####----
#IPHONE NZV

#MODELING on the iphone nzv dataset with rf

training_nzv <- iphoneNZV[inTraining,] #8505 obs in the training
testing_nzv <- iphoneNZV[-inTraining,] #3639 obs in the testing

down_sample_nzv <- downSample(training_nzv[,1:2], training_nzv$iphonesentiment) #downSample will randomly sample a data set so that all classes have the same frequency as the minority class.
names(down_sample_nzv)[names(down_sample_nzv) == 'Class'] <- 'iphonesentiment'

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(999)
rf_down_nzv <- train(iphonesentiment~., data = down_sample_nzv, method = "rf", trainControl = fitControl)

testPredrf_down_nzv <- predict(rf_down_nzv, testing)

#performace measurment
metricrf_down_nzv <- postResample(testPredrf_down_nzv, testing$iphonesentiment)
metricrf_down_nzv


#Create a confusion matrix from rf predictions 
cmrf_down_nzv <- confusionMatrix(testPredrf_down_nzv, testing$iphonesentiment) 
cmrf_down_nzv


#so far, the best predictions have been obtained from the complete dataset and with random forest.


#FEATURE ENGINEERING

#create a new dataset that will be used for recoding sentiment
iphoneRC <- iphone
class(iphoneRC$iphonesentiment)
#recode sentiment to combine factor levels 1 & 2 and 6 & 7


iphoneRC$iphonesentiment <- revalue(iphoneRC$iphonesentiment, c("Very Negative" = "Negative", "Negative" = "Negative", "Somewhat Negative" = "Somewhat Negative", "Neutral" = "Neutral", "Somewhat Positive" = "Somewhat Positive", "Positive" = "Positive", "Very Positive" = "Positive"))
levels(iphoneRC$iphonesentiment)

# inspect results
summary(iphoneRC)
str(iphoneRC)

set.seed(998)

training_bin <- iphoneRC[inTraining,] #8505 obs in the training
testing_bin <- iphoneRC[-inTraining,] #3639 obs in the testing

unique(training_bin$iphonesentiment)
unique(testing_bin$iphonesentiment)

down_sample_bin <- downSample(training_bin[,1:44], training_bin$iphonesentiment) #downSample will randomly sample a data set so that all classes have the same frequency as the minority class.
names(down_sample_bin)[names(down_sample_bin) == 'Class'] <- 'iphonesentiment'

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(999)
rf_down_bin <- train(iphonesentiment~., data = down_sample_bin, method = "rf", trainControl = fitControl)

testPredrf_down_bin <- predict(rf_down_bin, testing_bin)

#performace measurment
metricrf_down_bin <- postResample(testPredrf_down_bin, testing_bin$iphonesentiment)
metricrf_down_bin


#Create a confusion matrix from rf predictions 
cmrf_down_bin <- confusionMatrix(testPredrf_down_bin, testing_bin$iphonesentiment) 
cmrf_down_bin

#prediction get worst, and notice that the rf behaves completely different in these two dataset!
#compare with the cm of rf on downsample





#PCA

preprocessParams <- preProcess(down_sample[,-45], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

#PCA needed 11 components to capture 95 percent of the variance

#preprocessParams <- preProcess(down_sample[,-45], method=c("center", "scale", "pca"), thresh = 0.80)
#print(preprocessParams)

#PCA needed 5 components to capture 80 percent of the variance
#if we lower the threshold of the variance we need less components

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, down_sample[,-45])

# add the dependent to training
train.pca$iphonesentiment <- down_sample$iphonesentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testing[,-45])

# add the dependent to training
test.pca$iphonesentiment <- testing$iphonesentiment

# inspect results
str(train.pca)
str(test.pca)

#N.B. there are 11 principal components in train.pca and test.pca (+ the iphonesentiment)

#model using the rf (best learner)

#train a RANDOM FOREST using down_sample dataset (1015 * 45)

#removing sampling = up from the cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(999)


rf_down_pca <- train(iphonesentiment~., data = train.pca, method = "rf", trainControl = fitControl)

testPredrf_down_pca <- predict(rf_down_pca, test.pca)

#performace measurment
metricrf_down_pca <- postResample(testPredrf_down_pca, test.pca$iphonesentiment)
metricrf_down_pca
metricrf_down_pca
#Accuracy      Kappa 
#0.21214619 0.04073951 


#Create a confusion matrix from rf predictions 
cmrf_down_pca <- confusionMatrix(testPredrf_down_pca, test.pca$iphonesentiment) 
cmrf_down_pca


#importing the iphonelargematrix

#original data
iphonelargematrix <- read.csv("iphonelargematrix.csv", header = TRUE)

colnames(down_sample)


dim(iphonelargematrix)
#deleting all unuseful columns in largematrix and selecting the same of the small iphone matrix used to train the models
iphonelargematrix$id <- NULL
iphonelargematrix$iphone <- NULL
iphonelargematrix$ios <- NULL
iphonelargematrix$iphonecampos <- NULL
iphonelargematrix$iphonecamneg <- NULL
iphonelargematrix$iphonecamunc <- NULL
iphonelargematrix$iphonedispos <- NULL
iphonelargematrix$iphonedisneg <- NULL
iphonelargematrix$iphonedisunc <- NULL
iphonelargematrix$iphoneperpos <- NULL
iphonelargematrix$iphoneperneg <- NULL
iphonelargematrix$iphoneperunc <- NULL
iphonelargematrix$iosperpos <- NULL
iphonelargematrix$iosperneg <- NULL
iphonelargematrix$iosperunc <- NULL


#changing type of predictors from factor to integer
for(i in c(1:44)) {
  iphonelargematrix[,i] <- as.integer(iphonelargematrix[,i])
}

str(iphonelargematrix)

Predrf_iphonelarge <- predict(rf_down, iphonelargematrix)

summary(Predrf_iphonelarge)
#Very Negative          Negative Somewhat Negative           Neutral Somewhat Positive          Positive 
#585                         684               122             10048               840             71362 
#Very Positive 
#546 

#aggregating levels:
#Negative: 585+684+122 = 1391
#Neutral: 10048
#Positive: 840+71362+546 = 72748


prop.table(table(Predrf_iphonelarge))
#Very Negative          Negative Somewhat Negative           Neutral Somewhat Positive          Positive 
#0.006948816       0.008124770       0.001449155       0.119353344       0.009977788       0.847660565 
#Very Positive 
#0.006485562 


# create a data frame for plotting.
# you can add more sentiment levels if needed
# Replace sentiment values 
pieData_iphone <- data.frame(COM = c("negative", "neutral", "positive"), 
                      values = c(1391, 10048, 72748))


require(plotly)

# create pie chart
plot_ly(pieData_iphone, labels = ~COM, values = ~ values, type = "pie",
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste( values),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = F) %>%
  layout(title = 'iPhone Sentiment', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)


