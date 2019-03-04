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
galaxy_original <- read.csv("galaxy_smallmatrix_labeled_8d.csv", header = TRUE)

#sentiment_galaxy with robocop
galaxy_sentiment_robo = read.csv("sentiment.csv", header = TRUE)

galaxy_original$galaxysentimentrobo <- galaxy_sentiment_robo$galaxySentiment

hist(galaxy_original$galaxysentiment)
hist(galaxy_original$galaxysentimentrobo)

summary(galaxy_original$galaxysentimentrobo)
#Min.         1st Qu.    Median      Mean   3rd Qu.      Max. 
#-105.0000    0.0000    0.0000   -0.1734    0.0000 1278.0000 

summary(galaxy_original$galaxysentiment)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.000   5.000   3.824   5.000   5.000 

unique(galaxy_original$galaxysentimentrobo)

class(galaxy_original$galaxysentimentrobo) #integer
hist(galaxy_original$galaxysentimentrobo, breaks = 68) #majority concentrated in zero
sort(unique(galaxy_original$galaxysentimentrobo))


hist(galaxy_sentiment_robo$galaxySentiment)

prop.table(table(galaxy_sentiment_robo$galaxySentiment))

#dicretizing the dependent variable in 7 classes (check the threshold!!!!!!)
disfixed7g <- arules::discretize(galaxy_original$galaxysentimentrobo, method = "fixed", breaks= c(-Inf, -50, -10, -1, 1, 10, 50, Inf))

summary(disfixed7g)
#[-Inf,-50)  [-50,-10)   [-10,-1)     [-1,1)     [1,10)    [10,50)  [50, Inf] 
###128         11         10      12586         26        176         36 

#replacing the numerical dependent with the new factor dependent
galaxy_original$galaxysentimentrobo <- disfixed7g
unique(galaxy_original$galaxysentimentrobo)

levels(galaxy_original$galaxysentimentrobo)

#deleting the old sentiment (wrong)
galaxy_original$galaxysentiment <- NULL

#plot of the dependent galaxysentimentrobo
ggplot(galaxy_original, aes(x=galaxysentimentrobo)) + geom_bar() #majority are -1,1 -> unbalance in dependent variable

galaxy_original$galaxysentimentrobo <- ordered(galaxy_original$galaxysentimentrobo, levels = c("[-Inf,-50)","[-50,-10)","[-10,-1)","[-1,1)","[1,10)","[10,50)","[50, Inf]"), labels = c("Very Negative", "Negative", "Somewhat Negative", "Neutral", "Somewhat Positive", "Positive", "Very Positive"))

unique(galaxy_original$galaxysentimentrobo)

levels(galaxy_original$galaxysentimentrobo)


#plot of the dependent galaxysentimentrobo with lables
ggplot(galaxy_original, aes(x=galaxysentimentrobo)) + geom_bar()

#looking for all zeros rows
onlyattributesg <- galaxy_original[,1:58]
#creating column sum of attributes
onlyattributesg$sum_attributes <- apply(onlyattributesg, 1, sum)

#indexes of rows with all attributes = 0
noattributerowsg <- which(onlyattributesg$sum_attributes == 0) #0 rows (empty)

#columns related to galaxy (10)

#galaxy_original$samsunggalaxy
#galaxy_original$samsungcampos
#galaxy_original$samsungcamneg
#galaxy_original$samsungcamunc
#galaxy_original$samsungdispos
#galaxy_original$samsungdisneg
#galaxy_original$samsungdisunc
#galaxy_original$samsungperpos
#galaxy_original$samsungperneg
#galaxy_original$samsungperunc

#DOUBT: do I have to delete also these 4? 
#they could be referred to the operative system of the other phones, and not necessarly to samsung galaxy!
#galaxy_original$googleandroid
#galaxy_original$googleperneg
#galaxy_original$googleperpos
#galaxy_original$googleperunc


#creating dataset with rows having all columns related to galaxy = 0. 
#they did not talk at all about galaxy.
#11866 observations
galaxycommentzero <- galaxy_original %>% 
  filter(samsunggalaxy == 0 & samsungcamneg == 0 & samsungcampos == 0 & samsungcamunc == 0 &
           samsungdisneg == 0 & samsungdispos == 0 & samsungdisunc == 0 & samsungperpos == 0 & 
           samsungperneg == 0 & samsungperunc == 0
         & googleandroid == 0 & googleperpos == 0 & googleperneg == 0 & googleperunc == 0)
summary(galaxycommentzero$galaxysentimentrobo)
#they have been all classified as Neutral by the function but this is not correct, since they didn't talk at all about galaxy
#we cannot conclude that they are neutral because they didn't take any position
#I will delete these rows (11866):

galaxycommentzeroindex <- which(galaxy_original$samsunggalaxy == 0 & galaxy_original$samsungcamneg == 0 & galaxy_original$samsungcampos == 0 & galaxy_original$samsungcamunc == 0 &
                                  galaxy_original$samsungdisneg == 0 & galaxy_original$samsungdispos == 0 & galaxy_original$samsungdisunc == 0 & galaxy_original$samsungperpos == 0 & 
                                  galaxy_original$samsungperneg == 0 & galaxy_original$samsungperunc == 0
                                & galaxy_original$googleandroid == 0 & galaxy_original$googleperpos == 0 & galaxy_original$googleperneg == 0 & galaxy_original$googleperunc == 0)
galaxy_original <- galaxy_original[-galaxycommentzeroindex,] 
#so now iphone dataset is composed of 1107 rows and 59 columns 

names(galaxy_original)


#deleting all columns related to galaxy (14 columns)

galaxy_original$samsunggalaxy <- NULL
galaxy_original$samsungcampos <- NULL
galaxy_original$samsungcamneg <- NULL
galaxy_original$samsungcamunc <- NULL
galaxy_original$samsungdispos <- NULL
galaxy_original$samsungdisneg <- NULL
galaxy_original$samsungdisunc <- NULL
galaxy_original$samsungperpos <- NULL
galaxy_original$samsungperneg <- NULL
galaxy_original$samsungperunc <- NULL

galaxy_original$googleandroid <- NULL
galaxy_original$googleperneg <- NULL
galaxy_original$googleperpos <- NULL
galaxy_original$googleperunc <- NULL


#now we have 45 variables

#create a new dataset that will be used for recoding sentiment

class(galaxy_original$galaxysentimentrobo) #ordered factor
#recode sentiment to combine factor levels 1 & 2 and 6 & 7

galaxy_original$galaxysentimentrobo <- revalue(galaxy_original$galaxysentimentrobo, c("Very Negative" = "Negative", "Negative" = "Negative", "Somewhat Negative" = "Negative", "Neutral" = "Neutral", "Somewhat Positive" = "Positive", "Positive" = "Positive", "Very Positive" = "Positive"))
levels(galaxy_original$galaxysentimentrobo)


#inspect results
summary(galaxy_original)
str(galaxy_original)


#plot of the dependent galaxysentimentrobo with lables
ggplot(galaxy_original, aes(x=galaxysentimentrobo)) + geom_bar()

#downsampling/upsampling to solve the unbalance in the dependent variable (before feature selection and before modeling)
#ignacio says that downsampling is more correct

up_sample_g <- upSample(galaxy_original[,1:44], galaxy_original$galaxysentimentrobo) #upSample samples with replacement to make the class distributions equal
#2160 obs of 45 var

down_sample_g <- downSample(galaxy_original[,1:44], galaxy_original$galaxysentimentrobo) #downSample will randomly sample a data set so that all classes have the same frequency as the minority class.
#447 obs of 45 var

ggplot(up_sample_g, aes(x=Class)) + geom_bar() 
ggplot(down_sample_g, aes(x=Class)) + geom_bar() 

#renames the dependent variable
names(up_sample_g)[names(up_sample_g) == 'Class'] <- 'galaxysentiment'
names(down_sample_g)[names(down_sample_g) == 'Class'] <- 'galaxysentiment'

ggplot(down_sample_g, aes(x=galaxysentiment)) + geom_bar() 


#FEATURE SELECTION WITH 3 METHODS: 

#I will work on the balanced dataset "down_sample_g"
summary(down_sample_g)

#1. REMOVING REDUNDANT VARIABLES THROUGH CORRELATION (will create galaxy_relevant dataset)

correlationMatrixg <- cor(down_sample_g[,1:44])
print(correlationMatrixg)
highlyCorrelatedg <- findCorrelation(correlationMatrixg, cutoff=0.75, names = F)
print(highlyCorrelatedg)
length(highlyCorrelatedg) #33
names(down_sample_g[,highlyCorrelatedg])


#new dataset just with relevant attributes (13 = 12 attributes + 1 dependent)
galaxy_relevant <- down_sample_g[,-highlyCorrelatedg]
names(galaxy_relevant)

#correlation between attributes and dependent variable
#reload the dataset and take the dependent as numerical
galaxy_untouched <- read.csv("galaxy_smallmatrix_labeled_8d.csv", header = TRUE)
galaxy_untouched$galaxysentiment <- galaxy_sentiment_robo$galaxySentiment
cor(galaxy_untouched[,1:58], galaxy_untouched$galaxysentiment)
#non significant linear relationships

#computing sparsity of the dataset (= count zero elements / total elements)
sum(down_sample_g == 0)/(dim(down_sample_g)[1]*dim(down_sample_g)[2])
#0.80 (we have much more numbers than in iphone)




#2. RFE (will create galaxy_rfe dataset)


#creating the new dataset galaxy_rfe
galaxy_rfe = down_sample_g

#now there's no need to sample because we are dealing with 447 obs

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrlg <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 45 galaxysentiment) 
rfeResultsg <- rfe(galaxy_rfe[,1:44], 
                  galaxy_rfe$galaxysentiment, 
                  sizes=(1:44), 
                  rfeControl=ctrlg)

# Get results
rfeResultsg
#The top 5 variables (out of 18):
#iphone, iphoneperpos, iphonedisneg, htcdisneg, htccamneg

# Plot results
plot(rfeResultsg, type=c("g", "o"))

# create new data set with rfe recommended features
galaxy_rfe <- galaxy_rfe[,predictors(rfeResultsg)]

# add the dependent variable to galaxyRFE
galaxy_rfe$galaxysentiment <- down_sample_g$galaxysentiment

# review outcome
str(galaxy_rfe)

names(galaxy_rfe)


#2. NEAR ZERO VARIANCE
#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: 
#frequency ratio, percentage unique, zero variance and near zero variance 
nzvMetricsg <- nearZeroVar(down_sample_g, saveMetrics = TRUE)
nzvMetricsg

#nearZeroVar() with saveMetrics = FALSE returns an vector 
nzvg <- nearZeroVar(down_sample_g, saveMetrics = FALSE)
nzvg

##create a new data set and remove near zero variance features (22 attributes remain)
galaxyNZV <- down_sample_g[,-nzvg]
str(galaxyNZV)


names(galaxyNZV)


#DATASETS CREATED SO FAR:
#down_sample_g 45 vars
#galaxy_relevant 12 vars
#galaxy_rfe 19 vars
#galaxyNZV 22 vars


#creating the dataset given by the union of galaxy_relevant and galaxy_rfe attributes 
#for future comparisons with down_sample_g (45 var)


relevant_attributes <- names(galaxy_relevant)
rfe_attributes <- names(galaxy_rfe)

union_attributes = c(relevant_attributes, rfe_attributes)
as.vector(union_attributes)

class(union_attributes)

union_attributes <- unique(union_attributes) #for deleting duplicate columns coming from the two techniques
#they are 28 columns at the end

#creating thenew dataset union_down_sample_g (447*28)
union_down_sample_g <- down_sample_g[,union_attributes]

#reorder columns and putting galaxysentiment at the end of the dataset
union_down_sample_g <- union_down_sample_g[,c(1:11, 13:28, 12)]

dim(union_down_sample_g)

####----
#OUT OF THE BOX 
#MODELING on the down_sample_g dataset with 447 obs of 45 var

set.seed(998)

#define an 70%/30% train/test split of the dataset

inTraining_g <- createDataPartition(down_sample_g$galaxysentiment, p = .70, list = FALSE)


training_g <- down_sample_g[inTraining_g,] #315 obs in the training
testing_g <- down_sample_g[-inTraining_g,] #132 obs in the testing


#10 fold cross validation (10 folds, 3 repetitions, without up/downsampling because it was made before)
fitControl_g <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#train C5.0 model on training_g coming from downsample_g
set.seed(999)
C5.0_g <- train(galaxysentiment~., data = training_g, method = "C5.0", trControl=fitControl_g, metric = "Accuracy")


summary(C5.0_g)

#predictor variables
predictors(C5.0_g) #returns which predictors were used in the model 

#make predictions
testPredC5.0_g <- predict(C5.0_g, testing_g)

#performace measurment
metricC5.0_g <- postResample(testPredC5.0_g, testing_g$galaxysentiment)
metricC5.0_g

#Accuracy     Kappa 
#0.8560606 0.7840909 


#Create a confusion matrix from C5.0 predictions 
cmC5.0_g <- confusionMatrix(testPredC5.0_g, testing_g$galaxysentiment) 
cmC5.0_g



#train a RANDOM FOREST on training_g coming from downsample_g

set.seed(999)

rf_g <- train(galaxysentiment~., data = training_g, method = "rf", trainControl = fitControl_g)

testPredrf_g <- predict(rf_g, testing_g)

#performace measurment
metricrf_g <- postResample(testPredrf_g, testing_g$galaxysentiment)
metricrf_g
#Accuracy     Kappa 
#0.8787879 0.8181818  

#Create a confusion matrix from rf predictions 
cmrf_g <- confusionMatrix(testPredrf_g, testing_g$galaxysentiment) 
cmrf_g



#train a SVM RADIAL on training_g coming from down_sample
set.seed(999)
svmradial_g <- train(galaxysentiment~., data = training_g, method = "svmRadial", trControl=fitControl_g)

#make predictions
testPredsvmradial_g <- predict(svmradial_g, testing_g)

#performace measurment
metricsvmradial_g <- postResample(testPredsvmradial_g, testing_g$galaxysentiment)
metricsvmradial_g
#Accuracy     Kappa 
#0.8712121 0.8068182 


#Create a confusion matrix from SVM RADIAL predictions 
cmsvmradial_up_g <- confusionMatrix(testPredsvmradial_g, testing_g$galaxysentiment) 
cmsvmradial_up_g




#train knn model on training_g coming from down_sample
set.seed(999)
knn_g <- train(galaxysentiment~., data = training_g, method = "kknn", trControl=fitControl_g)

#make predictions
testPredknn_g <- predict(knn_g, testing_g)

#performace measurment
metricknn_g <- postResample(testPredknn_g, testing_g$galaxysentiment)
metricknn_g
#Accuracy     Kappa 
#0.8030303 0.7045455

#Create a confusion matrix from KNN predictions 
cmknn_g <- confusionMatrix(testPredknn_g, testing_g$galaxysentiment) 
cmknn_g


#THE BEST MODEL SO FAR IS RANDOM FOREST with the best performance on the test set

#MODELING on the union_down_sample_g dataset and compare it with the down_sample_g

training_union_g <- union_down_sample_g[inTraining_g,] #315 obs in the training
testing_union_g <- union_down_sample_g[-inTraining_g,] #132 obs in the testing

set.seed(999)
rf_union_g <- train(galaxysentiment~., data = training_union_g, method = "rf", trainControl = fitControl_g)

testPredrf_union_g <- predict(rf_union_g, testing_union_g)

#performace measurment
metricrf_union_g <- postResample(testPredrf_union_g, testing_union_g$galaxysentiment)
metricrf_union_g
#Accuracy     Kappa 
#0.8787879 0.8181818


#Create a confusion matrix from rf predictions 
cmrf_union_g <- confusionMatrix(testPredrf_union_g, testing_union_g$galaxysentiment) 
cmrf_union_g



#importing the galaxylargematrix

galaxylargematrix <- read.csv("galaxylargematrix.csv", header = TRUE)

colnames(training_union_g)
colnames(galaxylargematrix)

#restricting the dataset only to the relevant variables used to train before in the training union dataset
galaxylargematrix_attributes <- galaxylargematrix[,colnames(training_union_g)]

colnames(galaxylargematrix_attributes)

str(galaxylargematrix_attributes)

head(galaxylargematrix_attributes)

#changing type of predictors from factor to integer
for(i in c(1:27)) {
  galaxylargematrix_attributes[,i] <- as.integer(as.character(galaxylargematrix_attributes[,i]))
}

str(galaxylargematrix_attributes)


dim(galaxylargematrix_attributes)

Predrf_galaxylarge <- predict(rf_union_g, galaxylargematrix_attributes)

summary(Predrf_galaxylarge)
#Negative  Neutral Positive 
####700    33878    49609 

prop.table(table(Predrf_galaxylarge))
#Negative     Neutral    Positive 
#0.008314823 0.402413674 0.589271503 


# create a data frame for plotting.
# you can add more sentiment levels if needed
# Replace sentiment values 
pieData_galaxy <- data.frame(COM = c("negative", "neutral", "positive"), 
                             values = c(700, 33878, 49609))


require(plotly)

# create pie chart
plot_ly(pieData_galaxy, labels = ~COM, values = ~ values, type = "pie",
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste( values),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        showlegend = F) %>%
  layout(title = 'Galaxy Sentiment', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


dim(galaxylargematrix)

#Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)

