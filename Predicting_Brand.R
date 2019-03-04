getwd()

#setwd()

#rm(list = ls(all = TRUE))
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("ModelMetrics")
#install.packages("RcppRoll")
#install.packages("bindrcpp")
#install.packages("backports")
#install.packages("ddalpha")
#install.packages("DEoptimR")
#install.packages("dimRed")
#install.packages("gower")

library(caret)


###----------------
#PREDICTING BRAND PREFERENCE
#rm(list = ls(all = TRUE))
library(e1071)


#importing data
Surveycomplete <- read.csv(file="C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task2_Classification Predict which Brand of Products Customers Prefer/Survey_Key_and_Complete_Responses_excelcsv.csv", header=TRUE, sep=";")
Surveycomplete
str(Surveycomplete)
summary(Surveycomplete)
head(Surveycomplete)

#changing the class of the dependent variable Brand to a factor
Surveycomplete$brand <- factor(Surveycomplete$brand, levels = c(0,1), labels = c("Acer", "Sony"))
class(Surveycomplete$brand)
levels(Surveycomplete$brand)
head(Surveycomplete)

#changing the class of the predictor education level from integer to ordered factor
#recoding elevel to 0,1,2,3,4 and associating the right labels to those levels
#R now treats elevel as ordinal variable

Surveycomplete$elevel <- ordered(Surveycomplete$elevel, 
                                 levels = c(0,1,2,3,4),
                                 labels = c("Less than High School Degree", "High School Degree", "Some College", "4-Year College Degree", "Master's, Doctoral or Professional Degree"))

#check that elevel  is now ordered factor
class(Surveycomplete$elevel)
head(Surveycomplete)
str(Surveycomplete)
summary(Surveycomplete)

#changing the class of the predictor zipcode from integer to factor with the right levels and labels associated
Surveycomplete$zipcode <- factor(Surveycomplete$zipcode, 
                                    levels = c(0,1,2,3,4,5,6,7,8),
                                    labels = c(
                                    "New England",
                                    "Mid-Atlantic",
                                    "East North Central",
                                    "West North Central",
                                    "South Atlantic",
                                    "East South Central",
                                    "West South Central",
                                    "Mountain",
                                    "Pacific"))                            
class(Surveycomplete$zipcode)
head(Surveycomplete)



#changing the class of car to factor
Surveycomplete$car <- factor(Surveycomplete$car)


###---------------
#trial exercise, running a logistic model on brand and all the columns as predictors
mylogit <- glm(brand ~ ., data = Surveycomplete, family = "binomial")
summary(mylogit)
?glm
confint(mylogit)
predictors(mylogit)

#the variable car has to be deleted from the dataset because I do not see any correlation between the 
#primary car used and the brand preferred in this specific case - first of all the dependent variable has 
#just two values (Acer and Sony), representing computer products of the same range (if we had for example Apple and Lenovo, 
#the primary car could have an impact on the computer preferred)
#second, it has too many distinct values (20) and it could be hard to handle and interpret all of them
#to avoid noise coming from this variable, I will remove it!

####-------------
#deleting the car variable
Surveycomplete$car <- NULL
head(Surveycomplete)


#studying the distribution of the numerical predictors through histograms and density plots
par(mfrow = c(1, 3))
hist(Surveycomplete$salary, main = "Distribution of Salary")
hist(Surveycomplete$age, breaks=40, col="red", main = "Distribution of Age")
hist(Surveycomplete$credit, breaks=40, col = "green", main = "Distribution of Credit")  #qui cambia i numeri sull'asse delle x

  # Filled Density Plots
  par(mfrow = c(1, 3))
  
  d_age <- density(Surveycomplete$age)
  plot(d_age, main="Kernel Density of age")
  polygon(d_age, col="red", border="blue")

  d_salary <- density(Surveycomplete$salary)
  plot(d_salary, main="Kernel Density of salary")
  polygon(d_salary, col="red", border="blue")

  d_credit <- density(Surveycomplete$credit)
  plot(d_credit, main="Kernel Density of credit")
  polygon(d_credit, col="red", border="blue")

#numerical variables vs brand
  
library(ggplot2)
  
par(mfrow = c(1, 3))
  
ggplot(Surveycomplete, aes(x=age, fill=brand)) +
geom_histogram()


ggplot(Surveycomplete, aes(x=salary, fill=brand)) +
  geom_histogram()

ggplot(Surveycomplete, aes(x=credit, fill=brand)) +
  geom_histogram()
  
  
#studying the distribution of categorical and ordinal predictors
par(mfrow = c(1, 2))
barplot(table(Surveycomplete$elevel), main = "Education level distribution")
barplot(table(Surveycomplete$zipcode), main = "Zipcode distribution")

#N.B. almost all the predictors seem to be uniformly distributed across the values
#this will not bring particular/interesting/additional information when trying to predict the response!
#looking for OUTLIERS (boxplots)

#bar stacked 
#TO DO: REDUCE THE SIZE OF THE LEGEND 
par(mfrow = c(1, 1))
countscomp <- table(Surveycomplete$elevel, Surveycomplete$zipcode)
barplot(countscomp, main="Clients Distribution by zipcode and elevel - Complete survey", 
        xlab="zipcode", col=c("ghostwhite","gold","cyan3", "steelblue", "slateblue"),
        legend.text = c("Less than High School Degree", "High School Degree","Some College","4-Year College Degree","Master's, Doctoral or Professional Degree"), args.legend = list(x="topright"))

barplot(countscomp, main="Clients Distribution by zipcode and elevel - Complete survey", 
        xlab="zipcode", ylab = "Frequency", col=c("ghostwhite","gold", "cyan3", "steelblue", "slateblue"))#,
       # legend.text = c("Less than High School Degree", "High School Degree","Some College","4-Year College Degree","Master's, Doctoral or Professional Degree"), args.legend = list(x="topright"))


Surveycomplete$brand

#distribution of brand across zipcodes
par(mfrow = c(1, 1))
countscompzipbrand <- table(Surveycomplete$brand, Surveycomplete$zipcode)
barplot(countscompzipbrand, main="Brand Preference across zipcodes - Complete survey", 
        xlab="zipcode", ylab = "Frequency",
        col=c("khaki1","yellowgreen"),
        legend.text = c("Acer", "Sony"))

#distribution of brand across education levels
par(mfrow = c(1, 1))
countscompelevelbrand <- table(Surveycomplete$brand, Surveycomplete$elevel)
barplot(countscompelevelbrand, main="Brand Preference across Education Levels - Complete survey", 
        xlab="Education Level", ylab="Frequency", col=c("khaki1","yellowgreen"),
        legend.text = c("Acer", "Sony"))


par(mfrow = c(1, 3))
boxplot(Surveycomplete$salary, main="Salary", sub=paste("Outlier rows: ", boxplot.stats(Surveycomplete$salary)$out))  # box plot for salary
boxplot(Surveycomplete$age, main="Age", sub=paste("Outlier rows: ", boxplot.stats(Surveycomplete$age)$out))  # box plot for age
boxplot(Surveycomplete$credit, main="Credit", sub=paste("Outlier rows: ", boxplot.stats(Surveycomplete$credit)$out))  # box plot for credit
#there are no outliers!

#decision tree to detect the relevant variables wrt brand. rpart package to visualize it
#install.packages("rpart")
#install.packages("rpart.plot")

library("caret")
library("rpart")
library("rpart.plot")

par(mfrow = c(1, 1))

tree_var <- rpart(brand~., data = Surveycomplete, cp=.02)
rpart.plot(tree_var, box.palette = "RdBu", shadow.col = "gray", nn = T)
# the most significant variables to predict brand are, by descending order: salary and age. 
#The rest of the variables plays no role in predicting brand. 
tree_var

#Variable importance -> ASK
#age  salary zipcode  credit 
#62      35       1       1 


summary(tree_var)

#scatterplot: salary, age, brand
ggplot(Surveycomplete, aes(x=age, y=salary, shape = factor(brand))) + 
  geom_point(aes(colour = factor(brand)), size = 4) +
  geom_point(colour = "grey90", size = 1.5)

##splitting data in brand Acer and brand Sony
MyDataAcer  <- subset(Surveycomplete, brand == "Acer")
MyDataSony  <- subset(Surveycomplete, brand == "Sony")
head(MyDataAcer)

##Salary binning distribution in ACER and SONY
par(mfrow = c(1, 2))

#binning  salary in acer
b <- c(-Inf, 50000, 110000, Inf)
b
names <- c("Low Salary [20-50K]", "Medium Salary [50K-110K]", "High Salary [110K-150K]")
#head(Surveycomplete)
salarybinacer.cat <- cut(MyDataAcer$salary, breaks = b, labels = names)
summary(MyDataAcer)
barplot(table(salarybinacer.cat), main = "salary binned dist on ACER")

#binning  salary in sony
salarybinsony.cat <- cut(MyDataSony$salary, breaks = b, labels = names)
barplot(table(salarybinsony.cat), main = "salary binned dist on SONY")


#studying the distribution of the DEPENDENT VARIABLE BRAND in the whole dataset
summary(Surveycomplete) 
# we have 3783 rows corresponding to Acer and 6217 rows corresponding to Sony
#this unbalance in the dependent variable could lead to biased predictions and misleading accuracies.
# we should address this issue before training any model...or maybe it's not necessary in this case!?
#https://www.linkedin.com/pulse/dealing-imbalanced-classification-problems-case-study-victor-asila/


#Partitioning the data into training and testing data
set.seed(101)

#getting the indexes of the training through a stratified random split of the data
indexSurvey = createDataPartition(Surveycomplete$brand, p = 0.75, list = F )
#N.B. the argument list = F forces the results to be in a matrix

#creating training set and test set
trainSurvey = Surveycomplete[indexSurvey,]
testSurvey = Surveycomplete[-indexSurvey,]

# Explore data partition
dim(trainSurvey)
#nrow(trainSurvey) another way to do the same
dim(testSurvey)
#nrow(testSurvey) another way to do the same
names(trainSurvey)
head(trainSurvey)
head(testSurvey)

# Setting levels for both training and testing data
#levels(trainSurvey$brand) <- make.names(levels(factor(trainSurvey$brand))) gives authomatically names X0 and X1
levels(trainSurvey$brand) <- c("Acer", "Sony")
#levels(testSurvey$brand) <- make.names(levels(factor(testSurvey$brand))) gives authomatically names X0 and X1
levels(testSurvey$brand) <- c("Acer", "Sony")
levels(trainSurvey$brand)
levels(testSurvey$brand)

#N.B. 0 is Acer and 1 is Sony.

# Setting up train controls: 3 separate 10-fold validations are used. 
#tunelength is always an integer which is used to tune our algorithm.
repeats = 3 #number of repetitions
numbers = 10 #number of folds k (in the cv)
#tunel = 10 #tunelength is how many tuning parameters are evaluated (random numbers of k - usually short)
#another parameter is tunegrid
rdaGrid = data.frame(k = (3:6)) #takes numbers from 3 to 6 for the number of nearest neighbours
#TO DO: RUN THIS FOR A LARGER RANGE OK K (and only for odd numbers), and also srqt(number of obs in the training set)

x = trainControl(method = "repeatedcv", number = numbers, repeats = repeats, classProbs = TRUE)
#method is the type of resampling; repeated cv is used to specify repeated K-fold cross-validation



###------------KNN WITH 5 PREDICTORS

#TO DO: consider also the ROC metric in the future!
#N.B. when specifying the dependent variable in the train function, just write the name of the variable, without referring to the dataset!
modelKNN <- train(brand~., data = trainSurvey, method = "knn", preProcess = c("center","scale"),trControl = x,metric = "Accuracy",tuneGrid = rdaGrid)
modelKNN
#TO DO: predictors also can be written after the tilde; salary + age

#TO DO: run the weighted knn, to give the right weight to categorical predictors while computing distances

###We have centered and scaled the predictors for the training
#the best model is the one with k = 3 (best accuracy, best kappa)

#N.B. Lastly, the trainControl function will pick the tuning parameters associated with the best results
#In the output the grid of results are the average resampled estimates of performance. The note at the bottom tells the user that knn with k=3 was found to be optimal. 
#Based on this value, a final knn model is fit to the whole data set using this specification and this is the model that is used to predict future samples.

# Summary of KNN; simpleplot
#plot(modelKNN)


#nicer plot for summary of KNN
#install.packages("labeling")
library("labeling")
#The following figure shows the relationship between the resampled performance values and the number of knn neighbours.
ggplot(modelKNN)
#N.B. ADD this graph to the report!

#predict on the test set
KNNClasses <- predict(modelKNN, newdata = testSurvey)
str(KNNClasses)
KNNClasses #returns the predicted class for every obs of the test set
KNNProbs <- predict(modelKNN, newdata = testSurvey, type = "prob") #
KNNProbs #returns the class probabilities from the model
head(KNNProbs)

#compute the confusion matrix and associated statistics for the KNN model fit   
confusionMatrix(data = KNNClasses, testSurvey$brand)

###-----------KNN WITH 2 PREDICTORS

#preparing the dataset, leaving only salary, age (predictors) and brand (label) as variables
Surveycomplete2pred <- Surveycomplete[, c(1,2,6)]
Surveycomplete2pred 

trainSurvey2pred <- trainSurvey[,c(1,2,6)]
head(trainSurvey2pred)
testSurvey2pred <- testSurvey[,c(1,2,6)]
head(testSurvey2pred)

modelKNN2pred <- train(brand~., data = trainSurvey2pred, method = "knn", preProcess = c("center","scale"),trControl = x,metric = "Accuracy",tuneGrid = rdaGrid)
modelKNN2pred

KNNClasses2pred <- predict(modelKNN2pred, newdata = testSurvey2pred)
str(KNNClasses2pred)
KNNClasses2pred #returns the predicted class for every obs of the test set
KNNProbs2pred <- predict(modelKNN2pred, newdata = testSurvey2pred, type = "prob") #
KNNProbs2pred #returns the class probabilities from the model
head(KNNProbs2pred)

#compute the confusion matrix and associated statistics for the KNN model fit   
confusionMatrix(data = KNNClasses2pred, testSurvey2pred$brand)


####---------
#DECISION TREE "Single C5.0 Tree" -> C5.0Tree WITH 5 PREDICTORS

#install.packages("C50")
#install.packages("inum")

library(C50)
modelTree <- train(brand~., data = trainSurvey, method = "C5.0Tree", preProcess = c("center","scale"),trControl = x, metric = "Accuracy")
modelTree

TreeClasses <- predict(modelTree, newdata = testSurvey)
confusionMatrix(TreeClasses, testSurvey$brand)


####------------
#DECISION TREE "Single C5.0 Tree" -> C5.0Tree WITH 2 PREDICTORS

#install.packages("C50")
#install.packages("inum")

library(C50)
modelTree2pred <- train(brand~., data = trainSurvey2pred, method = "C5.0Tree",trControl = x, metric = "Accuracy")
modelTree2pred

TreeClasses2pred <- predict(modelTree2pred, newdata = testSurvey2pred)
confusionMatrix(TreeClasses2pred, testSurvey2pred$brand)


##-------RANDOM FOREST WITH 2 PREDICTORS

install.packages("randomForest")
library(randomForest)
#head(trainSurvey2pred)
#find the optimal mrtry 
bestmtry2pred <- tuneRF(trainSurvey2pred[,1:2], trainSurvey2pred[,3], stepFactor = 1.5, improve = 1e-5, ntree = 30)
#bestmtry is 1

##TO DO: re-run the best mtry for rf considering all the 5 predictors

#running the random forest with mtry = 1 and 100 trees
#training
#forest2pred100trees <- train(brand~., data = trainSurvey2pred, method = "rf",trControl = x, metric = "Accuracy", tuneGrid = expand.grid(mtry = 1), ntree = 100)
#forest2pred100trees
#rfClasses100 <- predict(forest2pred100trees, newdata = testSurvey2pred)
#confusionMatrix(rfClasses100, testSurvey2pred$brand)


#random forest with mtry = 1 and 600 trees <- BEST RF AMONG ALL
forest2pred600trees <- train(brand~., data = trainSurvey2pred, method = "rf",trControl = x, metric = "Accuracy", tuneGrid = expand.grid(mtry = 1), ntree = 600)
forest2pred600trees
rfClasses600 <- predict(forest2pred600trees, newdata = testSurvey2pred)
confusionMatrix(rfClasses600, testSurvey2pred$brand)

##THE MODEL THAT I AM GOING TO USE IS KNN , k = 5 with two predictors.

#How do these models compare in terms of their resampling results? 
#The resamples function can be used to collect, summarize and contrast the resampling results.
resamps <- resamples(list(KNN = modelKNN2pred, tree = modelTree2pred))
summary(resamps)

#function to visualize these results
xyplot(resamps, what = "BlandAltman") 

#Since, for each resample, there are paired results, a paired t-test can be used to assess 
#whether there is a difference in the average resampled accuracies.

diffs <- diff(resamps)
diffs
summary(diffs)

###------------------
#RANDOM FOREST  Random Forest	ranger	Classification, Regression	e1071, ranger, dplyr	mtry, splitrule, min.node.size
#Random Forest
library("caret")
#install.packages("e1071")
library("e1071")
#install.packages("ranger")
library("ranger")
#install.packages("dplyr")
library("dplyr")


####------
#PREDICTING BRAND IN MISSING CASES

#predicting on missing cases with "modelKNN2pred"

Surveyincomplete <- read.csv(file="C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task2_Classification Predict which Brand of Products Customers Prefer/SurveyIncomplete.csv", header=TRUE, sep=",")
head(Surveyincomplete)

predictions <- predict(modelKNN2pred, newdata = Surveyincomplete)
head(predictions)
str(predictions)
predictions #returns the predicted class for every obs of the test set

#adding the new column of predictions in the dataset
Surveyincomplete$predictions <- predictions
head(Surveyincomplete)

#scatterplot of salary, age, brand
ggplot(Surveyincomplete, aes(x=age, y=salary, shape = factor(predictions))) + 
  geom_point(aes(colour = factor(predictions)), size = 4) +
  geom_point(colour = "grey90", size = 1.5)


summary(Surveyincomplete)
summary(Surveycomplete)

par(mfrow = c(1, 3))
hist(Surveyincomplete$salary, main = "Distribution of Salary")
hist(Surveyincomplete$age, breaks=40, col="red", main = "Distribution of Age")
hist(Surveyincomplete$credit, breaks=40, col = "green", main = "Distribution of Credit")  #qui cambia i numeri sull'asse delle x

# Filled Density Plots
par(mfrow = c(1, 3))

d_age <- density(Surveyincomplete$age)
plot(d_age, main="Kernel Density of age")
polygon(d_age, col="red", border="blue")

d_salary <- density(Surveyincomplete$salary)
plot(d_salary, main="Kernel Density of salary")
polygon(d_salary, col="red", border="blue")

d_credit <- density(Surveyincomplete$credit)
plot(d_credit, main="Kernel Density of credit")
polygon(d_credit, col="red", border="blue")


#studying the distribution of categorical and ordinal predictors
par(mfrow = c(1, 2))
barplot(table(Surveyincomplete$elevel), main = "Education level distribution")
barplot(table(Surveyincomplete$zipcode), main = "Zipcode distribution")

# Stacked Bar Plot with Colors and Legend
par(mfrow = c(1, 1))
counts <- table(Surveyincomplete$elevel, Surveyincomplete$zipcode)
barplot(counts, main="Clients Distribution by zipcode and elevel - Incomplete survey",
        xlab="zipcode", col=c("khaki1","yellowgreen", "cyan3", "steelblue", "slateblue"),
        #legend = rownames(counts)
        legend.text = c("Less than High School Degree", "High School Degree","Some College","4-Year College Degree","Master's, Doctoral or Professional Degree"), args.legend = list(x="topright"))


#combining the brand from the complete and the predictions from the incomplete

par(mfrow = c(1, 1))
complete <- Surveycomplete$brand
incomplete <- Surveyincomplete$predictions
tabcomp <- table(complete)
tabincomp <- table(incomplete)
pie(tabcomp, main = "Brand preference of 10K clients")


#acer and sony distribution in complete survey
par(mfrow = c(1, 2))
lbls <- c("Acer", "Sony")
pct <- round(tabb/sum(tabb)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(tabcomp,labels = lbls, col=c("khaki1","yellowgreen"),
    main="Brand Preference of 10K RESPONDENT Clients")

#acer and sony distribution in incomplete survey
lbls <- c("Acer", "Sony")
pct <- round(tabb/sum(tabb)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(tabincomp,labels = lbls,col=c("khaki1","yellowgreen") ,
    main="Brand Preference of 5K NOT RESPONDENT Clients")


tabb <- table(complete) + table(incomplete)
tabb
  #barplot
barplot(tabb, main = "Brand preference of all CB")


  #simple PIE
pie(tabb, main = "Brand preference of 15000 clients")


  #Nicer Pie Chart with Percentages

lbls <- c("Acer", "Sony")
pct <- round(tabb/sum(tabb)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(tabb,labels = lbls, col=rainbow(length(lbls)),
    main="Brand Preference of all CB")


pie(tabb,labels = lbls, col = c("yellow", "orange"),
    main="Brand Preference of all CB")

