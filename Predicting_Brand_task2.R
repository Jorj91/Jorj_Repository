getwd()

#setwd()

rm(list = ls(all = TRUE))
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

##--------------------------------------- 
#(EXAMPLE FROM THE RESOURCES) LINEAR REGRESSION (WHOLEYEAR)
#IMPORTING DATA

WholeYear <- read.csv(file="C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task2_Classification Predict which Brand of Products Customers Prefer/WholeYear.csv", header=TRUE, sep=",")


set.seed(998)


#define an 75%/25% train/test split of the dataset

inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)


training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

training
testing



#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

??fitControl

#train Stepwise Linear regression model
LMFit1 <- train(SolarRad~., data = training, method = "leapSeq", trControl=fitControl)

#predictor variables
predictors(LMFit1) #returns which predictors were used in the model 


#make predictions
testPredLM1 <- predict(LMFit1, testing)


#performace measurment
ab <- postResample(testPredLM1, testing$SolarRad)
ab 
?postResample #Given two numeric vectors of data, the mean squared error and R-squared are calculated. 
#For two factors, the overall agreement rate and Kappa are determined.

#plot predicted verses actual (in test)
plot(testPredLM1,testing$SolarRad)

##END OF RESOURCE EXAMPLE 



###TASK2 -------------------
#PREDICTING BRAND PREFERENCE
#rm(list = ls(all = TRUE))
#?rm
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


#studying the distribution of categorical and ordinal predictors
par(mfrow = c(1, 2))
barplot(table(Surveycomplete$elevel), main = "Education level distribution")
barplot(table(Surveycomplete$zipcode), main = "Zipcode distribution")

#N.B. almost all the predictors seem to be uniformly distributed across the values
#this will not bring particular/interesting/additional information when trying to predict the response!
#looking for OUTLIERS (boxplots)

par(mfrow = c(1, 3))
boxplot(Surveycomplete$salary, main="Salary", sub=paste("Outlier rows: ", boxplot.stats(Surveycomplete$salary)$out))  # box plot for salary
boxplot(Surveycomplete$age, main="Age", sub=paste("Outlier rows: ", boxplot.stats(Surveycomplete$age)$out))  # box plot for age
boxplot(Surveycomplete$credit, main="Credit", sub=paste("Outlier rows: ", boxplot.stats(Surveycomplete$credit)$out))  # box plot for credit
#there are no outliers!
?boxplot

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

#...??? why not the opposite?
#?rpart

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


#IDEM ON AGE_ (TO DO ONE DAY)

#binning age 
#d <- c(-Inf, 40, 60, Inf)
#d
#names_d <- c("Adult [20-40]", "Middle-Aged [40-60]", "Old [60-80]")
#head(Surveycomplete)
#Surveycomplete$agebin.cat <- cut(Surveycomplete$age, breaks = d, labels = names_d)
#Surveycomplete


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

#?trainControl

#by using train() function we run:

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
#?tuneRF
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
?resamples

#function to visualize these results
xyplot(resamps, what = "BlandAltman") 
#TO DO: better interpret the graph


#Since, for each resample, there are paired results, a paired t-test can be used to assess 
#whether there is a difference in the average resampled accuracies.

diffs <- diff(resamps)
diffs
summary(diffs)

?diff

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


#RANDOM FOREST WITH DEFAULT SETTINGS 
#modelRF <- train(brand~., data = trainSurvey, method = "ranger", preProcess = c("center","scale"),trControl = x,metric = "Accuracy")
#modelRF

##RF with mtry = 8, splitrule = gini and min.node.size = 1 is the best RF 

library("caret")
#mtry: Number of variables randomly sampled as candidates at each split
#mtry <- sqrt(ncol(trainSurvey))
#rm(mtry)
#tunegrid <- expand.grid(.mtry=mtry)
#tunegrid <- expand.grid(mtry = 7, num.trees = 38)

#tunegrid

#the following does not work
#rf_1 <- train(brand~., data=trainSurvey, method="ranger", metric="Accuracy", num.trees = 38, trControl=x)

#rf_1 <- train(brand~., data=trainSurvey, method="ranger", metric="Accuracy", trControl=x)
#rf_1

?rf
#rf_ <- train(brand~., data=trainSurvey, method="rf", metric="Accuracy", trControl=x, ntree=30, mtry = 3)
#rf_


#install.packages("randomForest")
#library("randomForest")
#?tuneRF
#bestmtry <- tuneRF(trainSurvey[,1:5], trainSurvey[,6], stepFactor=1.5, improve=1e-5, ntree=200)
#print(bestmtry)
#mtry optimal is 3... so this is a significance test 

#modelRFmtryopt <- train(brand~., data = trainSurvey, method = "rf", trControl = x,metric = "Accuracy", mtry = bestmtry)

#library("caret")
#modelRF <- train(brand~., data = trainSurvey, method = "ranger", preProcess = c("center","scale"),trControl = x,metric = "Accuracy")
#modelRF


#method = 'ranger'
#Type: Classification, Regression

#Tuning parameters:
  
 # mtry (#Randomly Selected Predictors)
  #  splitrule (Splitting Rule)
   # min.node.size (Minimal Node Size)
    #Required packages: e1071, ranger, dplyr
    
    #A model-specific variable importance metric is available.

str(Surveycomplete)


#modelRF <- train(brand~., data = trainSurvey, method = "ranger", trControl = x, metric = "Accuracy")
#modelRF

#TreeClasses <- predict(modelTree, newdata = testSurvey)
#confusionMatrix(TreeClasses, testSurvey$brand)

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

#combining the brand from the complete and the predictions from the incomplete

par(mfrow = c(1, 1))
complete <- Surveycomplete$brand
incomplete <- Surveyincomplete$predictions
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

#TO DO: CHANGE HORRIBLE COLORS OF THE PIE!

#d1$colours <- c("orange", "red", "blue", "yellow", "green") <- possible values for col
pie(tabb,labels = lbls, col = c("yellow", "orange"),
    main="Brand Preference of all CB")



#TO DO LIST AFTER CODE REVIEW WITH JAVIER

#pacman package (package manager)
#function: read_excel
#function: replace (with numerical) and function revalue (with factors)
#prp function (belonging to rpart) for plotting the tree
#to change the 2 barplot colors: col=c(rgb(0.2, 0.4, 0.6), "lightgreen") 
#(the first one is a combination of red, green and black)
#check r graph gallery to do nicer charts


#CODE REVIEW ALL TOGETHER
#LARA
#for loop with if condition inside
#if.. else... ifelse

#library(ggplot)
#my_plot <- ggplot(df) +
#aes(x= , y=)+
#geom_point()+
#geom_line()+
#ggtitle("....")+
#scale_x_continuous

#library(plotly) can do plots by itself, but also can access ggplot. it does interactive plots!
#and also it identifies who are the guys in the scatterplots for example (nrow and ncol of the obs)

#for loop to run different number of trees in the rf (10,20,50,100)

#N.B. list can include different kinds of objects (character, integer...), whilst the vector only objects of the same type

#geom_jitter() in ggplot of salary, age and brand



#LESSON OF IGNACIO ABOUT LOOPS

#FOR LOOP -> if I know how many times I want to repeat the instructions
#WHILE LOOP -> if I do not know it

#counters are variables to keep track of how many times the tasks has been done. They are defined initialized in the loop
#you can call them whatever


#accumulators are variables to store something (to add values)
#they can be: integer, real, vectors, lists

#in the while loop the instructions inside the braces are executed while the condition within braces is met
##for instance, while i <=10 ...

#FUNCTIONS IN R
#n.b. in general, the order of arguments matter!!!!!!!!! (not in the specific case of addition)
#n.b. write the functions at the beginning of the code
#right after inspecting your dataset and before doing anything else!
#comment the purpose of the function

#FIBONACCI SERIES
#1,1,2,3,5,8,13,21,34,55,89,...
#write a function to print the ten first numbers of the fibonacci
#with a for loop or with a while loop

fibonacci <- function(n){ 
  var <- c(1,1)
  while (length(var) < n) 
  { new <- var[length(var)] + var[length(var)-1]
  var <- c(var,new)} 
  return(var)
}

fibonacci(10)


