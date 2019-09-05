
#Installing packages reuired  
#install.packages("caret")
#install.packages("NeuralNetTools")
#install.packages("pROC")
#Loading required libraries
library(caret)
library(NeuralNetTools)
require(pROC)


#Reading data from the working directory
CC<- read.csv("risk.csv", header=TRUE)

#Setting seed for reproducibility and constant results
set.seed(2)

#Shuffling the data
n <- nrow(CC)
shuffled_data <- CC[sample(n),]

#Splitting the data into 70% - 30%
Cervical_train <- 1:round(0.7 * n)
training <- shuffled_data[Cervical_train, ]
Cervical_test<- (round(0.7 * n) + 1):n
testing <- shuffled_data[Cervical_test, ]

#Training five models namely Random forest, GBM,C5.0,Neural net, KNN
Randomf <- train(as.factor(Biopsy) ~., data=training, method='rf')

#Summary of the model
print(Randomf)

#Variable Importance plot
v<- varImp(Randomf, scale = FALSE)
plot(v)

# Training model GBM
Gbmm <- train(as.factor(Biopsy) ~., data=training, method='gbm')

#Summary of the model
print(Gbmm)

#Training Decision tree (C5.0) model
c5.0<- train(as.factor(Biopsy) ~., data=training, method='C5.0')

#Summary of C5.0 model
print(c5.0)

#Training Neural Net (Deep Learning) model
nnetm <- train(as.factor(Biopsy) ~., data=training, method='nnet')

#Result of the model
print(nnetm)

plotnet(nnetm)
plot(nnetm)

#Training K-Nearest Neighbour
knnm <- train(as.factor(Biopsy) ~., data=training, method='knn')
plot(knnm)
print(knnm)

#Predicting model with testing values
r1 <- predict(Randomf, newdata=testing)
r2 <- predict(Gbmm, newdata=testing)
r3 <- predict(c5.0, newdata=testing)
r4 <- predict(nnetm, newdata=testing)
r5 <- predict(knnm, newdata=testing)

#Creating data frame from predictor table
Dframe <- data.frame(r1, r2,r3,r4,r5,  Biopsy = testing$Biopsy)

#ensembling using extreme gradient boosting
ens <- train(as.factor(Biopsy) ~ ., data = Dframe, method = "xgbLinear",
             trcontrol = trainControl(method = 'cv', number = 10, verboseIter = TRUE ))
print(ens)

#Predictor model for ensemble
r6 <- predict(ens, newdata = testing)

#Plotting ROC curve for ensemble model
preprobs <- predict(ens,testing,type='prob')
roc<- multiclass.roc(testing$Biopsy,preprobs[,2])
print(roc)
auc(roc)
plot(roc(testing$Biopsy,preprobs[,2]))

#Creating confusion matrix for diifernet models
cmatrix1 <- confusionMatrix(table(r1, testing$Biopsy),positive = "1") 
cmatrix1

cmatrix2 <- confusionMatrix(table(r2, testing$Biopsy),positive = "1") 
cmatrix2

cmatrix3 <- confusionMatrix(table(r3, testing$Biopsy),positive = "1") 
cmatrix3

cmatrix4 <- confusionMatrix(table(r4, testing$Biopsy),positive = "1") 
cmatrix4

cmatrix5 <- confusionMatrix(table(r5, testing$Biopsy),positive = "1") 
cmatrix5

#Creating confusion matrix for ensemble model
cmatrix6 <- confusionMatrix(table(r6, testing$Biopsy),positive = "1")
cmatrix6


#=================================================================================
#Information Gain for feature selection
#install.packages("FSelector")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-11.0.1')
library(FSelector)
weights <- information.gain(Biopsy~., CC)
weights

weights <- gain.ratio(Biopsy~., CC)
weights

weights <- symmetrical.uncertainty(Biopsy~.,CC)
weights
