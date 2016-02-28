
library(caret)
library(rpart)
library(randomForest)
library(rattle)
library(rpart.plot)

#Download and store file into local folder and read the data

traindata <- read.csv("D:/PracticalMachineLearning_Assignment/data/pml_training.csv", na.strings=c("NA",""), header=TRUE)
testdata <- read.csv("D:/PracticalMachineLearning_Assignment/data/pml_testing.csv", na.strings=c("NA",""), header=TRUE)

# Checking for the number of variables and observations
cat("Dimension of pml-training.csv : ",dim(traindata)); cat("Dimension of pml-testing.csv : ", dim(testdata));

### Pre-process Data:

# Remove those variables (user_name  raw_timestamp_part_1	raw_timestamp_part_2	cvtd_timestamp	new_window	num_window) which will not provide much weightage for prediction
# Remove near zero covariates
# Remove Columns with more than 80% missing values

Cleandata <- function(dataFrame){
# 1.  Deleting variables containing User,timestamp and window size which are not used ( Columns 1 to 7)
    subsetTraining <- dataFrame[,-c(1:7)] 

# 2. Remove near Zero covariates  
    nsv <- nearZeroVar(subsetTraining, saveMetrics = T)
    subsetTraining <- subsetTraining[,!nsv$nzv]

# 3. Remove Variables  that have more than  80% threshold of NA's 
  nav <- sapply(colnames(subsetTraining), function(x) 
  if(sum(is.na(subsetTraining[, x])) > 0.8*nrow(subsetTraining)){return(T)}else{return(F)})
    procTraining <- subsetTraining[, !nav]

}
Cleantrain<-Cleandata(traindata);
dim(Cleantrain); 


#The training set has  `r nrow(Cleantrain)`  samples and  `r ncol(Cleantrain) - 1`  (excluding the "classe" variable) potential predictors after cleaning.

### Partition Data:
#To estimate the out-of-sample error,  randomly split the full training data into two sets as training (70%) and testing (30%).

# split data into two parts
set.seed(123)
inTrain <- createDataPartition(y=Cleantrain$classe, p=0.7, list=FALSE)
training <- Cleantrain[inTrain,]
testing <- Cleantrain[-inTrain,]

## Machine Learning Models Evaluation Process
### Train Models: 
#Train the Splitted data with **classification tree and Random Forest Model **

modFitCtm <- rpart(classe ~ ., data=training, method="class")
#fancyRpartPlot(modFitCtm,under.cex= 0.9) # Plot nder text is too small to view in html
rpart.plot(modFitCtm, main="Fig: 1 Classification Tree", extra=102, under=TRUE, faclen=0)


## Random forest Model
modFitRfm <- randomForest(classe ~. , data=training);
varImpPlot(modFitRfm ,main="Fig: 2 Random Forest Features Ranking ")


### Predict Test data with Trained Models:
#To find the best fitted model, test our splitted 30 % of test dataset against each model as shown below


prediction1 <- predict(modFitCtm, testing,type = "class")
ctm<-confusionMatrix(prediction1, testing$classe)
ctm$table


#- **Predict Testing Data with Random forest Model  **

prediction2 <- predict(modFitRfm, testing)
rfm<-confusionMatrix(prediction2, testing$classe)
rfm$table


### Compare Accuracy of Models:

#- **Accuracy of Classification tree model **

ctm$overall

#- **Accuracy of Random Forest model **

rfm$overall
rfmAccur <- rfm$overall['Accuracy']


#Random Forest model Accuracy `r rfmAccur` is much better in comparing to Classification tree model Accuracy `r ctm$overall['Accuracy']`.

## Best Model Selection and Prediction
#Comparing model accuracy of the two above generated models -random forests and classification tree, **Random forests model ** has overall better accuracy.Estimated __out of sample error rate__ for the random forests model is `r 1 - rfmAccur`  which is very low

### Prediction with best model:
#Predict the given new test set through best model **Random Forest ** and output results (Predicted Value of Classe Variable) for automatic grader of coursera Submission.


# predict on test set
(prediction <- as.character(predict(modFitRfm, newdata=testdata)))


# create function to write predictions to files
pml_write_files <- function(x) {
    n <- length(x)
    for(i in 1:n) {
        filename <- paste0("./output/problem_id_", i, ".txt")
        write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
    }
}

# create prediction files to submit
pml_write_files(prediction)


## Conclusion
#From the above process ,it look like **Randow forest Model is overfit ** with accuracy of `r rfmAccur` with the given processed trained data . However, we cannot expect the model to predict with same accuracy with different set of situations like all data collected with no NA and/or using different device, then we may need to retrain model to perform same as shown in the analysis or it may fail to predict with better accuracy.
       

