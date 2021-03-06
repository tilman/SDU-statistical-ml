rm(list = ls()) # clear/reset current environment
#dev.off() # clear plots
library(randomForest)
library(class)
library(caret)
library(rpart)
library(furrr)
library(ggplot2)
library(kernlab)
library(caTools)

set.seed(423)


load("/Users/Tilman/Downloads/idList-corner-100-new.Rdata") #orig
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

minmaxNorm <- function(line){
  return((line - min(line)) / (max(line)-min(line)))
}
getDisjunctNormed <- function(split, datasetSize){
  id <- do.call(rbind, idList[datasetSize])
  id <- as.data.frame(id)
  sapply(id, class)
  id<-transform(id, V1=as.factor(V1)) #needed so we have a categorization not a regression problem
  
  
  split_point <- round(nrow(id)*split)
  
  train <- id[0:split_point,]
  test <- id[(split_point+1):nrow(id),]
  
  train_shuffle <- train[sample(nrow(train)),]
  test_shuffle <- test[sample(nrow(test)),]
  
  train_data <- t(apply(train_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  test_data <- t(apply(test_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  
  train_labels <- train_shuffle[,1]
  test_labels <- test_shuffle[,1]
  
  return(list(
    train = list(data = train_data, labels = train_labels),
    test  = list(data = test_data,  labels = test_labels)
  ))
}
getAllInNormed <- function(split,datasetSize){
  id <- do.call(rbind, idList[datasetSize])
  id <- as.data.frame(id)
  
  sapply(id, class)
  id<-transform(id, V1=as.factor(V1)) #needed so we have a categorization not a regression problem
  
  dataset_shuffle <- id[sample(nrow(id)),]
  dataset_shuffle[,-1] <- t(apply(dataset_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  
  split_point <- round(nrow(dataset_shuffle)*split)
  
  train_data <- dataset_shuffle[0:split_point,-1]
  train_labels <- dataset_shuffle[0:split_point,1]
  
  test_data <- dataset_shuffle[(split_point+1):nrow(dataset_shuffle),-1]
  test_labels <- dataset_shuffle[(split_point+1):nrow(dataset_shuffle),1]
  
  return(list(test=list(data=test_data, labels=test_labels),train=list(data=train_data, labels=train_labels)))
}

timer <- {}
timerStart <- function(name){
  timer.startTime <<- Sys.time()
  timer.name <<- name
  cat(name, "started at", timer.startTime,"\n")
  return(timer.startTime)
}
timerEnd <- function(point){
  cur <- Sys.time()
  diff <- difftime(cur,timer.startTime,units="secs")
  cat(timer.name, "(", point,")","ended at", cur, "and took", diff, "seconds \n")
  return(diff)
}

#dataset <- getAllInNormed(0.63829, 0:5) #30 person train, 17 test
dataset <- getDisjunctNormed(0.63829, 0:5) #30 person train, 17 test
train = dataset$train
test = dataset$test

#pca_res <- prcomp(train$data)
#pca_pred <- predict(pca_res, test$data)

#Hiscore:
#1
PCA = 60
C = 10
KERNEL = "vanilladot"
# Acc: 80.87059  Train time: 1264.421  Test time: 2.044616




# new hiscore 0:5 getAllInNormed

#1
PCA = 60
C = 10
KERNEL = "vanilladot"
# Acc: 63.14625  Train time: 5.911036  Test time: 0.4771001

{
  #TRAINING
  timerStart("RF TRAIN")
  svm_res <- ksvm(x=train$data, y=train$labels, scaled=FALSE)
  #svm_res <- ksvm(x=pca_res$x[,0:PCA], y=train$labels, kernel=KERNEL, C=C)
  print(svm_res) 
  time_rfTrainDuration <- timerEnd("")
  
  #TESTING
  timerStart("RF TEST")
  svm_pred <- predict(svm_res, newdata = test$data, type = "response")
  #svm_pred <- predict(svm_res, newdata = pca_pred[,0:PCA], type = "response")
  time_rfTestDuration <- timerEnd("")
  
  #EVAL
  cm<-table(svm_pred, test$labels)
  acc <- accuracy(cm)
  cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
}

