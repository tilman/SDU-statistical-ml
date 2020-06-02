rm(list = ls()) # clear/reset current environment
#dev.off() # clear plots
library(randomForest)
library(class)
#library(caret)
library(rpart)
#library(furrr)

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
  
  
  split_point <- nrow(id)*split
  
  train <- id[0:split_point,]
  test <- id[(split_point+1):nrow(id),]
  
  train_shuffle <- train[sample(nrow(train)),]
  test_shuffle <- test[sample(nrow(test)),]
  
  train_data <- t(apply(train_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  test_data <- t(apply(test_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  #train_data <- train_shuffle[,-1]
  #test_data <- test_shuffle[,-1]
  
  train_labels <- train_shuffle[,1]
  test_labels <- test_shuffle[,1]
  
  return(list(
    train = list(data = train_data, labels = train_labels),
    test  = list(data = test_data,  labels = test_labels)
  ))
}
getAllInNormed <- function(split, datasetSize){
  id <- do.call(rbind, idList[datasetSize])
  id <- as.data.frame(id)
  
  sapply(id, class)
  id<-transform(id, V1=as.factor(V1)) #needed so we have a categorization not a regression problem
  
  dataset_shuffle <- id[sample(nrow(id)),]
  dataset_shuffle[,-1] <- t(apply(dataset_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  
  split_point <- nrow(dataset_shuffle)*split
  
  train_data <- dataset_shuffle[0:split_point,-1]
  test_data <- dataset_shuffle[(split_point+1):nrow(dataset_shuffle),-1]
  
  train_labels <- dataset_shuffle[0:split_point,1]
  test_labels <- dataset_shuffle[((split_point+1)):nrow(dataset_shuffle),1]
  
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

#dataset <- getAllInNormed(0.63829,1:47) #30 person train, 17 test
dataset <- getDisjunctNormed(0.63829,1:47) #30 person train, 17 test
train = dataset$train
test = dataset$test



#Hiscore:
#Final hypers with 4
PCA = 40 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 4 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
# Acc: 83.34706  Train time: 127.5661  Test time: 7.273665 # disjunct, no norm
# Acc: 85.67647  Train time: 138.7246  Test time: 6.371468 # disjunct, with image wise norm
# Acc: 93.97353  Train time: 129.8136  Test time: 7.184726 # all in, no norm
# Acc: 93.74706  Train time: 139.1029  Test time: 7.46046  # all in, with image wise norm

timerStart("PCA TRAIN")
pca_res <- prcomp(train$data, .rank=PCA)
time_rfTrainDuration <- timerEnd("")
timerStart("PCA TEST")
pca_pred <- predict(pca_res, test$data)
time_rfTrainDuration <- timerEnd("")

{
  #TRAINING
  timerStart("RF TRAIN")
  rf <- randomForest(train$labels ~ ., data = pca_res$x[,1:PCA], ntree = NTREE, mtry = MTRY, nodesize = NODESIZE)#, sampsize = SAMPSIZE)
  time_rfTrainDuration <- timerEnd("")
  
  #TESTING
  timerStart("RF TEST")
  rf_pred <- predict(rf, pca_pred)
  time_rfTestDuration <- timerEnd("")
  
  #EVAL
  cm<-table(rf_pred, test$labels)
  acc <- accuracy(cm)
  cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
}
# name, dataset split, dataset size, acc, train time, test time
