rm(list = ls()) # clear/reset current environment
#dev.off() # clear plots
library(randomForest)
library(class)
library(caret)
library(rpart)
library(furrr)

set.seed(423)


load("/Users/Tilman/Downloads/idList-corner-100-new.Rdata") #orig
#load("/Users/Tilman/Downloads/idList-mid-100-new.Rdata")
#load("/Users/Tilman/Downloads/idList-corner-200-new.Rdata") #orig
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
  #dataset_shuffle[,-1] <- t(apply(dataset_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  
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

#dataset <- getAllInNormed(0.63829) #30 person train, 17 test
dataset <- getDisjunctNormed(0.63829,1:47) #30 person train, 17 test
train = dataset$train
test = dataset$test


# 1 Try min-max norm, over all comps
#min_train <- min(train$data)
#minmax_train <- (max(train$data) - min(train$data))
#train$data <- (train$data - min_train) / minmax_train
#test$data <- (test$data - min_train) / minmax_train #!important to norm test with train, other is not possible in reallife!

# 2 Try min-max norm, component wise
#!important to norm test with train, other is not possible in reallife!
#for(i in 1:ncol(train$data)) test$data[i] <- (test$data[i] - min(train$data[i])) / (max(train$data[i])-min(train$data[i]))
#for(i in 1:ncol(train$data)) train$data[i] <- (train$data[i] - min(train$data[i])) / (max(train$data[i])-min(train$data[i]))

# 3 Try min-max norm, image wise => moved to loading
#!important to norm test with train, other is not possible in reallife!
#for(i in 1:nrow(train$data)) train$data[i,] <- (train$data[i,] - min(train$data[i,])) / (max(train$data[i,])-min(train$data[i,]))
#for(i in 1:nrow(test$data)) test$data[i,] <- (test$data[i,] - min(test$data[i,])) / (max(test$data[i,])-min(test$data[i,]))

# 4 first #3, then #2
#for(i in 1:nrow(train$data)) train$data[i,] <- (train$data[i,] - min(train$data[i,])) / (max(train$data[i,])-min(train$data[i,]))
#for(i in 1:nrow(test$data)) test$data[i,] <- (test$data[i,] - min(test$data[i,])) / (max(test$data[i,])-min(test$data[i,]))
#for(i in 1:ncol(train$data)) test$data[i] <- (test$data[i] - min(train$data[i])) / (max(train$data[i])-min(train$data[i]))
#for(i in 1:ncol(train$data)) train$data[i] <- (train$data[i] - min(train$data[i])) / (max(train$data[i])-min(train$data[i]))

# 5 Try z-norm, over all comps

# PCA Hiscores:
# 100dpi, 29.26388s train, 3.693782 test for all.
# 100dpi, 27.88836  train, 3.502278 test for 40 PCs.
# 200dpi, 575.5289  train, 85.80151 test for 40 PCs.
timerStart("PCA TRAIN")
pca_res <- prcomp(train$data, .rank=PCA)
time_rfTrainDuration <- timerEnd("")
timerStart("PCA TEST")
pca_pred <- predict(pca_res, test$data, .rank=PCA)
time_rfTrainDuration <- timerEnd("")


#Hiscore:
#Final hypers with 4
PCA = 40 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 4 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
#Acc: 83.82941  Train time: 133.2597  Test time: 8.204128

# 1: Try min-max norm, over all comps
#Acc: 83.42941  Train time: 124.6357  Test time: 6.744413
# 2: Try min-max norm, component wise
#Acc: 71.84412  Train time: 142.3847  Test time: 8.287076
# 3: Try min-max norm, image wise
#Acc: 85.73824  Train time: 124.0188  Test time: 7.470023> 
# 4: first #3, then #2
#Acc: 85.57353  Train time: 125.5589  Test time: 7.533212
# 5: move image wise norm to loader function (t(apply(...)))
# Acc: 85.69706  Train time: 126.461  Test time: 9.109428


# FINAL (with new split technique)
# no norm + corner:
# getDisjunctNormed(0.63829):
# Acc: 83.82941  Train time: 133.2597  Test time: 8.204128
# getAllInNormed(0.63829):
# Acc: 93.71765  Train time: 125.7874  Test time: 7.498511

# normed (image wise) + corner:
# getDisjunctNormed(0.63829):
# Acc: 85.70882  Train time: 127.8839  Test time: 6.348983 (acc improved 1,87941% with norm)
# getAllInNormed(0.63829):
# Acc: 93.83235  Train time: 129.8509  Test time: 7.377851 (acc improved 0,1147% with norm)

# normed + mid:
# getDisjunctNormed(0.63829):
# Acc: 77.33235  Train time: 139.4109  Test time: 7.707785 (acc decreased 8,37647% on mid in respect to corner+norm)
# getAllInNormed(0.63829):
# Acc: 87.90294  Train time: 133.6811  Test time: 7.910317 (acc decreased 5,92941% on mid in respect to corner+norm)

#PCA = 40 #try1 40 - 60
#NTREE = 200 #ab 200 fast stable, 300 wenig besser
#MTRY = 4 #try3 4-8, aber relativ stablil
#NODESIZE = 5 #try2 bis 1
#SAMPSIZE = 58000 #stable
# normed (image wise) + corner + faster params:
# getDisjunctNormed(0.63829):
# Acc: 85.70882  Train time: 127.8839  Test time: 6.348983 (acc improved 1,87941% with norm)
# getAllInNormed(0.63829):
# Acc: 93.83235  Train time: 129.8509  Test time: 7.377851 (acc improved 0,1147% with norm)


PCA = 40 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 4 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
#SAMPSIZE = 58000 #stable
#Acc: 85.65882  Train time: 130.3742  Test time: 5.624332


{
  #TRAINING
  timerStart("RF TRAIN")
  rf <- randomForest(train$labels ~ ., data = pca_res$x[,0:PCA], ntree = NTREE, mtry = MTRY, nodesize = NODESIZE)#, sampsize = SAMPSIZE)
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

