rm(list = ls()) # clear/reset current environment
#dev.off() # clear plots
library(randomForest)
library(class)
library(caret)
library(rpart)
set.seed(423)

#load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Lecture 1 - Exo 1/idList-co-100.rdata")
load("/Users/Tilman/Downloads/idList-mid-100-new.Rdata")
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
getDisjunct <- function(split){
  id <- do.call(rbind, idList[split])
  id <- as.data.frame(id)
  sapply(id, class)
  id<-transform(id, V1=as.factor(V1)) #needed so we have a categorization not a regression problem
  dataset_shuffle <- id[sample(nrow(id)),]
  data <- dataset_shuffle[,-1]
  labels <- dataset_shuffle[,1]
  return(list(data=data, labels=labels))
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

train <- getDisjunct(0:30)
test <- getDisjunct(31:47)

pca_count_val <- c(20,25,30,40,50,70,90,120,150,200,250,300,340)
counter = 0
res = list()
cat("pca_count_i", "i",
    "acc", "pca_count", "ntree", "mtry", "nodesize", "sampsize",
    "time_pcaTrainDuration", "time_rfTrainDuration", "time_pcaTestDuration", "time_rfTestDuration","\n")
for(pca_count_i in c(1:length(pca_count_val))){
  pca_count <- pca_count_val[pca_count_i]
  res[[pca_count_i]]=list()
  #amount of parameters we look at each decission point
  mtry_seq <- seq(pca_count * 0.1, pca_count * 0.8, pca_count * 0.1)
  
  #minimum amount of values in last node of tree -> defines tree depth, lower value, deeper tree. 
  # => start by min size of 1% (60) of each class and end by 50% (2760) of each class in one node, stepping by 5% (300) of each class
  nodesize_seq <- seq((nrow(train$data) / 10) * 0.01, (nrow(train$data) / 10) * 0.5, (nrow(train$data) / 10) * 0.05)
  
  #sample size used for OOB bags: increase -> less randomness, decrease -> more randomness
  sampsize_seq <- nrow(train$data) * c(0.5, 0.6, 0.7, 0.8) 
  
  #number of random trees to grow
  ntree_seq <- c(300, 500, 700)
  
  hyperparam_grid <- expand.grid(mtry = mtry_seq, nodesize = nodesize_seq, sampsize = sampsize_seq, ntree = ntree_seq)
  
   
  for(i in c(1:nrow(hyperparam_grid))){
  #   #TRAINING
  #   timerStart("PCA TRAIN START")
  #   pca_res <- prcomp(train$data, rank. = pca_count)
  #   time_pcaTrainDuration <- timerEnd("PCA TRAIN END")
  #   timerStart("RF TRAIN START")
  #   rf <- randomForest(train$labels ~ ., 
  #                      data = pca_res$x, 
  #                      ntree = hyperparam_grid$ntree[i], 
  #                      mtry = hyperparam_grid$mtry[i],
  #                      nodesize = hyperparam_grid$nodesize[i],
  #                      sampsize = hyperparam_grid$sampsize[i])
  #   time_rfTrainDuration <- timerEnd("RF TRAIN END")
  #   #TESTING
  #   timerStart("PCA TRAIN START")
  #   pca_pred <- predict(pca_res, test$data)
  #   time_pcaTestDuration <- timerEnd("PCA TRAIN END")
  #   timerStart("RF TRAIN START")
  #   rf_pred <- predict(rf, pca_pred)
  #   time_rfTestDuration <- timerEnd("RF TRAIN END")
  #   
  #   #EVAL
  #   cm<-table(rf_pred, test$labels)
  #   acc <- accuracy(cm)
  #   
  #   cat("\nRandomForest had accuracy of", accuracy(cm),"with",PCA_COMPONENT_COUNT,"PCA Components.\nTraining took",trainTime," and prediction took",predTime)
  #   cat(pca_count_i, i,
  #       acc, pca_count, hyperparam_grid$ntree[i], hyperparam_grid$mtry[i], hyperparam_grid$nodesize[i], hyperparam_grid$sampsize[i],
  #       time_pcaTrainDuration, time_rfTrainDuration, time_pcaTestDuration, time_rfTestDuration,"\n")
  #   
  #   res[[pca_count_i]][[i]] = list(pca_count_i=pca_count_i, i=i,
  #                  acc=acc, pca_count=pca_count, ntree=hyperparam_grid$ntree[i], mtry=hyperparam_grid$mtry[i], nodesize=hyperparam_grid$nodesize[i], sampsize=hyperparam_grid$sampsize[i],
  #                  time_pcaTrainDuration=time_pcaTrainDuration, time_rfTrainDuration=time_rfTrainDuration, time_pcaTestDuration=time_pcaTestDuration, time_rfTestDuration=time_rfTestDuration)
    res[[pca_count_i]][[i]] = list(pca_count_i=pca_count_i, i=i)
  }
}
