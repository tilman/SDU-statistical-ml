rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)

load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Exercises/idList-corner-100-new.Rdata")
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
run_knn <- function(train_split, test_split, train_classes, test_classes, k){
  start_time <- Sys.time()
  test_prediction <- knn(train_split,test_split,cl=train_classes,k)
  end_time <- Sys.time()
  confusion_matrix <- table(test_prediction, test_classes)
  runtime = end_time - start_time
  acc = accuracy(confusion_matrix)
  cat("K:",k," Accuracy:",acc," Runtime:",runtime, "\n")
  df <- list(K=k,Accuracy=acc,Runtime=runtime)
  return(df)
}
minmax_norm <- function(train, test){
  for(i in c(1:ncol(train))){
    #train[,i] <- (train[,i] - min(train[,i])) / (max(train[,i]) - min(train[,i]))
    #test[,i] <- (test[,i] - min(train[,i])) / (max(train[,i]) - min(train[,i]))
    train <- (train - min(train)) / (max(train) - min(train))
    test <- (test - min(train)) / (max(train) - min(train))
  }
  res <- list(train=train, test=test)
  return(res)
}



# interpretation from 2.1.2 for best parameters:
# with 25 PCA components (90% Variance threshold) and a K of 3 we get an accuracy of 97.72% in a way smaller
# time as with 36 PCA Components (95% variance threshold).
# Cecause precision just differs in a really small amount we have choosen the way faster method 
# with 25 PCA components

accs <- c(1:10)
runtimes <- c(1:10)
id <- do.call(rbind, idList[1:10]) # transform multi dimension data frame to list of datapoints
id <- as.data.frame(id)
id[,1] <- factor(id[,1])
id <- id[sample(nrow(id)),] # shuffle dataset
folds <- createFolds(id$V1, k=10)

dataset = list()
# normalize before PCA
for (i in 1:10) {
  dataset$train <- id[-folds[[i]], -1]
  dataset$test <- id[folds[[i]],-1]
  dataset$train_labels <- id[-folds[[i]], 1]
  dataset$test_labels <- id[folds[[i]],1]
  
  norm <- minmax_norm(dataset$train, dataset$test)
  
  pca_res <- prcomp(norm$train, rank. = 25)
  pca_predict <- predict(pca_res, norm$test)
  ret <- run_knn(pca_res$x, pca_predict, dataset$train_labels, dataset$test_labels, 3)
  
  accs[i] = ret$Accuracy
  runtimes[i] = ret$Runtime
}
mean(accs)
mean(runtimes)
# Results:
# K: 3  Accuracy: 98.3  Runtime: 4.160327 
# K: 3  Accuracy: 98.075  Runtime: 4.186833 
# K: 3  Accuracy: 98.15  Runtime: 4.217398 
# K: 3  Accuracy: 97.85  Runtime: 4.164934 
# K: 3  Accuracy: 98.275  Runtime: 4.604813 
# K: 3  Accuracy: 97.95  Runtime: 4.55579 
# K: 3  Accuracy: 98.325  Runtime: 4.243776 
# K: 3  Accuracy: 97.95  Runtime: 4.217786 
# K: 3  Accuracy: 98.125  Runtime: 4.102729 
# K: 3  Accuracy: 97.95  Runtime: 4.147619 
# Accuracy mean: 98.095 %
# KNN Runtime mean: 4.2602 sec


dataset = list()
# normalize after PCA
for (i in 1:10) {
  dataset$train <- id[-folds[[i]], -1]
  dataset$test <- id[folds[[i]],-1]
  dataset$train_labels <- id[-folds[[i]], 1]
  dataset$test_labels <- id[folds[[i]],1]
  
  pca_res <- prcomp(dataset$train, rank. = 25)
  pca_predict <- predict(pca_res, dataset$test)
  # pca_res, pca_predict are unnormalized
  
  norm <- minmax_norm(pca_res$x, pca_predict)
  # pca_res, pca_predict are now normalized and copied into dataset object
  
  ret <- run_knn(norm$train, norm$test, dataset$train_labels, dataset$test_labels, 3)
  
  accs[i] = ret$Accuracy
  runtimes[i] = ret$Runtime
}
mean(accs)
mean(runtimes)
# Results:
# K: 3  Accuracy: 98.35  Runtime: 4.345533 
# K: 3  Accuracy: 98.05  Runtime: 4.257726 
# K: 3  Accuracy: 98.1  Runtime: 4.402697 
# K: 3  Accuracy: 97.825  Runtime: 4.152635 
# K: 3  Accuracy: 98.25  Runtime: 4.015694 
# K: 3  Accuracy: 97.975  Runtime: 4.152205 
# K: 3  Accuracy: 98.275  Runtime: 4.212077 
# K: 3  Accuracy: 97.875  Runtime: 4.264799 
# K: 3  Accuracy: 98.05  Runtime: 4.256526 
# K: 3  Accuracy: 97.925  Runtime: 4.218956 
# Accuracy mean: 98.0675 %
# KNN Runtime mean: 4.227885 sec, but normalization is faster
