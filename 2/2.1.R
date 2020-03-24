rm(list = ls()) # clear/reset current environment
library(spatstat)
library(class)
library(caret)

load("/Users/Tilman/Downloads/idList-co-100.Rdata")

splitAllIn <- function(data,title){
  id <- do.call(rbind, idList[1:10]) #transform multi dimension data frame to list of datapoints
  dataset_shuffle <- id[sample(nrow(id)),]
  test_split <- dataset_shuffle[0:20000,-1]
  train_split <- dataset_shuffle[(20001):40000,-1]
  test_classes <- dataset_shuffle[0:20000,1]
  train_classes <- dataset_shuffle[(20001):40000,1]
  data <- list(test=test_split,train=train_split,test_labels=test_classes,train_labels=train_classes,title=title)
  return(data)
}
splitDisjunct <- function(data,title){
  id_train <- do.call(rbind, idList[1:5])
  id_train <- as.data.frame(id_train)
  id_train$V1 <- factor(id_train$V1)
  id_test <- do.call(rbind, idList[6:10])
  id_test <- as.data.frame(id_test)
  id_test$V1 <- factor(id_test$V1)
  
  test_split <- id_test[0:2000,-1]
  train_split <- id_train[0:2000,-1]
  test_classes <- id_test[0:2000,1]
  train_classes <- id_train[0:2000,1]
  data <- list(test=test_split,train=train_split,test_labels=test_classes,train_labels=train_classes,title=title)
  return(data)
}
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

dataAllIn <- splitAllIn(idList,title="All Persons In")
dataDisjunct <- splitDisjunct(idList,title="Disjunct")
for(dataset in list(dataAllIn,dataDisjunct)){
  cat("\n\n Dataset split:",dataset$title)
  
  # 2.1.1: Show the standard deviation...
  pca_res <- prcomp(dataset$train)
  std_dev <- pca_res$sdev # pca standard deviation
  var <- std_dev^2 # pca variance
  prop_var <- var/sum(var) # pca proportional variance (between 0 and 1)
  cum_prop_var <- cumsum(prop_var) # # cumsum of proportional variance
  
  barplot(std_dev[1:20], xlab = "Principal Component", ylab = "Standard deviation",main=dataset$title)
  cat("\nStandard deviation (Components 0 to 10)", std_dev[0:10])
  barplot(prop_var[1:20], xlab = "Principal Component", ylab = "Proportion of the Variance",main=dataset$title)
  cat("\nProportion of the Variance (Components 0 to 10)", prop_var[0:10])
  barplot(cum_prop_var[1:20], xlab = "Principal Component", ylab = "Cumsum of the proportion of the Variance",main=dataset$title)
  cat("\nCumsum of the proportion of the Variance (Components 0 to 10)", cum_prop_var[0:10])
  
  # 2.1.2 and 2.1.3 code
  for(variance_threshold in c(0.8,0.9,0.95,0.99)){
    
    # get the index of the PCA component where we reach our variance threshold
    rank <- which(cum_prop_var > variance_threshold)[1] 
    cat("\nVariance threshold",variance_threshold*100,"% needs ", rank, " PCA components. Proportional cumsum of the variance from all of them is:",cum_prop_var[rank],"\n")
    
    pca_res <- prcomp(dataset$train, rank. = rank)
    pca_predict <- predict(pca_res, dataset$test)
    for(k in c(3,7,12)) { # For each test vary “k” in kNN, try 3 reasonable values
      run_knn(pca_res$x, pca_predict, dataset$train_labels, dataset$test_labels, k)
    }
  }
}

# 2.1.2 and 2.1.3 Results for alle Persons In:
#
# Variance threshold 80 % needs  15  PCA components. Proportional cumsum of the variance from all of them is: 0.8058439 
# K: 3  Accuracy: 96.79  Runtime: 6.480006 
# K: 7  Accuracy: 96.57  Runtime: 6.312029 
# K: 12  Accuracy: 96.29  Runtime: 6.012458 
# 
# 
# Variance threshold 90 % needs  25  PCA components. Proportional cumsum of the variance from all of them is: 0.9058394 
# K: 3  Accuracy: 97.615  Runtime: 10.39099 
# K: 7  Accuracy: 97.295  Runtime: 10.27383 
# K: 12  Accuracy: 96.98  Runtime: 10.24507 
# 
# 
# Variance threshold 95 % needs  36  PCA components. Proportional cumsum of the variance from all of them is: 0.9509988 
# K: 3  Accuracy: 97.7  Runtime: 21.17455 
# K: 7  Accuracy: 97.445  Runtime: 22.58979 
# K: 12  Accuracy: 97.045  Runtime: 22.51741 
# 
# 
# Variance threshold 99 % needs  74  PCA components. Proportional cumsum of the variance from all of them is: 0.990089 
# K: 3  Accuracy: 97.66  Runtime: 52.41418 
# K: 7  Accuracy: 97.37  Runtime: 53.20585 
# K: 12  Accuracy: 97.04  Runtime: 52.35225 

