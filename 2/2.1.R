rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
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
  
  test_split <- id_test[0:20000,-1]
  train_split <- id_train[0:20000,-1]
  test_classes <- id_test[0:20000,1]
  train_classes <- id_train[0:20000,1]
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
for(dataset in list(dataDisjunct)){
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

# 2.1.2 and 2.1.3 Results:
# 
# Dataset split: All Persons In
# 
# Standard deviation (Components 0 to 10) 0.8688004 0.707551 0.6392667 0.5574933 0.5434544 0.4673228 0.4594847 0.3975414 0.3703828 0.3610791
# Proportion of the Variance (Components 0 to 10) 0.1699586 0.1127246 0.09201684 0.06998137 0.06650118 0.04917418 0.04753848 0.03558507 0.03088907 0.02935674
# Cumsum of the proportion of the Variance (Components 0 to 10) 0.1699586 0.2826832 0.3747 0.4446814 0.5111826 0.5603567 0.6078952 0.6434803 0.6743694 0.7037261
# 
# 
# Variance threshold 80 % needs  15  PCA components. Proportional cumsum of the variance from all of them is: 0.8061658 
# K: 3  Accuracy: 97.01  Runtime: 6.316975 
# K: 7  Accuracy: 96.775  Runtime: 6.525208 
# K: 12  Accuracy: 96.49  Runtime: 6.130448 
# 
# Variance threshold 90 % needs  25  PCA components. Proportional cumsum of the variance from all of them is: 0.905796 
# K: 3  Accuracy: 97.72  Runtime: 10.35009 
# K: 7  Accuracy: 97.43  Runtime: 10.10074 
# K: 12  Accuracy: 97.125  Runtime: 10.59192 
# 
# Variance threshold 95 % needs  36  PCA components. Proportional cumsum of the variance from all of them is: 0.9508849 
# K: 3  Accuracy: 97.915  Runtime: 24.41177 
# K: 7  Accuracy: 97.545  Runtime: 24.39653 
# K: 12  Accuracy: 97.225  Runtime: 21.28739 
# 
# Variance threshold 99 % needs  74  PCA components. Proportional cumsum of the variance from all of them is: 0.9901393 
# K: 3  Accuracy: 97.955  Runtime: 53.18392 
# K: 7  Accuracy: 97.53  Runtime: 51.94326 
# K: 12  Accuracy: 97.165  Runtime: 52.11162 
# 
# 
# 
# 
# 
# 
# Dataset split: Disjunct
#
# Standard deviation (Components 0 to 10) 0.7591435 0.6627278 0.6046395 0.492342 0.4858367 0.453112 0.4063359 0.3751744 0.3421654 0.3044527
# Proportion of the Variance (Components 0 to 10) 0.1619149 0.1233984 0.1027146 0.06810402 0.0663162 0.05768332 0.0463884 0.03954625 0.03289358 0.02604226
# Cumsum of the proportion of the Variance (Components 0 to 10) 0.1619149 0.2853133 0.3880278 0.4561318 0.522448 0.5801314 0.6265198 0.666066 0.6989596 0.7250019
# Variance threshold 80 % needs  14  PCA components. Proportional cumsum of the variance from all of them is: 0.8030988 
#
#
# K: 3  Accuracy: 78.27  Runtime: 5.582443 
# K: 7  Accuracy: 78.565  Runtime: 5.95058 
# K: 12  Accuracy: 78.48  Runtime: 5.657591 
# 
# Variance threshold 90 % needs  24  PCA components. Proportional cumsum of the variance from all of them is: 0.9041168 
# K: 3  Accuracy: 82.665  Runtime: 10.27751 
# K: 7  Accuracy: 82.57  Runtime: 10.73692 
# K: 12  Accuracy: 82.23  Runtime: 9.624222 
# 
# Variance threshold 95 % needs  35  PCA components. Proportional cumsum of the variance from all of them is: 0.9507284 
# K: 3  Accuracy: 83.335  Runtime: 22.57845 
# K: 7  Accuracy: 83.22  Runtime: 21.62002 
# K: 12  Accuracy: 82.995  Runtime: 19.5195 
# 
# Variance threshold 99 % needs  75  PCA components. Proportional cumsum of the variance from all of them is: 0.9903329 
# K: 3  Accuracy: 83.77  Runtime: 62.75142
# K: 7  Accuracy: 83.595  Runtime: 57.43834 
# K: 12  Accuracy: 83.095  Runtime: 1.049605 
# 
