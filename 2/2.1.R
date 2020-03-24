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
# Standard deviation (Components 0 to 10) 0.6229586 0.5173791 0.4950781 0.4112954 0.2983075 0.2644365 0.2588353 0.2155745 0.1907695 0.1779427
# Proportion of the Variance (Components 0 to 10) 0.2229243 0.1537648 0.1407947 0.09717326 0.05111725 0.04016816 0.03848452 0.02669523 0.02090533 0.01818859
# Cumsum of the proportion of the Variance (Components 0 to 10) 0.2229243 0.3766891 0.5174838 0.614657 0.6657743 0.7059425 0.744427 0.7711222 0.7920275 0.8102161
# 
# 
# Variance threshold 80 % needs  10  PCA components. Proportional cumsum of the variance from all of them is: 0.8102161 
# K: 3  Accuracy: 93.85  Runtime: 0.045187 
# K: 7  Accuracy: 94.3  Runtime: 0.046345 
# K: 12  Accuracy: 94.4  Runtime: 0.0508101 
# 
# Variance threshold 90 % needs  18  PCA components. Proportional cumsum of the variance from all of them is: 0.9030284 
# K: 3  Accuracy: 94.95  Runtime: 0.07384801 
# K: 7  Accuracy: 94.45  Runtime: 0.08878803 
# K: 12  Accuracy: 94.8  Runtime: 0.08113694 
# 
# Variance threshold 95 % needs  28  PCA components. Proportional cumsum of the variance from all of them is: 0.9516174 
# K: 3  Accuracy: 95.3  Runtime: 0.1112211 
# K: 7  Accuracy: 95.25  Runtime: 0.124707 
# K: 12  Accuracy: 95.3  Runtime: 0.127069 
# 
# Variance threshold 99 % needs  60  PCA components. Proportional cumsum of the variance from all of them is: 0.9901545 
# K: 3  Accuracy: 95.9  Runtime: 0.2792039 
# K: 7  Accuracy: 95.75  Runtime: 0.285553 
# K: 12  Accuracy: 95.5  Runtime: 0.290463
# 
