# 
# Exercise 2.3 : Preprocessing
#


rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)

load("C:/Users/maxim/Documents/_SDU/_Statistical Machine Learning/Lecture 1/idList-co-100.rdata")
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

id <- do.call(rbind, idList[1:10]) # transform multi dimension data frame to list of datapoints
id_mat <- data.matrix(id, rownames.force = NA)

imageSize <- sqrt(ncol(id_mat)-1)
# apply(variable, 2=apply by row, function rev = reverse)
# t = transpose
rotate <- function(x){ t(apply(x, 2, rev)) }


sigmaVal = c(0.1, 0.2, 0.3, 0.4, 0.5, 1, 2, 3, 4, 5)
sigAcc <- c()
sigTimes <- c()

for (sig in 1:length(sigmaVal)) {
  
  
  # blur() = applies a gaussian blur to a pixel image
  # -> bleed = flag to indicate whether to allow blur to extend outside the domain or not
  # -> varcov = variance-covariance matrix
  # im() = create an object representing a 2d pixel image
  smoothImage <- function(grayImg) {
    smoothed <- as.matrix(blur(as.im(grayImg), sigma = sigmaVal[sig], 
                               normalise = FALSE, bleed = TRUE,
                               varcov = NULL))
    return(smoothed)
  }
  
  # Now we can smooth all images
  for(i in 1:nrow(id_mat)){ 
    # Creating an array with the row of id
    rotated <- c(id_mat[i,2:ncol(id)]) 
    # Creating/Smoothing an image of this array
    image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
    image <- smoothImage(image)
    # Replacing the row with the smoothed version
    id_mat[i,2:ncol(id_mat)] <- matrix(image, nrow = 1, ncol = ncol(id_mat) - 1, byrow = FALSE)
  }
  
  id <- as.data.frame(id_mat)
  id[,1] <- factor(id[,1])
  
  
  # Performing the cross-validation again 
  accs <- c(1:10)
  runtimes <- c(1:10)
  
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
  
  
  sigAcc[sig] <- mean(accs)
  sigTimes[sig] <- mean(runtimes)
}


plot(sigmaVal, sigAcc, main="Evolution of accuracy",
     xlab = "sigma's value",ylab="accuracy")

plot(sigmaVal, sigAcc, main="Evolution of runtimes",
     xlab = "sigma's value",ylab="runtime")