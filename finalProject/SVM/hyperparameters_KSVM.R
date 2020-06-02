rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)
library(kernlab)
library(ggplot2)

set.seed(423)

load("C:/Users/maxim/Documents/_SDU/_Statistical Machine Learning/Project/idList-corner-100-new.Rdata")

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

minmaxNorm <- function(line){
  return((line - min(line)) / (max(line)-min(line)))
}
getDisjunctNormed <- function(split, datasetSize){
  id <- do.call(rbind, idList[datasetSize])
  id <- as.data.frame(id)
  sapply(id, class)
  id<-transform(id, V1=as.factor(V1)) # needed so we have a categorization not a regression problem
  
  
  split_point <- nrow(id)*split
  
  train <- id[0:split_point,]
  test <- id[(split_point+1):nrow(id),]
  
  train_shuffle <- train[sample(nrow(train)),]
  test_shuffle <- test[sample(nrow(test)),]
  
  # without minmax norm : 
  train_data <- train_shuffle[,-1]
  test_data <- test_shuffle[,-1]
  
  # with minmax norm :
  #train_data <- t(apply(train_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  #test_data <- t(apply(test_shuffle[,-1], 1, minmaxNorm)) #apply minmax norm
  
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
  
  # minmax norm : 
  #dataset_shuffle[,-1] <- t(apply(dataset_shuffle[,-1], 1, minmaxNorm))
  
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

maxVal <- function(x,y){
  index <- match(max(y),y)
  x_max <- x[index]
  return(x_max)
}


#dataset <- getAllInNormed(0.63829, 0:47) #30 person train, 17 test
#dataset <- getDisjunctNormed(0.63829, 0:47) #30 person train, 17 test

# Reduced version for test
dataset <- getDisjunctNormed(0.625, 0:8) #5 person train, 3 test
#dataset <- getAllInNormed(0.625, 0:8) #5 person train, 3 test
train = dataset$train
test = dataset$test

pca_res <- prcomp(train$data)
pca_pred <- predict(pca_res, test$data)

# Top parameters saved without Min/max norm :
PCA = 60 #Dis = 60, All = 40
C = 10
KERNEL = "rbfdot"


# First try gave us :
# For 30 train, 17 test, vanilladot, PCA = 60 and C = 10 
# Acc: 80.87059  Train time: 1264.421  Test time: 2.044616

# Default parameter for "type" is : type = "C-svc" (C classification)
# Which is what we want so we don't need to change it

# Default parameter for the cost is : C = 1

# Default parameter for kpar is automatic so it already calculate a good sigma
# Then we don't need to find a better sigma 

# ----------------------------------------------------- #
# Trying out the different Kernel :
# Random parameter of 80 for the PCA
{
  kern <- c("vanilladot","rbfdot","polydot","tanhdot","laplacedot",
            "besseldot")
  # Error occured for anovadot : 
  # Error in y[lowerl:n2, , drop = FALSE] : beyond limits
  # So we drop it and we will not use it
  
  # For 5 train, 3 test, vanilladot, PCA = 80
  # Acc: 79.03333  Train time: 10.0835  Test time: 0.1881878
  # For 5 train, 3 test, rbfdot, PCA = 80
  # Acc: 87.91667  Train time: 40.52881  Test time: 15.47986
  # For 5 train, 3 test, polydot, PCA = 80
  # Acc: 79.03333  Train time: 17.2034  Test time: 4.690776
  # For 5 train, 3 test, tanhdot, PCA = 80
  # Acc: 27.53333  Train time: 48.37872  Test time: 16.5682
  # For 5 train, 3 test, laplacedot, PCA = 80
  # Acc: 80.35  Train time: 156.3173  Test time: 70.95135
  # For 5 train, 3 test, besseldot, PCA = 80
  # Acc: 17.3  Train time: 481.7594  Test time: 279.851
  
  i <- 1
  acc_kern <- c()
  kern_test_time <- c()
  kern_train_time <- c()
  
  for (k in kern) {
    {
      #TRAINING
      timerStart("RF TRAIN")
      svm_res <- ksvm(x=pca_res$x[,0:PCA], y=train$labels, kernel=k)
      time_rfTrainDuration <- timerEnd("")
      
      #TESTING
      timerStart("RF TEST")
      svm_pred <- predict(svm_res, newdata = pca_pred[,0:PCA], type = "response")
      time_rfTestDuration <- timerEnd("")
      
      #EVAL
      cm<-table(svm_pred, test$labels)
      acc <- accuracy(cm)
      cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
      acc_kern[i] <- acc
      kern_train_time[i] <- time_rfTrainDuration
      kern_test_time[i] <- time_rfTestDuration
      i <- i + 1
    }
  }
  
  # Plotting : 
  kerndata <- data.frame(c(1,2,3,4,5,6),acc_kern)
  
  ggplot(kerndata, aes(x = c(1,2,3,4,5,6), y = acc_kern)) + geom_point() +
    ggtitle("Accuracy change with different kernel") +
    labs(y="Accuracy", x = "Kernel number") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) +
    scale_y_continuous(breaks=c(0,25,50,75,90))
  
  # Plotting Kern/Time: 
  kern_train_data <- data.frame(c(1,2,3,4,5,6),kern_train_time)
  
  ggplot(kern_train_data, aes(x = c(1,2,3,4,5,6), y = kern_train_time)) + geom_point() +
    ggtitle("Training time with different kernel") +
    labs(y="Training time (s)", x = "Kernel number")
  
  # Plotting Kern/Time: 
  kern_test_data <- data.frame(c(1,2,3,4,5,6),kern_test_time)
  
  ggplot(kern_test_data, aes(x = c(1,2,3,4,5,6), y = kern_test_time)) + geom_point() +
    ggtitle("Testing time with different kernel") +
    labs(y="Testing time (s)", x = "Kernel number")
  
}
# TL;DR the best one is rbfdot, so we will keep it for the next step

# ----------------------------------------------------- #
# Trying out different values of PCA : 
{
  n_pca <- c(25,40,50,60,75,100,150,200)
  # For 5 train, 3 test, rbfdot, PCA = 10 
  # Acc: 75.91667  Train time: 17.36071  Test time: 7.153335
  # For 5 train, 3 test, rbfdot, PCA = 25 
  # Acc: 87.43333  Train time: 21.82063  Test time: 6.536356
  # For 5 train, 3 test, rbfdot, PCA = 50 
  # Acc: 89.28333  Train time: 26.62863  Test time: 12.00393
  # For 5 train, 3 test, rbfdot, PCA = 75 
  # Acc: 88.43333  Train time: 40.37022  Test time: 15.94472
  # For 5 train, 3 test, rbfdot, PCA = 100 
  # Acc: 86.75  Train time: 48.74665  Test time: 19.76837
  # For 5 train, 3 test, rbfdot, PCA = 150 
  # Acc: 85.65  Train time: 78.70104  Test time: 28.90045
  # For 5 train, 3 test, rbfdot, PCA = 200 
  # Acc: 84.36667  Train time: 100.1763  Test time: 37.68221
  # For 5 train, 3 test, rbfdot, PCA = 300 
  # Acc: 81.31667  Train time: 160.5723  Test time: 59.61389
  # For 5 train, 3 test, rbfdot, PCA = 324 
  # Acc: 80.2  Train time: 178.2202  Test time: 72.97281
  
  i <- 1
  acc_pca <- c()
  val_pca <- c()
  acc_test_time <- c()
  acc_train_time <- c()
  
  for (k in n_pca) {
    {
      #TRAINING
      timerStart("RF TRAIN")
      svm_res <- ksvm(x=pca_res$x[,0:k], y=train$labels, kernel=KERNEL)
      time_rfTrainDuration <- timerEnd("")
      
      #TESTING
      timerStart("RF TEST")
      svm_pred <- predict(svm_res, newdata = pca_pred[,0:k], type = "response")
      time_rfTestDuration <- timerEnd("")
      
      #EVAL
      cm<-table(svm_pred, test$labels)
      acc <- accuracy(cm)
      cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
      acc_pca[i] <- acc
      val_pca[i] <- k
      acc_train_time[i] <- time_rfTrainDuration
      acc_test_time[i] <- time_rfTestDuration
      i <- i + 1
    }
  }
  
  # Plotting : 
  pcadata <- data.frame(val_pca, acc_pca)
  ggplot(pcadata, aes(x = val_pca, y = acc_pca)) + geom_point() +
    ggtitle("Accuracy change with different PCA (and rbfdot)") +
    labs(y="Accuracy", x = "PCs") +    
    scale_x_continuous(breaks=c(25,40,50,60,75,100,150,200))
  
  # Plotting : 
  pca_train_data <- data.frame(val_pca, acc_train_time)
  
  ggplot(pca_train_data, aes(x = val_pca, y = acc_train_time)) + geom_point() +
    ggtitle("Training time with different PCA (and best KERNEL)") +
    labs(y="Training time (s)", x = "PCs")
  
  # Plotting : 
  pca_test_data <- data.frame(val_pca, acc_test_time)
  
  ggplot(pca_test_data, aes(x = val_pca, y = acc_test_time)) + geom_point() +
    ggtitle("Testing time with different PCA (and best KERNEL)") +
    labs(y="Testing time (s)", x = "PCs")
}
# TL;DR the best PC is 50, so we will keep it for the next step

# ----------------------------------------------------- #
# Trying out different values of the cost in the SVM : 
# The cost defines the weight of how much samples inside the margin 
# contribute to the overall error;
# We can adjust how hard or soft our large margin classification should be;
# With a C of 0, samples inside the margins are not penalized anymore;
# With an infinite C you have the other possible extreme of hard margins.
{
  val_cost <- c(3^-4,3^-2,1,3^2,3^4,3^8,3^12)
  
  cost_test_time <- c()
  cost_train_time <- c()
  
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 0
  # No support vectors found, so we aren't using it
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 3^-4
  # Acc: 74.2  Train time: 140.1115  Test time: 49.39791
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 3^-2
  # Acc: 84.63333  Train time: 45.07714  Test time: 19.61515
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 1
  # Acc: 89.21667  Train time: 21.97692  Test time: 7.870736
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 3^2
  # Acc: 90.31667  Train time: 15.96048  Test time: 8.384299
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 3^4
  # Acc: 89.45  Train time: 16.56259  Test time: 6.29735
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 3^8
  # Acc: 89.36667  Train time: 15.71031  Test time: 6.797012
  # For 5 train, 3 test, rbfdot, PCA = 50, C = 3^12 
  # Acc: 89.36667  Train time: 15.25282  Test time: 6.456466
  
  
  acc_cost <- c()
  num_cost <- c()
  
  for (k in val_cost) {
    {
      #TRAINING
      timerStart("RF TRAIN")
      #svm_res <- ksvm(x=train$data, y=train$labels, scaled=FALSE, kernel="vanilladot", C=1)
      svm_res <- ksvm(x=pca_res$x[,0:PCA], y=train$labels, kernel=KERNEL, C = k)
      time_rfTrainDuration <- timerEnd("")
      
      #TESTING
      timerStart("RF TEST")
      svm_pred <- predict(svm_res, newdata = pca_pred[,0:PCA], type = "response")
      time_rfTestDuration <- timerEnd("")
      
      #EVAL
      cm<-table(svm_pred, test$labels)
      acc <- accuracy(cm)
      cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
      acc_cost[i] <- acc
      num_cost[i] <- k
      cost_train_time[i] <- time_rfTrainDuration
      cost_test_time[i] <- time_rfTestDuration
      i <- i + 1
    }
  }
  
  # Plotting : 
  costdata <- data.frame(num_cost,acc_cost)
  
  ggplot(costdata, aes(x = num_cost, y = acc_cost)) + geom_point() +
    ggtitle("Accuracy change with different cost") +
    labs(y="Accuracy", x = "Cost value") +
    scale_x_continuous(trans = "log10")
  
  # Plotting : 
  cost_train_data <- data.frame(num_cost,cost_train_time)
  
  ggplot(cost_train_data, aes(x = num_cost, y = cost_train_time)) + geom_point() +
    ggtitle("Training time with different cost") +
    labs(y="Training time (s)", x = "Cost value") +
    scale_x_continuous(trans = "log10")
  
  # Plotting : 
  cost_test_data <- data.frame(num_cost,cost_test_time)
  
  ggplot(cost_train_data, aes(x = num_cost, y = cost_test_time)) + geom_point() +
    ggtitle("Testing time with different cost") +
    labs(y="Testing time (s)", x = "Cost value") +
    scale_x_continuous(trans = "log10")
  
  
}
# TL;DR the best plot is obtained with 3^2 so we will keep C = 10 

KERNEL_Disjunct = kern[maxVal(c(1,2,3,4,5,6),acc_kern)]
PCA_Disjunct = n_pca[maxVal(val_pca, acc_pca)]
C_Disjunct = val_cost[maxVal(num_cost,acc_cost)]
# The kernel with the best accuracy is :  rbfdot
# The number of PC with the best accuracy is :  60
# The cost with the best accuracy is :  10



KERNEL_AllIn = kern[maxVal(c(1,2,3,4,5,6),acc_kern)]
PCA_AllIn = n_pca[maxVal(val_pca, acc_pca)]
C_AllIn = val_cost[maxVal(num_cost,acc_cost)]
# The kernel with the best accuracy is :  rbfdot
# The number of PC with the best accuracy is :  40
# The cost with the best accuracy is :  10


cat("The kernel with the best accuracy is : ", KERNEL )
cat("The number of PC with the best accuracy is : ", PCA )
cat("The cost with the best accuracy is : ", C )
