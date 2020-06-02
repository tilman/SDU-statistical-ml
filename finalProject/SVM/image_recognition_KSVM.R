rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)
library(kernlab)
library(ggplot2)

set.seed(423)

load("C:/Users/maxim/Documents/_SDU/_Statistical Machine Learning/Project/idList-corner-100-new.Rdata")

# ----------------------------------------------------- #
#                       FUNCTIONS
# ----------------------------------------------------- #

# In getallIn and getDisjunct we can select whether 
# we want the min/max normalization or not

{
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
  
  maxVal <- function(x,y){
    index <- match(max(y),y)
    x_max <- x[index]
    return(x_max)
  }
  
  
}

# ----------------------------------------------------- #
#                       PARAMETERS
# ----------------------------------------------------- #

# Full dataset :

#dataset <- getAllInNormed(0.63829, 0:47) #30 person train, 17 test
dataset <- getDisjunctNormed(0.63829, 0:47) #30 person train, 17 test

# Reduced version for test :

#dataset <- getDisjunctNormed(0.625, 0:8) #5 person train, 3 test
#dataset <- getAllInNormed(0.625, 0:8) #5 person train, 3 test


train = dataset$train
test = dataset$test

pca_res <- prcomp(train$data)
pca_pred <- predict(pca_res, test$data)

# Hyperparameters
PCA = 60 #Disjunct = 60, AllIn = 40
C = 10
KERNEL = "rbfdot"

# Default parameter for "type" is : type = "C-svc" (C classification)
# Which is what we want so we don't need to change it

# Default parameter for kpar is automatic so it already calculate a good sigma
# Then we don't need to find a better sigma 

# ----------------------------------------------------- #
#                         KSVM
# ----------------------------------------------------- #

{
  #TRAINING
  timerStart("RF TRAIN")
  svm_res <- ksvm(x=pca_res$x[,0:PCA], y=train$labels, kernel=KERNEL, C=C)
  print(svm_res) 
  time_rfTrainDuration <- timerEnd("")
  
  #TESTING
  timerStart("RF TEST")
  svm_pred <- predict(svm_res, newdata = pca_pred[,0:PCA], type = "response")
  time_rfTestDuration <- timerEnd("")
  
  #EVAL
  cm<-table(svm_pred, test$labels)
  acc <- accuracy(cm)
  cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
}


# SVM Classification plot for PC1 and PC2 :
# Not really relevant but I keep it here in case
# I found a way to overcome the multiclass problem
# since kernlab and e1071 only work in bidimension

data <- data.frame(x = pca_res$x[,1],y = pca_res$x[,2], class = train$labels)
svm_model <- ksvm(train$labels ~ pca_res$x[,1] + pca_res$x[,2],
                  data = data, kernel=KERNEL, C=C)

ggplot(data, aes(x,y, color=class)) +
  geom_point(size=1.5) + 
  ggtitle("SVM Classification plot (PC1 and PC2)") +
  labs(y="PC2", x = "PC1")

