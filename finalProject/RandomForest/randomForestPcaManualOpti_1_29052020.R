rm(list = ls()) # clear/reset current environment
#dev.off() # clear plots
library(randomForest)
library(class)
library(caret)
library(rpart)
set.seed(423)

load("/Users/Tilman/Downloads/idList-corner-100-new.Rdata")
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

pca_res <- prcomp(train$data)
pca_pred <- predict(pca_res, test$data)

# amount of PCA components to use
pca_count_seq <- c(10,50,100,150,300)
#amount of parameters we look at each decission point
#mtry_seq <- seq(pca_count * 0.1, pca_count * 0.8, pca_count * 0.1)
mtry_seq <- c(1,2,4,8)
#minimum amount of values in last node of tree -> defines tree depth, lower value, deeper tree. 
# => start by min size of 1% (60) of each class and end by 50% (2760) of each class in one node, stepping by 5% (300) of each class
#nodesize_seq <- seq((nrow(train$data) / 10) * 0.01, (nrow(train$data) / 10) * 0.5, (nrow(train$data) / 10) * 0.05)
nodesize_seq <- c(5,20,40,60,80)
#sample size used for OOB bags: increase -> less randomness, decrease -> more randomness
#sampsize_seq <- nrow(train$data) * c(0.5, 0.6, 0.7, 0.8) 
sampsize_seq <- c(42000, 48000, 54000)
#number of random trees to grow
ntree_seq <- c(50, 200, 300)



# History

#0
#      acc pca ntree mtry nodesize sampsize time_rfTrainDuration
# 83.71471  50   300    2        5    54000             171.3973

#1 
PCA = 50
NTREE = 300
MTRY = 2
NODESIZE = 20
SAMPSIZE = 58000
#Acc: 82.53235  Train time: 141.8065  Test time: 6.440617

#2
PCA = 50 #try1 40 - 60
NTREE = 200 #ab 200 fast stable, 300 wenig besser
MTRY = 8 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
#Acc: 82.90882  Train time: 169.1146  Test time: 4.149586

#3
PCA = 60 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 2 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
#Acc: 83.35294  Train time: 183.9439  Test time: 9.735827

#4
PCA = 40 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 2 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
#Acc: 83.82941  Train time: 133.2597  Test time: 8.204128

#5
PCA = 60 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 8 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
#Acc: 83.35294  Train time: 183.9439  Test time: 9.735827

#6
PCA = 35 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 2 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
#Acc: 83.51176  Train time: 118.9732  Test time: 4.645366

#7
PCA = 38 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 8 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
#Acc: 82.25588  Train time: 144.2221  Test time: 3.838687

# hypergrid best off:
#      acc pca ntree mtry nodesize sampsize time_rfTrainDuration
# 83.71471  50   300    2        5    54000             171.3973




#stay with 4
#4
PCA = 40 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 2 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable

{
  #TRAINING
  timerStart("RF TRAIN")
  rf <- randomForest(train$labels ~ ., data = pca_res$x[,0:PCA], ntree = NTREE, mtry = MTRY, nodesize = NODESIZE, sampsize = SAMPSIZE)
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

