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

res3 = list()

cat("pca_count_i", "i",
    "acc", "pca_count", "ntree", "mtry", "nodesize", "sampsize",
    "time_pcaTrainDuration", "time_rfTrainDuration", "time_pcaTestDuration", "time_rfTestDuration","\n")

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

hyperparam_grid <- expand.grid(pca = pca_count_seq, mtry = mtry_seq, nodesize = nodesize_seq, sampsize = sampsize_seq, ntree = ntree_seq)

FILENAME = "/content/drive/My Drive/machine_learning/R/sdu_stats_ml/hypergrid_results_0-450_28052020.RData"

save(res3, file=FILENAME)
for(i in c(650:900)){
    #TRAINING
    timerStart("PCA TRAIN")
    pca_res <- prcomp(train$data, rank. = hyperparam_grid$pca[i])
    time_pcaTrainDuration <- timerEnd("")
    timerStart("RF TRAIN")
    rf <- randomForest(train$labels ~ .,
                       data = pca_res$x,
                       ntree = hyperparam_grid$ntree[i],
                       mtry = hyperparam_grid$mtry[i],
                       nodesize = hyperparam_grid$nodesize[i],
                       sampsize = hyperparam_grid$sampsize[i])
    time_rfTrainDuration <- timerEnd("")
    #TESTING
    timerStart("PCA TEST")
    pca_pred <- predict(pca_res, test$data)
    time_pcaTestDuration <- timerEnd("")
    timerStart("RF TEST")
    rf_pred <- predict(rf, pca_pred)
    time_rfTestDuration <- timerEnd("")
     
   #EVAL
   cm<-table(rf_pred, test$labels)
   acc <- accuracy(cm)
   
   cat(i,
       acc, hyperparam_grid$pca[i], hyperparam_grid$ntree[i], hyperparam_grid$mtry[i], hyperparam_grid$nodesize[i], hyperparam_grid$sampsize[i],
       time_pcaTrainDuration, time_rfTrainDuration, time_pcaTestDuration, time_rfTestDuration,"\n")

   res3[[i]] = list(i=i, acc=acc,
                   pca=hyperparam_grid$pca[i], ntree=hyperparam_grid$ntree[i], mtry=hyperparam_grid$mtry[i], nodesize=hyperparam_grid$nodesize[i], sampsize=hyperparam_grid$sampsize[i],
                   time_pcaTrainDuration=time_pcaTrainDuration, time_rfTrainDuration=time_rfTrainDuration, time_pcaTestDuration=time_pcaTestDuration, time_rfTestDuration=time_rfTestDuration)
   #save(res3, file=FILENAME)
}
