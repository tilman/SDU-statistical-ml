rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)
library(randomForest)
library(rpart)
library(furrr)

load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Exercises/idList-corner-100-new.Rdata")
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}


# RandomForest CV
minmaxNorm <- function(line){
  return((line - min(line)) / (max(line)-min(line)))
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

#Hiscore:
#Final hypers with 4
PCA = 40 #try1 40 - 60
NTREE = 300 #ab 200 fast stable, 300 wenig besser
MTRY = 4 #try3 4-8, aber relativ stablil
NODESIZE = 5 #try2 bis 1
SAMPSIZE = 58000 #stable
# Acc: 85.67647  Train time: 138.7246  Test time: 6.371468 # with image wise norm


# CV Disjunct Random Forest
# We only take 40 so we can get 10 folds of exactly 4 people
id <- do.call(rbind, idList[1:40])
id <- as.data.frame(id)
sapply(id, class)
id<-transform(id, V1=as.factor(V1)) #needed so we have a categorization not a regression problem

accs <- c(1:10)
traintimes <- c(1:10)
testtimes <- c(1:10)
folds <- createFolds(id$V1, k=10)

for (i in 1:10) {
  train_split <- id[-folds[[i]],]
  test_split <- id[folds[[i]],]
  train_labels <- train_split[,1]
  test_labels <- test_split[,1]
  
  train_data <- t(apply(train_split[,-1], 1, minmaxNorm))
  test_data <- t(apply(test_split[,-1], 1, minmaxNorm))
  
  
  timerStart("PCA TRAIN")
  pca_res <- prcomp(train_data, .rank=PCA)
  time_rfTrainDuration <- timerEnd("")
  timerStart("PCA TEST")
  pca_pred <- predict(pca_res, test_data)
  time_rfTrainDuration <- timerEnd("")
  
  {
    #TRAINING
    timerStart("RF TRAIN")
    rf <- randomForest(train_labels ~ ., data = pca_res$x[,0:PCA], ntree = NTREE, mtry = MTRY, nodesize = NODESIZE)#, sampsize = SAMPSIZE)
    time_rfTrainDuration <- timerEnd("")
    
    #TESTING
    timerStart("RF TEST")
    rf_pred <- predict(rf, pca_pred)
    time_rfTestDuration <- timerEnd("")
    
    #EVAL
    cm<-table(rf_pred, test_labels)
    acc <- accuracy(cm)
    cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
    
    accs[i] <- acc
    traintimes[i] <- time_rfTrainDuration
    testtimes[i] <- time_rfTestDuration
  }
}

var(accs) # 0.04316667
mean(accs) # 94.8175%

sets <- c(1:10)

mean <- as.list(rep(mean(accs), 10))

plot(sets, accs, main = "Accuracy for each test set - Disjunct", ylim = c(90, 100))
lines(sets, mean, col="red")
legend("topleft", legend="Mean Accuracy", col="red",lty=1:1, cex=0.6)
plot(sets, traintimes)

# CV AllPersonsIn RandomForest
dataset_shuffle <- id[sample(nrow(id)),]

accs_API <- c(1:10)
traintimes_API <- c(1:10)
testtimes_API <- c(1:10)
folds_API <- createFolds(dataset_shuffle$V1, k=10)

for (i in 1:10) {
  train_split <- dataset_shuffle[-folds_API[[i]],]
  test_split <- dataset_shuffle[folds_API[[i]],]
  train_labels <- train_split[,1]
  test_labels <- test_split[,1]
  
  train_data <- t(apply(train_split[,-1], 1, minmaxNorm))
  test_data <- t(apply(test_split[,-1], 1, minmaxNorm))
  
  
  timerStart("PCA TRAIN")
  pca_res <- prcomp(train_data, .rank=PCA)
  time_rfTrainDuration <- timerEnd("")
  timerStart("PCA TEST")
  pca_pred <- predict(pca_res, test_data)
  time_rfTrainDuration <- timerEnd("")
  
  {
    #TRAINING
    timerStart("RF TRAIN")
    rf <- randomForest(train_labels ~ ., data = pca_res$x[,0:PCA], ntree = NTREE, mtry = MTRY, nodesize = NODESIZE)#, sampsize = SAMPSIZE)
    time_rfTrainDuration <- timerEnd("")
    
    #TESTING
    timerStart("RF TEST")
    rf_pred <- predict(rf, pca_pred)
    time_rfTestDuration <- timerEnd("")
    
    #EVAL
    cm<-table(rf_pred, test_labels)
    acc <- accuracy(cm)
    cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
    
    accs_API[i] <- acc
    traintimes_API[i] <- time_rfTrainDuration
    testtimes_API[i] <- time_rfTestDuration
  }
}
var(accs_API) # 0.05344444
mean(accs_API) # 94.8825%

mean_API <- as.list(rep(mean(accs_API), 10))

plot(sets, accs_API, main = "Accuracy for each test set - All Persons In", ylim = c(90, 100))
lines(sets, mean_API, col="red")
legend("topleft", legend="Mean Accuracy", col="red",lty=1:1, cex=0.6)
plot(sets, traintimes)