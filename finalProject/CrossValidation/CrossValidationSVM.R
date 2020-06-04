rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)
library(kernlab)
library(ggplot2)

set.seed(423)

load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Exercises/idList-corner-100-new.Rdata")
id <- do.call(rbind, idList[1:45])
id <- as.data.frame(id)
sapply(id, class)
id<-transform(id, V1=as.factor(V1)) #needed so we have a categorization not a regression problem

sets <- c(1:5)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

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

maxVal <- function(x,y){
  index <- match(max(y),y)
  x_max <- x[index]
  return(x_max)
}

# ----------------------------------------------------- #
#                       DISJUNCT
# ----------------------------------------------------- #

# Hyperparameters
PCA = 60 #Dis = 60, All = 40
C = 1 #Dis = 1, All = 10
KERNEL = "rbfdot"

accs <- c(1:5)
traintimes <- c(1:5)
testtimes <- c(1:5)
folds <- createFolds(id$V1, k=5)

for (i in 1:5) {
  train_split <- id[-folds[[i]],]
  test_split <- id[folds[[i]],]
  train_labels <- train_split[,1]
  test_labels <- test_split[,1]
  
  train_data <- t(apply(train_split[,-1], 1, minmaxNorm))
  test_data <- t(apply(test_split[,-1], 1, minmaxNorm))
  
  pca_res <- prcomp(train_data)
  pca_pred <- predict(pca_res, test_data)
  
  {
    #TRAINING
    timerStart("RF TRAIN")
    svm_res <- ksvm(x=pca_res$x[,0:PCA], y=train_labels, kernel=KERNEL, C=C)
    print(svm_res) 
    time_rfTrainDuration <- timerEnd("")
    
    #TESTING
    timerStart("RF TEST")
    svm_pred <- predict(svm_res, newdata = pca_pred[,0:PCA], type = "response")
    time_rfTestDuration <- timerEnd("")
    
    #EVAL
    cm<-table(svm_pred, test_labels)
    acc <- accuracy(cm)
    cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
    
    accs[i] <- acc
    traintimes[i] <- time_rfTrainDuration
    testtimes[i] <- time_rfTestDuration
    
    }
  
  
}

mean(accs) # 96.90667
var(accs) # 0.01221296
mean(traintimes) # 571.2651
mean(testtimes) # 93.74246

training_error <- c(0.018792, 0.019028, 0.018431, 0.018375, 0.018181)*100

mean <- c(rep(mean(accs), 5))
accuracy_disjunct <- data.frame(sets, mean, accs)

ggplot(data = accuracy_disjunct, aes(sets))+geom_point(aes(y = accs))+geom_line(aes(y = mean, colour = "Mean Accuracy"))+
  ggtitle("Accuracy for each test set - Disjunct - SVM")+
  labs(y="Accuracy (%)", x = "CV Sets", colour = "") +
  ylim(90,100)+theme(legend.position = c(.2, .99))

t_error <- data.frame(sets, training_error)
ggplot(data = t_error, aes(sets))+ geom_point(aes(y=training_error))+
  ggtitle("Training Error for each set - Disjunct - SVM") +
  labs(y="Training Error (%)", x = "CV Sets")
# ----------------------------------------------------- #
#                     ALL PERSONS IN
# ----------------------------------------------------- #

# Hyperparameters
PCA = 40 #Dis = 60, All = 40
C = 10 #Dis = 1, All = 10
KERNEL = "rbfdot"

dataset_shuffle <- id[sample(nrow(id)),]

accs_API <- c(1:5)
traintimes_API <- c(1:5)
testtimes_API <- c(1:5)
folds_API <- createFolds(dataset_shuffle$V1, k=5)

for (i in 1:5) {
  train_split <- dataset_shuffle[-folds_API[[i]],]
  test_split <- dataset_shuffle[folds_API[[i]],]
  train_labels <- train_split[,1]
  test_labels <- test_split[,1]
  
  train_data <- t(apply(train_split[,-1], 1, minmaxNorm))
  test_data <- t(apply(test_split[,-1], 1, minmaxNorm))
  
  pca_res <- prcomp(train_data)
  pca_pred <- predict(pca_res, test_data)
  
  {
    #TRAINING
    timerStart("RF TRAIN")
    svm_res <- ksvm(x=pca_res$x[,0:PCA], y=train_labels, kernel=KERNEL, C=C)
    print(svm_res) 
    time_rfTrainDuration <- timerEnd("")
    
    #TESTING
    timerStart("RF TEST")
    svm_pred <- predict(svm_res, newdata = pca_pred[,0:PCA], type = "response")
    time_rfTestDuration <- timerEnd("")
    
    #EVAL
    cm<-table(svm_pred, test_labels)
    acc <- accuracy(cm)
    cat("Acc:",acc," Train time:",time_rfTrainDuration," Test time:",time_rfTestDuration)
    
    accs_API[i] <- acc
    traintimes_API[i] <- time_rfTrainDuration
    testtimes_API[i] <- time_rfTestDuration
    
  }
}
mean(accs_API) # 97.70444
var(accs_API) # 0.01070062
mean(traintimes_API) # 297.9864
mean(testtimes_API) #45.7713

training_error_API <- c(0.003028, 0.002847, 0.003, 0.003014, 0.002708)*100

mean_API <- c(rep(mean(accs_API), 5))
accuracy_API <- data.frame(sets, mean_API, accs_API)

ggplot(data = accuracy_API, aes(sets))+geom_point(aes(y = accs_API))+geom_line(aes(y = mean_API, colour = "Mean Accuracy"))+
  ggtitle("Accuracy for each test set - All Persons In - SVM")+
  labs(y="Accuracy (%)", x = "CV Sets", colour = "") +
  ylim(90,100)+theme(legend.position = c(.2, .99))

t_error_API <- data.frame(sets, training_error_API)
ggplot(data = t_error_API, aes(sets))+ geom_point(aes(y=training_error_API))+
  ggtitle("Training Error for each set - All Persons In - SVM") +
  labs(y="Training Error (%)", x = "CV Sets")
