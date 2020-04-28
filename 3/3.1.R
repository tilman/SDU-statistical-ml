rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)
load("/Users/Tilman/Downloads/idList-corner-100-new.Rdata")
#load("/Users/Tilman/Downloads/idList-co-100.Rdata")

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
run_knn <- function(train_split, test_split, train_classes, test_classes, k){
  start_time <- Sys.time()
  test_prediction <- knn(train_split,test_split,cl=train_classes,k)
  end_time <- Sys.time()
  confusion_matrix <- table(test_prediction, test_classes)
  runtime = difftime(end_time,start_time,units="secs")
  acc = accuracy(confusion_matrix)
  cat("K:",k," Accuracy:",acc," Runtime:",runtime, "\n")
  df <- list(K=k,Accuracy=acc,Runtime=runtime)
  return(df)
}
run_kmeans_knn <- function(l,id_train,id_train_labels,id_test,id_test_labels){
  cipher_cluster <- c()
  label_cluster <- c()
  start_time <- Sys.time()
  for( i in 0:9) {
    clusterData <- kmeans(id_train[ id_train_labels == i, -1], l)
    cipher_cluster[[i + 1]] <- clusterData$centers  #cluster centers aka. Cluster means
    label_cluster[[i + 1]] <- c(1:l)*0 + i
  }
  end_time <- Sys.time()
  kmeans_runtime = end_time - start_time
  print(cat("kmeans_runtime",kmeans_runtime))
  train_lab <- factor(unlist(label_cluster))
  train_dat <- do.call(rbind, cipher_cluster)
  
  # cipher_cluster_test <- c()
  # label_cluster_test <- c()
  # for( i in 0:9) {
  #   clusterData <- kmeans(id_test[ id_test_labels == i, -1], l)
  #   cipher_cluster_test[[i + 1]] <- clusterData$centers  #cluster centers aka. Cluster means
  #   label_cluster_test[[i + 1]] <- c(1:l)*0 + i
  # }
  # test_lab <- factor(unlist(label_cluster_test))
  # test_dat <- do.call(rbind, cipher_cluster_test)
  # return(run_knn(train_dat, test_dat, train_lab, test_lab, 3))
  return(run_knn(train_dat, id_test[,-1], train_lab, id_test_labels, 3))
}





# 3.1.1
# small disjunct train/test split with only 2 persons each
id_train <- do.call(rbind, idList[1:2])
id_train <- as.data.frame(id_train)
id_train$V1 <- factor(id_train$V1)
id_test <- do.call(rbind, idList[3:4])
id_test <- as.data.frame(id_test)
id_test$V1 <- factor(id_test$V1)

id_test_labels <- id_test[,1]
id_train_labels <- id_train[,1]

set.seed(2345)
accs <- c()
runtimes <- c()
for(l in c(10,20,30,50,75,100,150,200,300,400)){
  ret <- run_kmeans_knn(l,id_train,id_train_labels,id_test,id_test_labels)
  accs[l] = ret$Accuracy
  runtimes[l] = ret$Runtime
}
# runtime and accuracy of raw training data
baseline <- run_knn(id_train[0:4000,-1], id_test[0:4000,-1], id_train_labels, id_test_labels, 3)
# => K: 3  Accuracy: 35.8  Runtime: 12.05315 

# 3.1.2
plot(accs_1,ylab="Accuracy",ylim=c(min(accs,na.rm = TRUE)-0.5,baseline$Accuracy+0.5),xlab="# Clusters",main="baseline accuracy in red based on raw data")
abline(h=baseline$Accuracy, col = "red")
plot(runtimes,ylab="Runtime",ylim=c(0.0001,baseline$Runtime+0.5),xlab="# Clusters",main="baseline runtime in red based on raw data")
abline(h=baseline$Runtime, col = "red")


# 3.1.2
# now perform above for bigger dataset
# big disjunct train/test split with 30 persons in train and 17 in test
id_train <- do.call(rbind, idList[1:30])
id_train <- as.data.frame(id_train)
id_train$V1 <- factor(id_train$V1)
id_test <- do.call(rbind, idList[31:47])
id_test <- as.data.frame(id_test)
id_test$V1 <- factor(id_test$V1)

id_test_labels <- id_test[,1]
id_train_labels <- id_train[,1]
accs <- c()
runtimes <- c()
for(l in c(10,20,30,50,75,100,150,200,250,350,400,450,500,600,700,800,900,1000)){
  ret <- run_kmeans_knn(l,id_train,id_train_labels,id_test,id_test_labels)
  accs[l] = ret$Accuracy
  runtimes[l] = ret$Runtime
}
# runtime and accuracy of raw training data
#baseline <- run_knn(id_train[,-1], id_test[,-1], id_train_labels, id_test_labels, 3)
baseline$Accuracy = 83.45
baseline$Runtime = 1778.6646
#$K [1] 3
#$Accuracy [1] 83.45
#$Runtime Time difference of 29.64441 mins
plot(accs,ylab="Accuracy",ylim=c(min(accs,na.rm = TRUE)-0.5,baseline$Accuracy+0.5),xlab="# Clusters",main="baseline accuracy in red based on raw data")
abline(h=baseline$Accuracy, col = "red")
plot(runtimes,ylab="Runtime",ylim=c(min(runtimes,na.rm = TRUE)-0.5,baseline$Runtime+0.5),log="y",xlab="# Clusters",main="baseline runtime in red based on raw data")
abline(h=baseline$Runtime, col = "red")
plot(runtimes,ylab="Runtime",xlab="# Clusters",main="baseline runtime: 29.64m (1778s)")

