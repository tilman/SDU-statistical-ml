library(class)

set.seed(423)

load("~/Downloads/id100.rda")
dataset_shuffle <- id[sample(nrow(id)),]
test_split <- dataset_shuffle[0:2000,-1]
train_split <- dataset_shuffle[2001:4000,-1]
test_classes <- dataset_shuffle[0:2000,1]
train_classes <- dataset_shuffle[2001:4000,1]

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
run_knn <- function(train_split, test_split, train_classes, k){
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

#1.4.1
run_knn(train_split, test_split, train_classes, 3)

#1.4.2
accs = c()
runtimes = c()
Ks = c()
for (i in c(1:100) ) {
  if(i %% 2 == 0){
    
  } else {
    ret <- run_knn(train_split, test_split, train_classes, i)
    accs[i] = ret$Accuracy
    runtimes[i] = ret$Runtime
    Ks[i] = ret$K
  }
}
plot(accs,type = "o")
plot(runtimes,type = "o")
plot(Ks,type = "o")


# 1.4.3: cross validation
#10% testing rest is training => the 10% slide over the data (so 10 runs in total)
accs <- c(1:10)
runtimes <- c(1:10)
folds <- createFolds(id$X1, k=10)
for (i in 1:10) {
  train_split <- id[-folds[[i]], -1]
  test_split <- id[folds[[i]],-1]
  train_classes <- id[-folds[[i]], 1]
  test_classes <- id[folds[[i]],1]
  
  ret <- run_knn(train_split, test_split, train_classes, k=3)
  accs[i] = ret$Accuracy
  runtimes[i] = ret$Runtime
}
print(runtimes)
print(accs)
mean(accs)
var(accs)



# 1.4.4 
load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Exercises/idList-corner-100-new.Rdata")

# Individual 
aList <- c(1:10)
for (i in c(1:10)) {
  id <- do.call(rbind, idList[i])
  id <- as.data.frame(id)
  id_shuffle <- id[sample(nrow(id)),]
  test_split <- id_shuffle[0:2000,-1]
  train_split <- id_shuffle[2001:4000,-1]
  
  test_classes <- id_shuffle[0:2000,1]
  train_classes <- id_shuffle[2001:4000,1]
  
  ret <- run_knn(train_split, test_split, train_classes, k=11)
  aList[i] <- ret$Accuracy
  cat("Folder:",i ," Accuracy:",ret$Accuracy, "\n")
}
aList
mean(aList)
var(aList)
# K=3:
# Accuracys: 99.20 98.60 97.40 99.35 99.60 98.50 97.85 96.50 99.05 99.30
# Mean: 98.535
# Var: 0.9994722
# K=10:
# Accuracys: 98.85 98.15 96.50 99.05 99.40 96.65 96.05 94.70 98.90 99.15
# Mean: 97.74
# Var: 2.669889

# all persons in
id <- do.call(rbind, idList[1:10])
dataset_shuffle <- id[sample(nrow(id)),]
test_split <- dataset_shuffle[0:2000,-1]
train_split <- dataset_shuffle[2001:4000,-1]
test_classes <- dataset_shuffle[0:2000,1]
train_classes <- dataset_shuffle[2001:4000,1]
ret <- run_knn(train_split, test_split, train_classes, k=3)
# train: 20000, test: 20000, K: 3  Accuracy: 98.03  Runtime: 4.366966 min
# train: 20000, test: 20000, K: 10  Accuracy: 97.36  Runtime: 4.412956 
# train: 2000, test: 2000, K: 3  Accuracy: 94.45  Runtime: 2.661257 sec
# train: 2000, test: 2000, K: 10  Accuracy: 92.5  Runtime: 2.483967 sec

# disjunct between persons
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
ret <- run_knn(train_split, test_split, train_classes, k=3)
# train: 20000, test: 20000, K: 3  Accuracy: 83.81  Runtime: 4.397083
# train: 20000, test: 20000, K: 10  Accuracy: 83.07  Runtime: 4.330401 
# train: 2000, test: 2000, K: 3  Accuracy: 95.95  Runtime: 2.636359
# train: 2000, test: 2000, K: 10  Accuracy: 95.6  Runtime: 2.699462 

