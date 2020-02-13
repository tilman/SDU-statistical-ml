library(class)

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
  confusion_matrix <- table(test_prediction, test_classes)
  
  end_time <- Sys.time()
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

#cross validation
#10% testing rest is training => the 10% slide over the data (so 10 runs in total)