#install.packages("devtools")
#devtools::install_github("ymattu/MlBayesOpt")
library(MlBayesOpt)

rm(list = ls()) # clear/reset current environment
#dev.off() # clear plots
library(randomForest)
library(class)
library(caret)
library(rpart)
set.seed(423)

#load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Lecture 1 - Exo 1/idList-co-100.rdata")
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


set.seed(71)
res0 <- rf_opt(train_data = train$data,
               train_label = train$labels,
               test_data = test$data,
               test_label = test$labels,
               mtry_range = c(2, 300),
               num_tree = 10,
               init_points = 1,
               n_iter = 1)

