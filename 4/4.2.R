library(randomForest)
library(class)
library(caret)
library(rpart)

load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Lecture 1 - Exo 1/idList-co-100.rdata")
id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)

sapply(id, class)
id<-transform(id, V1=as.factor(V1))

set.seed(423)
dataset_shuffle <- id[sample(nrow(id)),]
train_data <- dataset_shuffle[1:30000,]
test_data <- dataset_shuffle[30001:40000,]
#train_labels <- train_data$V1

pca_res <- prcomp(train_data[,-1])
summary(pca_res)
#95% is 36 PC

pca_95 <- prcomp(train_data[,-1], rank. = 36)
#summary(pca_95)
train_data_pca <- pca_95$x

datatree <- rpart(V1 ~ ., data=train_data, method="class")
print(datatree, digits = 3)

predictions1 <- predict(datatree,test_data[,-1], type = "class")
table(predictions1, test_data$V1)



rf <- randomForest(train_data$V1 ~ ., data=train_data_pca)
rf



# Cross validation

accs <- c(1:10)
folds <- createFolds(id$X1, k=10)
for (i in 1:10) {
  train_split <- id[-folds[[i]],]
  test_split <- id[folds[[i]],]
  pca_95 <- prcomp(train_data[,-1], rank. = 36)
  #summary(pca_95)
  train_data_pca <- pca_95$x
  rf <- randomForest(train_split$V1 ~ ., train_data_pca)
  accs[i] <- rf$err.rate
}