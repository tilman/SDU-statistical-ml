library(spatstat)
library(class)
library(caret)

load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Lecture 1 - Exo 1/idList-co-100.rdata")

set.seed(423)

#PCA on All persons in
dataset_shuffle <- id[sample(nrow(id)),]
train_split <- dataset_shuffle[0:20000,-1]
test_split <- dataset_shuffle[20001:40000,-1]
train_classes <- dataset_shuffle[0:20000,1]
test_classes <- dataset_shuffle[20001:40000,1]

train_split.pca <- prcomp(train_split)
comp_80 <- 15
comp_90 <- 25
comp_95 <- 36
comp_99 <- 74
pca_data <- train_split.pca$x

summary(train_split.pca)
str(train_split.pca)

#test.pca <- predict(train_split.pca, test_split)

cm <- confusionMatrix(test_classes, )
cm$table

#accuracy
sum(diag(cm$table))/sum(cm$table)*100



#PCA on Disjunct
id_train2 <- do.call(rbind, idList[1:5])
id_train2 <- as.data.frame(id_train2)
id_train2$V1 <- factor(id_train2$V1)
id_test2 <- do.call(rbind, idList[6:10])
id_test2 <- as.data.frame(id_test2)
id_test2$V1 <- factor(id_test2$V1)

test2_split <- id_test2[0:20000,-1]
train2_split <- id_train2[0:20000,-1]

train2_split.pca <- prcomp(train2_split)

summary(train2_split.pca)

comp2_80 <- 14
comp2_90 <- 24
comp2_95 <- 35
comp2_99 <- 75

