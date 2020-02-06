library(class)

dataset_shuffle <- id[sample(nrow(id)),]
test_split <- dataset_shuffle[0:2000,-1]
train_split <- dataset_shuffle[2001:4000,-1]
test_classes <- dataset_shuffle[0:2000,1]
train_classes <- dataset_shuffle[2001:4000,1]
test_prediction <- knn(train_split,test_split,cl=train_classes,k=3)
confusion_matrix <- table(test_prediction, test_classes)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)