rm(list = ls()) # clear/reset current environment
dev.off()
library(spatstat)
library(class)
library(caret)
library(kernlab)

load("C:/Users/maxim/Documents/_SDU/_Statistical Machine Learning/Lecture 1/id100.rda")

dataset <- id[,-1]
labels <- id[,1]
set.seed(423)
dataset_shuffle <- id[sample(nrow(id)),]

acc <- c()
i <- 1
train_split <- dataset_shuffle[0:3000,]
test_split <- dataset_shuffle[3001:4000,]

classifier <- ksvm(train_split[,1]~., data = train_split,
                   kernel ="vanilladot")

prediction <- predict(classifier,test_split)

agreement <- prediction == test_split[,1]
prop.table(table(agreement))

acc[i] <- prop.table(table(agreement))[2]

barplot(c)
