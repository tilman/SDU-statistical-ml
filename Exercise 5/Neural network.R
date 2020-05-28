rm(list = ls()) # clear/reset current environment
dev.off()
library(spatstat)
library(class)
library(caret)
library(RSNNS)

load("C:/Users/maxim/Documents/_SDU/_Statistical Machine Learning/Lecture 1/id100.rda")

dataset <- id[,-1]
labels <- id[,1]
set.seed(423)
dataset_shuffle <- id[sample(nrow(id)),]
# Training and testing datasets
lev <- levels(dataset_shuffle$X1) # Number of classes

# Create a list probabilities, for all labels
nnTrainingClass <- matrix(nrow = length(dataset_shuffle$X1), ncol = 10, data = 0) 


for(i in 1:length(dataset_shuffle$X1)) {
  # Set probabilities to one for matching class
  matchList <- match(lev,toString(dataset_shuffle$X1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nnTrainingClass)

train_split <- dataset_shuffle[0:3000,]
test_split <- dataset_shuffle[3001:4000,]
train_10 <- trainingClass[0:3000,]
test_10 <- trainingClass[3001:4000,]


# TESTING THE NUMBER OF NODES
#
acc <- c()
neur <- c()
l <- 1
vrb <- c(5,10,15,20,25,30,35,40,45,50,100,150)
for (k in vrb) {
  network <- mlp(train_split[,-1], train_10, size = k)
  predictions <- predict(network, test_split[,-1])
  #Using the predict function we have recieved "predictions"
  responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
  for(i in 1:nrow(predictions)) {
    # returns the number corresponding to the position of the max in the predictions (closest to 1)
    responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculating the accuracy
  agreement_rbf <- responselist[,1] == test_split[,1]
  # table(agreement_rbf)
  
  acc[l] <- prop.table(table(agreement_rbf))[2]
  neur[l] <- k
  l <- l + 1
}


plot(neur,acc,
     main = "Effect of the number of nodes in a layer",
     xlab = "number of nodes", 
     ylab = "accuracy",)

# We can see from the plot that around 35 is the "best" 

# TESTING THE NUMBER OF LAYERS 
#
acc <- c()
neur <- c()
u <- c(30,30,30,30,30,30)
v <- 6

network <- mlp(train_split[,-1], train_10, size = u)
predictions <- predict(network, test_split[,-1])
#Using the predict function we have recieved "predictions"
responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
for(i in 1:nrow(predictions)) {
  # returns the number corresponding to the position of the max in the predictions (closest to 1)
  responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
}
responselist <- data.frame(responselist)
responselist[,1] <- as.factor(responselist[,1])
# Calculating the accuracy
agreement_rbf <- responselist[,1] == test_split[,1]
# table(agreement_rbf)

acc[v] <- prop.table(table(agreement_rbf))[2]
neur[v] <- length(u)

plot(neur[1:4],acc[1:4],
     main = "Effect of the number of hidden layers",
     xlab = "number of layers", 
     ylab = "accuracy",)

# In our case one layer seems to be the best option
# So for the next experiment we go for 30 and 1 layer

# TESTING THE STANDARD BACK PROPAGATION
#

acc <- c()
neur <- c()
l <- 1
vrb <- c(0.1,0.2,0.3)

for (k in vrb) {
  network <- mlp(train_split[,-1], train_10, size = 30,
                 learnFunc = "Std_Backpropagation", 
                 learnFuncParams = c(k))
  predictions <- predict(network, test_split[,-1])
  #Using the predict function we have recieved "predictions"
  responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
  for(i in 1:nrow(predictions)) {
    # returns the number corresponding to the position of the max in the predictions (closest to 1)
    responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculating the accuracy
  agreement_rbf <- responselist[,1] == test_split[,1]
  # table(agreement_rbf)
  
  acc[l] <- prop.table(table(agreement_rbf))[2]
  neur[l] <- k
  l <- l + 1
}


plot(neur,acc,
     main = "Effect of the use of Std-Back propagation",
     xlab = "Learning Rate", 
     ylab = "accuracy")

