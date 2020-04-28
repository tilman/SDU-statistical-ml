library(spatstat)
library(class)
library(caret)

load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Lecture 1 - Exo 1/idList-co-100.rdata")

#3.2.1
data<-do.call(rbind, idList[1])
data <- as.data.frame(data)

data0 <- data[1:5,]
data1 <- data[401:405,]
data2 <- data[801:805,]
data3 <- data[1201:1205,]
data4 <- data[1601:1605,]
data5 <- data[2001:2005,]
data6 <- data[2401:2405,]
data7 <- data[2801:2805,]
data8 <- data[3201:3205,]
data9 <- data[3601:3605,]

data_final <- rbind(data0,data1,data2,data3,data4,data5,data6,data7,data8,data9)
data_used <- data_final[-1]

distance <- dist(data_used)

hc = hclust(distance)
plot(hc, labels = data_final$V1)

#3.2.2
set.seed(2345)
cipher_cluster <- c()
label_cluster <- c()
for( i in 0:9) {
  clusterData <- kmeans(data[ data$V1 == i, ], 5)
  cipher_cluster[[i + 1]] <- clusterData$centers
  label_cluster[[i + 1]] <- c(1:5)*0 + i
}
train_lab <- factor(unlist(label_cluster))
train_dat <- do.call(rbind, cipher_cluster)
train_dat <- as.data.frame(train_dat)

distance1 = dist(train_dat[-1])
hc1 = hclust(distance1)
plot(hc1, labels = train_dat$V1)
