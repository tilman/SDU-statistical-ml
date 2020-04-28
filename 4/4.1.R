rm(list = ls()) # clear/reset current environment
try(dev.off()) # clear plots
library(spatstat)
library(class)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Lecture 1 - Exo 1/id100.rda")

dataset <- id[,-1]
labels <- id[,1]

pca_res <- prcomp(dataset, rank. = 5)
# first row: pca_res$x[1,] (5 pca components), 
#   (abstraction of dataset$train[1,]) 
#   with label dataset$train_labels[1]

pca_res$x
#pca_predict <- predict(pca_res, dataset$test)

entropy <- function(dataSegmentLabels){
  res = 0;
  for(i in c(0:9)){
    #print(i)
    count = length(dataSegmentLabels[dataSegmentLabels == i])
    if(count > 0) { 
      p = count/length(dataSegmentLabels) 
      res = res - p*log2(p)
    }
  }
  return(res)
}



# 4.1 do the first split in a decission tree and calculate the info gain. Do this for every of the 5 pca components
# compute the decision point. use range from -2 to 2 and with a resolution of 200
# a decision point splits the dataset in a left and a right part, based on the threshold

#for all PCAs
#for(dp = -2 to 2 with stepsize 4/200){
#  split dataset on dp
#  calc Ent(allClasses) = 1
#  Ent(S2) = sum of all weighted ents
#  InfoGain = 1 - Ent(S2)
#}

#-1.6 till 1.6 because of 
#> max(pca_res$x) -> 1.571129
#> min(pca_res$x) -> -1.536284

ent_before = entropy(labels)
steps = 50
dps = seq(-1.6, 1.6, by=3.2/steps)
info_gain = c()
colors=c("blue","purple","red","green","magenta")
PCs=c("PC1","PC2","PC3","PC4","PC5")

for(PCi in seq(1,5)){
  info_gain = c()
  for(i in seq(1,steps+1) ){
    left = labels[ pca_res$x[,PCi] >= dps[i] ]
    right = labels[ pca_res$x[,PCi] < dps[i] ]
    ent_after = (length(left)/length(labels))*entropy(left) + (length(right)/length(labels))*entropy(right)
    info_gain[i] = ent_before - ent_after
  }
  print(cat("dp:",dps[i],", info_gain:",info_gain,"\n"))
  if(PCi == 1){
    plot(dps, info_gain, ylim=0:1, type="o", col=colors[PCi])
  } else {
    lines(dps, info_gain, type="o", col=colors[PCi])
  }
}
legend(-1.6, 1, PCs, cex=0.8, col=colors, pch=21:22, lty=1:5)




# 4.1.2
pca_res <- prcomp(id[,-1], rank. = 5)
dataAndLabels <- data.frame(label = id[,1], pca_res$x)
#dataAndLabels <- data.frame(label = id[,1], id[,-1])
tree <- rpart(dataAndLabels$label ~ .,method = "class", data = dataAndLabels)
rpart.plot(tree,type = 4, extra = 100, box.palette = "RdYlGn")
#fancyRpartPlot(tree)


# 4.1.3
load("/Users/baptiste/Documents/SDU-TEK/Statistical Machine Learning/Lecture 1 - Exo 1/idList-co-100.rdata")
id <- do.call(rbind, idList[1:10])
id <- as.data.frame(id)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}


accs <- c(1:5)
folds <- createFolds(id$V1, k=5) 
for (i in 1:5) {
  train_split <- id[-folds[[i]],]
  test_split <- id[folds[[i]],]
  tree<-
}