rm(list = ls()) # clear/reset current environment
dev.off() # clear plots
library(spatstat)
library(class)
library(caret)
load("/Users/Tilman/Downloads/idList-co-100.Rdata")

id <- do.call(rbind, idList[1:10]) # transform multi dimension data frame to list of datapoints
id_mat <- data.matrix(id, rownames.force = NA)
rotate <- function(x) t(apply(x, 2, rev))
imageSize = 18

# Show first 10 images

data <- id_mat[,2:ncol(id_mat)]
labels <- id_mat[,1]
id_pca <- prcomp(data)

std_dev <- id_pca$sdev # pca standard deviation
var <- std_dev^2 # pca variance
prop_var <- var/sum(var) # pca proportional variance (between 0 and 1)
cum_prop_var <- cumsum(prop_var) # cumsum of proportional variance


par(mfrow=c(2,5),mar=rep(2,2))
# 2.4.1
for(i in 1:10){
  cipherNumber <- -400+i*400+1
  rotated <- c(id_mat[cipherNumber,2:ncol(id_mat)])
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('image:',i)) )
  
}

par(mfrow=c(5,2))
# 2.4.2
for(i in 1:10){
  rotated <- 1 - id_pca$rotation[,i]
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('Eigenvector:',i)) )
}

par(mfrow=c(5,2))
# 2.4.3
for(i in 1:10){
  trunc <- id_pca$x[cipherNumber,cum_prop_var < 0.99] %*% 
            t(id_pca$rotation[,cum_prop_var < 0.99])
  rotated <- scale(trunc, center = -1 * id_pca$center, scale=FALSE)
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('Reconstruction:',i)) )
}


