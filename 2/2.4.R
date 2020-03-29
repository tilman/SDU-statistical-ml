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


par(mfrow=c(2,5),mar=c(2,2,2,2))
# 2.4.1
for(i in 1:10){
  cipherNumber <- -400+i*400+1
  rotated <- c(id_mat[cipherNumber,2:ncol(id_mat)])
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('image:',i)) )
}

par(mfrow=c(2,5),mar=c(2,2,2,2))
# 2.4.2
for(i in 1:10){
  rotated <- id_pca$rotation[,i]
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('Eigenvector:',i)) )
}

par(mfrow=c(2,5),mar=c(2,2,2,2))
# 2.4.3
for(i in 1:10){
  cipherNumber <- -400+i*400+1
  trunc <- id_pca$x[cipherNumber,cum_prop_var < 0.99] %*% #73 eigenvectors
            t(id_pca$rotation[,cum_prop_var < 0.99])
  rotated <- scale(trunc, center = -1 * id_pca$center, scale=FALSE)
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('Reconstruction:',i)) )
}

# 2.4.4
par(mfrow=c(2,5),mar=c(2,2,2,2))
for(i in 1:10){
  cipherNumber <- -400+i*400+1
  trunc <- id_pca$x[cipherNumber,cum_prop_var < 0.90] %*% #73 eigenvectors
    t(id_pca$rotation[,cum_prop_var < 0.90])
  rotated <- scale(trunc, center = -1 * id_pca$center, scale=FALSE)
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('Reconstruction:',i)) )
}

#2.4.5
# scores for cipher 43 
id_pca$x[43,cum_prop_var < 0.72]
# scores for cipher 456
id_pca$x[456,cum_prop_var < 0.72]

# scores for mean of all ciphers 0
scores_0 <- id_pca$x[0:400,cum_prop_var < 0.72]
means_0 <- c()
for(i in 1:10){ means_0[i] <- mean(scores_0[,i]) }
print(means_0)

# scores for mean of all ciphers 1
scores_1 <- id_pca$x[401:800,cum_prop_var < 0.72]
means_1 <- c()
for(i in 1:10){ means_1[i] <- mean(scores_1[,i]) }
print(means_1)






# plot image with the means
trunc <- means_0 %*% t(id_pca$rotation[,cum_prop_var < 0.72])
rotated <- scale(trunc, center = -1 * id_pca$center, scale=FALSE)
rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
image <- rotate(image)
image( image, zlim=c(0,1), col=gray(0:100/100), main='Reconstruction: mean 0 with 10 Eigenvectors' )


trunc <- means_1 %*% t(id_pca$rotation[,cum_prop_var < 0.72])
rotated <- scale(trunc, center = -1 * id_pca$center, scale=FALSE)
rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
image <- rotate(image)
image( image, zlim=c(0,1), col=gray(0:100/100), main='Reconstruction: mean 1 with 10 Eigenvectors' )






par(mfrow=c(2,5),mar=c(2,2,2,2))
# 2.4.2
for(i in 1:10){
  rotated <- id_pca$rotation[,i]
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  rotated <- means_0[i] * rotated
  rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
  image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
  image <- rotate(image)
  image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('score:',means_0[i])) )
}
