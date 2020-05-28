plotImages <- function(student){
  id <- do.call(rbind, idList[1:47]) # transform multi dimension data frame to list of datapoints
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
  #first student
  for(i in 1:10){
    cipherNumber <- -200+i*200+1 + ((student-1)*2000)
    print(cipherNumber)
    rotated <- c(id_mat[cipherNumber,2:ncol(id_mat)])
    #rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated))) #norm
    image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
    image <- rotate(image)
    image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('image:',i)) )
  }
  
  
  # plot first 10 eigenvectors
  par(mfrow=c(2,5),mar=c(2,2,2,2))
  for(i in 1:10){
    rotated <- id_pca$rotation[,i]
    rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
    image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
    image <- rotate(image)
    image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('Eigenvector:',i)) )
  }
  
  # plot reconstruction of image with EVs with cumprop higher as 90% -> 73 EVs
  par(mfrow=c(2,5),mar=c(2,2,2,2))
  for(i in 1:10){
    cipherNumber <- -200+i*200+1 + ((student-1)*2000)
    trunc <- id_pca$x[cipherNumber,cum_prop_var < 0.999] %*% #73 eigenvectors
      t(id_pca$rotation[,cum_prop_var < 0.999])
    print(length(trunc))
    rotated <- scale(trunc, center = -1 * id_pca$center, scale=FALSE)
    rotated <- ((rotated - min(rotated)) / (max(rotated) - min(rotated)))
    image <- matrix(rotated, nrow = imageSize, ncol = imageSize, byrow = FALSE)
    image <- rotate(image)
    image( image, zlim=c(0,1), col=gray(0:100/100), main=capture.output(cat('Reconstruction:',i)) )
  }
}


load("/Users/Tilman/Downloads/idList-corner-100-new.Rdata")
plotImages(6)
load("/Users/Tilman/Downloads/idList-mid-100-new.Rdata")
plotImages(6)


# Preprocessing Todo:
# 1. found grayscale background (probably caused by normalization) (idList-corner-100-new.Rdata, student 6) -> 
# 2. 