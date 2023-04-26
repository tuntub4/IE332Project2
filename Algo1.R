#install the required packages
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(jpeg)

install_tensorflow(extra_packages="pillow")
install_keras()

#set the working directory to location of files on the computer
setwd("C:/Users/eparmen/Desktop/Project2")
getwd()

#code to load the model
model <- load_model_tf("./dandelion_model")
summary(model)



#function that makes modifies images
#image is the image to be changed
#P is the percent of pixels I get to change
random <- function (x, P) {
  #test_image <- image_load(paste("./grass/",image,sep=""))#, target_size = target_size)
  #x <- image_to_array(test_image)
  x_size <- length(x[,1,1])
  y_size <- length(x[1,,1])
  #now loop through the image
  for (i in 1:x_size) {
    for (j in 1:y_size) {
      if ((((j-1) * x_size) + i) %% P == 0) {  #condition for what pixels you want to change
        #RGB values should be 0 to 1
        x[i,j,1] <- 1 #modifies red value
        x[i,j,2] <- 1 #modifies green value
        x[i,j,3] <- 0 #modifies blue value
      }
    }
  }
  # Convert the modified x array back to an image
  # "./grass/modified_grass.jpg" is what you want to save the modified image as
  writeJPEG(x, "./grass/modified_grass.jpg")
  
  # Read the modified image file
  modified_image <- jpeg::readJPEG("./grass/modified_grass.jpg")
  
  # Display the modified image
  graphics::plot(1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="", ylab="")
  graphics::rasterImage(modified_image, 0, 0, 1, 1)
}



#testing
target_size <- c(224, 224)

#modified code given from Brightspace
res=c("","")
f=list.files("./grass")
for (i in f){
  test_image <- image_load(paste("./grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  #x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  
  #change the image
  P <- 5
  random(x, P)
  
  #load the new image - point to where you saved the modified image
  new_img <- image_load("./grass/modified_grass.jpg",
                        target_size = target_size)
  new <- image_to_array(new_img)
  new <- array_reshape(new, c(1, dim(new)))
  
  
  pred <- model %>% predict(new)
  #if(pred[1,2]<0.50){
  print(pred)
  #}
}

res=c("","")
f=list.files("./dandelions")
for (i in f){
  test_image <- image_load(paste("./dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  #x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  
  #change the image
  P <- 5
  random(x, P)
  
  #load the new image
  new_img <- image_load("./grass/modified_grass.jpg",
                        target_size = target_size)
  new <- image_to_array(new_img)
  new <- array_reshape(new, c(1, dim(new)))
  
  
  pred <- model %>% predict(new)
  #if(pred[1,2]<0.50){
  print(pred)
  #}
  #if(pred[1,1]<0.50){
  #  print(i)
  #}
}



#code to run model without changing an image
res=c("","")
f=list.files("./grass")
for (i in f){
  test_image <- image_load(paste("./grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  
  pred <- model %>% predict(x)
  print(pred)
}

res=c("","")
f=list.files("./dandelions")
for (i in f){
  test_image <- image_load(paste("./dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  
  pred <- model %>% predict(x)
  print(pred)
}


