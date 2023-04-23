# Install packages
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
install_tensorflow(extra_packages="pillow")
install_keras()

# Load classifier model
classifier = load_model_tf("/home/jupyter/dandelion_model")

width <- 224
height<- 224
target_size <- c(width, height)
rgb <- 3 #color channels

# Identify misclassified grass images
f=list.files("home/jupyter/grass")
for (i in f){
  test_image <- image_load(paste("home/jupyter/grass/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if(pred[1,2]<0.50){
    print(i)
  }
}

# Identify misclassified dandelion images
f=list.files("home/jupyter/dandelions")
for (i in f){
  test_image <- image_load(paste("home/jupyter/dandelions/",i,sep=""),
                           target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if(pred[1,1]<0.50){
    print(i)
  }
}
