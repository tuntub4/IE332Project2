library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(imager)
install_tensorflow(extra_packages="pillow")
install_keras()

classifier = load_model_tf("/home/jupyter/dandelion_model")

create_perturbed_images("/home/jupyter/grass/")

suppressWarnings(evaluate_pixel_diffs(classifier, "/home/jupyter/grass/", "/home/jupyter/grass_perturbed/"))

# Utility Function to Load and Resize Images
load_image <- function(image_path) {
  img <- keras::image_load(image_path, target_size = c(224, 224))
  img <- keras::image_to_array(img)
  img <- img / 255.0
  img <- array_reshape(img, c(1, 224, 224, 3))
  return(img)
}

# Produces an adversarial image that attempts to make the inputted model misclassify
fgsm <- function(model, input_image, epsilon = 0.01) {
  # Create an input tensor for the images
  input_tensor <- tf$Variable(input_image, dtype = tf$float32)
  with(tf$GradientTape() %as% tape, {
    tape$watch(input_tensor)
    prediction <- model(input_tensor)
    loss <- tf$reduce_sum(tf$nn$softmax_cross_entropy_with_logits(labels = prediction, logits = prediction))
  })
  
  # Calculate gradient from loss with respect to the input tensor
  gradient <- tape$gradient(loss, input_tensor)
  gradient_abs <- tf$abs(gradient)
  
  # Define ceiling for number of modified pixels (1%)
  num_pixels <- 224 * 224
  num_modified_pixels <- as.integer(ceiling(num_pixels * 0.01))
  
  # Select indices to change pixels
  total_elements <- as.integer(tf$reduce_prod(tf$shape(gradient_abs)))
  gradient_abs_1d <- tf$reshape(gradient_abs, c(total_elements))
  top_pixels_indices <- tf$nn$top_k(gradient_abs_1d, k = num_modified_pixels)$indices
  top_pixels_indices <- tf$cast(top_pixels_indices, tf$int64)
  reshaped_indices <- tf$reshape(top_pixels_indices, shape = tf$cast(c(num_modified_pixels, 1), tf$int32))
  ordered_indices <- tf$sort(reshaped_indices, axis = 0)
  mask_sparse <- tf$SparseTensor(ordered_indices, tf$ones(num_modified_pixels), dense_shape = tf$cast(tf$shape(gradient_abs_1d), tf$int64))
  mask <- tf$sparse$to_dense(mask_sparse)
  mask <- tf$reshape(mask, tf$shape(gradient_abs))
  
  # Modify the pixels in the direction of the gradient
  perturbation <- epsilon * tf$sign(gradient) * tf$cast(mask, tf$float32)
  perturbed_image <- input_image + perturbation
  perturbed_image <- tf$clip_by_value(perturbed_image, 0, 1)
  return(perturbed_image)
}

# Given a folder path, creates perturbed images for all images in that folder
create_perturbed_images <- function(input_image_path) {
  # List files TODO: (needs to not be hard coded for grass in future)
  f=list.files(input_image_path)
  
  for (i in f){
    input_image <- load_image(paste("/home/jupyter/grass/",i,sep=""))
    # Apply fgsm to each image
    perturbed_output <- fgsm(classifier, input_image, epsilon=0.01)
    perturbed_image_array <- drop(as.array(perturbed_output)) 
    perturbed_image_array <- aperm(perturbed_image_array, c(2, 1, 3))
    perturbed_image <- as.cimg(perturbed_image_array)
    # Save image in perturbed folder
    save.image(perturbed_image, paste("/home/jupyter/grass_perturbed/pert_", i, sep=""))
  }
}

# Evaluates the difference between original and adversarial images and model prediction of adversarial
evaluate_pixel_diffs <- function(classifier, input_image_path, output_image_path) {
  # using threshold to avoid floating point calc errors
  threshold <- 0.001
  
  # get file names for original and adversary images
  in_f=list.files(input_image_path)
  out_f=list.files(output_image_path)
  
  # Iterate through each image title, find perturbed version, calculate number of different pixels, and display pred results
  for (i in in_f){
    cat(paste("\n",i))
    input_image <- load_image(paste("/home/jupyter/grass/",i,sep=""))
    output_image <- load_image(paste("/home/jupyter/grass_perturbed/pert_", i, sep=""))
    
    diff_pixels <- abs(input_image - output_image)
    diff_pixels <- drop(diff_pixels)
    diff_pixels <- apply(diff_pixels, c(1, 2), function(x) any(x > threshold))
    cat("\nSum Diff Pixels: ", sum(diff_pixels))
    
    # Plot of pixel differences
    plot(as.cimg(drop(input_image) - drop(output_image)))
    
    pred <- classifier %>% predict(output_image)
    cat(paste("\n",pred))
  }
}