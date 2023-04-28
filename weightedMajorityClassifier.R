library(imager)
library(tidyr)
library(dplyr)
library(tensorflow)
library(keras)
install_tensorflow(extra_packages="pillow")
install_keras()

# Subalgorithm 1 - Contrast Color Overlay Square
# @params
# model - keras binary classifier
# input_path - path of input image
# @return
# Returns updated pixel locations and color channel values
contrast_color_overlay_square <- function(model, input_path) {
  #Load Image
  img <- load.image(input_path)
  img <- resize(img, 224,224)
  
  
  # Determine class of input folder if viewing first image
  test_image <- image_load(input_path, target_size = c(224,224))
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  original_class = which(pred > 0.5) - 1
  
  # Calculate center of image
  image_height <- dim(img)[1]
  image_width <- dim(img)[2]
  center_x <- image_width / 2
  center_y <- image_height / 2
  
  # Calculate square of 1% or less pixels at the center of the image
  total_pixels <- image_height * image_width
  pixel_budget <- total_pixels * 0.01
  center_offset <- floor(sqrt(pixel_budget) / 2) - 1
  
  # Determine ranges to perturb
  center_x_indices <- (center_x-center_offset):(center_x+center_offset)
  center_y_indices <- (center_y-center_offset):(center_y+center_offset)
  
  # Change the color of the selected pixels
  img[center_x_indices, center_y_indices, 1] <- original_class  # Red channel (1 if orig image is grass, else 0)
  img[center_x_indices, center_y_indices, 2] <- 1  # Green channel
  img[center_x_indices, center_y_indices, 3] <- 0  # Blue channel
  
  return(img[center_x_indicies, center_y_indices,])
}

# Subalgorithm 2 - Yellow - Green Shift
# Define the function
YGS <- function(model, input_path) {
  
    img <- load.image(input_path)
    
    # Resize the image
    img <- resize(img, 224, 224)
    
    # Calculate the threshold value for the 99th percentile of pixel values
    threshold <- quantile(img, probs = 0.99, na.rm = TRUE)
    
    # Get the position of pixels above the threshold with red or yellow value and replace them with blue
    high_pixels <- which(img[,,1] >= threshold | img[,,2] >= threshold)
    x_index <- high_pixels %% nrow(img)
    y_index <- ceiling(high_pixels / nrow(img))
    img[high_pixels] <- c(0, 0, 1)
    
    # Get the position of pixels in the grass and replace them with yellow
    low_pixels <- which(img[,,2] >= 0.5 & img[,,1] <= 0.5)
    img[low_pixels] <- c(1, 1, 0)
  
  # Return the position of the changed pixels
  return(img[x_index, y_index,])
}

alg1 <- contrast_color_overlay_square
alg2 <- YGS
alg3 <- alg1 # Replace with other algorithms
alg4 <- alg1 # Replace with other algorithms
alg5 <- alg1 # Replace with other algorithms

# Apply each subalgorithm to image and map pixel selection to determine votes
weighted_majority <- function(img, weights, algorithms) {
  model <- load_model_tf("/home/jupyter/dandelion_model")
  alg_outputs <- lapply(algorithms, function(alg) alg(model, img))
  combined_votes <- Reduce("+", Map("*", alg_outputs, weights))
  return(combined_votes)
}

# Creates an adversary image based on the highest voted pixels of 5 subalgorithms
weighted_adversarial_function <- function(img, pixel_budget_ratio) {
  # Load image and convert it to grayscale
  input_img <- load.image(img)
  input_img <- resize(input_img, 224, 224)
  grayscale_img <- grayscale(input_img)
  
  # Initialize weights and algorithms list, weights start as 1/n
  weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  algorithms <- list(alg1, alg2, alg3, alg4, alg5)
  
  # Apply the weighted majority algorithm
  combined_votes <- weighted_majority(grayscale_img, weights, algorithms)
  
  # Calculate the pixel budget
  total_pixels <- length(grayscale_img)
  pixel_budget <- round(total_pixels * pixel_budget_ratio)
  
  # Select top pixel_budget pixels
  pixel_ranking <- rank(-combined_votes, ties.method = "random")
  selected_pixels <- (pixel_ranking <= pixel_budget)
  
  # Modify the original image with the selected pixels
  output_img <- input_img
  # Replacing selected pixels with inverse value
  output_img[selected_pixels] <- 255 - output_img[selected_pixels]
  
  # Return the modified image
  return(output_img)
}

output_image <- weighted_adversarial_function("/home/jupyter/grass/5.jpg", 0.01)
