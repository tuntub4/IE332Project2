# Install Packages
library(keras)
library(tensorflow)
library(reticulate)
library(imager)
install_tensorflow(extra_packages="pillow")
install_keras()

# Creates perturbed images by modifying a square of pixels to yellow for grass or green for dandelions
# @params
# model - keras CNN classifying model
# input_directory_path - file path to input images
# output_directory_path - file path to save perturbed images
create_images_constant <- function(model, input_directory_path, output_directory_path) {
  # Get file list from input directory
  image_files <- list.files(input_directory_path, pattern = "\\.(jpg|jpeg|png)$")
  original_class = NA
  for (i in image_files){
    # Load image
    img <- load.image(file.path(input_directory_path,i))
    img <- resize(img, 224,224)
    
    
    # Determine class of input folder if viewing first image
    if(is.na(original_class)) {
      test_image <- image_load(paste(input_directory_path,i,sep=""),
                               target_size = c(224,224))
      x <- image_to_array(test_image)
      x <- array_reshape(x, c(1, dim(x)))
      x <- x/255
      pred <- model %>% predict(x)
      original_class = which(pred > 0.5) - 1
    }
    
    
    # Calculate center of image
    image_height <- dim(img)[1]
    image_width <- dim(img)[2]
    center_x <- image_width / 2
    center_y <- image_height / 2
    
    # Calculate 502 pixels with the smallest distance to the center
    center_x_indices <- (center_x-11):(center_x+11)
    center_y_indices <- (center_y-11):(center_y+11)
    
    # Change the color of the selected pixels to yellow (0, 1, 0 in the RGB scale)
    img[center_x_indices, center_y_indices, 1] <- original_class  # Red channel (1 if orig image is grass, else 0)
    img[center_x_indices, center_y_indices, 2] <- 1  # Green channel
    img[center_x_indices, center_y_indices, 3] <- 0  # Blue channel
    
    save.image(img, paste(output_directory_path, i, sep=""))
  }
}        