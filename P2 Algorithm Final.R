library(imager)
library(magick)

input_dir <- "C:/Users/uae91/OneDrive/Desktop/332_data-main/data-for-332/grass"
output_dir <- "C:/Users/uae91/OneDrive/Desktop/332_data-main/data-for-332/Test/Grass"

# Load dandelion path, take and rescale each individual image
dandelions_path <- "C:/Users/uae91/OneDrive/Desktop/332_data-main/data-for-332/Bait/DandelionFlower.jpg"
dandelions_image <- image_read(dandelions_path)
dandelions_image <- image_scale(dandelions_image, "224x224")

# Scanning image files and forming a list
image_files <- list.files(input_dir, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)

# Creates the output directory if not present
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Loop through each image file
for (file in image_files) {
  # Load the grass image
  grass_image <- image_read(file)
  
  # Resize the image
  grass_image <- image_scale(grass_image, "224x224")
  
  # Define the position of the dandelions overlay
  overlay_width <- dim(image_data(dandelions_image))[2]
  overlay_height <- dim(image_data(dandelions_image))[1]
  
  if (dim(image_data(grass_image))[2] < overlay_width) {
    x_start <- 0
  } else {
    x_start <- sample.int(dim(image_data(grass_image))[2] - overlay_width, 1)
  }
  
  if (dim(image_data(grass_image))[1] < overlay_height) {
    y_start <- 0
  } else {
    y_start <- sample.int(dim(image_data(grass_image))[1] - overlay_height, 1)
  }
  
  # Overlay the dandelions ontop of the grass
  grass_image <- image_composite(grass_image, dandelions_image, offset = paste0("+", x_start, "+", y_start))
  
  # Save the overlaid image into output directory 
  new_image_path <- file.path(output_dir, paste0("dandelions_", basename(file)))
  image_write(grass_image, new_image_path)
}

# Repeating with dandelions overlayed with grass

input_dir <- "C:/Users/uae91/OneDrive/Desktop/332_data-main/data-for-332/dandelions"
output_dir <- "C:/Users/uae91/OneDrive/Desktop/332_data-main/data-for-332/Test/Dandelions"

# Load the grass directory and rescale individual images 
grass_path <- "C:/Users/uae91/OneDrive/Desktop/332_data-main/data-for-332/Bait/Grass.jpg"
grass_image <- image_read(grass_path)
grass_image <- image_scale(grass_image, "224x224")

# Scan image files in the input directory form list
image_files <- list.files(input_dir, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)

# Creates the output directory if not present
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Loop through each image file in the input directory
for (file in image_files) {
  # Load the dandelion image
  dandelion_image <- image_read(file)
  dandelion_image <- image_scale(dandelion_image, "224x224")
  
  # Define the position of the grass overlay
  overlay_width <- dim(image_data(grass_image))[2]
  overlay_height <- dim(image_data(grass_image))[1]
  
  if (dim(image_data(dandelion_image))[2] < overlay_width) {
    x_start <- 0
  } else {
    x_start <- sample.int(dim(image_data(dandelion_image))[2] - overlay_width, 1)
  }
  
  if (dim(image_data(dandelion_image))[1] < overlay_height) {
    y_start <- 0
  } else {
    y_start <- sample.int(dim(image_data(dandelion_image))[1] - overlay_height, 1)
  }
  
  # Overlay the grass over a dandelion image
  overlaid_image <- image_composite(dandelion_image, grass_image, offset = paste0("+", x_start, "+", y_start))
  
  # Save the overlaid image into the output directory
  new_image_path <- file.path(output_dir, paste0("grass_", basename(file)))
  image_write(overlaid_image, new_image_path)
}