# This algorithm aims to change 1% pixels for each picture that has the highest yellow/red value into green/blue, and for the grass, it does the opposite.  
# Load the necessary packages
library(imager)

# Setting the input and output directories
input_dir <- "/home/jupyter/dandelions"
output_dir <- "/home/jupyter/dan2"

# Scaning image files in the input directory and making them into a list
image_files <- list.files(input_dir, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)

# Create vectors that will store the run time (To calculate complexity)
wall_time <- c()
image_number <- c()

# Create the output directory if it doesn't exist, so it's not necessary to create a new folder by hand
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Loop through each image file
for (i in seq_along(image_files)) {
  # Record the start time
  start_time <- Sys.time()
  
  # Load the image
  img <- load.image(image_files[i])
  
  # Resize the image
  img <- resize(img, 224, 224)
  
  # Calculate the threshold value for the 99th percentile of pixel values, this will set a threhold for 1% pixels with highest red value.
  threshold <- quantile(img, probs = 0.99, na.rm = TRUE)
  
  # Replace the pixels above the threshold with blue
  img[img >= threshold] <- c(0, 0, 1) # Setting the blue channel to 1, and the red and green channels to 0
  
  output_file <- file.path(output_dir, paste0("image_", i, ".jpg"))
  save.image(img, output_file)
  
  # Record the end time and calculate the time difference
  end_time <- Sys.time()
  time_difference <- difftime(end_time, start_time, units = "secs")
  
  # Put the time difference into the vector
  wall_time <- c(wall_time, time_difference)
  image_number <- c(image_number, i)
}

# Print a message which shows that the operation is complete
cat("Dandelions Pixel replacement shift complete. Modified images saved in", output_dir, "\n")

# Create a plot of wall time vs. image number
plot(image_number, wall_time, xlab = "Image Number", ylab = "Wall Time (secs)")


# Do the operation again for grass
# Setting the input and output directories
input_dir <- "/home/jupyter/grass"
output_dir <- "/home/jupyter/gra2"

# Scaning image files in the input directory and making them into a list
image_files <- list.files(input_dir, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Loop through each image file
for (i in seq_along(image_files)) {
  # Load the image
  img <- load.image(image_files[i])
  
  # Resize the image
  img <- resize(img, 224, 224)
  
  # Calculate the threshold value for the 99th percentile of pixel values
  threshold <- quantile(img, probs = 0.99, na.rm = TRUE)
  
  # Calculate the threshold value for the 99th percentile of green pixel values
  green_pixels <- img[,,2] # Extract green channel
  threshold <- quantile(green_pixels, probs = 0.99, na.rm = TRUE)
  
  # Replace the most green pixels with yellow
  yellow_pixels <- c(1, 1, 0) # Setting red channel to 1, and green and blue channels to 0
  img[,,1][green_pixels >= threshold] <- yellow_pixels[1]
  img[,,2][green_pixels >= threshold] <- yellow_pixels[2]
  img[,,3][green_pixels >= threshold] <- yellow_pixels[3]
  
  output_file <- file.path(output_dir, paste0("image_", i, ".jpg"))
  save.image(img, output_file)
}

# Print a message which shows that the operation is complete
cat("Grass Pixel shift operation complete. Modified images saved in", output_dir, "\n")
