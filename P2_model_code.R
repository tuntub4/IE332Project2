library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
install_tensorflow(extra_packages="pillow")
install_keras()
setwd("C:\\Users\\49733\\Downloads\\P2 Files-20230419T185246Z-001\\P2 Files")
model<-load_model_tf("./dandelion_model")

setwd("C:/Users/49733/Downloads/P2 Files-20230419T185246Z-001/P2 Files")
label_list <- dir("grass/")
output_n <- length(label_list)
width <- 224
height<- 224
target_size <- c(width, height)
rgb <- 3 #color channels


setwd("C:/Users/49733/Downloads/P2 Files-20230419T185246Z-001/P2 Files/grass")


# Create an empty data frame to store the results
results <- data.frame(file = character(), probability = numeric(), stringsAsFactors = FALSE)

# Loop through each file in label_list
for (i in seq_along(label_list)) {
  # Load the current image
  test_image <- image_load(label_list[i], target_size = target_size)
  
  # Preprocess the image
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1, dim(x)))
  x <- x / 255
  
  # Make a prediction on the image
  pred <- model %>% predict(x)
  
  # Get the predicted class label
  class_idx <- which.max(pred)
  class_label <- label_list[class_idx]
  
  # Append the result to the results data frame
  results <- rbind(results, data.frame(file = label_list[i], probability = max(pred), stringsAsFactors = FALSE))
}

# Print the results
results <- results[order(-results$probability),]
print(results)
