library(keras)

# Data Preparation -----------------------------------------------------

batch_size <- 128
num_classes <- 10
epochs <- 12

# Input image dimensions
img_rows <- 28
img_cols <- 28

# The data, shuffled and split between train and test sets
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Redefine  dimension of train/test inputs
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

# Convert class vectors to binary class matrices
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)



library(sqldf)
user_cluster_data <- read.csv("/media/data6TB/extracted/Hierarchical_Fedarated_Learning/user_cluster_data.csv", sep = ",", stringsAsFactors = FALSE)

for(i in 2:15)
{
  time_df <- matrix(nrow = i, ncol = 2)
  for(j in 1:i)
  {
    start_time <- Sys.time()
    library(sqldf)
    data <- sqldf(paste0("select uid,div_",i,",min_index,max_index from user_cluster_data where div_",i," = ",j))
    
    x_train_c <- x_train[1:data[nrow(data),4],,,]
    y_train_c <- y_train[1:data[nrow(data),4],]
    x_train_c <- array_reshape(x_train_c, c(nrow(x_train_c), img_cols, img_rows, 1))
    end_time <- Sys.time()
    time2 <- end_time - start_time
    # Define Model -----------------------------------------------------------
    
    # Define model
    start_time <- Sys.time()
    model1 <- keras_model_sequential() %>%
      layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
                    input_shape = input_shape) %>% 
      layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
      layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
      layer_dropout(rate = 0.25) %>% 
      layer_flatten() %>% 
      layer_dense(units = 128, activation = 'relu') %>% 
      layer_dropout(rate = 0.5) %>% 
      layer_dense(units = num_classes, activation = 'softmax')
    
    model_cluster <-  load_model_weights_hdf5(model1,"/media/data6TB/extracted/Hierarchical_Fedarated_Learning/mnist.hdf5")
    # Train model
    model_cluster  %>% compile(
      loss = loss_categorical_crossentropy,
      optimizer = optimizer_adadelta(),
      metrics = c('accuracy')
    )
    
    # Train model
    model_cluster %>% fit(
      x_train_c, y_train_c,
      batch_size = batch_size,
      epochs = epochs,
      validation_split = 0.2
    )
    end_time <- Sys.time()
    time3 <- end_time - start_time
   time_df[j,1] <- time2
   time_df[j,2] <- time3
   path <- paste0("~/Desktop/time/mnist/cluster",i,"_",j,"_time.csv")
   write.csv(time_df,path, sep = ",")
  }
}


