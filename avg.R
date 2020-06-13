avg_da <- matrix(nrow = 14, ncol = 2) #avg_time calculation
for(i in 2:15)
{
  path <- paste0("~/Desktop/time/mnist/cluster",i,"_",i,"_time.csv")
  da <- read.csv(path, sep = ",", header =  TRUE)
  da <- da[,2:3]
  avg_da[i-1,1] <- mean(da[,1])
  avg_da[i-1,2] <- mean(da[,2])
}

write.csv(avg_da,"~/Desktop/time/avg_mnist.csv", sep = ",")

avg_da <- matrix(nrow = 14, ncol = 2)#avg_time calculation
for(i in 2:15)
{
  path <- paste0("~/Desktop/time/cifar10/cluster",i,"_",i,"_time.csv")
  da <- read.csv(path, sep = ",", header =  TRUE)
  da <- da[,2:3]
  avg_da[i-1,1] <- mean(da[,1])
  avg_da[i-1,2] <- mean(da[,2])
}

write.csv(avg_da,"~/Desktop/time/avg_cifar10.csv", sep = ",")

#fedavg algo
setwd("/media/data6TB/extracted/Hierarchical_Fedarated_Learning/mnist_weight")
time4 <- matrix(nrow = 13, ncol = 1)
for(i in 3:15)
{
  start_time <- Sys.time()
  data_w <- list()
  for(j in 1:i)
  {
    path <- paste0("cluster",i,"_",j,"_weights.hdf5")
    data_w[[j]] <- load_model_weights_hdf5(model1, path)
  }
  sum_weight <- get_weights(data_w[[1]])
  for(k in 2:i)
  {
   temp_w <- get_weights(data_w[[k]])
   for(l in 1:8)
   {
     sum_weight[[l]] <- sum_weight[[l]] + temp_w[[l]]
   }
  }
  for(l in 1:8)
  {
    sum_weight[[l]] <- sum_weight[[l]]/i
  }
  end_time <- Sys.time()
  total_time <- end_time - start_time
  time4[i-2,1] <- total_time
}

setwd("/media/data6TB/extracted/Hierarchical_Fedarated_Learning/cifar10")
time4 <- matrix(nrow = 14, ncol = 1)
for(i in 2:15)
{
  start_time <- Sys.time()
  data_w <- list()
  for(j in 1:i)
  {
    path <- paste0("cluster",i,"_",j,"_weights.hdf5")
    data_w[[j]] <- load_model_weights_hdf5(model_cluster, path)
  }
  sum_weight <- get_weights(data_w[[1]])
  for(k in 2:i)
  {
    temp_w <- get_weights(data_w[[k]])
    for(l in 1:12)
    {
      sum_weight[[l]] <- sum_weight[[l]] + temp_w[[l]]
    }
  }
  for(l in 1:12)
  {
    sum_weight[[l]] <- sum_weight[[l]]/i
  }
  end_time <- Sys.time()
  total_time <- end_time - start_time
  time4[i-1,1] <- total_time
}


setwd("/media/data6TB/extracted/Hierarchical_Fedarated_Learning/cifar10/fed_avg")
#time4 <- matrix(nrow = 14, ncol = 1)
#for(i in 2:15)
#{
  start_time <- Sys.time()
  data_w <- list()
  for(j in 1:200)
  {
    path <- paste0("fed",j,"_weights.hdf5")
    data_w[[j]] <- load_model_weights_hdf5(model_cluster, path)
  }
  sum_weight <- get_weights(data_w[[1]])
  for(k in 2:i)
  {
    temp_w <- get_weights(data_w[[k]])
    for(l in 1:12)
    {
      sum_weight[[l]] <- sum_weight[[l]] + temp_w[[l]]
    }
  }
  for(l in 1:12)
  {
    sum_weight[[l]] <- sum_weight[[l]]/200
  }
  end_time <- Sys.time()
  total_time <- end_time - start_time
  #time4[i-1,1] <- total_time
#}

  
  setwd("/media/data6TB/extracted/Hierarchical_Fedarated_Learning/fed_avg1")
  #time4 <- matrix(nrow = 14, ncol = 1)
  #for(i in 2:15)
  #{
  start_time <- Sys.time()
  data_w <- list()
  for(j in 1:200)
  {
    path <- paste0("fed",j,"_weights.hdf5")
    data_w[[j]] <- load_model_weights_hdf5(model1, path)
  }
  start_time <- Sys.time()
  sum_weight <- w_sum[[1]]
  for(k in 2:i)
  {
    temp_w <- (w_sum[[k]])
    for(l in 1:8)
    {
      sum_weight[[l]] <- sum_weight[[l]] + temp_w[[l]]
    }
  }
  for(l in 1:8)
  {
    sum_weight[[l]] <- sum_weight[[l]]/200
  }
  end_time <- Sys.time()
  time3 <- end_time - start_time
