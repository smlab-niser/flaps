user_cluster_data$min_index <- rep(0,200)
user_cluster_data$max_index <- rep(0,200)
user_cluster_data[1,18] <- 1
user_cluster_data[1,19] <- 250
for(i in 2:200)
{
   user_cluster_data[i,18] <- user_cluster_data[i-1,19] + 1
   user_cluster_data[i,19] <- user_cluster_data[i,18] + user_cluster_data[i,1] - 1
} 