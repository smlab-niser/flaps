start_time <- Sys.time()
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

#user division
#num_cluster <- data.frame(cluster = seq(from = 10, to = 200, by = 10))
#num_user <- list()
#for(i in 1:nrow(num_cluster))
#{
 # num_user[[i]] <- data.frame(division = rand_vect(num_cluster[i,],60000))
#}
user_image_div <- data.frame(num = rand_vect(200, 60000))
rand <- data.frame(rand = runif(200, min = 0, max = 1))
uid <- data.frame(uid = seq(1,200,1))
user_image_div <- cbind(user_image_div,uid,rand)
end_time <- Sys.time()
cl <- end_time - start_time
#write.csv(user_image_div, file = "/data/extracted/Hierarchical_Fedarated_Learning/cifar10/user_image_div_cifar10.csv", sep = ",", row.names = FALSE, col.names = TRUE)


#cluster
library(cluster)
library(factoextra)
start_time <- Sys.time()
cluster2 <- kmeans(user_image_div, 2, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl2 <- end_time - start_time

start_time <- Sys.time()
cluster3 <- kmeans(user_image_div, 3, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl3 <- end_time - start_time

start_time <- Sys.time()
cluster4 <- kmeans(user_image_div, 4, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl4 <- end_time - start_time

start_time <- Sys.time()
cluste5 <- kmeans(user_image_div, 5, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl5 <- end_time - start_time

start_time <- Sys.time()
cluster6 <- kmeans(user_image_div, 6, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl6 <- end_time - start_time

start_time <- Sys.time()
cluster7 <- kmeans(user_image_div, 7, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl7 <- end_time - start_time

start_time <- Sys.time()
cluster8 <- kmeans(user_image_div, 8, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl8 <- end_time - start_time

start_time <- Sys.time()
cluster9 <- kmeans(user_image_div, 9, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl9 <- end_time - start_time

start_time <- Sys.time()
cluster10 <- kmeans(user_image_div, 10, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl10 <- end_time - start_time

start_time <- Sys.time()
cluster11 <- kmeans(user_image_div, 11, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl11 <- end_time - start_time

start_time <- Sys.time()
cluster12 <- kmeans(user_image_div, 12, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl12 <- end_time - start_time

start_time <- Sys.time()
cluster13 <- kmeans(user_image_div, 13, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl13 <- end_time - start_time

start_time <- Sys.time()
cluster14 <- kmeans(user_image_div, 14, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl14 <- end_time - start_time

start_time <- Sys.time()
cluster15 <- kmeans(user_image_div, 15, nstart = 25)
#fviz_cluster(cluster2, user_image_div)
end_time <- Sys.time()
cl15 <- end_time - start_time

time1 <- c(cl+cl2, cl+cl3, cl+cl4, cl+cl5, cl+cl6, cl+cl7, cl+cl8, cl+cl9, cl+cl10, 
           cl+cl11, cl+cl12, cl+cl13, cl+cl14, cl+cl15)

cluster3 <- kmeans(user_image_div, 3, nstart = 25)
#fviz_cluster(cluster3, user_image_div)

cluster4 <- kmeans(user_image_div, 4, nstart = 25)
#fviz_cluster(cluster4, user_image_div)

cluster5 <- kmeans(user_image_div, 5, nstart = 25)
#fviz_cluster(cluster5, user_image_div)

cluster6 <- kmeans(user_image_div, 6, nstart = 25)
#fviz_cluster(cluster6, user_image_div)

cluster7 <- kmeans(user_image_div, 7, nstart = 25)
#fviz_cluster(cluster7, user_image_div)

cluster8 <- kmeans(user_image_div, 8, nstart = 25)
#fviz_cluster(cluster8, user_image_div)

cluster9 <- kmeans(user_image_div, 9, nstart = 25)
#fviz_cluster(cluster9, user_image_div)

cluster10 <- kmeans(user_image_div, 10, nstart = 25)
#fviz_cluster(cluster10, user_image_div)

cluster11 <- kmeans(user_image_div, 11, nstart = 25)
#fviz_cluster(cluster11, user_image_div)

cluster12 <- kmeans(user_image_div, 12, nstart = 25)
#fviz_cluster(cluster12, user_image_div)

cluster13 <- kmeans(user_image_div, 13, nstart = 25)
#fviz_cluster(cluster13, user_image_div)

cluster14 <- kmeans(user_image_div, 14, nstart = 25)
#fviz_cluster(cluster14, user_image_div)

cluster15 <- kmeans(user_image_div, 15, nstart = 25)
#fviz_cluster(cluster15, user_image_div)

div_2 <- data.frame(div_2 = cluster2$cluster)
div_3 <- data.frame(div_3 = cluster3$cluster)
div_4 <- data.frame(div_4 = cluster4$cluster)
div_5 <- data.frame(div_5 = cluster5$cluster)
div_6 <- data.frame(div_6 = cluster6$cluster)
div_7 <- data.frame(div_7 = cluster7$cluster)
div_8 <- data.frame(div_8 = cluster8$cluster)
div_9 <- data.frame(div_9 = cluster9$cluster)
div_10 <- data.frame(div_10 = cluster10$cluster)
div_11 <- data.frame(div_11 = cluster11$cluster)
div_12 <- data.frame(div_12 = cluster12$cluster)
div_13 <- data.frame(div_13 = cluster13$cluster)
div_14 <- data.frame(div_14 = cluster14$cluster)
div_15 <- data.frame(div_15 = cluster15$cluster)

cluster_data <- do.call("cbind", list(div_2,div_3,div_4,div_5,div_6,div_7,div_8,div_9,div_10,div_11,div_12,div_13,div_14,div_15))
write.csv(cluster_data, "/data/extracted/Hierarchical_Fedarated_Learning/cifar10/cluster_datafar10_.csv", sep = ",", col.names = TRUE, row.names = FALSE)

user_cluster_data <- cbind(user_image_div, cluster_data)