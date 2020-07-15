library(openxlsx)
data <- read.xlsx("D:/extended_ara/time_all.xlsx", sheet =1, colNames = T)
dat <- as.data.frame(sapply(data, as.numeric))
dat <- dat[1:19,]
t_cl <- data.frame(rep(2:20, 4), stringsAsFactors = F)
t_name <- data.frame(rep("o", 76), stringsAsFactors = F)
t_name[1:19,] <- data.frame(rep("MNIST-CNN",19), stringsAsFactors = F)
t_name[20:38,] <- data.frame(rep("CIFAR-10-CNN",19), stringsAsFactors = F)
t_name[39:57,] <- data.frame(rep("MobilenetV2|TinyImagenet-200",19), stringsAsFactors = F)
t_name[58:76,] <- data.frame(rep("InceptionresnetV2|TinyImagenet-200",19), stringsAsFactors = F)
t2 <- matrix(nrow = 76, ncol = 1)
t2[1:19,1] <- dat[,3]
t2[20:38,1] <- dat[,7]
t2[39:57,1] <- dat[,11]
t2[58:76,1] <- dat[,15]
t2 <- as.data.frame(t2)
t3 <- matrix(nrow = 76, ncol = 1)
t3[1:19,1] <- dat[,4]
t3[20:38,1] <- dat[,8]
t3[39:57,1] <- dat[,12]
t3[58:76,1] <- dat[,16]
t3 <- as.data.frame(t3)
t4 <- matrix(nrow = 76, ncol = 1)
t4[1:19,1] <- dat[,5]
t4[20:38,1] <- dat[,9]
t4[39:57,1] <- dat[,13]
t4[58:76,1] <- dat[,17]
t4 <- as.data.frame(t4)
t5 <- matrix(nrow = 84, ncol = 1)
t5[1,1] <- 41.140
t5[2:20,1] <- dat[,6]
t5[21,1] <- 6.320
t5[22,1] <- 169.69
t5[23:41,1] <- dat[,10]
t5[42,1] <- 10.15
t5[43,1] <- 3036.480
t5[44:62,1] <- dat[,14]
t5[63,1] <- 535.410
t5[64,1] <- 5182.44
t5[65:83,1] <- dat[,18]
t5[84,1] <- 602.04
t5 <- as.data.frame(t5)

data_t2 <- data.frame(t2, t_name, t_cl, stringsAsFactors = F)
colnames(data_t2) <- c("T2","Architecture","Cluster")
data_t2$Legend <- as.factor(data_t2$Architecture)
library(ggplot2)
T2 <- ggplot(data_t2, aes(x=Cluster, y=T2))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  labs( x=("Cluster") , y=("Time 2"))+
  theme_classic()+  
  theme(legend.text = element_text(size = 10,   family = 'mono'),plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10, family  = 'mono' ), 
        axis.title.y = element_text(  size = 10, family  = 'mono' ),)+
  scale_color_manual(values = rainbow(4))


data_t3 <- data.frame(t3, t_name, t_cl, stringsAsFactors = F)
colnames(data_t3) <- c("T3","Architecture","Cluster")
data_t3$Legend <- as.factor(data_t3$Architecture)
library(ggplot2)
T3 <- ggplot(data_t3, aes(x=Cluster, y=T3))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  theme_classic()+
  theme(legend.text = element_text(size = 10,   family = 'mono'),plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10 , family  = 'mono'), 
        axis.title.y = element_text(  size = 10, family  = 'mono' ),)+
  labs( x=("Cluster") , y=("Time 3"))+
  scale_color_manual(values = rainbow(4))


data_t4 <- data.frame(t4, t_name, t_cl, stringsAsFactors = F)
colnames(data_t4) <- c("T4","Architecture","Cluster")
data_t4$Legend <- as.factor(data_t4$Architecture)
library(ggplot2)
T4 <- ggplot(data_t4, aes(x=Cluster, y=T4))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  theme_classic()+
  theme(legend.text = element_text(size = 10,   family = 'mono'),plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10, family  = 'mono' ), 
        axis.title.y = element_text(  size = 10, family  = 'mono' ),)+
  labs( x=("Cluster") , y=("Time 4"))+
  scale_color_manual(values = rainbow(4))
  


t_cl_t5 <- data.frame(rep(1:21, 4), stringsAsFactors = F)
t_name_t5 <- data.frame(rep("o", 84), stringsAsFactors = F)
t_name_t5[1:21,] <- data.frame(rep("MNIST-CNN",21), stringsAsFactors = F)
t_name_t5[22:42,] <- data.frame(rep("CIFAR-10-CNN",21), stringsAsFactors = F)
t_name_t5[43:63,] <- data.frame(rep("MobilenetV2|TinyImagenet-200",21), stringsAsFactors = F)
t_name_t5[64:84,] <- data.frame(rep("InceptionresnetV2|TinyImagenet-200",21), stringsAsFactors = F)
data_t5 <- data.frame(t5, t_name_t5, t_cl_t5, stringsAsFactors = F)
colnames(data_t5) <- c("Total","Architecture","Cluster")
data_t5$Legend <- as.factor(data_t5$Architecture)
display <- data.frame(rep("no", 84), stringsAsFactors = F)
for(i in 1:nrow(data_t5))
{
  if(data_t5[i,3] == 1 )
    display[i,1] = "yes"
}
for(i in 1:nrow(data_t5))
{
  if(data_t5[i,3] == 21 )
    display[i,1] = "yes1"
}
data_t5 <- cbind(data_t5,display)
colnames(data_t5) <- c("Total","Architecture","Cluster","Legend","Display")
library(ggplot2)
T5 <- ggplot(data_t5, aes(x=Cluster, y=Total))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  geom_point(data=data_t5[data_t5$Display == "yes",],color="orange",size=3)+
  geom_point(data=data_t5[data_t5$Display == "yes1",],color="steelblue",size=3)+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=0.8)+
  geom_vline(xintercept = 21, linetype="dashed", color = "deepskyblue", size=0.8)+
  geom_text(x=2.5, y=5200, label="Central", color = "orange", size=5)+
  geom_text(x=20.5, y=3000, label="FL",color = "steelblue", size=5)+
  theme_classic()+
  theme(legend.text = element_text(size = 10,   family = 'mono'), plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10, family  = 'mono' ), 
        axis.title.y = element_text(  size = 10, family  = 'mono' ),
        )+
  labs( x=("Cluster") , y=("Total time"))+
  scale_color_manual(values = rainbow(4))

data_total <- cbind(dat[,6], dat[,10], dat[,14], dat[,18])
tot <- t(data_total)
for(i in 2:20)
{
  tot[,i-1] <- tot[,i-1]*i
}
tot <- t(tot)
tot <- as.data.frame(tot)
t6 <- matrix(nrow = 84, ncol = 1)
t6[1,1] <- 41.140
t6[2:20,1] <- tot[,1]
t6[21,1] <- 6.320*200
t6[22,1] <- 169.69
t6[23:41,1] <- tot[,2]
t6[42,1] <- 10.15*200
t6[43,1] <- 3036.480
t6[44:62,1] <- tot[,3]
t6[63,1] <- 535.410*200
t6[64,1] <- 5182.44
t6[65:83,1] <- tot[,4]
t6[84,1] <- 602.04*200
t6 <- as.data.frame(t6)

data_t6 <- data.frame(t6, t_name_t5, t_cl_t5,display,data_t5[,4], stringsAsFactors = F)
colnames(data_t6) <- c("Total","Architecture","Cluster","Display","Legend")
library(ggplot2)
T6 <- ggplot(data_t6, aes(x=Cluster, y=Total))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  geom_point(data=data_t5[data_t5$Display == "yes",],color="orange",size=3)+
  geom_point(data=data_t5[data_t5$Display == "yes1",],color="steelblue",size=3)+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=0.8)+
  geom_vline(xintercept = 21, linetype="dashed", color = "deepskyblue", size=0.8)+
  geom_text(x=2.5, y=75000, label="Central", color = "orange", size=5)+
  geom_text(x=20.5, y=100000, label="FL",color = "steelblue", size=5)+
  theme_classic()+
  theme(legend.text = element_text(size = 10,   family = 'mono'), plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10, family  = 'mono' ), 
        axis.title.y = element_text(  size = 10 , family  = 'mono'),
  )+
  labs( x=("Cluster") , y=("Total computation time"))+
  scale_color_manual(values = rainbow(4))

library(ggpubr)
T7 <-ggarrange(T2,T3,T4,
           ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
T8 <- ggarrange(T5,T6,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
ggarrange(T7,T8,
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

laf <- read.xlsx("D:/extended_ara/laf-1.xlsx", sheet =1, 
                 colNames = T, rowNames = T)
loss <- matrix(nrow = 84, ncol = 1)
loss[1:21,1] <- laf[,1]
loss[22:42,1] <- laf[,4]
loss[43:63,1] <- laf[,7]
loss[64:84,1] <- laf[,10]
loss <- as.data.frame(loss)
AUC <- matrix(nrow = 84, ncol = 1)
AUC[1:21,1] <- laf[,2]
AUC[22:42,1] <- laf[,5]
AUC[43:63,1] <- laf[,8]
AUC[64:84,1] <- laf[,11]
AUC <- as.data.frame(AUC)
Fscore <- matrix(nrow = 84, ncol = 1)
Fscore[1:21,1] <- laf[,3]
Fscore[22:42,1] <- laf[,6]
Fscore[43:63,1] <- laf[,9]
Fscore[64:84,1] <- laf[,12]
Fscore <- as.data.frame(Fscore)

name <- data.frame(name =rep(c("Central","2","3","4","5","6","7","8","9","10",
                         "11","12","13","14","15","16","17","18","19","20",
                         "FL"),4), stringsAsFactors = F)
cluster <- data.frame(Cluster = rep(1:21,4) )
d_laf <- cbind(loss,AUC,Fscore,t_name_t5,name,cluster) 
colnames(d_laf) <- c("Loss","AUC", "Fscore","Architecture","Name","Cluster")
d_laf$Legend <- as.factor(d_laf$Architecture)
loss_g <- ggplot(d_laf, aes(x=Cluster, y=Loss))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  geom_point(data=d_laf[d_laf$Name == "Central",],color="orange",size=3)+
  geom_point(data=d_laf[d_laf$Name == "FL",],color="steelblue",size=3)+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=0.8)+
  geom_vline(xintercept = 21, linetype="dashed", color = "deepskyblue", size=0.8)+
  geom_text(x=2, y=0.5, label="Central", color = "orange", size=5)+
  geom_text(x=20, y=0.4, label="FL",color = "steelblue", size=5)+
theme_classic()+
  theme(legend.text = element_text(size = 10,   family = 'mono'), plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10, family  = 'mono' ), 
        axis.title.y = element_text(  size = 10, family  = 'mono' ),
  )+
  labs( x=("Cluster") , y=("Loss"))+
  scale_color_manual(values = rainbow(4))


AUC_g <- ggplot(d_laf, aes(x=Cluster, y=AUC))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  geom_point(data=d_laf[d_laf$Name == "Central",],color="orange",size=3)+
  geom_point(data=d_laf[d_laf$Name == "FL",],color="steelblue",size=3)+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=0.8)+
  geom_vline(xintercept = 21, linetype="dashed", color = "deepskyblue", size=0.8)+
  geom_text(x=2, y=0.83, label="Central", color = "orange", size=5)+
  geom_text(x=20, y=0.85, label="FL",color = "steelblue", size=5)+
  theme_classic()+
  theme(legend.text = element_text(size = 10,   family = 'mono'), plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10, family  = 'mono' ), 
        axis.title.y = element_text(  size = 10, family  = 'mono' ),
  )+
  labs( x=("Cluster") , y=("AUC"))+
  scale_color_manual(values = rainbow(4))


fscore_g <- ggplot(d_laf, aes(x=Cluster, y=Fscore))+
  geom_line(aes(color = Legend ), size = 1.25, linetype = "twodash")+
  geom_point(data=d_laf[d_laf$Name == "Central",],color="orange",size=3)+
  geom_point(data=d_laf[d_laf$Name == "FL",],color="steelblue",size=3)+
  geom_vline(xintercept = 1, linetype="dashed", color = "orange", size=0.8)+
  geom_vline(xintercept = 21, linetype="dashed", color = "deepskyblue", size=0.8)+
  geom_text(x=2, y=0.8, label="Central", color = "orange", size=5)+
  geom_text(x=20.5, y=0.925, label="FL",color = "steelblue", size=5)+
  theme_classic()+
  theme(legend.text = element_text(size = 10, face = "bold"), plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(  size = 10, family  = 'mono' ), 
        axis.title.y = element_text(  size = 10, family  = 'mono' ),
  )+
  labs( x=("Cluster") , y=("Fscore"))+
  scale_color_manual(values = rainbow(4))


library(ggpubr)
ggarrange(loss_g,AUC_g,fscore_g,
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
ggarrange(loss_g,AUC_g,fscore_g,
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")