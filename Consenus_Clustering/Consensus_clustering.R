library(tidyverse)
library(magrittr)
library(ggpubr)
library(dplyr)
library(diceR)
library(MASS)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour-blind friendly palette

plotClassifiers <- function(data,Num_Clust){
  #Antal körningar för varje algoritm
  clust_reps = 2
  #Vilka algoritmer som ska köras
  clus_alg = c("hc", "km")
  num_clus_alg = length(clus_alg)
  
  #Make the Clusters for the Concsenus clustering 
  c_clust <- consensus_cluster(data, nk = Num_Clust, reps = clust_reps, algorithms = clus_alg,progress = FALSE)
  
  data_clus = cbind(data,c_clust)
  
  #label
  
  labels = cbind("x1","x2","class")
  
  plots = list()
  
  count_alg=0
  for(alg in clus_alg){
    for(rep_clus in 1:clust_reps){
      #new_lab = paste(alg, rep_clus)
      #labels = cbind(labels,new_lab)
      print(c_clust)
      print(c_clust[,count_alg + rep_clus])
      
      data <- cbind(data,c_clust[,count_alg + rep_clus])
      
      data <- set_colnames(data, labels)
      
      plot <- ggplot() +
        ggtitle(new_lab) +
        geom_jitter(aes(x = x1, y = x2, colour = class),data = data, size = 1)
      
      plots[[new_lab]] <- plot
    }
    count_alg = count_alg + clust_reps
  }
  
  g <- ggpubr::ggarrange(plotlist = plots, ncol = clust_reps, nrow = num_clus_alg, common.legend = TRUE, legend = "bottom")
  
  #Find a consesus Clustering
  consens_clust = CSPA(c_clust, Num_Clust)

  data <- cbind(data,consens_clust)
  
  return(g)
}

p = 2

n=100

angle <- pi/4
rotation_matrix <- matrix(c(cos(angle),-sin(angle),sin(angle),cos(angle)),2,2)

varMatrix <- diag(x = 1, nrow = p, ncol = p) #Variance matrix
data <- mvrnorm(n, mu=c(-5,0), Sigma = varMatrix)

data <- data %>% 
  as_tibble() %>% 
  set_colnames(c('x1', 'x2'))

data2 <- mvrnorm(n, mu=c(0,0), Sigma = diag(x=1, nrow=p, ncol=p))
data2 <- data2 %>%
  as_tibble() %>%
  set_colnames(c('x1','x2'))

data3 <- mvrnorm(n, mu=c(5,0), Sigma = diag(x=1, nrow=p, ncol=p))
data3 <- data3 %>%
  as_tibble() %>%
  set_colnames(c('x1','x2'))

normal_on_axis_data <- rbind(data,data2,data3)

Num_Clust = 3 
data = normal_on_axis_data

plot = plotClassifiers(normal_on_axis_data,3)

plot
