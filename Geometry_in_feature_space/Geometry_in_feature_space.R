library(tidyverse)
library(magrittr)
library(rpart)         # A package for building CART trees
#library(rpart.plot)    # Nice plotting of CART trees built with rpart
library(randomForest)  # A package to fit a random forest
library(MASS) #For LDA & QDA
library(klaR) #For RDA
library(fields) #for rdist to compute nearest centroid
library(FNN) #for kNN
library(ggpubr)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour-blind friendly palette


#Samuel: Plot, CART+DA(?)
#Klas: kNN
#Daniel: Neareast centroid + RF
#Hampus: Data

# p <- 2 #feature dimensions
# n <- 100 #data points
# varMatrix <- diag(x = 1, nrow = p, ncol = p) #Variance matrix
# meanVec <- rep(1,p) #Mean vector
# data <- mvrnorm(n, mu=meanVec, Sigma = varMatrix)
# data <- data %>% 
#   as_tibble() %>% 
#   set_colnames(c('x1', 'x2')) %>%
#   mutate(class = rep("A", n))
# 
# data2 <- mvrnorm(n, mu=rep(5,p), Sigma = diag(x=1, nrow=p, ncol=p))
# data2 <- data2 %>%
#   as_tibble() %>%
#   set_colnames(c('x1','x2')) %>%
#   mutate(class = rep("B", n))
# 
# data_train <- bind_rows(data[1:(0.7*n),], data2[1:(0.7*n),])
# data_train$class <- as.factor(data_train$class)
# data_test <- bind_rows(data[(0.7*n+1):n,], data2[(0.7*n+1):n,])
# data_test$class <- as.factor(data_test$class)

## Plots LDA, QDA, CART ##
# 'data' should contain columns 'x1', 'x2' (features) and 'class' (class type)
plotClassifiers <- function(data){
  h <- 0.03 #Grid resolution
  #Margins
  m1 <- abs(0.1*max(dist(data$x1)))
  m2 <- abs(0.1*max(dist(data$x2)))
  x1 <- seq(min(data$x1)-m1, max(data$x1)+m1, by = h)
  x2 <- seq(min(data$x2)-m2, max(data$x2)+m2, by = h)
  x_pred <- expand.grid(x1,x2)
  colnames(x_pred) <- c('x1','x2')
  
  print('Fitting kNN, k = 2')
  data_mat <- matrix(c(data$x1, data$x2), ncol = 2)
  fit_kNN_2 <- FNN::knn(train = data_mat, cl = data$class, test = x_pred, k=2, algorithm = "kd_tree")
  y_pred_kNN_2 <- as.factor(fit_kNN_2)
  
  print('Fitting kNN, k = 10')
  fit_kNN_10 <- FNN::knn(train = data_mat, cl = data$class, test = x_pred, k=10, algorithm = "kd_tree")
  y_pred_kNN_10 <- as.factor(fit_kNN_10)
  
  print('Fitting LDA')
  fit_lda <- MASS::lda(class ~ x1 + x2, data = data)
  y_pred_lda <- predict(fit_lda, x_pred)$class
  
  print('Fitting QDA')
  fit_qda <- MASS::qda(class ~ x1 + x2, data = data)
  y_pred_qda <- predict(fit_qda, x_pred)$class
  
  print('Fitting RDA')
  fit_rda <- klaR::rda(formula = class ~ x1 + x2, data = data)
  y_pred_rda <- predict(fit_rda, x_pred)$class
  
  print('Fitting CART')
  fit_cart <- rpart::rpart(class ~ x1 + x2, data = data, parms = list(split = "gini"),
                           control = rpart::rpart.control(cp = 0, minsplit = 1, minbucket = 1))
  y_pred_cart <- predict(fit_cart, x_pred)
  y_pred_cart[y_pred_cart[,1] == 1, 1] <- 'A'
  y_pred_cart[y_pred_cart[,2] == 1, 1] <- 'B'
  if(length(levels(data$class)) == 3){
    y_pred_cart[y_pred_cart[,3] == 1, 1] <- 'C'
  }
  y_pred_cart <- as.factor(y_pred_cart[,1])
  
  print('Fitting RF')
  fit_rf <- randomForest::randomForest(class ~ x1 + x2, data = data)
  y_pred_rf <- predict(fit_rf, x_pred)
  
  print('Fitting Nearest Centroid')
  centroid_fit <- aggregate(. ~ class, data, mean)
  y_pred_centroid <- centroid_fit[apply(rdist(x_pred,centroid_fit[,2:3]), 1, which.min), 1]
  
  
  data_pred <- tibble(
    x1 = x_pred$x1,
    x2 = x_pred$x2,
    class_kNN_2 = y_pred_kNN_2,
    class_kNN_10 = y_pred_kNN_10,
    class_centroid = y_pred_centroid,
    class_lda = y_pred_lda,
    class_qda = y_pred_qda,
    class_rda = y_pred_rda,
    class_cart = y_pred_cart,
    class_rf = y_pred_rf
  )
  
  p_kNN_2 <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_kNN_2),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("kNN, k=2")
  
  p_kNN_10 <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_kNN_10),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("kNN, k=10")
  
  p_centroid <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_centroid),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("Nearest Centroid")
  
  
  p_lda <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_lda),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("LDA")
  
  p_qda <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_qda),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("QDA")
  
  p_rda <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_rda),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("RDA")
  
  p_cart <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_cart),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("CART")
  
  p_rf <- ggplot() +
    geom_tile(
      aes(x = x1, y = x2, colour = class, fill = class_rf),
      colour = "transparent",
      data = data_pred,
      width = h,
      height = h,
      alpha = 0.4) +
    ggtitle("RF")
  
  #Adjustment for different number of classes
  cbVec <- cbPalette[2:3]
  shapeVec <- c(16, 17)
  if(length(levels(data$class)) == 3){
    cbVec <- cbPalette[2:4]
    shapeVec <- c(16, 17, 15)
  }
  print(length(levels(data$class)))
  print(cbVec)
  print(shapeVec)
  
  data_plot <- ggplot() + ggtitle("Raw Data")
  
  plots <- lapply(list(p_kNN_2, p_kNN_10, p_centroid, p_lda, p_qda, p_rda, p_cart, p_rf,data_plot), function(p) {
    p +	geom_jitter(
      aes(x = x1, y = x2, colour = class, shape = class),data = data, size = 1) +
      scale_colour_manual(values = cbPalette[-1], guide = FALSE) +
      scale_fill_manual(values = cbPalette[-1], guide = FALSE) +
      scale_shape_discrete(guide = FALSE) +
      guides(  # Repeatedly using the class factor messes up the legend
        # Manual setup needed
        colour = guide_legend(
          title = "Class",
          override.aes = list(
            fill = "transparent",
            colour = cbVec,
            shape = shapeVec,
            size = 2,
            linetype = 0))) +
      scale_x_continuous(name = "Feature 1", expand = c(0, 0)) +
      scale_y_continuous(name = "Feature 2", expand = c(0, 0)) +
      theme_minimal() +
      theme(panel.grid = element_blank())
  })
  
  g <- ggpubr::ggarrange(plotlist = plots, ncol = 3, nrow = 3, common.legend = TRUE, legend = "bottom")
  #g <- ggpubr::ggarrange(plotlist = plots, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
  return(g)
}

### Generate all datasets ###
p <- 2 #feature dimensions
n <- 100 #data points

#Generate data classified by flag of Japan
japan_data <- matrix(runif(2*n,-1,1),n,2)
r <- sqrt(2/pi)
japan_class <- rowSums(japan_data^2)<r^2
japan_class[japan_class] <- "A"
japan_class[japan_class!="A"] <- "B"
japan_data <- tibble(x1 = japan_data[,1], x2 = japan_data[,2]) %>%
  mutate(class = as.factor(japan_class))




#Generate data with square in corner
square_data <- matrix(runif(2*n,-1,1),n,2)
cut_off <- sqrt(2)-1
square_class <- square_data[,1]<cut_off & square_data[,2]<cut_off
square_class[square_class] <- "A"
square_class[square_class!="A"] <- "B"
square_data <- tibble(x1 = square_data[,1], x2 = square_data[,2]) %>%
  mutate(class = as.factor(square_class))

angle <- pi/4
rotation_matrix <- matrix(c(cos(angle),-sin(angle),sin(angle),cos(angle)),2,2)
rotated_square_data <- t(rotation_matrix%*%t(square_data[c("x1","x2")]))
rotated_square_data <- tibble(x1 = rotated_square_data[,1], x2 = rotated_square_data[,2]) %>%
  mutate(class = as.factor(square_class))


varMatrix <- diag(x = 1, nrow = p, ncol = p) #Variance matrix
data <- mvrnorm(n, mu=c(-5,0), Sigma = varMatrix)
data <- data %>% 
  as_tibble() %>% 
  set_colnames(c('x1', 'x2')) %>%
  mutate(class = rep("A", n))
data2 <- mvrnorm(n, mu=c(0,0), Sigma = diag(x=1, nrow=p, ncol=p))
data2 <- data2 %>%
  as_tibble() %>%
  set_colnames(c('x1','x2')) %>%
  mutate(class = rep("B", n))
data3 <- mvrnorm(n, mu=c(5,0), Sigma = diag(x=1, nrow=p, ncol=p))
data3 <- data3 %>%
  as_tibble() %>%
  set_colnames(c('x1','x2')) %>%
  mutate(class = rep("C", n))

normal_on_axis_data <- rbind(data,data2,data3)
normal_on_axis_data$class <- as.factor(normal_on_axis_data$class)

normal_on_line_data <- t(rotation_matrix%*%t(normal_on_axis_data[c("x1","x2")]))
normal_on_line_data <- normal_on_line_data%>%
  as_tibble() %>%
  set_colnames(c('x1','x2'))
normal_on_line_data <- mutate(normal_on_line_data, class=as.factor(normal_on_axis_data$class))



varMatrix <- diag(x = 1, nrow = p, ncol = p) #Variance matrix
h=sqrt(5^2-2.5^2)
dataA <- mvrnorm(n, mu=c(0,h), Sigma = varMatrix)
dataA <- dataA %>% 
  as_tibble() %>% 
  set_colnames(c('x1', 'x2')) %>%
  mutate(class = rep("A", n))
dataB <- mvrnorm(n, mu=c(-2.5,-h), Sigma = diag(x=1, nrow=p, ncol=p))
dataB <- dataB %>%
  as_tibble() %>%
  set_colnames(c('x1','x2')) %>%
  mutate(class = rep("B", n))
dataC <- mvrnorm(n, mu=c(2.5,-h), Sigma = diag(x=1, nrow=p, ncol=p))
dataC <- dataC %>%
  as_tibble() %>%
  set_colnames(c('x1','x2')) %>%
  mutate(class = rep("C", n))


normal_in_triangle_data <- rbind(dataA,dataB,dataC)
normal_in_triangle_data$class <- as.factor(normal_in_triangle_data$class)




#varMatrix <- diag(x = 1, nrow = p, ncol = p) #Variance matrix
varMatrix <- matrix(c(1,0,0,10),nrow = p, ncol = p)

data <- mvrnorm(n, mu=c(-sqrt(5),-sqrt(5)), Sigma = varMatrix)
data <- data %>% 
  as_tibble() %>% 
  set_colnames(c('x1', 'x2')) %>%
  mutate(class = rep("A", n))

varMatrix <- matrix(c(1,-0.5,-0.5,1),nrow = p, ncol = p)
data2 <- mvrnorm(n, mu=c(0,0), Sigma = varMatrix)
data2 <- data2 %>%
  as_tibble() %>%
  set_colnames(c('x1','x2')) %>%
  mutate(class = rep("B", n))

varMatrix <- matrix(c(10,0,0,1),nrow = p, ncol = p)
data3 <- mvrnorm(n, mu=c(sqrt(5),sqrt(5)), Sigma =varMatrix)
data3 <- data3 %>%
  as_tibble() %>%
  set_colnames(c('x1','x2')) %>%
  mutate(class = rep("C", n))

normal_on_axis_data <- rbind(data,data2,data3)
normal_on_axis_data$class <- as.factor(normal_on_axis_data$class)

normal_on_line_data <- normal_on_axis_data[c("x1","x2")]
normal_on_line_data <- normal_on_line_data%>%
  as_tibble() %>%
  set_colnames(c('x1','x2'))
normal_on_line_data <- mutate(normal_on_line_data, class=as.factor(normal_on_axis_data$class))


## Plot ##
data_list <- list(japan_data, square_data, rotated_square_data, normal_on_axis_data, normal_on_line_data, normal_in_triangle_data)
#data_list <- list(normal_on_line_data)

#plots <- lapply(data_list, plotClassifiers)
plotnames <- c("Norm_cov3")

plot <- plotClassifiers(rotated_square_data)

for(i in 1:length(data_list)){
  ggexport(plots[[i]], filename = paste("plot-", plotnames[i], ".png", sep=""))
}

