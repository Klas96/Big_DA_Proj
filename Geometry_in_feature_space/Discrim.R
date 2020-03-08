library(rpart)         # A package for building CART trees
#library(rpart.plot)    # Nice plotting of CART trees built with rpart
#library(randomForest)  # A package to fit a random forest
library(MASS) #For LDA & QDA
library(klaR) #For RDA
library(tidyverse)
library(magrittr)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour-blind friendly palette

#Samuel: Plot, CART+DA(?)
#Klas: kNN
#Daniel: Neareast centroid + RF
#Hampus: Data

p <- 2 #feature dimensions
n <- 100 #data points
varMatrix <- diag(x = 1, nrow = p, ncol = p) #Variance matrix
meanVec <- rep(1,p) #Mean vector
data <- mvrnorm(n, mu=meanVec, Sigma = varMatrix)
data <- data %>% 
  as_tibble() %>% 
  set_colnames(c('x1', 'x2')) %>%
  mutate(class = rep("A", n))

data2 <- mvrnorm(n, mu=rep(5,p), Sigma = diag(x=1, nrow=p, ncol=p))
data2 <- data2 %>%
  as_tibble() %>%
  set_colnames(c('x1','x2')) %>%
  mutate(class = rep("B", n))

data_train <- bind_rows(data[1:(0.7*n),], data2[1:(0.7*n),])
data_train$class <- as.factor(data_train$class)
data_test <- bind_rows(data[(0.7*n+1):n,], data2[(0.7*n+1):n,])
data_test$class <- as.factor(data_test$class)

## Plots LDA, QDA, CART ##
# 'data' should contain columns 'x1', 'x2' (features) and 'class' (class type)
plotClassifiers <- function(data){
  h <- 0.03 #Grid resolution
  #Margins
  m1 <- 0.1*max(dist(data$x1))
  m2 <- 0.1*max(dist(data$x2))
  x1 <- seq(min(data$x1)-m1, max(data$x1)+m1, by = h)
  x2 <- seq(min(data$x2)-m2, max(data$x2)+m2, by = h)
  x_pred <- expand.grid(x1,x2)
  colnames(x_pred) <- c('x1','x2')
  
  fit_lda <- MASS::lda(class ~ x1 + x2, data = data)
  y_pred_lda <- predict(fit_lda, x_pred)$class
  
  fit_qda <- MASS::qda(class ~ x1 + x2, data = data)
  y_pred_qda <- predict(fit_qda, x_pred)$class
  
  fit_rda <- klaR::rda(formula = class ~ x1 + x2, data = data)
  y_pred_rda <- predict(fit_rda, x_pred)$class
  
  fit_cart <- rpart::rpart(class ~ x1 + x2, data = data)
  y_pred_cart <- predict(fit_cart, x_pred)
  y_pred_cart[y_pred_cart[,1] == 1, 1] <- 'A'
  y_pred_cart[y_pred_cart[,2] == 1, 1] <- 'B'
  y_pred_cart <- as.factor(y_pred_cart[,1])
  
  fit_rf <- randomForest::randomForest(class ~ x1 + x2, data = data)
  y_pred_rf <- predict(fit_rf, x_pred)
  
  centroid_fit <- aggregate(. ~ class, data, mean)
  y_pred_centroid <- (q %>% pull(1))[apply(rdist(x_pred,q %>% select(2,3)), 1, which.min)]
  
  data_pred <- tibble(
    x1 = x_pred$x1,
    x2 = x_pred$x2,
    class_centroid = y_pred_centroid,
    class_lda = y_pred_lda,
    class_qda = y_pred_qda,
    class_rda = y_pred_rda,
    class_cart = y_pred_cart,
    class_rf = y_pred_rf
  )
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
  
  plots <- lapply(list(p_centroid, p_lda, p_qda, p_rda, p_cart, p_rf), function(p) {
    p +	geom_jitter(
      aes(x = x1, y = x2, colour = class, shape = class),
      data = data, height = 0.1, width=0.1, size = 1) +
      scale_colour_manual(values = cbPalette[-1], guide = FALSE) +
      scale_fill_manual(values = cbPalette[-1], guide = FALSE) +
      scale_shape_discrete(guide = FALSE) +
      guides(  # Repeatedly using the class factor messes up the legend
        # Manual setup needed
        colour = guide_legend(
          title = "Class",
          override.aes = list(
            fill = "transparent",
            colour = cbPalette[2:3],
            shape = c(16, 17),
            size = 2,
            linetype = 0))) +
      scale_x_continuous(name = "Feature 1", expand = c(0, 0)) +
      scale_y_continuous(name = "Feature 2", expand = c(0, 0)) +
      theme_minimal() +
      theme(panel.grid = element_blank())
  })
  
  ggpubr::ggarrange(plotlist = plots, ncol = 6, common.legend = TRUE, legend = "bottom")
}


