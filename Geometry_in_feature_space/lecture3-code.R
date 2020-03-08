## ----setup, include=FALSE------------------------------------------------
# General purpose packages (data handling, pretty plotting, ...)
library(tidyverse)
library(latex2exp) # Latex in ggplot2 labels
library(kableExtra) # Pretty printing for tables
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour-blind friendly palette

# Packages for actual computation
# library(MASS)  # Contains mvrnorm to simulate multivariate normal
#                # Use as MASS::mvrnorm is recommended since MASS::select
#                # clashes with dplyr::select
# library(FNN)   # Fast Nearest Neighbor Search Algorithms and Applications
# library(caret) # A huge package useful for all kinds of statistical tasks.
#                # Can be used for the whole data analysis process from
#                # data exploration, to model fitting, prediction and analysis
#                # Here: Used to create folds for cross-validation

# Download breast cancer data
UCI_data_URL <- RCurl::getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data')
names <- c('id_number', 'diagnosis', 'radius_mean',
         'texture_mean', 'perimeter_mean', 'area_mean',
         'smoothness_mean', 'compactness_mean',
         'concavity_mean','concave_points_mean',
         'symmetry_mean', 'fractal_dimension_mean',
         'radius_se', 'texture_se', 'perimeter_se',
         'area_se', 'smoothness_se', 'compactness_se',
         'concavity_se', 'concave_points_se',
         'symmetry_se', 'fractal_dimension_se',
         'radius_worst', 'texture_worst',
         'perimeter_worst', 'area_worst',
         'smoothness_worst', 'compactness_worst',
         'concavity_worst', 'concave_points_worst',
         'symmetry_worst', 'fractal_dimension_worst')
breast_cancer <- as_tibble(read.table(
  textConnection(UCI_data_URL), sep = ',', col.names = names)) %>%
  mutate(diagnosis = as.factor(case_when(
    diagnosis == "M" ~ "Malignant",
    diagnosis == "B" ~ "Benign")))
# # After downloading and preparing the data we can save it for future use
# saveRDS(breast_cancer, "breast-cancer.RDa")
# # And then load from the hard disk again
# breast_cancer <- readRDS("breast-cancer.RDa")


## ----choose-k-in-knn, fig.width=5, fig.height=2.5, fig.align="center", echo=FALSE, warning=FALSE, cache=TRUE, message=FALSE----
data_train <- breast_cancer %>%
  select(diagnosis, compactness_mean, symmetry_mean) %>%
  rename(class = diagnosis, x1 = compactness_mean, x2 = symmetry_mean)

# Create a grid where we predict the class
h <- 0.005 # Step-width
x1_arr <- seq(0, 0.4, by = h)
x2_arr <- seq(0.1, 0.32, by = h)
data_test <- expand.grid(x1_arr, x2_arr)

ks <- c(1, 10, 100)
class_pred <- lapply(ks, function(k) {
  FNN::knn(
    data_train[,2:3],   # training data (variables)
    data_test,          # test data (variables)
    data_train$class,   # training data (classes)
    k = k)              # k
})

# Predictions for plotting
data_pred <- tibble(
  class = as.factor(do.call(c, class_pred)),
  x1 = rep(data_test[,1], times = length(ks)),
  x2 = rep(data_test[,2], times = length(ks)),
  k = factor(
      rep(sprintf("k = %d", ks), each = dim(data_test)[1]),
      levels = sprintf("k = %d", ks)))

# Show data together with decision boundary
p_knn <- ggplot() +
  geom_tile(
    aes(x = x1, y = x2, fill = class),
    data = data_pred, alpha = 0.5,
    width = h, height = h,
    colour = "transparent") +
  geom_point(
    aes(x = x1, y = x2, colour = class, fill = class),
    data = data_train, size = .8) +
  facet_wrap(~ k, ncol = 3) +
  scale_colour_manual("Diagnosis", values = cbPalette[-1]) +
  scale_fill_manual("Diagnosis", values = cbPalette[-1], guide = FALSE) +
  scale_x_continuous(
    "Compactness", expand = c(0, 0),
    breaks = c(0.0, 0.1, 0.2, 0.3, NULL)) +
  scale_y_continuous("Symmetry", expand = c(0, 0)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank())

p_knn


## ----global-vs-local-rules-plot-function, include=FALSE------------------
# Create two plots to illustrate global vs local rules
# Or equivalently: Bias vs Variance trade-off

# This function applies LDA and kNN to a two-class classification problem
# The true boundary of the data is described by the input function `boundary`
# and takes a vector of x2 values returning a x1 value (for visual reasons
# boundaries go from top to bottom).

# In addition the function simulates data and classifies it according to the
# true boundary. The data is then sub-sampled into M training samples where
# each training sample randomly contains frac * 100% of all simulated data.
# In addition the `k` for kNN can be specified.

# LDA and kNN are applied to each training sample and the resulting
# decision boundaries are plotted (grey lines). In addition they are
# averaged and plotted as a strong black line.

generate_data <- function(boundary) {
  data_boundary <- tibble(
    x2 = seq(-6, 6, by = 0.1)) %>%
    mutate(x1 = boundary(x2))

  # Simulate data points in [-2, 2] x [-6, 6] and classify as 0 if left
  # of the wiggly line and 1 if on the right
  X_all <- matrix(runif(2 * 800), ncol = 2)
  X_all[,1] <- 4 * (X_all[,1] - 0.5)
  X_all[,2] <- 12 * (X_all[,2] - 0.5)
  colnames(X_all) <- c("x1", "x2")
  data_all <- X_all %>%
    as_tibble %>%
    mutate(class = as.factor(case_when(
      x1 < boundary(x2) ~ 0,
      TRUE ~ 1)))

  # Return data_all
  list(boundary = data_boundary, all = data_all)
}

plot_global_vs_local_rules <- function(boundary, M = 10, frac = 0.2, k = 3) {
  data_gen <- generate_data(boundary)

  # Create M different training data sets (all 10% of original data)
  data_train <- lapply(1:M, function(i) sample_frac(data_gen$all, frac))

  p_base <- ggplot(mapping = aes(x = x1, y = x2)) +
    geom_point(
      aes(colour = class),
      data = data_gen$all,
      size = 0.6,
      alpha = 0.5) +
    geom_path(data = data_gen$boundary, size = 0.6, colour = "red") +
    scale_colour_manual(values = cbPalette[-1], guide = FALSE) +
    scale_x_continuous(name = TeX("x_1"), expand = c(0, 0)) +
    scale_y_continuous(name = TeX("x_2"), expand = c(0, 0)) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  #### Classify points with LDA, to see a global rule at work
  h <- 0.01
  x1s <- seq(-2, 2, by = h)
  x2s <- c(-6, 6) # We have a linear decision boundary somewhat parallel
          # to x2 axis, so we need to investigate all the x1 values
          # but only the x2 value in the first and last row
  X_pred <- expand.grid(x1s, x2s)
  colnames(X_pred) <- c("x1", "x2")
  # Use LDA and kNN (k = 3) to construct global and local decision boundaries
  data_pred_lda <- do.call(bind_rows, lapply(data_train, function(train) {
    classes <- as.numeric(
      predict(MASS::lda(class ~ x1 + x2, train), X_pred)$class)
    # Find decision boundaries numerically
    # Create a matrix with the same dimensions as the whole grid and
    # put the predicted class at each grid point.
    # Note that the lower left corner of what is in the plot will be
    # the upper left corner.
    # The class on the left of the decision boundary has a lower index than
    # the one on the right and which.max finds the first index at which
    # the higher class appears
    x1_indices <- apply(
        matrix(classes, ncol = length(x1s), byrow = TRUE),
        1, which.max)
    # Since we are finding the first index of x1 where the second class
    # is already winning, lets take the midpoint between that one and the
    # x1 value before.
    # Also, since we are dealing with a linear decision boundary, only the
    # first and last x1 and x2 values are needed
    x1 <- (x1s[x1_indices] + x1s[x1_indices - 1]) / 2
    x2 <- x2s
    c(x1 = x1[1], x1end = x1[2], x2 = x2[1], x2end = x2[2])
  }))

  p1 <- p_base +
    geom_segment(
      aes(xend = x1end, yend = x2end),
      data = data_pred_lda,
      colour = cbPalette[1],
      size = 0.4,
      alpha = 0.8) +
    geom_segment(
      aes(xend = x1end, yend = x2end),
      data = data_pred_lda %>% summarize_all(mean),
      size = 0.6) +
    ggtitle("LDA")

  #### Classify the points with kNN, to see a local rule at work
  h <- 0.05
  x1s <- seq(-2, 2, by = h)
  x2s <- seq(-6, 6, by = h) # The decision boundary could be wild so we need
                # to look at many x2s
  X_pred <- expand.grid(x1s, x2s)
  colnames(X_pred) <- c("x1", "x2")
  # Use LDA and kNN (k = 3) to construct global and local decision boundaries
  data_pred_knn <- lapply(data_train, function(train) {
    classes <- as.numeric(
      FNN::knn(as.matrix(train[,1:2]), X_pred, train$class, k = k))
    # As above
    x1_indices <- apply(
        matrix(classes, ncol = length(x1s), byrow = TRUE),
        1, which.max)
    # Since we are finding the first index of x1 where the second class
    # is already winning, lets take the midpoint between that one and the
    # x1 value before.
    # Take all x2 values here since kNN boundaries are not linear
    x1 <- (x1s[x1_indices] + x1s[x1_indices - 1]) / 2
    x2 <- x2s
    tibble(x1, x2)
  })

  data_pred_knn <- bind_cols(
    do.call(bind_rows, data_pred_knn),
    tibble(m = as.factor(rep(1:M, each = length(x2s)))))

  data_pred_knn_overall <- data_pred_knn %>%
    select(-m) %>%
    group_by(x2) %>%
    summarize(x1 = mean(x1))

  p2 <- p_base +
    geom_path(
      aes(group = m),
      data = data_pred_knn,
      colour = cbPalette[1],
      size = 0.4,
      alpha = 0.8) +
    geom_path(data = data_pred_knn_overall, size = 0.6) +
    ggtitle(TeX(paste0("kNN ($k = ", k, "$)")))

  list(lda_plot = p1, knn_plot = p2)
}


## ----global-rule-simple-boundary, fig.width=3, fig.height=3.5, fig.align="center", echo=FALSE, warning=FALSE, results=FALSE, cache=TRUE, message=FALSE----
set.seed(2636472)
# Create a straight and simple boundary; chosen arbitrarily
boundary_simple <- function(x2) {
  -0.075 + 0.35 / 6 * x2
}

plots_simple <- plot_global_vs_local_rules(boundary_simple)
plots_simple$lda_plot


## ----local-rule-simple-boundary, fig.width=3, fig.height=3.5, fig.align="center", echo=FALSE, warning=FALSE, results=FALSE, cache=TRUE, message=FALSE----
plots_simple$knn_plot


## ----global-rule-complex-boundary, fig.width=3, fig.height=3.5, fig.align="center", echo=FALSE, warning=FALSE, results=FALSE, cache=TRUE, message=FALSE----
set.seed(2636472)
# Create a wiggly true decision boundary; chosen arbitrarily
boundary_complex <- function(x2) {
  3.5 / (x2 + 9) * sin((x2 + 9) / 0.7) + 0.1 * sin((x2 + 7) / 0.2)
}

plots_complex <- plot_global_vs_local_rules(boundary_complex)
plots_complex$lda_plot


## ----local-rule-complex-boundary, fig.width=3, fig.height=3.5, fig.align="center", echo=FALSE, warning=FALSE, results=FALSE, cache=TRUE, message=FALSE----
plots_complex$knn_plot


## ----local-vs-global-performance, echo=FALSE, warning=FALSE, message=FALSE----
set.seed(2636472)
data_gen_simple <- generate_data(boundary_simple)
set.seed(2636472)
data_gen_complex <- generate_data(boundary_complex)

# Caret creates stratified folds
fold_indices <- caret::createFolds(data_gen_simple$all$class, k = 10)
avg_cv_errs <- sapply(
  list(data_gen_simple, data_gen_complex), function(data_gen) {
  cv_errs <- sapply(fold_indices, function(fold) {
    # Separate into training and test data per fold
    train <- data_gen$all[-fold,]
    X_test <- data_gen$all[fold,1:2]
    class_test <- data_gen$all[fold,]$class
    # Run LDA and predict classes
    fit <- MASS::lda(class ~ x1 + x2, train)
    class_pred_lda <- predict(fit, X_test)$class
    # Run kNN and predict classes
    class_pred_knn <- FNN::knn(train[,1:2], X_test, train$class, k = 3)

    c(
      lda = sum(class_pred_lda != class_test),
      knn = sum(class_pred_knn != class_test))
  })
  apply(cv_errs, 1, function(errs) {
    sum(errs) / dim(data_gen$all)[1]
  })
})

colnames(avg_cv_errs) <- c("simple", "complex")
rownames(avg_cv_errs) <- c("LDA", "kNN (k = 3)")
knitr::kable(
  avg_cv_errs,
  "latex",
  digits = 3,
  booktabs = TRUE,
  caption = "Average cross-validation errors for ten folds") %>%
  add_header_above(c(" ", "Boundary" = 2))


## ----nc-lda-qda-plots, fig.width=5, fig.height=2.5, fig.align="center", echo=FALSE, warning=FALSE, results=FALSE, cache=TRUE, message=FALSE----
# Keep all classes this time
data_train <- as_tibble(datasets::iris) %>%
  select(Species, Sepal.Length, Sepal.Width) %>%
  rename(class = Species, x1 = Sepal.Length, x2 = Sepal.Width)

# Calculate centroids per class
data_centroid <- data_train %>%
  group_by(class) %>%
  summarise(
    x1 = mean(x1),
    x2 = mean(x2))

# Classify with nearest centroid method
h <- 0.03
x1s <- seq(4, 8, by = h)
x2s <- seq(1.8, 4.8, by = h)
X_pred <- expand.grid(x1s, x2s)
colnames(X_pred) <- c("x1", "x2")
n_pred <- dim(X_pred)[1]

centroids <- as.matrix(data_centroid[,2:3])

y_pred <- apply(
  apply(centroids, 1,
    function(c) {
      # Calculate squared Euclidean norm for each vector in the grid
      # for the current centroid c
      rowSums(
        (X_pred - matrix(
          rep.int(c, n_pred), nrow = n_pred, byrow =TRUE)) ^ 2)
    }),
  1, which.min) # Determine if the vector is closest to centroid 1, 2, or 3

data_pred <- tibble(
  x1 = X_pred[,1],
  x2 = X_pred[,2],
  class = y_pred %>%
    as.factor %>%
    (function(f) { levels(f) <- levels(data_train$class); f })(.))

# Same data as for nearest centroid example
fit_lda <- MASS::lda(class ~ x1 + x2, data_train)
fit_qda <- MASS::qda(class ~ x1 + x2, data_train)

y_pred_lda <- predict(fit_lda, X_pred)$class
y_pred_qda <- predict(fit_qda, X_pred)$class

data_pred_da <- cbind(
  data_pred,
  tibble(class_lda = y_pred_lda, class_qda = y_pred_qda)) %>%
  rename(class_centroid = class)

p_centroid <- ggplot() +
  geom_tile(
    aes(x = x1, y = x2, colour = class, fill = class_centroid),
    colour = "transparent",
    data = data_pred_da,
    width = h,
    height = h,
    alpha = 0.4) +
  ggtitle("Nearest Centroids")

p_lda <- ggplot() +
  geom_tile(
    aes(x = x1, y = x2, colour = class, fill = class_lda),
    colour = "transparent",
    data = data_pred_da,
    width = h,
    height = h,
    alpha = 0.4) +
  ggtitle("LDA")

p_qda <- ggplot() +
  geom_tile(
    aes(x = x1, y = x2, colour = class, fill = class_qda),
    colour = "transparent",
    data = data_pred_da,
    width = h,
    height = h,
    alpha = 0.4) +
  ggtitle("QDA")

plots <- lapply(list(p_centroid, p_lda, p_qda), function(p) {
  p + geom_jitter(
    aes(x = x1, y = x2, colour = class, shape = class),
    data = data_train, height = 0.1, width=0.1, size = 1) +
  scale_colour_manual(values = cbPalette[-1], guide = FALSE) +
  scale_fill_manual(values = cbPalette[-1], guide = FALSE) +
  scale_shape_discrete(guide = FALSE) +
  guides(  # Repeatedly using the class factor messes up the legend
       # Manual setup needed
    colour = guide_legend(
      title = "Species",
      override.aes = list(
        fill = "transparent",
        colour = cbPalette[2:4],
        shape = c(16, 17, 15),
        size = 2,
        linetype = 0))) +
  scale_x_continuous(name = "Sepal Length", expand = c(0, 0)) +
  scale_y_continuous(name = "Sepal Width", expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank())
})

ggpubr::ggarrange(
  plotlist = plots, ncol = 3,
  common.legend = TRUE, legend = "bottom")


## ----nc-lda-qda-performance, echo=FALSE, warning=FALSE, message=FALSE----
fold_indices <- caret::createFolds(data_train$class, k = 10)

cv_errs <- sapply(fold_indices, function(fold) {
  X_train <- data_train[-fold, 2:3]
  class_train <- data_train[-fold,]$class
  X_test <- data_train[fold, 2:3]
  class_test <- data_train[fold,]$class

  centroids <- data_train[-fold,] %>%
    group_by(class) %>%
    summarise(
      x1 = mean(x1),
      x2 = mean(x2)) %>%
    select(x1, x2) %>%
    as.matrix

  class_pred_nc <- apply(
    apply(centroids, 1,
      function(c) {
        # Calculate squared Euclidean norm for each vector in the grid
        # for the current centroid c
        rowSums(
          (X_test - matrix(
            rep.int(c, dim(X_test)[1]),
            nrow = dim(X_test)[1], byrow =TRUE)) ^ 2)
      }),
    1, which.min) # Determine closest centroid

  fit_lda <- MASS::lda(class ~ x1 + x2, data_train[-fold,])
  fit_qda <- MASS::qda(class ~ x1 + x2, data_train[-fold,])

  class_pred_lda <- predict(fit_lda, X_test)$class
  class_pred_qda <- predict(fit_qda, X_test)$class

  c(
    nc = sum(as.integer(class_test) != class_pred_nc),
    lda = sum(class_test != class_pred_lda),
    qda = sum(class_test != class_pred_qda))
})

avg_cv_errs <- matrix(rowSums(cv_errs) / dim(data_train)[1], nrow = 1)
colnames(avg_cv_errs) <- c("NC", "LDA", "QDA")
knitr::kable(
  avg_cv_errs,
  "latex",
  digits = 3,
  booktabs = TRUE,
  caption = "Average cross-validation errors for ten folds")


## ----choose-k-in-knn-revisited-reminder, fig.width=5, fig.height=2.5, fig.align="center", echo=FALSE, warning=FALSE, cache=TRUE, message=FALSE----
p_knn


## ----choose-k-in-knn-revisited-roc, fig.width=3.5, fig.height=3.5, fig.align="center", echo=FALSE, warning=FALSE, cache=TRUE, message=FALSE----
data_train <- breast_cancer %>%
  select(diagnosis, compactness_mean, symmetry_mean) %>%
  rename(class = diagnosis, x1 = compactness_mean, x2 = symmetry_mean) %>%
  mutate(class = ordered(class, levels = c("Benign", "Malignant")))
  # Order the classes to be able to create a ROC curve later

n_fold <- 5
fold_indices <- caret::createFolds(data_train$class, k = n_fold)

ks <- c(1, 3, 5, 10, 100)
fpr_grid <- seq(0, 1, by = 0.01)
cv_outcomes <- lapply(ks, function(k) {
  cv_outcome <- lapply(fold_indices, function(fold) {
    # Split data into training and test sets
    X_train <- as.matrix(data_train[-fold, 2:3])
    class_train <- data_train[-fold,]$class
    X_test <- as.matrix(data_train[fold, 2:3])
    class_test <- data_train[fold,]$class
    # Determine predictions
    class_pred <- FNN::knn(
      X_train,      # training data (variables)
      X_test,       # test data (variables)
      class_train,  # training data (classes)
      k = k,        # k
      prob = TRUE)  # Return class probabilities
    class_pred_train <- FNN::knn(
      X_train,      # training data (variables)
      X_train,      # training data again
      class_train,  # training data (classes)
      k = k)        # k

    # Retrieve probabilities for the predicted class (always above 50%)
    prob <- attr(class_pred, "prob")
    # FNN::knn returns the probability for the predicted class
    # Here we need the probability of a positive outcome (Malignant)
    prob <- ifelse(class_pred == "Benign", 1 - prob, prob)

    roc_obj <- pROC::roc(class_test, prob)
    tibble(
      tpr = approx(
        1 - roc_obj$specificities,
        roc_obj$sensitivities,
        fpr_grid)$y,
      acc = rep(
        sum(factor(class_test, ordered = FALSE) != class_pred),
        times = length(fpr_grid)),
      acc_train = rep(
        mean(
          factor(class_train, ordered = FALSE) != class_pred_train)))
  }) %>% do.call(bind_rows, .)
  bind_cols(tibble(
    k = rep.int(k, n_fold * length(fpr_grid)),
    fold = rep(1:n_fold, each = length(fpr_grid))),
    cv_outcome)
}) %>% do.call(bind_rows, .)

cv_outcomes <- bind_cols(
  cv_outcomes, tibble(fpr = rep(fpr_grid, times = n_fold * length(ks))))

data_roc <- cv_outcomes %>%
  group_by(k, fpr) %>%
  summarize(tpr = mean(tpr)) %>%
  arrange(k, fpr)

ggplot(data_roc, aes(x = fpr, y = tpr)) +
  geom_line(aes(colour = as.factor(k))) +
  geom_line(
    data = tibble(fpr = c(0, 1), tpr = c(0, 1)),
    linetype = 2,
    colour = "red") +
  scale_x_continuous(
    "False Positive Rate", lim = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(
    "True Positive Rate", lim = c(0, 1), expand = c(0, 0)) +
  scale_colour_manual("k", values = cbPalette) +
  theme_minimal() +
  theme(legend.position = "bottom")


## ----choose-k-in-knn-revisited-table, echo=FALSE, warning=FALSE, message=FALSE----
data_acc <- cv_outcomes %>%
  select(k, fold, acc, acc_train) %>%
  unique %>%
  group_by(k) %>%
  summarize(acc_train = mean(acc_train), acc = sum(acc) / dim(data_train)[1])

colnames(data_acc) <- c("\\(k\\)", "\\(R^{tr}\\)", "\\(R^{cv}\\)")

kable(
  data_acc,
  "latex",
  digits = c(0, 3, 3),
  booktabs = TRUE,
  caption = "Average training and cross-validation errors for five folds",
  escape = FALSE)

