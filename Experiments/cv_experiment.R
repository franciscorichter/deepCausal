# Load required libraries and package functions
library(deepCausal)
library(ggplot2)
library(dplyr)

# Define lambda values, number of repetitions, and CV folds
lambdas <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
repetitions <- 30      # adjust as needed
cv_folds <- 5

# Initialize a results data frame to store CV RMSE for each fold, repetition, lambda, and model type
results <- data.frame(Lambda = numeric(), RMSE = numeric(), Model = character(), Rep = integer(), Fold = integer(), stringsAsFactors = FALSE)

# Select a system for simulation (using the first system as an example)
systems <- define_systems()
selected_system <- systems[[2]]

# Loop over lambda values and repetitions
for(lambda in lambdas) {
  cat("Processing lambda =", lambda, "\n")
  for(rep in 1:repetitions) {
    
    # Simulate training data for Environment 1 and Environment 2
    train_data_G1 <- selected_system$data_func(
      n = 100,
      environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1)
    )
    train_data_G2 <- selected_system$data_func(
      n = 100,
      environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
    )
    
    # Train a linear causal model (default when hidden_sizes is not provided)
    model_linear <- train_causal(
      data_G1 = train_data_G1,
      data_G2 = train_data_G2,
      lambda = lambda,
      target = "Y",
      cv_folds = cv_folds,
      verbose = FALSE
    )
    
    # Train a neural network causal model (specify hidden_sizes)
    model_nn <- train_causal(
      data_G1 = train_data_G1,
      data_G2 = train_data_G2,
      lambda = lambda,
      target = "Y",
      hidden_sizes = c(3, 3),
      cv_folds = cv_folds,
      verbose = T
    )
    
    # Record the CV RMSE values for the linear model
    for(fold in 1:cv_folds) {
      results <- rbind(results, data.frame(
        Lambda = lambda,
        RMSE = model_linear$cv_rmse[fold],
        Model = "Linear",
        Rep = rep,
        Fold = fold,
        stringsAsFactors = FALSE
      ))
    }
    
    # Record the CV RMSE values for the neural network model
    for(fold in 1:cv_folds) {
      results <- rbind(results, data.frame(
        Lambda = lambda,
        RMSE = model_nn$cv_rmse[fold],
        Model = "Neural Network",
        Rep = rep,
        Fold = fold,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Convert Lambda to factor for plotting (preserves ordering)
results$Lambda <- factor(results$Lambda, levels = as.character(lambdas))

# Option 1: Boxplot of CV RMSE vs. Lambda, colored by model type
p_box <- ggplot(results, aes(x = Lambda, y = RMSE, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(title = "CV RMSE Distribution Across Lambda Values",
       x = "Lambda",
       y = "CV RMSE",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Option 2: Violin plot of CV RMSE vs. Lambda, colored by model type
p_violin <- ggplot(results, aes(x = Lambda, y = RMSE, fill = Model)) +
  geom_violin(alpha = 0.7) +
  labs(title = "CV RMSE Distribution Across Lambda Values",
       x = "Lambda",
       y = "CV RMSE",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plots
print(p_box)
print(p_violin)
