
# Load necessary libraries
library(deepCausal)
library(ggplot2)

# Define parameters
k <- 5  # Number of folds for cross-validation
lambdas <- seq(0.1, 1, by = 0.1)  # Lambda values to explore

cat("Starting data simulation...
")
systems <- define_systems()
selected_system <- systems[[1]]  # Example: Complex Non-linear System with Many Variables

# Environment parameters
mu_A1_env1 <- 0; sigma_A1_env1 <- 1
mu_A1_env2 <- 1; sigma_A1_env2 <- 1
mu_A1_env3 <- 2; sigma_A1_env3 <- 2

mu_A2_env1 <- 0; sigma_A2_env1 <- 1
mu_A2_env2 <- 1; sigma_A2_env2 <- 1
mu_A2_env3 <- 2; sigma_A2_env3 <- 2

# Generate initial dataset
train_data <- selected_system$data_func(1000, environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 1, sigma_A2 = 1))

# Split data into k folds
set.seed(123)
folds <- sample(rep(1:k, length.out = nrow(train_data)))

# Dataframe to collect results
mse_results <- data.frame(Lambda = numeric(), MSE = numeric(), Model = character(), Sample = character())

# Cross-validation function
cross_validate <- function(train_data, model_type, lambdas, folds) {
  results <- data.frame()
  
  for (lambda in lambdas) {
    cat(sprintf("
Testing lambda = %.2f
", lambda))
    
    mse_folds <- c()
    
    for (i in 1:k) {
      cat(sprintf("Fold %d of %d
", i, k))
      
      # Splitting data into training and validation sets
      train_fold <- train_data[folds != i, ]
      validation_fold <- train_data[folds == i, ]
      
      if (model_type == "linear") {
        model_params <- train_predictive_models(data_G1 = train_fold, data_G2 = train_fold, nn_params = NULL, model_type = "linear", lambda = lambda)
      } else if (model_type == "neural_network") {
        nn_par <- list(hidden_sizes = c(3, 3))
        model_params <- train_predictive_models(train_fold, train_fold, nn_par, "neural_network", lambda)
      }
      
      # Calculate validation MSE
      mse <- evaluate_models(models = model_params, data = validation_fold, nn_params = ifelse(model_type == "neural_network", nn_par, NULL), model_type = model_type)
      mse_folds <- c(mse_folds, mse)
    }
    
    # Average MSE over k folds
    avg_mse <- mean(mse_folds)
    results <- rbind(results, data.frame(Lambda = lambda, MSE = avg_mse, Model = model_type, Sample = "Validation"))
  }
  
  return(results)
}

# Perform cross-validation for both linear and neural network models
mse_linear <- cross_validate(train_data, "linear", lambdas, folds)
mse_nn <- cross_validate(train_data, "neural_network", lambdas, folds)

# Combine and plot results
mse_results <- rbind(mse_results, mse_linear, mse_nn)

# Plot the cross-validation results
ggplot(mse_results, aes(x = Lambda, y = MSE, color = Model)) +
  geom_line() +
  geom_point() +
  labs(title = "Cross-Validation MSE for Linear and Neural Network Models",
       x = "Lambda",
       y = "Mean Squared Error (MSE)",
       color = "Model Type") +
  theme_minimal()

