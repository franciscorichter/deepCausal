# Load the package (assumes deepCausal is installed and available)
library(deepCausal)

# 1. Load system definitions and select a system.
systems <- define_systems()
selected_system <- systems[[1]]  # Use the first system (e.g., Non-linear System with External Actions)

# 2. Simulate training data for two environments.
# Environment 1 (Group 1)
data_G1 <- selected_system$data_func(
  n = 100,
  environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1)
)

# Environment 2 (Group 2)
data_G2 <- selected_system$data_func(
  n = 100,
  environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
)

# 3. Train a linear causal model using train_causal (linear model since no hidden_sizes specified)
model_linear <- train_causal(
  data_G1 = data_G1,
  data_G2 = data_G2,
  lambda = 0.3,       # trade-off parameter
  target = "Y",       # target variable name in data_G1 and data_G2
  verbose = TRUE      # print configuration details
)

# 4. Train a neural network causal model (with hidden layers specified)
model_nn <- train_causal(
  data_G1 = data_G1,
  data_G2 = data_G2,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),  # Two hidden layers with 3 neurons each
  verbose = TRUE
)

# 5. Inspect the trained models.
cat("Linear Model:\n")
print(model_linear)

cat("\nNeural Network Model:\n")
print(model_nn)

# 6. Simulate a test dataset with different environment parameters.
test_data <- selected_system$data_func(
  n = 100,
  environment = list(mu_A1 = 3, sigma_A1 = 1, mu_A2 = 3, sigma_A2 = 1)
)

# 7. Evaluate the trained causal models on the test data.
performance_linear <- evaluate_causal_model(model_linear, test_data)
performance_nn <- evaluate_causal_model(model_nn, test_data)

cat("\nTest Performance (Linear Model):\n")
cat("  MSE: ", performance_linear$MSE, "\n")
cat("  RMSE:", performance_linear$RMSE, "\n")

cat("\nTest Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn$MSE, "\n")
cat("  RMSE:", performance_nn$RMSE, "\n")

