library(deepCausal)
library(ggplot2)

### System 1 Usage (Modified Non-linear System with External Actions)
cat("### Using System 1: Modified Non-linear System with External Actions ###\n")
systems <- pilot_systems()
selected_system1 <- systems[[1]]

# Simulate training data for Environment 1 (no intervention) for System 1
data_env1_sys1 <- selected_system1$data_func(
  n = 100,
  environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1)
)

# Simulate training data for Environment 2 (with intervention) for System 1
data_env2_sys1 <- selected_system1$data_func(
  n = 100,
  environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
)

# Train a linear causal model on System 1
model_linear_sys1 <- train_causal(
  data_G1 = data_env1_sys1,
  data_G2 = data_env2_sys1,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train a neural network causal model on System 1
model_nn_sys1 <- train_causal(
  data_G1 = data_env1_sys1,
  data_G2 = data_env2_sys1,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  verbose = TRUE
)

cat("\nSystem 1 - Linear Model CV RMSE:", model_linear_sys1$cv_performance, "\n")
cat("System 1 - Neural Network Model CV RMSE:", model_nn_sys1$cv_performance, "\n\n")


# For System 1: simulate test data and evaluate out-of-sample performance
test_data_sys1 <- selected_system1$data_func(
  n = 100,
  environment = list(mu_A1 = 2, sigma_A1 = 1, mu_A2 = 2, sigma_A2 = 1)
)

performance_linear_sys1 <- evaluate_causal_model(model_linear_sys1, test_data_sys1)
performance_nn_sys1 <- evaluate_causal_model(model_nn_sys1, test_data_sys1)

cat("\nSystem 1 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys1$MSE, "\n")
cat("  RMSE:", performance_linear_sys1$RMSE, "\n")

cat("\nSystem 1 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys1$MSE, "\n")
cat("  RMSE:", performance_nn_sys1$RMSE, "\n")


### System 2 Usage (7-Variable System with Interventions)
cat("### Using System 2: 7-Variable System with Interventions ###\n")
selected_system2 <- systems[[2]]

# Simulate training data for Environment 1 (no intervention) for System 2
data_env1_sys2 <- selected_system2$data_func(
  n = 100,
  environment = list(mu_X1 = 0, sigma_X1 = 1, A1 = 0, A2 = 0)
)

# Simulate training data for Environment 2 (with interventions) for System 2
data_env2_sys2 <- selected_system2$data_func(
  n = 100,
  environment = list(mu_X1 = 2, sigma_X1 = 1.5, A1 = 1, A2 = 3)
)

# Train a linear causal model on System 2
model_linear_sys2 <- train_causal(
  data_G1 = data_env1_sys2,
  data_G2 = data_env2_sys2,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train a neural network causal model on System 2
model_nn_sys2 <- train_causal(
  data_G1 = data_env1_sys2,
  data_G2 = data_env2_sys2,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  verbose = TRUE
)

cat("\nSystem 2 - Linear Model CV RMSE:", model_linear_sys2$cv_performance, "\n")
cat("System 2 - Neural Network Model CV RMSE:", model_nn_sys2$cv_performance, "\n")

# Optionally, simulate test data and evaluate models for System 2.
test_data_sys2 <- selected_system2$data_func(
  n = 100,
  environment = list(mu_X1 = 3, sigma_X1 = 1, A1 = 1, A2 = 3)
)

performance_linear_sys2 <- evaluate_causal_model(model_linear_sys2, test_data_sys2)
performance_nn_sys2 <- evaluate_causal_model(model_nn_sys2, test_data_sys2)

cat("\nSystem 2 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys2$MSE, "\n")
cat("  RMSE:", performance_linear_sys2$RMSE, "\n")

cat("\nSystem 2 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys2$MSE, "\n")
cat("  RMSE:", performance_nn_sys2$RMSE, "\n")

