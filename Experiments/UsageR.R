set.seed(123)

### System 1 Usage (Modified Non-linear System with External Actions)
cat("### Using System 1: Modified Non-linear System with External Actions ###\n")
systems <- pilot_systems()
selected_system1 <- systems[[1]]
# Simulate training data for Environment 1 (no intervention) for System 1
data_env1_sys1 <- selected_system1$data_func(
  n = 1000,
  environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1)
)

# Simulate training data for Environment 2 (with intervention) for System 1
data_env2_sys1 <- selected_system1$data_func(
  n = 1000,
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
  lambda = 0.6,
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
  n = 1000,
  environment = list(sd_eps = 1)
)

# Simulate training data for Environment 2 (with interventions) for System 2
data_env2_sys2 <- selected_system2$data_func(
  n = 1000,
  environment = list(sd_eps = 1.5)
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
  environment = list(sd_eps = 1)
)

performance_linear_sys2 <- evaluate_causal_model(model_linear_sys2, test_data_sys2)
performance_nn_sys2 <- evaluate_causal_model(model_nn_sys2, test_data_sys2)

cat("\nSystem 2 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys2$MSE, "\n")
cat("\nSystem 2 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys2$MSE, "\n")
cat("  RMSE:", performance_nn_sys2$RMSE, "\n")


### System 3 Usage (Nonlinear Confounded Mediator System)
cat("### Using System 3: Nonlinear Confounded Mediator System ###\n")
selected_system3 <- systems[[3]]

# Env 1
data_env1_sys3 <- selected_system3$data_func(
  n = 1000,
  environment = list(mu_A = 0, sd_A = 1, sd_eps = 1)
)
# Env 2 (shift A and noise)
data_env2_sys3 <- selected_system3$data_func(
  n = 1000,
  environment = list(mu_A = 1, sd_A = 0.5, sd_eps = 1.2)
)

# Train linear
model_linear_sys3 <- train_causal(
  data_G1 = data_env1_sys3,
  data_G2 = data_env2_sys3,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train NN
model_nn_sys3 <- train_causal(
  data_G1 = data_env1_sys3,
  data_G2 = data_env2_sys3,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  verbose = TRUE
)

cat("\nSystem 3 - Linear Model CV RMSE:", model_linear_sys3$cv_performance, "\n")
cat("System 3 - Neural Network Model CV RMSE:", model_nn_sys3$cv_performance, "\n")

# Test
test_data_sys3 <- selected_system3$data_func(
  n = 100,
  environment = list(mu_A = 0.5, sd_A = 1, sd_eps = 1)
)

performance_linear_sys3 <- evaluate_causal_model(model_linear_sys3, test_data_sys3)
performance_nn_sys3 <- evaluate_causal_model(model_nn_sys3, test_data_sys3)

cat("\nSystem 3 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys3$MSE, "\n")
cat("  RMSE:", performance_linear_sys3$RMSE, "\n")

cat("\nSystem 3 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys3$MSE, "\n")
cat("  RMSE:", performance_nn_sys3$RMSE, "\n")


### System 4 Usage (Heteroskedastic Nonlinear Interaction System)
cat("### Using System 4: Heteroskedastic Nonlinear Interaction System ###\n")
selected_system4 <- systems[[4]]

# Env 1
data_env1_sys4 <- selected_system4$data_func(
  n = 1000,
  environment = list(mu_A = 0, sd_A = 1, sd_eps = 1, sdY = 1)
)
# Env 2 (action shift and higher output noise)
data_env2_sys4 <- selected_system4$data_func(
  n = 1000,
  environment = list(mu_A = 1, sd_A = 1, sd_eps = 1, sdY = 2)
)

# Train linear
model_linear_sys4 <- train_causal(
  data_G1 = data_env1_sys4,
  data_G2 = data_env2_sys4,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train NN
model_nn_sys4 <- train_causal(
  data_G1 = data_env1_sys4,
  data_G2 = data_env2_sys4,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  verbose = TRUE
)

cat("\nSystem 4 - Linear Model CV RMSE:", model_linear_sys4$cv_performance, "\n")
cat("System 4 - Neural Network Model CV RMSE:", model_nn_sys4$cv_performance, "\n")

# Test
test_data_sys4 <- selected_system4$data_func(
  n = 100,
  environment = list(mu_A = 0.5, sd_A = 1, sd_eps = 1, sdY = 1.5)
)

performance_linear_sys4 <- evaluate_causal_model(model_linear_sys4, test_data_sys4)
performance_nn_sys4 <- evaluate_causal_model(model_nn_sys4, test_data_sys4)

cat("\nSystem 4 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys4$MSE, "\n")
cat("  RMSE:", performance_linear_sys4$RMSE, "\n")

cat("\nSystem 4 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys4$MSE, "\n")
cat("  RMSE:", performance_nn_sys4$RMSE, "\n")
