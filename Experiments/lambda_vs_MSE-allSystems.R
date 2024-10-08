
# Load system definitions
systems <- define_systems()

# Train predictive models
train_predictive_models <- function(data_G1, data_G2, nn_params, model_type, lambda) {
  functional_forms <- define_functional_forms()
  
  cat("\nTraining Model (", model_type, ") with Lambda =", lambda, "...\n")
  if (model_type == "linear") {
    combined_params <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$linear)
  } else {
    combined_params <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$neural_network, parameters = nn_params, nn = TRUE)
  }
  
  return(list(combined_params = combined_params))
}

# Evaluate models with the combined objective
evaluate_models <- function(models, data, nn_params, model_type) {
  
  functional_forms <- define_functional_forms()
  
  combined_weights <- models$combined_params
  
  X <- data[, -ncol(data)]
  
  if (model_type == "linear") {
    combined_pred <- make_predictions(X, combined_weights, functional_forms$linear)
  } else {
    combined_pred <- make_predictions(X, combined_weights, functional_forms$neural_network, parameters = nn_params)
  }
  
  mse <- mean((data[, ncol(data)] - combined_pred)^2)
  
  return(mse)
}

# Experiment runner
run_experiment <- function(selected_system, n_train = 1000, n_test = 1000, lambdas = seq(0, 1, by = 0.25), nn_params = list(hidden_sizes = c(3, 3))) {
  
  cat("Starting data simulation...\n")
  
  # Simulate training and testing data
  train_data_G1 <- selected_system$data_func(n_train,environment = list(mu_A1=0,sigma_A1=0,mu_A2=0,sigma_A2=0))
  train_data_G2 <- selected_system$data_func(n_train, environment = list(mu_A1=1,sigma_A1=1,mu_A2=2,sigma_A2=0.5))
  test_data <- selected_system$data_func(n_test, environment = list(mu_A1=2,sigma_A1=0.8,mu_A2=1.5,sigma_A2=1))
  
  
  
  cat("Starting model training and evaluation...\n")
  
  mse_results <- data.frame(Lambda = numeric(), MSE = numeric(), Sample = character(), Functional_Form = character())
  
  for (lambda in lambdas) {
    # Train linear model
    linear_params <- train_predictive_models(data_G1 = train_data_G1, data_G2 = train_data_G2, nn_params = nn_params, model_type = "linear", lambda)
    
    # Calculate in-sample MSE (training data)
    data_type = "in-sample"
    cat("\nEvaluating models (", data_type, ")...\n")
    mse_linear_in_sample <- evaluate_models(models = linear_params, data = rbind(train_data_G1, train_data_G2), nn_params = nn_params,model_type =  "linear")
    
    # Calculate out-of-sample MSE (testing data)
    data_type = "out-of-sample"
    cat("\nEvaluating models (", data_type, ")...\n")
    mse_linear_out_sample <- evaluate_models(linear_params, test_data, nn_params, "linear")
    
    # Train neural network model
    nn_params <- train_predictive_models(train_data_G1, train_data_G2, nn_params, "neural_network", lambda)
    
    # Calculate in-sample MSE (training data)
    data_type = "in-sample"
    cat("\nEvaluating models (", data_type, ")...\n")
    mse_nn_in_sample <- evaluate_models(nn_params, rbind(train_data_G1, train_data_G2), nn_params, "neural_network")
    
    # Calculate out-of-sample MSE (testing data)
    data_type = "out-of-sample"
    cat("\nEvaluating models (", data_type, ")...\n")
    mse_nn_out_sample <- evaluate_models(nn_params, test_data, nn_params, "neural_network")
    
    # Collect results for linear model
    mse_results <- rbind(mse_results,
                         data.frame(Lambda = lambda, MSE = mse_linear_in_sample, Sample = "In-sample", Functional_Form = "Linear"),
                         data.frame(Lambda = lambda, MSE = mse_linear_out_sample, Sample = "Out-of-sample", Functional_Form = "Linear"))
    
    # Collect results for neural network model
    mse_results <- rbind(mse_results,
                         data.frame(Lambda = lambda, MSE = mse_nn_in_sample, Sample = "In-sample", Functional_Form = "Neural Network"),
                         data.frame(Lambda = lambda, MSE = mse_nn_out_sample, Sample = "Out-of-sample", Functional_Form = "Neural Network"))
    
    cat("Lambda =", lambda, ": In-sample MSE (Linear) =", mse_linear_in_sample, ", Out-of-sample MSE (Linear) =", mse_linear_out_sample, "\n")
    cat("Lambda =", lambda, ": In-sample MSE (Neural Network) =", mse_nn_in_sample, ", Out-of-sample MSE (Neural Network) =", mse_nn_out_sample, "\n")
  }
  
  # Plot results
  p <- ggplot(mse_results, aes(x = Lambda, y = MSE, color = Functional_Form, linetype = Sample)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("blue", "red")) +
    scale_linetype_manual(values = c("In-sample" = "dashed", "Out-of-sample" = "solid")) +
    labs(title = "MSE vs Lambda", x = "Lambda", y = "Mean Squared Error (MSE)", color = "Functional Form", linetype = "Sample") +
    theme_minimal()
  
  print(p)
  
  return(mse_results)
}

# Select system (e.g., System 2)
selected_system <- systems[[1]]

# Run experiment on System 2
results <- run_experiment(selected_system)

