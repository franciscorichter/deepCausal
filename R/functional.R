# Define the functional forms for linear and neural network models
define_functional_forms <- function() {
  functional_forms <- list(
    linear = function(weights, data, parameters = NULL) {
      num_features <- ncol(data) - 1  # Number of features (excluding Y)
      intercept <- weights[1]
      coef <- weights[2:(num_features + 1)]  # Adjust based on number of features
      return(intercept + as.matrix(data[, 1:num_features]) %*% coef)
    },
    neural_network = function(weights, data, parameters = list(hidden_sizes = numeric(0))) {
      num_features <- ncol(data) - 1  # Number of features (excluding Y)
      hidden_sizes <- parameters$hidden_sizes
      num_layers <- length(hidden_sizes)
      
      if (num_layers == 0) {
        intercept <- weights[1]
        coef <- weights[2:(num_features + 1)]  # Adjust based on number of features
        linear_combination <- intercept + as.matrix(data[, 1:num_features]) %*% coef
        return(1 / (1 + exp(-linear_combination)))  # Sigmoid transformation
      }
      
      start <- 1
      W <- list()
      b <- list()
      
      for (i in 1:num_layers) {
        if (i == 1) {
          W[[i]] <- matrix(weights[start:(start + num_features * hidden_sizes[i] - 1)], nrow = num_features, ncol = hidden_sizes[i])
          start <- start + num_features * hidden_sizes[i]
        } else {
          W[[i]] <- matrix(weights[start:(start + hidden_sizes[i-1] * hidden_sizes[i] - 1)], nrow = hidden_sizes[i-1], ncol = hidden_sizes[i])
          start <- start + hidden_sizes[i-1] * hidden_sizes[i]
        }
        b[[i]] <- matrix(weights[start:(start + hidden_sizes[i] - 1)], nrow = 1, ncol = hidden_sizes[i])
        start <- start + hidden_sizes[i]
      }
      
      # Output layer weights and bias
      W[[num_layers + 1]] <- matrix(weights[start:(start + hidden_sizes[num_layers] - 1)], nrow = hidden_sizes[num_layers], ncol = 1)
      b[[num_layers + 1]] <- weights[start + hidden_sizes[num_layers]]
      
      A <- as.matrix(data[, 1:num_features])
      for (i in 1:num_layers) {
        Z <- A %*% W[[i]] + matrix(b[[i]], nrow = nrow(A), ncol = hidden_sizes[i], byrow = TRUE)
        A <- 1 / (1 + exp(-Z))  # Sigmoid activation function
      }
      
      # Final layer
      Z_final <- A %*% W[[num_layers + 1]] + b[[num_layers + 1]]
      
      return(as.vector(Z_final))
    }
  )
  return(functional_forms)
}

# Objective function using combined MSE and CD term
obj_func <- function(weights, data_G1, data_G2, lambda, model_func, parameters = NULL) {
  
  # Calculate MSE for each environment
  mse_G1 <- mean((data_G1$Y - model_func(weights, data_G1, parameters))^2)
  mse_G2 <- mean((data_G2$Y - model_func(weights, data_G2, parameters))^2)
  
  # Calculate CD as the absolute difference between the MSEs
  cd <- abs(mse_G1 - mse_G2)
  
  # Calculate combined MSE over both environments
  data_G = rbind(data_G1,data_G2)
  combined_mse <- mean((data_G$Y - model_func(weights, data_G, parameters))^2)
  
  # Return the combined objective function
  return((1 - lambda) * combined_mse + lambda * cd)
}


# Training function with fixed lambda
train_model <- function(data_G1, data_G2, lambda, objective_func, functional_form, parameters = NULL, nn = FALSE) {
  num_features <- ncol(data_G1) - 1  # All features excluding Y
  
  if (nn) {
    hidden_sizes <- parameters$hidden_sizes
    if (length(hidden_sizes) == 0) {  # Logistic regression (no hidden layers)
      num_params <- num_features + 1  # Weights plus bias
    } else {
      num_params <- sum(num_features * hidden_sizes[1] + sum(hidden_sizes[-1] * hidden_sizes[-length(hidden_sizes)]) + hidden_sizes + hidden_sizes[length(hidden_sizes)] + 1)
    }
    initial_weights <- runif(num_params)
    cat("Neural Network Configuration:\n")
    cat("Number of Layers:", length(hidden_sizes), "\n")
    if (length(hidden_sizes) > 0) {
      cat("Number of Neurons per Layer:", hidden_sizes, "\n")
    }
    cat("Total Parameters:", num_params, "\n")
  } else {
    initial_weights <- runif(num_features + 1)  # For linear models (weights + bias)
    cat("Linear Model Configuration:\n")
    cat("Total Parameters:", length(initial_weights), "\n")
  }
  
  start_time <- Sys.time()
  
  optim_res <- optim(
    initial_weights,
    function(w) objective_func(w, data_G1, data_G2, lambda, functional_form, parameters),
    method = "BFGS"
  )
  
  end_time <- Sys.time()
  training_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  cat("Training Time:", round(training_time, 2), "minutes\n")
  
  return(optim_res$par)
}

# Train predictive models with fixed lambda values
train_predictive_models <- function(data_G1, data_G2, define_forms_func, nn_params, model_type, lambda) {
  functional_forms <- define_forms_func()
  
  cat("\nTraining Model (", model_type, ") with Lambda =", lambda, "...\n")
  if (model_type == "linear") {
    combined_params <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$linear)
  } else {
    combined_params <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$neural_network, parameters = nn_params, nn = TRUE)
  }
  
  return(list(combined_params = combined_params))
}

# Evaluate models with the combined objective
evaluate_models <- function(models, test_data, define_forms_func, nn_params, model_type) {
  functional_forms <- define_forms_func()
  
  cat("\nEvaluating models...\n")
  
  combined_weights <- models$combined_params
  
  if (model_type == "linear") {
    combined_pred <- make_predictions(test_data, combined_weights, functional_forms$linear)
  } else {
    combined_pred <- make_predictions(test_data, combined_weights, functional_forms$neural_network, parameters = nn_params)
  }
  
  mse_combined <- mean((test_data$Y - combined_pred)^2)
  
  return(mse_combined)
}

# Prediction function
make_predictions <- function(new_data, model_params, model_func, parameters = NULL) {
  return(model_func(model_params, new_data, parameters))
}

# Generalized data simulation function for multiple environments
simulate_data <- function(system, n, mu_A1, sigma_A1, mu_A2, sigma_A2) {
  environments <- length(mu_A1)
  data_list <- list()
  
  for (env in 1:environments) {
    data_list[[env]] <- system(n = n, environment = env, mu_A1 = mu_A1[env], sigma_A1 = sigma_A1[env], mu_A2 = mu_A2[env], sigma_A2 = sigma_A2[env])
  }
  
  return(data_list)
}


# Train and evaluate models
train_and_evaluate <- function(lambdas, data_G1, data_G2, test_data, define_forms_func) {
  functional_forms <- define_forms_func()
  
  results <- data.frame(
    Lambda = double(),
    MSE = double(),
    Functional_Form = character(),
    Sample = character(),
    stringsAsFactors = FALSE
  )
  
  for (lambda in lambdas) {
    cat("\nEvaluating models for lambda =", lambda, "\n")
    
    # Train Linear Model
    cat("Starting training for Linear Model...\n")
    linear_params <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$linear)
    mse_train_linear <- mean((rbind(data_G1, data_G2)$Y - functional_forms$linear(linear_params, rbind(data_G1, data_G2)))^2)
    mse_test_linear <- mean((test_data$Y - functional_forms$linear(linear_params, test_data))^2)
    cat("Linear Model training and evaluation complete.\n")
    
    # Train Neural Network (3,3)
    cat("Starting training for Neural Network (3,3)...\n")
    nn_params <- list(hidden_sizes = c(3, 3))
    nn_params_trained <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$neural_network, nn_params, nn = TRUE)
    mse_train_nn <- mean((rbind(data_G1, data_G2)$Y - functional_forms$neural_network(nn_params_trained, rbind(data_G1, data_G2), nn_params))^2)
    mse_test_nn <- mean((test_data$Y - functional_forms$neural_network(nn_params_trained, test_data, nn_params))^2)
    cat("Neural Network (3,3) training and evaluation complete.\n")
    
    # Store results
    results <- rbind(
      results,
      data.frame(Lambda = lambda, MSE = mse_train_linear, Functional_Form = "Linear", Sample = "In-sample"),
      data.frame(Lambda = lambda, MSE = mse_test_linear, Functional_Form = "Linear", Sample = "Out-of-sample"),
      data.frame(Lambda = lambda, MSE = mse_train_nn, Functional_Form = "NN (3,3)", Sample = "In-sample"),
      data.frame(Lambda = lambda, MSE = mse_test_nn, Functional_Form = "NN (3,3)", Sample = "Out-of-sample")
    )
  }
  
  cat("\nAll evaluations complete.\n")
  return(results)
}