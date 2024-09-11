#' Define the functional forms for linear and neural network models
#'
#' This function returns a list of functional forms that can be used for modeling,
#' including a linear model and a neural network model.
#'
#' @return A list containing two functions:
#'   \item{linear}{A linear model function that computes a linear combination of features.}
#'   \item{neural_network}{A neural network model function that computes a non-linear combination of features using one or more hidden layers.}
#' @export
define_functional_forms <- function() {
  functional_forms <- list(
    linear = function(weights, X, parameters = NULL) {
      # Ensure we use only the feature matrix X, excluding Y
      intercept <- weights[1]
      coef <- weights[2:(ncol(X) + 1)]  # Adjust based on number of features (X)
      return(intercept + as.matrix(X) %*% coef)
    },
    neural_network = function(weights, X, parameters = list(hidden_sizes = numeric(0))) {
      num_features <- ncol(X)
      hidden_sizes <- parameters$hidden_sizes
      num_layers <- length(hidden_sizes)
      
      A <- as.matrix(X)  # Only X, exclude Y
      
      if (num_layers == 0) {
        intercept <- weights[1]
        coef <- weights[2:(num_features + 1)]  # Adjust based on number of features
        linear_combination <- intercept + A %*% coef
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

#' Objective function using combined MSE and CD term
#'
#' This function calculates an objective function that combines Mean Squared Error (MSE)
#' and Causal Discrepancy (CD) between two different environments.
#'
#' @param weights A numeric vector of weights used by the model.
#' @param data_G1 A data frame containing data from the first environment.
#' @param data_G2 A data frame containing data from the second environment.
#' @param lambda A numeric value representing the trade-off between MSE and CD.
#' @param model_func A function that computes predictions based on the weights and data.
#' @param parameters A list of additional parameters for the model function (optional).
#'
#' @return The value of the combined objective function.
#' @export
obj_func <- function(weights, data_G1, data_G2, lambda, model_func, parameters = NULL) {
  
  # Separate X and Y for each environment, ensure no Y leakage
  X_G1 <- data_G1[, -ncol(data_G1)]  # All features except Y
  X_G2 <- data_G2[, -ncol(data_G2)]  # All features except Y
  
  # Calculate MSE for each environment
  mse_G1 <- mean((data_G1$Y - model_func(weights, X_G1, parameters))^2)
  mse_G2 <- mean((data_G2$Y - model_func(weights, X_G2, parameters))^2)
  
  # Calculate CD as the absolute difference between the MSEs
  cd <- abs(mse_G1 - mse_G2)
  
  # Combine the data from both environments
  X_combined <- rbind(X_G1, X_G2)
  Y_combined <- c(data_G1$Y, data_G2$Y)
  
  # Calculate combined MSE
  combined_mse <- mean((Y_combined - model_func(weights, X_combined, parameters))^2)
  
  # Return the combined objective function
  return((1 - lambda) * combined_mse + lambda * cd)
}

#' Training function with fixed lambda
#'
#' This function trains a model using the provided data from two environments
#' by minimizing the objective function with a fixed lambda value.
#'
#' @param data_G1 A data frame containing data from the first environment.
#' @param data_G2 A data frame containing data from the second environment.
#' @param lambda A numeric value representing the trade-off between MSE and CD.
#' @param objective_func A function that calculates the objective function.
#' @param functional_form A function representing the model to be trained.
#' @param parameters A list of additional parameters for the model function (optional).
#' @param nn A logical value indicating whether the model is a neural network.
#'
#' @return A numeric vector of trained model parameters.
#' @export
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

#' Train predictive models with fixed lambda values
#'
#' This function trains predictive models using the provided data from two environments
#' and a specified lambda value, supporting both linear and neural network models.
#'
#' @param data_G1 A data frame containing data from the first environment.
#' @param data_G2 A data frame containing data from the second environment.
#' @param define_forms_func A function that defines the functional forms for modeling.
#' @param nn_params A list of parameters for the neural network model (optional).
#' @param model_type A character string indicating the model type ("linear" or "neural_network").
#' @param lambda A numeric value representing the trade-off between MSE and CD.
#'
#' @return A list containing the trained model parameters.
#' @export
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

#' Evaluate models with the combined objective
#'
#' This function evaluates trained models using test data by calculating the Mean Squared Error (MSE).
#'
#' @param models A list containing the trained model parameters.
#' @param test_data A data frame containing the test data to evaluate the models.
#' @param define_forms_func A function that defines the functional forms for modeling.
#' @param nn_params A list of parameters for the neural network model (optional).
#' @param model_type A character string indicating the model type ("linear" or "neural_network").
#'
#' @return The MSE of the combined model on the test data.
#' @export
evaluate_models <- function(models, test_data, define_forms_func, nn_params, model_type) {
  functional_forms <- define_forms_func()
  
  cat("\nEvaluating models...\n")
  
  combined_weights <- models$combined_params
  
  X_test <- test_data[, -ncol(test_data)]
  
  if (model_type == "linear") {
    combined_pred <- make_predictions(X_test, combined_weights, functional_forms$linear)
  } else {
    combined_pred <- make_predictions(X_test, combined_weights, functional_forms$neural_network, parameters = nn_params)
  }
  
  mse_combined <- mean((test_data$Y - combined_pred)^2)
  
  return(mse_combined)
}

#' Prediction function
#'
#' This function generates predictions based on the provided model parameters and data.
#'
#' @param X_new A data frame containing the new data (without Y) for which predictions are to be made.
#' @param model_params A numeric vector of the trained model parameters.
#' @param model_func A function that computes predictions based on the model parameters and data.
#' @param parameters A list of additional parameters for the model function (optional).
#'
#' @return A numeric vector of predictions.
#' @export
make_predictions <- function(X_new, model_params, model_func, parameters = NULL) {
  return(model_func(model_params, X_new, parameters))
}