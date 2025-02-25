#' @title Objective Function for Multi-Environment MSE and Causal Discrepancy
#'
#' @description
#' Computes a weighted objective that balances overall least-squares error across
#' two environments (E1 and E2) and the discrepancy in errors between them.
#'
#' Specifically:
#' \enumerate{
#'   \item \strong{LS (Least-Squares) Term}: 
#'     \deqn{ f_{LS}(\beta) = \frac{1}{n_1 + n_2} \Bigl[\sum_{i=1}^{n_1}(Y_i^{(E1)} - f_{\beta}(X_i^{(E1)}))^2 
#'     + \sum_{i=1}^{n_2}(Y_i^{(E2)} - f_{\beta}(X_i^{(E2)}))^2\Bigr]. }
#'   \item \strong{CD (Causal Discrepancy) Term}:
#'     \deqn{ f_{CD}(\beta) = \Bigl| \mathrm{MSE}(E_1) - \mathrm{MSE}(E_2)\Bigr|. }
#' }
#'
#' The final objective is:
#' \deqn{ (1-\lambda) \cdot f_{LS}(\beta) \;+\; \lambda \cdot f_{CD}(\beta). }
#'
#' @param weights A numeric vector of model weights/parameters.
#' @param data_G1 A data frame for environment 1. Must contain the target variable.
#' @param data_G2 A data frame for environment 2. Must contain the target variable.
#' @param lambda A numeric value in [0,1] that controls the tradeoff between LS and CD.
#' @param model_func A function that predicts \eqn{\hat{y}} given (weights, X, parameters).
#' @param parameters An optional list of extra parameters for the model function (e.g. hidden layer sizes).
#' @param target A character string naming the target variable in \code{data_G1} and \code{data_G2}.
#'
#' @return A single numeric value giving the objective function (loss).
#' @examples
#' # Suppose data_G1 and data_G2 each have a column "Y" as the target
#' # and columns X1, X2, etc. for predictors.
#' # model_func could be a linear or neural network predictor.
#' # lambda in [0,1].
#' #
#' # obj_value <- obj_func(weights = runif(5),
#' #                       data_G1 = env1_data,
#' #                       data_G2 = env2_data,
#' #                       lambda = 0.5,
#' #                       model_func = my_model_predictor,
#' #                       parameters = list(...),
#' #                       target = "Y")
#'
#' @export
obj_func <- function(weights,
                     data_G1,
                     data_G2,
                     lambda,
                     model_func,
                     parameters = NULL,
                     target = "Y") {
  
  # Extract target & features for environment 1
  Y1 <- data_G1[[target]]
  X_G1 <- data_G1[, setdiff(names(data_G1), target), drop = FALSE]
  n1 <- length(Y1)
  
  # Extract target & features for environment 2
  Y2 <- data_G2[[target]]
  X_G2 <- data_G2[, setdiff(names(data_G2), target), drop = FALSE]
  n2 <- length(Y2)
  
  # Predict for each environment
  pred_G1 <- model_func(weights, X_G1, parameters)
  pred_G2 <- model_func(weights, X_G2, parameters)
  
  # Compute SSE (Sum of Squared Errors) for each environment
  sse_G1 <- sum((Y1 - pred_G1)^2)
  sse_G2 <- sum((Y2 - pred_G2)^2)
  
  # f_LS: Weighted (by total sample size) sum of squared errors
  f_LS <- (sse_G1 + sse_G2) / (n1 + n2)
  
  # MSE in each environment individually
  mse_G1 <- sse_G1 / n1
  mse_G2 <- sse_G2 / n2
  
  # f_CD: absolute difference in MSE between environments
  f_CD <- abs(mse_G1 - mse_G2)
  
  # Final objective: (1 - lambda)*LS + lambda*CD
  loss_value <- (1 - lambda) * f_LS + lambda * f_CD
  
  return(loss_value)
}



#' @title Train a Causal Model with Optional Cross-Validation
#'
#' @description
#' Optimizes model parameters to minimize a causal objective function that balances the
#' overall least-squares error across two environments (E1 and E2) and a discrepancy term.
#' In addition to training on the full training data, an optional K–fold cross validation
#' can be performed to assess the model's performance. The cross-validation splits each
#' environment's data into folds, trains on the union of the training folds, and evaluates
#' on the held-out fold (by merging the validation sets). The RMSE for each fold and the
#' average RMSE are returned.
#'
#' By default, the function uses \code{\link{obj_func}} as the loss function.
#' If \code{hidden_sizes} is \code{NULL} or empty, a linear model is assumed.
#' Otherwise, a feedforward neural network is used.
#'
#' @param data_G1 A data frame containing the first environment's training data.
#' @param data_G2 A data frame containing the second environment's training data.
#' @param lambda Numeric in [0,1] controlling the trade-off between the least-squares and
#'   discrepancy terms.
#' @param target Character string indicating the name of the target variable in \code{data_G1} and \code{data_G2}.
#' @param loss_func A function that computes the objective (loss). Defaults to \code{\link{obj_func}}.
#' @param model_func A function that predicts \eqn{\hat{y}} given (weights, X, parameters). If \code{NULL},
#'   it is chosen automatically from \code{\link{define_functional_forms}} based on \code{hidden_sizes}.
#' @param hidden_sizes An integer vector specifying the number of neurons in each hidden layer.
#'   If \code{NULL} or empty, a linear model is assumed.
#' @param method The optimization method to be passed to \code{\link[stats]{optim}}. Default is \code{"BFGS"}.
#' @param verbose Logical; if \code{TRUE}, prints detailed configuration, progress, and timing information.
#' @param cv_folds Integer; if greater than 1, performs K–fold cross validation on the training data.
#'   Defaults to 1 (i.e. no cross validation).
#' @param ... Further arguments passed to \code{\link[stats]{optim}} (e.g., control parameters).
#'
#' @return A list containing:
#' \describe{
#'   \item{params}{The optimized model parameters (numeric vector) trained on the full data.}
#'   \item{model_func}{The function used for predictions (linear or neural network).}
#'   \item{hidden_sizes}{The hidden layer configuration (if any).}
#'   \item{target}{The name of the target variable used.}
#'   \item{cv_performance}{The average RMSE across the cross-validation folds (or NA if cv_folds == 1).}
#'   \item{cv_rmse}{A numeric vector containing the RMSE for each fold (or NA if cv_folds == 1).}
#' }
#'
#' @seealso \code{\link{obj_func}}, \code{\link{evaluate_causal_model}}.
#'
#' @examples
#' \dontrun{
#'   # Assume data_G1 and data_G2 are training datasets with target column "Y"
#'   model_linear <- train_causal(data_G1, data_G2, lambda = 0.3, target = "Y", cv_folds = 5)
#'
#'   # For a neural network causal model:
#'   model_nn <- train_causal(data_G1, data_G2, lambda = 0.3, target = "Y",
#'                            hidden_sizes = c(3, 3), cv_folds = 5)
#' }
#'
#' @export
train_causal <- function(data_G1,
                         data_G2,
                         lambda,
                         target = "Y",
                         loss_func = obj_func,
                         model_func = NULL,
                         hidden_sizes = NULL,
                         method = "BFGS",
                         verbose = TRUE,
                         cv_folds = 1,
                         ...) {
  
  # Determine feature names (all columns except the target)
  feature_names <- setdiff(names(data_G1), target)
  num_features <- length(feature_names)
  
  if (verbose) {
    message("Starting training of a causal model...")
    message("Target variable: ", target)
    message("Predictor variables: ", paste(feature_names, collapse = ", "))
    message("Number of features: ", num_features)
  }
  
  # Decide model type based on hidden_sizes
  if (is.null(hidden_sizes) || length(hidden_sizes) == 0) {
    # Linear model
    if (is.null(model_func)) {
      model_func <- define_functional_forms()$linear
    }
    num_params <- num_features + 1  # intercept + weights
    if (verbose) {
      message("Training a linear model...")
      message("  Number of parameters: ", num_params)
    }
  } else {
    # Neural network model
    if (is.null(model_func)) {
      model_func <- define_functional_forms()$neural_network
    }
    total <- 0
    prev_size <- num_features
    for (hs in hidden_sizes) {
      total <- total + (prev_size * hs) + hs  # weights and biases for this layer
      prev_size <- hs
    }
    total <- total + (hidden_sizes[length(hidden_sizes)] * 1) + 1  # final layer
    num_params <- total
    if (verbose) {
      message("Training a neural network model...")
      message("  Hidden layer sizes: ", paste(hidden_sizes, collapse = ", "))
      message("  Total number of parameters: ", num_params)
    }
  }
  
  # Generate initial weights randomly (e.g., between -0.5 and 0.5)
  initial_weights <- runif(num_params, min = -0.5, max = 0.5)
  if (verbose) {
    message("Initial weights generated.")
  }
  
  # Define objective function for optim()
  objective_wrapper <- function(w) {
    loss_func(weights = w,
              data_G1 = data_G1,
              data_G2 = data_G2,
              lambda = lambda,
              model_func = model_func,
              parameters = list(hidden_sizes = hidden_sizes),
              target = target)
  }
  
  # Optimize on the full training data
  if (verbose) {
    message("Starting optimization using method: ", method)
  }
  start_time <- Sys.time()
  optim_res <- stats::optim(par = initial_weights,
                            fn = objective_wrapper,
                            method = method,
                            ...)
  end_time <- Sys.time()
  elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
  if (verbose) {
    message("Training complete. Elapsed time: ", elapsed, " seconds.")
    message("Optimization convergence code: ", optim_res$convergence)
  }
  
  # Optional: Perform cross-validation if cv_folds > 1
  cv_performance <- NA
  cv_rmse <- NA
  if (cv_folds > 1) {
    if (verbose) message("Performing ", cv_folds, "-fold cross-validation for training performance...")
    
    # Create fold assignments for each environment
    folds_G1 <- sample(rep(1:cv_folds, length.out = nrow(data_G1)))
    folds_G2 <- sample(rep(1:cv_folds, length.out = nrow(data_G2)))
    cv_rmse_vals <- numeric(cv_folds)
    
    for (k in 1:cv_folds) {
      # Partition data: training and validation for each environment
      train_G1 <- data_G1[folds_G1 != k, , drop = FALSE]
      valid_G1 <- data_G1[folds_G1 == k, , drop = FALSE]
      
      train_G2 <- data_G2[folds_G2 != k, , drop = FALSE]
      valid_G2 <- data_G2[folds_G2 == k, , drop = FALSE]
      
      # Define CV objective function for current fold
      obj_wrap_cv <- function(w) {
        loss_func(weights = w,
                  data_G1 = train_G1,
                  data_G2 = train_G2,
                  lambda = lambda,
                  model_func = model_func,
                  parameters = list(hidden_sizes = hidden_sizes),
                  target = target)
      }
      
      init_w_cv <- runif(num_params, min = -0.5, max = 0.5)
      optim_cv <- stats::optim(par = init_w_cv,
                               fn = obj_wrap_cv,
                               method = method,
                               ...)
      
      # Build a temporary model for CV evaluation
      temp_model <- list(
        params = optim_cv$par,
        model_func = model_func,
        hidden_sizes = hidden_sizes,
        target = target
      )
      
      # Merge validation data from both environments
      valid_data <- rbind(valid_G1, valid_G2)
      perf_cv <- evaluate_causal_model(temp_model, valid_data)
      cv_rmse_vals[k] <- perf_cv$RMSE
      if (verbose) {
        message("  Fold ", k, ": RMSE = ", round(cv_rmse_vals[k], 4))
      }
    }
    cv_performance <- mean(cv_rmse_vals)
    cv_rmse <- cv_rmse_vals
    if (verbose) {
      message("Average cross-validation RMSE: ", round(cv_performance, 4))
    }
  }
  
  # Return the final model (trained on full data) along with CV performance and fold RMSEs.
  model_list <- list(
    params = optim_res$par,
    model_func = model_func,
    hidden_sizes = hidden_sizes,
    target = target,
    cv_performance = cv_performance,
    cv_rmse = cv_rmse
  )
  
  return(model_list)
}





#' @title Evaluate Causal Model Performance and Return Predictions
#'
#' @description
#' Evaluates a trained causal model on a test dataset by computing performance metrics,
#' including Mean Squared Error (MSE) and Root Mean Squared Error (RMSE), and returns the
#' model's predictions on the test set.
#'
#' @param model A list representing the trained causal model (as returned by \code{train_causal()}).
#'   It should include at least the fields \code{params} and either \code{predictor} or \code{model_func},
#'   and a \code{target} field indicating the name of the response variable.
#' @param test_data A data frame containing the test data. It must include a column with the name given by \code{model$target}.
#'
#' @return A list containing:
#' \describe{
#'   \item{MSE}{Mean Squared Error computed on the test set.}
#'   \item{RMSE}{Root Mean Squared Error computed on the test set.}
#'   \item{predictions}{A numeric vector of predictions for the test set.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Assuming model_linear is a model returned by train_causal()
#'   results <- evaluate_causal_model(model_linear, test_data)
#'   print(results$MSE)
#'   print(results$RMSE)
#'   head(results$predictions)
#' }
#'
#' @export
evaluate_causal_model <- function(model, test_data) {
  # Ensure the model object specifies a target
  if (is.null(model$target)) {
    stop("The model object must include a 'target' field specifying the name of the response variable.")
  }
  target <- model$target
  
  # Check that test_data includes the target variable
  if (!(target %in% names(test_data))) {
    stop("Test data does not contain the target variable: ", target)
  }
  
  # Extract the target variable and predictors
  Y_test <- test_data[[target]]
  X_test <- test_data[, setdiff(names(test_data), target), drop = FALSE]
  
  # Determine which predictor function to use: either 'predictor' or 'model_func'
  if (!is.null(model$predictor) && is.function(model$predictor)) {
    pred_fn <- model$predictor
  } else if (!is.null(model$model_func) && is.function(model$model_func)) {
    pred_fn <- model$model_func
  } else {
    stop("The model object does not contain a valid predictor function (neither 'predictor' nor 'model_func').")
  }
  
  # Obtain predictions from the chosen predictor function
  if (is.null(model$nn_params)) {
    predictions <- pred_fn(model$params, X_test)
  } else {
    predictions <- pred_fn(model$params, X_test, parameters = model$nn_params)
  }
  
  # Compute performance metrics
  mse <- mean((Y_test - predictions)^2)
  rmse <- sqrt(mse)
  
  return(list(MSE = mse, RMSE = rmse, predictions = predictions))
}


