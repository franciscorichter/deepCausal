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


#' @title Train a Causal Model Across Two Environments
#'
#' @description
#' Optimizes model parameters to minimize a causal objective function that balances
#' overall least-squares error across two environments (E1, E2) and a discrepancy term.
#'
#' By default, it uses \code{\link{obj_func}} as the loss function, which computes:
#' \enumerate{
#'   \item \strong{LS (Least-Squares) Term}:
#'     \deqn{f_{LS} = \frac{\mathrm{SSE}_1 + \mathrm{SSE}_2}{n_1 + n_2}}
#'   \item \strong{CD (Causal Discrepancy) Term}:
#'     \deqn{f_{CD} = \Bigl|\mathrm{MSE}(E_1) - \mathrm{MSE}(E_2)\Bigr|}
#' }
#'
#' The final objective is:
#' \deqn{(1 - \lambda) \cdot f_{LS} + \lambda \cdot f_{CD}.}
#'
#' If \code{hidden_sizes} is \code{NULL} or empty, a \strong{linear model} is assumed.
#' Otherwise, a feedforward neural network is assumed (with sigmoid activations).
#'
#' @param data_G1 A data frame containing the first environment's data.
#' @param data_G2 A data frame containing the second environment's data.
#' @param lambda Numeric in [0,1], weighting the discrepancy term vs. LS term.
#' @param target Character string indicating the name of the target variable in \code{data_G1} and \code{data_G2}.
#' @param loss_func A function computing the objective. Defaults to \code{\link{obj_func}}.
#' @param model_func A function that predicts \eqn{\hat{y}} given (weights, X, parameters). By default, it is chosen automatically based on \code{hidden_sizes}.
#' @param hidden_sizes An integer vector specifying the number of neurons in each hidden layer. If empty or \code{NULL}, a linear model is assumed.
#' @param method The optimization method passed to \code{\link[stats]{optim}}. Default is \code{"BFGS"}.
#' @param verbose Logical. If \code{TRUE}, prints detailed configuration and training progress.
#' @param ... Further arguments passed to \code{\link[stats]{optim}} (e.g. \code{control}).
#'
#' @return A list containing:
#' \item{params}{The optimized model parameters (numeric vector).}
#' \item{model_func}{The function used for predictions (linear or neural network).}
#' \item{hidden_sizes}{Hidden layer structure (if any).}
#' \item{target}{The name of the target variable used.}
#'
#' @seealso \code{\link{obj_func}} for the default loss function.
#' @examples
#' \dontrun{
#' # For a linear model:
#' model_linear <- train_causal(data_G1, data_G2, lambda = 0.5, target = "Y")
#'
#' # For a neural network with two layers:
#' model_nn <- train_causal(data_G1, data_G2, lambda = 0.5, target = "Y",
#'                          hidden_sizes = c(3, 3))
#' }
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
  
  # Decide whether to train a linear model or a neural network
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
    total <- total + (hidden_sizes[length(hidden_sizes)] * 1) + 1  # final layer (to output)
    num_params <- total
    if (verbose) {
      message("Training a neural network model...")
      message("  Hidden layer sizes: ", paste(hidden_sizes, collapse = ", "))
      message("  Total number of parameters: ", num_params)
    }
  }
  
  # Generate initial weights uniformly (for example, between -0.5 and 0.5)
  initial_weights <- runif(num_params, min = -0.5, max = 0.5)
  if (verbose) {
    message("Initial weights generated.")
  }
  
  # Define the objective function for optim()
  objective_wrapper <- function(w) {
    loss_func(weights = w,
              data_G1 = data_G1,
              data_G2 = data_G2,
              lambda = lambda,
              model_func = model_func,
              parameters = list(hidden_sizes = hidden_sizes),
              target = target)
  }
  
  # Start optimization
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
  
  # Build the model object to return
  model_list <- list(
    params = optim_res$par,       # optimized weights
    model_func = model_func,      # function for predictions
    hidden_sizes = hidden_sizes,  # hidden layer configuration (if any)
    target = target               # name of the target variable
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


