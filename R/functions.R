############################################
# functions.R
############################################

#' @title Objective Function for Multi-Environment MSE and Causal Discrepancy
#'
#' @description
#' Computes a weighted objective that balances overall least-squares error across
#' two environments (E1 and E2) and the discrepancy in errors between them.
#'
#' Specifically, for each environment \eqn{E_j} (j=1,2) with sample size \eqn{n_j},
#' let \eqn{\text{MSE}(E_j)} be the mean squared error. Then:
#' \enumerate{
#'   \item \strong{LS (Least-Squares) Term}:
#'     \deqn{ f_{LS}(\beta) = \frac{n_1 \times \text{MSE}(E_1) + n_2 \times \text{MSE}(E_2)}{n_1 + n_2}. }
#'   \item \strong{CD (Causal Discrepancy) Term}:
#'     \deqn{ f_{CD}(\beta) = \Bigl|\text{MSE}(E_1) - \text{MSE}(E_2)\Bigr|. }
#' }
#'
#' The final objective is:
#' \deqn{ (1-\lambda)\, f_{LS}(\beta) + \lambda\, f_{CD}(\beta). }
#'
#' @param weights A numeric vector of model weights/parameters.
#' @param data_G1 A data frame for environment 1. Must contain the target variable named \code{target}.
#' @param data_G2 A data frame for environment 2. Must contain the target variable named \code{target}.
#' @param lambda A numeric value in [0,1] that controls the tradeoff between LS and CD.
#' @param model_func A function that predicts \eqn{\hat{y}} given (weights, X, parameters).
#' @param parameters An optional list of extra parameters for the model function (e.g. hidden layer sizes).
#' @param target A character string naming the target variable in \code{data_G1} and \code{data_G2}.
#'
#' @return A single numeric value giving the objective function (loss).
#'
#' @seealso \code{\link{train_causal}}, \code{\link{evaluate_causal_model}}
#'
#' @examples
#' \dontrun{
#'   # Suppose data_G1 and data_G2 each have a column "Y" as the target
#'   # and columns X1, X2, etc. for predictors.
#'   # model_func could be a linear or neural network predictor.
#'   # lambda in [0,1].
#'
#'   obj_value <- obj_func(weights = runif(5),
#'                        data_G1 = env1_data,
#'                        data_G2 = env2_data,
#'                        lambda = 0.5,
#'                        model_func = my_model_predictor,
#'                        parameters = list(...),
#'                        target = "Y")
#' }
#'
#' @export
obj_func <- function(weights,
                     data_G1,
                     data_G2,
                     lambda,
                     model_func,
                     parameters = NULL,
                     target = "Y") 
{
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
  
  # Weighted LS across E1+E2
  f_LS <- (sse_G1 + sse_G2) / (n1 + n2)
  
  # MSE in each environment individually
  mse_G1 <- sse_G1 / n1
  mse_G2 <- sse_G2 / n2
  
  # CD: absolute difference in MSE
  f_CD <- abs(mse_G1 - mse_G2)
  
  # Final objective
  loss_value <- (1 - lambda)*f_LS + lambda*f_CD
  return(loss_value)
}


#' @title Train a Causal Model with Optional Cross-Validation
#'
#' @description
#' Optimizes model parameters to minimize the Causal Dantzig objective:
#' a combination of least-squares error (LS) and environment discrepancy (CD).
#'
#' By default, this uses \code{\link{obj_func}} as the loss. If \code{hidden_sizes} is
#' \code{NULL} or empty, we use a linear model. Otherwise, we parse the weights as a feed-forward
#' neural network with sigmoid activations in hidden layers and a linear output.
#'
#' An optional K–fold cross validation can be performed to evaluate the training performance,
#' splitting each environment's data into K folds and training on the union of training folds,
#' evaluating on the union of validation folds.
#'
#' @param data_G1 A data frame for environment 1 (containing \code{target}).
#' @param data_G2 A data frame for environment 2 (containing \code{target}).
#' @param lambda A numeric in [0,1], controlling the trade-off between LS and CD.
#' @param target A character naming the response variable in \code{data_G1} and \code{data_G2}.
#' @param loss_func The objective function; defaults to \code{\link{obj_func}}.
#' @param model_func If \code{NULL}, picks either linear or neural from \code{\link{define_functional_forms}}.
#' @param hidden_sizes An integer vector specifying the neurons in each hidden layer (for a neural net).
#'   If empty or \code{NULL}, it's linear.
#' @param method The optimization method passed to \code{\link[stats]{optim}} (default "BFGS").
#' @param verbose Logical; if \code{TRUE}, prints diagnostic messages.
#' @param cv_folds Integer; if > 1, performs K-fold cross validation on each environment's data.
#' @param ... Additional arguments to \code{optim}.
#'
#' @return A list with fields:
#' \describe{
#'   \item{params}{The optimized parameter vector for the entire training data.}
#'   \item{model_func}{The function used for prediction (linear or neural network).}
#'   \item{hidden_sizes}{Hidden layer configuration (if any).}
#'   \item{target}{Name of the target variable.}
#'   \item{cv_performance}{Average RMSE across folds (or NA if \code{cv_folds=1}).}
#'   \item{cv_rmse}{A numeric vector of fold-specific RMSE values (or NA if \code{cv_folds=1}).}
#' }
#'
#' @seealso \code{\link{obj_func}}, \code{\link{evaluate_causal_model}}
#'
#' @examples
#' \dontrun{
#' # Suppose data_G1 and data_G2 each have "Y" as the target,
#' # and a bunch of numeric columns for X1..Xn. We'll do 5-fold CV:
#' out <- train_causal(data_G1, data_G2, lambda=0.3, target="Y", cv_folds=5)
#' print(out$cv_performance)
#' # Then evaluate on a new test set with evaluate_causal_model(out, test_data)
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
                         ...) 
{
  # Identify feature columns (everything but target)
  feature_names <- setdiff(names(data_G1), target)
  num_features <- length(feature_names)
  
  if (verbose) {
    message("Starting training of a causal model...")
    message("Target variable: ", target)
    message("Predictor variables: ", paste(feature_names, collapse = ", "))
    message("Number of features: ", num_features)
  }
  
  # Decide linear vs. neural based on hidden_sizes
  if (is.null(hidden_sizes) || length(hidden_sizes) == 0) {
    # linear
    if (is.null(model_func)) {
      model_func <- define_functional_forms()$linear
    }
    num_params <- num_features + 1
    if (verbose) {
      message("Training a linear model with ", num_params, " total parameters.")
    }
  } else {
    # neural
    if (is.null(model_func)) {
      model_func <- define_functional_forms()$neural_network
    }
    # Count parameters for multi-layer net
    total_p <- 0
    prev_size <- num_features
    for (hs in hidden_sizes) {
      # each hidden layer => (prev_size * hs) + hs (bias)
      total_p <- total_p + prev_size*hs + hs
      prev_size <- hs
    }
    # final output layer => prev_size->1 => (prev_size + 1)
    total_p <- total_p + prev_size*1 + 1
    num_params <- total_p
    
    if (verbose) {
      message("Training a neural net with hidden layers: ", paste(hidden_sizes, collapse=", "))
      message("Total parameters: ", num_params)
    }
  }
  
  # Make initial weights
  initial_weights <- runif(num_params, min=-0.5, max=0.5)
  if (verbose) {
    message("Initial weights generated. Starting optimization with method=", method)
  }
  
  # Wrap the user-chosen (or default) objective
  objective_wrapper <- function(w) {
    loss_func(weights = w,
              data_G1 = data_G1,
              data_G2 = data_G2,
              lambda  = lambda,
              model_func = model_func,
              parameters = list(hidden_sizes = hidden_sizes),
              target = target)
  }
  
  # Fit on entire data
  start_time <- Sys.time()
  opt_res <- stats::optim(par=initial_weights, fn=objective_wrapper,
                          method=method, ...)
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units="secs"))
  if (verbose) {
    message("Training finished in ", round(elapsed,2), " seconds; convergence code=", opt_res$convergence)
  }
  
  cv_performance <- NA
  cv_rmse <- NA
  
  # Cross-Validation if requested
  if (cv_folds > 1) {
    if (verbose) {
      message("Performing ", cv_folds, "-fold cross-validation on each environment...")
    }
    folds_G1 <- sample(rep(seq_len(cv_folds), length.out=nrow(data_G1)))
    folds_G2 <- sample(rep(seq_len(cv_folds), length.out=nrow(data_G2)))
    fold_rmses <- numeric(cv_folds)
    
    for (k in seq_len(cv_folds)) {
      train_part_G1 <- data_G1[folds_G1 != k, , drop=FALSE]
      valid_part_G1 <- data_G1[folds_G1 == k, , drop=FALSE]
      
      train_part_G2 <- data_G2[folds_G2 != k, , drop=FALSE]
      valid_part_G2 <- data_G2[folds_G2 == k, , drop=FALSE]
      
      # local objective for this fold
      local_obj <- function(w) {
        loss_func(weights = w,
                  data_G1 = train_part_G1,
                  data_G2 = train_part_G2,
                  lambda  = lambda,
                  model_func = model_func,
                  parameters = list(hidden_sizes=hidden_sizes),
                  target = target)
      }
      
      w_init <- runif(num_params, min=-0.5, max=0.5)
      opt_cv <- stats::optim(par=w_init, fn=local_obj, method=method, ...)
      
      # Evaluate on the union of valid_part_G1 and valid_part_G2
      fold_model <- list(
        params = opt_cv$par,
        model_func = model_func,
        hidden_sizes = hidden_sizes,
        target = target
      )
      combined_valid <- rbind(valid_part_G1, valid_part_G2)
      fold_eval <- evaluate_causal_model(fold_model, combined_valid)
      fold_rmses[k] <- fold_eval$RMSE
      
      if (verbose) {
        message(sprintf("  CV Fold %d => RMSE=%.4f", k, fold_rmses[k]))
      }
    }
    cv_performance <- mean(fold_rmses)
    cv_rmse       <- fold_rmses
    if (verbose) {
      message("Average cross-validation RMSE:", round(cv_performance,4))
    }
  }
  
  # Final return
  model_out <- list(
    params = opt_res$par,
    model_func = model_func,
    hidden_sizes = hidden_sizes,
    target = target,
    cv_performance = cv_performance,
    cv_rmse = cv_rmse
  )
  return(model_out)
}


#' @title Evaluate a Causal Model's Performance on a Test Set
#'
#' @description
#' Given a trained model (from \code{\link{train_causal}}) and a test data frame
#' that has the same target column name, computes MSE and RMSE for predictions.
#'
#' @param model A list representing the trained model, containing at least:
#'   \itemize{
#'     \item \code{params}: the numeric vector of parameters,
#'     \item \code{model_func}: the function used for predictions,
#'     \item \code{target}: the name of the target column.
#'   }
#' @param test_data A data frame containing the features and the target column.
#'
#' @return A list with:
#' \item{MSE}{Mean squared error on \code{test_data}.}
#' \item{RMSE}{Square root of that MSE.}
#' \item{predictions}{A numeric vector of length \code{nrow(test_data)}, the model's predictions.}
#'
#' @seealso \code{\link{train_causal}}
#'
#' @examples
#' \dontrun{
#'   # If 'model_linear' was returned by train_causal() with target="Y"
#'   # and test_data is a data frame that includes Y and the same features:
#'   res <- evaluate_causal_model(model_linear, test_data)
#'   cat("Test MSE:", res$MSE)
#' }
#'
#' @export
evaluate_causal_model <- function(model, test_data) {
  # check for target
  if (is.null(model$target)) {
    stop("The model object must include 'target' specifying the name of the response variable.")
  }
  target <- model$target
  
  # check that test_data has that column
  if (!(target %in% names(test_data))) {
    stop("Test data does not contain the target variable: '", target, "'")
  }
  
  # separate X vs Y
  Y_test <- test_data[[target]]
  X_test <- test_data[, setdiff(names(test_data), target), drop=FALSE]
  
  # pick predictor function
  pred_fn <- NULL
  if (!is.null(model$model_func) && is.function(model$model_func)) {
    pred_fn <- model$model_func
  } else if (!is.null(model$predictor) && is.function(model$predictor)) {
    pred_fn <- model$predictor
  } else {
    stop("No valid predictor found in 'model'. Expecting 'model_func' or 'predictor'.")
  }
  
  # decide if we have additional parameters (hidden_sizes, etc.)
  # We'll unify them as 'extra_params'
  extra_params <- NULL
  if (!is.null(model$hidden_sizes)) {
    extra_params <- list(hidden_sizes=model$hidden_sizes)
  }
  
  # do predictions
  if (!is.null(extra_params)) {
    preds <- pred_fn(model$params, X_test, parameters=extra_params)
  } else {
    preds <- pred_fn(model$params, X_test)
  }
  
  # compute metrics
  mse_val <- mean((Y_test - preds)^2)
  rmse_val <- sqrt(mse_val)
  
  list(MSE=mse_val, RMSE=rmse_val, predictions=preds)
}
