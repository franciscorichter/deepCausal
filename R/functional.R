#' @title Define Model Functional Forms (Linear and Neural Network)
#'
#' @description
#' Returns a list of two functions for use in a multi-environment causal inference setup:
#' \enumerate{
#'   \item \strong{linear}: A linear model that computes a simple linear combination of features.
#'   \item \strong{neural\_network}: A feed-forward neural network model with optional hidden layers,
#'     using sigmoid activation for hidden layers and a linear output layer.
#' }
#'
#' @details
#' The user must ensure \code{X} (the feature matrix/data frame) does \strong{not} include the target column.
#' Each function has the signature \code{function(weights, X, parameters = NULL)}, returning a numeric vector
#' of predictions, one per row of \code{X}.
#'
#' - \strong{linear(weights, X, parameters)}:
#'   \itemize{
#'     \item \code{weights}: numeric vector whose first element is interpreted as the intercept;
#'       subsequent elements are slope coefficients.
#'     \item \code{X}: a data frame or matrix of purely numeric features. The function internally checks
#'       for numeric columns only.
#'     \item The return is \eqn{\hat{y} = \beta_0 + X \times \boldsymbol{\beta}} (a length-\code{nrow(X)} vector).
#'   }
#' - \strong{neural\_network(weights, X, parameters)}:
#'   \itemize{
#'     \item If \code{parameters$hidden\_sizes} is empty, it does a single "logistic" step
#'       (\emph{i.e.} intercept + linear + sigmoid).
#'     \item Otherwise, it expects a flattened \code{weights} vector arranged layer-by-layer,
#'       with hidden layers using a sigmoid activation and a final linear output layer.
#'     \item Returns a numeric vector of length \code{nrow(X)} with the final network output.
#'   }
#'
#' @return A named list with two elements: \code{$linear} and \code{$neural\_network}.
#' 
#' @examples
#' forms <- define_functional_forms()
#' # Suppose 'X' is a numeric matrix of features, and 'w' is a vector of parameters
#' preds_linear <- forms$linear(w, X)
#' preds_net    <- forms$neural_network(w, X, parameters = list(hidden_sizes = c(5)))
#'
#' @export
define_functional_forms <- function() {
  
  check_is_numeric_matrix <- function(X) {
    if (!is.null(dim(X))) {
      # if X is a data frame or matrix, ensure numeric
      if (any(sapply(X, function(col) !is.numeric(col)))) {
        stop("All columns in 'X' must be numeric. Please remove or encode any factors/characters.")
      }
    } else {
      stop("'X' must be a matrix or data frame of numeric features.")
    }
  }
  
  linear_func <- function(weights, X, parameters = NULL) {
    # Basic checks
    check_is_numeric_matrix(X)
    
    intercept <- weights[1]
    # slopes must match ncol(X)
    n_feats <- ncol(X)
    coefs <- weights[2:(n_feats + 1)]
    
    # Do the linear combination
    out <- intercept + as.matrix(X) %*% coefs
    return(as.vector(out))
  }
  
  neural_func <- function(weights, X, parameters = list(hidden_sizes = numeric(0))) {
    # Basic checks
    check_is_numeric_matrix(X)
    
    # We'll define a small feed-forward net with optional hidden layers
    num_features <- ncol(X)
    hidden_sizes <- parameters$hidden_sizes
    num_layers <- length(hidden_sizes)
    
    A <- as.matrix(X)  # working activation matrix
    
    if (num_layers == 0) {
      # single logistic step
      intercept <- weights[1]
      coefs <- weights[2:(num_features + 1)]
      z <- intercept + A %*% coefs
      return(1 / (1 + exp(-z)))  # logistic
    }
    
    # Parse the layer-by-layer weights
    idx <- 1
    W <- list()
    b <- list()
    
    for (layer_i in seq_len(num_layers)) {
      if (layer_i == 1) {
        # first layer => [num_features, hidden_sizes[1]]
        layer_size <- num_features * hidden_sizes[layer_i]
        W[[layer_i]] <- matrix(weights[idx:(idx + layer_size - 1)], 
                               nrow = num_features, 
                               ncol = hidden_sizes[layer_i])
        idx <- idx + layer_size
      } else {
        # subsequent layer => [hidden_sizes[i-1], hidden_sizes[i]]
        layer_size <- hidden_sizes[layer_i - 1] * hidden_sizes[layer_i]
        W[[layer_i]] <- matrix(weights[idx:(idx + layer_size - 1)], 
                               nrow = hidden_sizes[layer_i - 1], 
                               ncol = hidden_sizes[layer_i])
        idx <- idx + layer_size
      }
      # biases for this layer
      bias_size <- hidden_sizes[layer_i]
      b[[layer_i]] <- weights[idx:(idx + bias_size - 1)]
      idx <- idx + bias_size
    }
    
    # final layer => from hidden_sizes[num_layers] to 1
    out_size <- hidden_sizes[num_layers]
    W[[num_layers + 1]] <- matrix(weights[idx:(idx + out_size - 1)], 
                                  nrow = out_size, ncol = 1)
    idx <- idx + out_size
    b[[num_layers + 1]] <- weights[idx]
    
    # forward pass
    for (layer_i in seq_len(num_layers)) {
      # Z = A %*% W + b
      # reshape b to [1, hidden_sizes[layer_i]] for broadcast
      bs <- matrix(b[[layer_i]], nrow = 1, ncol = hidden_sizes[layer_i])
      Z <- A %*% W[[layer_i]] + 
        matrix(bs, nrow = nrow(A), ncol = hidden_sizes[layer_i], byrow = TRUE)
      # sigmoid activation
      A <- 1 / (1 + exp(-Z))
    }
    
    # final linear layer
    # A => shape [nrow(A), out_size]
    # W[[num_layers+1]] => shape [out_size, 1]
    # b[[num_layers+1]] => single numeric
    Z_final <- A %*% W[[num_layers+1]] + b[[num_layers+1]]
    return(as.vector(Z_final))
  }
  
  list(
    linear = linear_func,
    neural_network = neural_func
  )
}

