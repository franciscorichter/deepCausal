###################################################################
# COMPLETE STANDALONE R SCRIPT
# (1) System definitions
# (2) Define functional forms (linear / neural network)
# (3) Objective function (obj_func) for multi-environment MSE + CD
# (4) train_causal for model fitting
# (5) evaluate_causal_model for test evaluation
# (6) run_experiment to tie it all together
# Then we do a quick demonstration.
###################################################################

##########################
# 1) Libraries
##########################
# Make sure these are installed:
# install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)

##########################
# 2) Define Systems for Simulation
##########################
# For demonstration, we define just ONE system from your "define_systems()"
# as an example: "Non-linear System with External Actions," 
# with external actions mu_A1, sigma_A1, mu_A2, sigma_A2 controlling distribution shifts.

define_systems <- function() {
  systems <- list(
    # System 1: Non-linear System with External Actions
    list(
      name = "System1_Nonlinear_External_Actions",
      description = paste(
        "X1 = epsilon1 + A1;",
        "Y  = sin(X1) + epsilonY;",
        "X2 = Y^2 + A2 + epsilon2."
      ),
      data_func = function(n, environment = c(mu_A1=0, sigma_A1=1, mu_A2=0, sigma_A2=1)) {
        epsilon1 <- rnorm(n)
        epsilonY <- rnorm(n)
        epsilon2 <- rnorm(n)
        
        A1 <- rnorm(n, mean = environment["mu_A1"], sd = environment["sigma_A1"])
        A2 <- rnorm(n, mean = environment["mu_A2"], sd = environment["sigma_A2"])
        
        X1 <- epsilon1 + A1
        Y  <- sin(X1) + epsilonY
        X2 <- Y^2 + A2 + epsilon2
        
        data.frame(X1, X2, Y)
      }
    )
  )
  return(systems)
}

##########################
# 3) Define Functional Forms (Linear, Neural)
##########################

# We unify these into a single function that returns two sub-functions
# (a) linear
# (b) neural_network

define_functional_forms <- function() {
  
  # Safety check function
  check_is_numeric_matrix <- function(X) {
    if (!is.null(dim(X))) {
      # if X is data frame or matrix, ensure numeric
      if (any(sapply(X, function(col) !is.numeric(col)))) {
        stop("All columns in 'X' must be numeric. Found factor/char or non-numeric.")
      }
    } else {
      stop("'X' must be a matrix or data frame of numeric features.")
    }
  }
  
  linear_func <- function(weights, X, parameters = NULL) {
    check_is_numeric_matrix(X)
    intercept <- weights[1]
    n_feats <- ncol(X)
    coefs <- weights[2:(n_feats + 1)]
    return(as.vector(intercept + as.matrix(X) %*% coefs))
  }
  
  neural_func <- function(weights, X, parameters = list(hidden_sizes = numeric(0))) {
    check_is_numeric_matrix(X)
    A <- as.matrix(X)
    hidden_sizes <- parameters$hidden_sizes
    num_layers <- length(hidden_sizes)
    num_features <- ncol(X)
    
    if (num_layers == 0) {
      # single logistic layer
      intercept <- weights[1]
      coefs <- weights[2:(num_features+1)]
      z <- intercept + A %*% coefs
      return(1/(1+exp(-z)))
    }
    
    # parse hidden layers
    idx <- 1
    W <- list()
    b <- list()
    
    for (layer_i in seq_len(num_layers)) {
      if (layer_i == 1) {
        layer_size <- num_features*hidden_sizes[layer_i]
        W[[layer_i]] <- matrix(weights[idx:(idx+layer_size-1)], nrow=num_features, ncol=hidden_sizes[layer_i])
        idx <- idx + layer_size
      } else {
        layer_size <- hidden_sizes[layer_i-1]*hidden_sizes[layer_i]
        W[[layer_i]] <- matrix(weights[idx:(idx+layer_size-1)], nrow=hidden_sizes[layer_i-1], ncol=hidden_sizes[layer_i])
        idx <- idx + layer_size
      }
      # biases
      bias_size <- hidden_sizes[layer_i]
      b[[layer_i]] <- weights[idx:(idx + bias_size -1)]
      idx <- idx + bias_size
    }
    
    # final layer => from hidden_sizes[num_layers] -> 1
    out_size <- hidden_sizes[num_layers]
    W[[num_layers+1]] <- matrix(weights[idx:(idx+out_size-1)], nrow=out_size, ncol=1)
    idx <- idx + out_size
    b[[num_layers+1]] <- weights[idx]
    
    # forward pass
    for (layer_i in seq_len(num_layers)) {
      # Z = A%*%W + b
      Wmat <- W[[layer_i]]
      # broadcast b
      bvec <- b[[layer_i]]
      Z <- A %*% Wmat + matrix(bvec, nrow=nrow(A), ncol=hidden_sizes[layer_i], byrow=TRUE)
      # sigmoid
      A <- 1/(1+exp(-Z))
    }
    # final linear layer
    W_out <- W[[num_layers+1]]
    b_out <- b[[num_layers+1]]
    Z_final <- A %*% W_out + b_out
    return(as.vector(Z_final))
  }
  
  list(
    linear = linear_func,
    neural_network = neural_func
  )
}

##########################
# 4) Objective: (1-lambda)*LS + lambda*CD  (two env)
##########################

obj_func <- function(weights,
                     data_G1,
                     data_G2,
                     lambda,
                     model_func,
                     parameters = NULL,
                     target = "Y") {
  Y1 <- data_G1[[target]]
  X_G1 <- data_G1[, setdiff(names(data_G1), target), drop=FALSE]
  n1 <- length(Y1)
  
  Y2 <- data_G2[[target]]
  X_G2 <- data_G2[, setdiff(names(data_G2), target), drop=FALSE]
  n2 <- length(Y2)
  
  pred1 <- model_func(weights, X_G1, parameters)
  pred2 <- model_func(weights, X_G2, parameters)
  
  sse1 <- sum((Y1 - pred1)^2)
  sse2 <- sum((Y2 - pred2)^2)
  
  # Weighted LS
  f_LS <- (sse1 + sse2)/(n1+n2)
  
  # environment-specific MSE
  mse1 <- sse1/n1
  mse2 <- sse2/n2
  f_CD <- abs(mse1 - mse2)
  
  return((1-lambda)*f_LS + lambda*f_CD)
}

##########################
# 5) train_causal with optional CV
##########################

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
  # figure out how many features
  feat_names <- setdiff(names(data_G1), target)
  num_features <- length(feat_names)
  
  if (verbose) {
    message("--- train_causal ---")
    message("Target:", target)
    message("Features:", paste(feat_names, collapse=", "))
    message("Num features:", num_features, ", lambda=", lambda)
  }
  
  # decide linear vs. neural
  forms <- define_functional_forms()
  if (is.null(hidden_sizes) || length(hidden_sizes) == 0) {
    if (is.null(model_func)) model_func <- forms$linear
    n_params <- num_features + 1
    if (verbose) message("Using linear model => total params:", n_params)
  } else {
    if (is.null(model_func)) model_func <- forms$neural_network
    # compute param count
    total_p <- 0
    prev_size <- num_features
    for (hs in hidden_sizes) {
      total_p <- total_p + prev_size*hs + hs
      prev_size <- hs
    }
    total_p <- total_p + prev_size*1 + 1
    n_params <- total_p
    if (verbose) message("Using neural net => hidden sizes=", paste(hidden_sizes, collapse=","), ", total params=", n_params)
  }
  
  # build objective
  obj_wrap <- function(w) {
    loss_func(weights = w,
              data_G1  = data_G1,
              data_G2  = data_G2,
              lambda   = lambda,
              model_func = model_func,
              parameters= list(hidden_sizes=hidden_sizes),
              target=target)
  }
  
  init_w <- runif(n_params, min=-0.5, max=0.5)
  
  # fit
  if (verbose) message("Starting optimization with method=", method)
  t0 <- Sys.time()
  opt_res <- optim(par=init_w, fn=obj_wrap, method=method, ...)
  t1 <- Sys.time()
  if (verbose) message("Done. Convergence code=", opt_res$convergence, " in", round(as.numeric(difftime(t1,t0,units="secs")),2),"secs")
  
  final_params <- opt_res$par
  
  # cross-validation if folds>1
  cv_perf <- NA
  cv_rmse <- NA
  if (cv_folds>1) {
    if (verbose) message("Performing CV with K=", cv_folds)
    
    folds1 <- sample(rep(seq_len(cv_folds), length.out=nrow(data_G1)))
    folds2 <- sample(rep(seq_len(cv_folds), length.out=nrow(data_G2)))
    fold_rmse <- numeric(cv_folds)
    
    for (k in seq_len(cv_folds)) {
      train1 <- data_G1[folds1!=k, , drop=FALSE]
      valid1 <- data_G1[folds1==k, , drop=FALSE]
      
      train2 <- data_G2[folds2!=k, , drop=FALSE]
      valid2 <- data_G2[folds2==k, , drop=FALSE]
      
      obj_fold <- function(w) {
        loss_func(weights=w,
                  data_G1=train1,
                  data_G2=train2,
                  lambda=lambda,
                  model_func=model_func,
                  parameters=list(hidden_sizes=hidden_sizes),
                  target=target)
      }
      w_init_cv <- runif(n_params, min=-0.5, max=0.5)
      opt_cv <- optim(par=w_init_cv, fn=obj_fold, method=method, ...)
      w_cv <- opt_cv$par
      
      # evaluate on valid1+valid2
      df_valid <- rbind(valid1, valid2)
      # evaluate
      res_eval <- evaluate_causal_model(
        model=list(
          params = w_cv,
          model_func=model_func,
          hidden_sizes=hidden_sizes,
          target=target
        ), 
        test_data=df_valid
      )
      fold_rmse[k] <- res_eval$RMSE
      if (verbose) message("Fold=",k," => RMSE=", round(fold_rmse[k],4))
    }
    cv_perf <- mean(fold_rmse)
    cv_rmse <- fold_rmse
    if (verbose) message("Avg CV RMSE=", round(cv_perf,4))
  }
  
  out <- list(
    params       = final_params,
    model_func   = model_func,
    hidden_sizes = hidden_sizes,
    target       = target,
    cv_performance = cv_perf,
    cv_rmse       = cv_rmse
  )
  return(out)
}

##########################
# 6) Evaluate Causal Model
##########################

evaluate_causal_model <- function(model, test_data) {
  if (is.null(model$target)) stop("model$target is missing.")
  target <- model$target
  if (! target %in% names(test_data)) stop("test_data missing column: ", target)
  
  Y_test <- test_data[[target]]
  X_test <- test_data[, setdiff(names(test_data), target), drop=FALSE]
  
  # pick predictor
  pred_fn <- NULL
  if (!is.null(model$model_func) && is.function(model$model_func)) {
    pred_fn <- model$model_func
  } else if (!is.null(model$predictor) && is.function(model$predictor)) {
    pred_fn <- model$predictor
  } else {
    stop("No valid model_func/predictor in model.")
  }
  
  # build extra params if needed
  extra_params <- NULL
  if (!is.null(model$hidden_sizes)) {
    extra_params <- list(hidden_sizes=model$hidden_sizes)
  }
  
  if (!is.null(extra_params)) {
    preds <- pred_fn(model$params, X_test, parameters=extra_params)
  } else {
    preds <- pred_fn(model$params, X_test)
  }
  
  mse_val <- mean((Y_test - preds)^2)
  rmse_val <- sqrt(mse_val)
  list(MSE=mse_val, RMSE=rmse_val, predictions=preds)
}

##########################
# 7) For convenience, a function to train using "train_causal"
#    but purely for fixed-lambda. We'll call it "train_predictive_models"
##########################

train_predictive_models <- function(data_G1, data_G2, nn_params, model_type, lambda) {
  # Decide: "linear" or "neural_network"
  if (model_type=="linear") {
    out <- train_causal(data_G1, data_G2,
                        lambda=lambda, target="Y",
                        hidden_sizes=NULL, # linear
                        method="BFGS",
                        verbose=FALSE,
                        cv_folds=1)
  } else if (model_type=="neural_network") {
    out <- train_causal(data_G1, data_G2,
                        lambda=lambda, target="Y",
                        hidden_sizes=nn_params$hidden_sizes,
                        method="BFGS",
                        verbose=FALSE,
                        cv_folds=1)
  } else {
    stop("Unknown model_type:", model_type)
  }
  return(list(combined_params=out$params, # for backwards compatibility
              # but also store entire model for easy evaluate
              model_func=out$model_func,
              hidden_sizes=out$hidden_sizes,
              target=out$target))
}

evaluate_models <- function(models, data, nn_params, model_type) {
  # 'models' was a list from train_predictive_models
  # We'll do the same approach as evaluate_causal_model
  # building a minimal 'model' object
  model <- list(
    params       = models$combined_params,
    model_func   = models$model_func,
    hidden_sizes = models$hidden_sizes,
    target       = models$target
  )
  res <- evaluate_causal_model(model, data)
  return(res$MSE)
}

##########################
# 8) The "run_experiment" function
##########################

run_experiment <- function(
    chosen_system,      
    n_train   = 100,  
    n_test    = 100,  
    lambdas   = seq(0,1,by=0.2),
    nn_params = list(hidden_sizes=c(3,3)),
    reps      = 10
) {
  results_df <- data.frame(
    Lambda          = numeric(0),
    MSE             = numeric(0),
    Functional_Form = character(0),
    Repetition      = integer(0),
    stringsAsFactors=FALSE
  )
  
  cat(">>> Starting simulation experiment...\n")
  cat("System Name:", chosen_system$name, "\n")
  
  for (lambda_val in lambdas) {
    cat("\nLambda =", lambda_val, "\n")
    for (rep_i in seq_len(reps)) {
      cat("  Repetition:", rep_i, "...\n")
      
      # Environment 1
      train_data_G1 <- chosen_system$data_func(
        n = n_train,
        environment = c(mu_A1=0, sigma_A1=1, mu_A2=0, sigma_A2=1)
      )
      # Environment 2
      train_data_G2 <- chosen_system$data_func(
        n = n_train,
        environment = c(mu_A1=1, sigma_A1=0.8, mu_A2=2, sigma_A2=0.5)
      )
      # Test environment
      test_data <- chosen_system$data_func(
        n = n_test,
        environment = c(mu_A1=3, sigma_A1=1.0, mu_A2=3, sigma_A2=1.0)
      )
      
      # (a) Train linear
      linear_mod <- train_predictive_models(
        data_G1    = train_data_G1,
        data_G2    = train_data_G2,
        nn_params  = nn_params,
        model_type = "linear",
        lambda     = lambda_val
      )
      # Evaluate on test
      mse_lin <- evaluate_models(
        models    = linear_mod,
        data      = test_data,
        nn_params = nn_params,
        model_type= "linear"
      )
      
      # (b) Train neural
      nn_mod <- train_predictive_models(
        data_G1    = train_data_G1,
        data_G2    = train_data_G2,
        nn_params  = nn_params,
        model_type = "neural_network",
        lambda     = lambda_val
      )
      # Evaluate
      mse_nn <- evaluate_models(
        models    = nn_mod,
        data      = test_data,
        nn_params = nn_params,
        model_type= "neural_network"
      )
      
      # store
      results_df <- rbind(
        results_df,
        data.frame(
          Lambda          = lambda_val,
          MSE             = mse_lin,
          Functional_Form = "Linear",
          Repetition      = rep_i,
          stringsAsFactors=FALSE
        ),
        data.frame(
          Lambda          = lambda_val,
          MSE             = mse_nn,
          Functional_Form = "Neural Network",
          Repetition      = rep_i,
          stringsAsFactors=FALSE
        )
      )
    }
  }
  
  # Plot results
  p <- ggplot(results_df, aes(x=factor(Lambda), y=MSE, fill=Functional_Form)) +
    geom_boxplot(alpha=0.6) +
    labs(
      title=paste("MSE Distribution -", chosen_system$name),
      x="Lambda", y="Test MSE"
    ) +
    theme_bw() +
    theme(legend.position="top")
  
  print(p)
  
  # Summaries
  summary_tab <- results_df %>%
    group_by(Lambda, Functional_Form) %>%
    summarise(
      mean_MSE = mean(MSE),
      sd_MSE   = sd(MSE),
      .groups="drop"
    ) %>%
    arrange(Functional_Form, Lambda)
  
  cat("\n=== Aggregate MSE Summary ===\n")
  print(summary_tab)
  
  return(list(
    raw_results=results_df,
    summary=summary_tab
  ))
}


###################################################################
# 9) Final demonstration (if run directly)
###################################################################
if (sys.nframe()==0) {
  cat("Loading all definitions...\n")
  
  #  (a) Load systems
  syss <- define_systems()
  # pick system 1
  sys1 <- syss[[1]]
  
  #  (b) run
  out <- run_experiment(
    chosen_system=sys1,
    n_train=100,
    n_test=100,
    lambdas=seq(0,0.8,by=0.2),
    nn_params=list(hidden_sizes=c(3,3)),
    reps=50  # fewer reps for quick run
  )
  
  # The function call prints a boxplot and a summary table automatically
  cat("Done with simulation. Use out$raw_results or out$summary as you wish.\n")
}
