library(deepCausal)
library(ggplot2)
library(dplyr)

# Load system definitions
systems <- define_systems()

# Experiment runner with multiple repetitions per lambda
run_experiment <- function(selected_system, 
                           n_train = 100, 
                           n_test = 100, 
                           lambdas = seq(0, 1, by = 0.2), 
                           nn_params = list(hidden_sizes = c(3, 3)), 
                           repetitions = 10) {  # Number of repetitions per lambda
  
  cat("Starting model training and evaluation...\n")
  
  mse_results <- data.frame(Lambda = numeric(), MSE = numeric(), Functional_Form = character(), Repetition = integer())
  
  for (lambda in lambdas) {
    cat("\nLambda =", lambda, "\n")
    
    for (rep in 1:repetitions) {
      cat("Repetition", rep, "for lambda =", lambda, "\n")
      
      # Generate fresh training and testing data for each repetition
      train_data_G1 <- selected_system$data_func(n_train, environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1))
      train_data_G2 <- selected_system$data_func(n_train, environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5))
      test_data <- selected_system$data_func(n_test, environment = list(mu_A1 = 3, sigma_A1 = 1.0, mu_A2 = 3, sigma_A2 = 1.0))
      
      # Train Linear Model
      linear_params <- train_predictive_models(
        data_G1 = train_data_G1,
        data_G2 = train_data_G2,
        nn_params = nn_params,
        model_type = "linear",
        lambda = lambda
      )
      
      # Evaluate Linear Model (Only Out-of-Sample MSE)
      mse_linear_out_sample <- evaluate_models(models = linear_params, data = test_data, model_type = "linear")
      
      # Train Neural Network Model
      nn_params_list <- train_predictive_models(
        data_G1 = train_data_G1,
        data_G2 = train_data_G2,
        nn_params = nn_params,
        model_type = "neural_network",
        lambda = lambda
      )
      
      # Evaluate Neural Network Model (Only Out-of-Sample MSE)
      mse_nn_out_sample <- evaluate_models(models = nn_params_list, data = test_data, nn_params = nn_params, model_type = "neural_network")
      
      # Collect results
      mse_results <- rbind(
        mse_results,
        data.frame(Lambda = lambda, MSE = mse_linear_out_sample, Functional_Form = "Linear", Repetition = rep),
        data.frame(Lambda = lambda, MSE = mse_nn_out_sample, Functional_Form = "Neural Network", Repetition = rep)
      )
    }
  }
  
  # Plot results with boxplots
  p <- ggplot(mse_results, aes(x = factor(Lambda), y = MSE, fill = Functional_Form)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(title = "MSE Distribution Across Lambda Values", x = "Lambda", y = "Mean Squared Error (MSE)", fill = "Functional Form") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  return(mse_results)
}

# Select system (e.g., System 1)
selected_system <- systems[[1]]

# Run the experiment with multiple repetitions per lambda
results <- run_experiment(selected_system, n_train = 100, repetitions = 10)

