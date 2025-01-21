# Experiment runner with bootstrapping and summary statistics
run_experiment_bootstrapping <- function(selected_system, n_train = 1000, n_test = 1000, lambdas = seq(0, 1, by = 0.25), 
                                         nn_params = list(hidden_sizes = c(3, 3)), n_reps = 10) {
  
  cat("Starting data simulation...\n")
  
  # Simulate testing data
  test_data <- selected_system$data_func(
    n_test, environment = list(mu_A1 = 3, sigma_A1 = 1.0, mu_A2 = 3, sigma_A2 = 1.0)
  )
  
  cat("Starting model training and evaluation with bootstrapping...\n")
  
  mse_results <- data.frame(Lambda = numeric(), MSE = numeric(), Sample = character(), Functional_Form = character(), Rep = numeric())
  
  for (lambda in lambdas) {
    cat("\nLambda =", lambda, "\n")
    
    for (rep in 1:n_reps) {
      # Simulate training data for each replication
      train_data_G1 <- selected_system$data_func(
        n_train, environment = list(mu_A1 = 0, sigma_A1 = 0.5, mu_A2 = 0, sigma_A2 = 0.5)
      )
      train_data_G2 <- selected_system$data_func(
        n_train, environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
      )
      
      # Train linear model
      linear_params <- train_predictive_models(
        data_G1 = train_data_G1,
        data_G2 = train_data_G2,
        nn_params = nn_params,
        model_type = "linear",
        lambda = lambda
      )
      
      mse_linear_out_sample <- evaluate_models(
        models = linear_params,
        data = test_data,
        nn_params = nn_params,
        model_type = "linear"
      )
      
      # Train neural network model
      NN_params <- train_predictive_models(
        data_G1 = train_data_G1,
        data_G2 = train_data_G2,
        nn_params = nn_params,
        model_type = "neural_network",
        lambda = lambda
      )
      
      mse_nn_out_sample <- evaluate_models(
        models = NN_params,
        data = test_data,
        nn_params = nn_params,
        model_type = "neural_network"
      )
      
      # Collect results for linear model
      mse_results <- rbind(
        mse_results,
        data.frame(Lambda = lambda, MSE = mse_linear_out_sample, Sample = "Out-of-sample", Functional_Form = "Linear", Rep = rep)
      )
      
      # Collect results for neural network model
      mse_results <- rbind(
        mse_results,
        data.frame(Lambda = lambda, MSE = mse_nn_out_sample, Sample = "Out-of-sample", Functional_Form = "Neural Network", Rep = rep)
      )
      
      cat("Lambda =", lambda, ", Rep =", rep, ": Out-of-sample MSE (Linear) =", mse_linear_out_sample, 
          ", Out-of-sample MSE (Neural Network) =", mse_nn_out_sample, "\n")
    }
  }
  
  # Calculate mean and SD of MSE for each Lambda and Functional_Form
  summary_results <- mse_results %>%
    group_by(Lambda, Functional_Form, Sample) %>%
    summarise(
      Mean_MSE = mean(MSE, na.rm = TRUE),
      SD_MSE = sd(MSE, na.rm = TRUE),
      SE_MSE = SD_MSE / sqrt(n_reps),
      .groups = "drop"
    )
  
  # Print summary statistics
  print(summary_results)
  
  # Plot results with points and boxplots
  p <- ggplot(mse_results, aes(x = factor(Lambda), y = MSE, color = Functional_Form)) +
    geom_boxplot(aes(fill = Functional_Form), alpha = 0.4, outlier.shape = NA) +
    geom_jitter(aes(shape = Functional_Form), position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +
    labs(title = "Out-of-sample MSE with Bootstrapping", x = "Lambda", y = "Mean Squared Error (MSE)", 
         color = "Functional Form", fill = "Functional Form", shape = "Functional Form") +
    theme_minimal()
  
  print(p)
  
  return(list(All_Results = mse_results, Summary_Results = summary_results))
}

# Example usage (assuming the required functions and data are properly defined):
results <- run_experiment_bootstrapping(selected_system, n_train = 1000, n_reps = 10)

