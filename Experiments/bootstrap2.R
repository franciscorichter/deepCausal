library(dplyr)

# Experiment runner with in-sample and out-of-sample results in the list
run_experiment_bootstrapping <- function(
    selected_system, 
    n_train = 1000, 
    n_test = 1000, 
    lambdas = seq(0, 0.2, by = 0.025), 
    nn_params = list(hidden_sizes = c(2, 2)), 
    n_reps = 100
) {
  cat("Starting data simulation...\n")
  cat("Starting model training and evaluation with bootstrapping...\n")
  
  # Data frame to collect all MSE results
  mse_results <- data.frame(
    Lambda           = numeric(),
    MSE              = numeric(),
    Sample           = character(),
    Functional_Form  = character(),
    Rep              = numeric()
  )
  
  # Loop over lambdas
  for (lambda in lambdas) {
    cat("\nLambda =", lambda, "\n")
    
    # Repeat the experiment n_reps times
    for (rep in seq_len(n_reps)) {
      
      # Simulate training and testing data
      train_data_G1 <- selected_system$data_func(
        n_train, environment = list(mu_A1 = 0, sigma_A1 = 0, mu_A2 = 0, sigma_A2 = 0)
      )
      train_data_G2 <- selected_system$data_func(
        n_train, environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
      )
      test_data <- selected_system$data_func(
        n_test, environment = list(mu_A1 = 3, sigma_A1 = 1.0, mu_A2 = 3, sigma_A2 = 1.0)
      )
      
      # Train linear model
      linear_params <- train_predictive_models(
        data_G1     = train_data_G1,
        data_G2     = train_data_G2,
        nn_params   = nn_params,
        model_type  = "linear",
        lambda      = lambda
      )
      
      # Evaluate linear model (in-sample and out-of-sample)
      mse_linear_in_sample <- evaluate_models(
        models      = linear_params,
        data        = rbind(train_data_G1, train_data_G2),
        nn_params   = nn_params,
        model_type  = "linear"
      )
      mse_linear_out_sample <- evaluate_models(
        models      = linear_params,
        data        = test_data,
        nn_params   = nn_params,
        model_type  = "linear"
      )
      
      # Train neural network model
      NN_params <- train_predictive_models(
        data_G1     = train_data_G1,
        data_G2     = train_data_G2,
        nn_params   = nn_params,
        model_type  = "neural_network",
        lambda      = lambda
      )
      
      # Evaluate neural network model (in-sample and out-of-sample)
      mse_nn_in_sample <- evaluate_models(
        models      = NN_params,
        data        = rbind(train_data_G1, train_data_G2),
        nn_params   = nn_params,
        model_type  = "neural_network"
      )
      mse_nn_out_sample <- evaluate_models(
        models      = NN_params,
        data        = test_data,
        nn_params   = nn_params,
        model_type  = "neural_network"
      )
      
      # Collect results
      mse_results <- rbind(
        mse_results,
        data.frame(
          Lambda            = lambda, 
          MSE               = mse_linear_in_sample, 
          Sample            = "In-sample", 
          Functional_Form   = "Linear", 
          Rep               = rep
        ),
        data.frame(
          Lambda            = lambda, 
          MSE               = mse_linear_out_sample, 
          Sample            = "Out-of-sample", 
          Functional_Form   = "Linear", 
          Rep               = rep
        ),
        data.frame(
          Lambda            = lambda, 
          MSE               = mse_nn_in_sample, 
          Sample            = "In-sample", 
          Functional_Form   = "Neural Network", 
          Rep               = rep
        ),
        data.frame(
          Lambda            = lambda, 
          MSE               = mse_nn_out_sample, 
          Sample            = "Out-of-sample", 
          Functional_Form   = "Neural Network", 
          Rep               = rep
        )
      )
      
      # Print progress for debugging/logging
      cat(
        "Lambda =", lambda, ", Rep =", rep, 
        ": In-sample MSE (Linear) =", mse_linear_in_sample, 
        ", Out-of-sample MSE (Linear) =", mse_linear_out_sample, 
        ", In-sample MSE (Neural Network) =", mse_nn_in_sample, 
        ", Out-of-sample MSE (Neural Network) =", mse_nn_out_sample, "\n"
      )
    }
  }
  
  # Calculate summary statistics
  summary_results <- mse_results %>%
    group_by(Lambda, Functional_Form, Sample) %>%
    summarise(
      Mean_MSE = mean(MSE, na.rm = TRUE),
      SD_MSE   = sd(MSE, na.rm = TRUE),
      SE_MSE   = SD_MSE / sqrt(n_reps),
      .groups  = "drop"
    )
  
  # Return the raw results plus summary stats
  return(list(
    All_Results    = mse_results, 
    Summary_Results = summary_results
  ))
}

selected_system <- systems[[1]]

results1.2 <- run_experiment_bootstrapping(
  selected_system, 
  lambdas = seq(0, 1, by = 0.1), 
  n_train = 50, 
  n_test = 100, 
  n_reps = 200,nn_params = list(hidden_sizes = c(3, 2))
)



all_results <- results1.2$All_Results

# Subset for in-sample only:
in_sample_results <- subset(all_results, Sample == "In-sample")

# Or out-of-sample only:
out_sample_results <- subset(all_results, Sample == "Out-of-sample")



library(ggplot2)

ggplot(in_sample_results, aes(x = factor(Lambda), y = MSE, color = Functional_Form)) +
  geom_boxplot(aes(fill = Functional_Form), alpha = 0.4, outlier.shape = NA) +
  
  # Mean line
  stat_summary(
    fun = mean, 
    geom = "line", 
    aes(group = Functional_Form), 
    size = 1
  ) +
  
  # Dashed confidence intervals around the mean
  stat_summary(
    fun.data = mean_cl_normal,   # or use a custom function
    geom = "errorbar",
    aes(group = Functional_Form),
    width = 0.2,                 
    linetype = "dashed"
  ) +
  
  labs(
    title = "In-sample MSE with Bootstrapping",
    x = "Lambda", 
    y = "Mean Squared Error (MSE)",
    color = "Functional Form", 
    fill  = "Functional Form"
  ) +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, 5))




