library(deepCausal)
library(ggplot2)
library(dplyr)
library(foreach)
library(doParallel)

# Define lambda values and number of repetitions
lambdas <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
repetitions <- 100

# Create a grid of jobs (each row is one experiment instance)
jobs <- expand.grid(Lambda = lambdas, Repetition = 1:repetitions)

# Register a parallel backend using available cores (reserve one for OS)
numCores <- parallel::detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Select a system for simulation (using the first one as an example)
systems <- define_systems()
selected_system <- systems[[1]]

# Run experiments in parallel over all jobs
results <- foreach(i = 1:nrow(jobs), .combine = rbind,
                   .packages = c("deepCausal", "dplyr", "ggplot2")) %dopar% {
                     lambda <- jobs$Lambda[i]
                     rep <- jobs$Repetition[i]
                     
                     # Simulate training data for Environment 1 and Environment 2
                     train_data_G1 <- selected_system$data_func(
                       n = 100,
                       environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1)
                     )
                     train_data_G2 <- selected_system$data_func(
                       n = 100,
                       environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
                     )
                     
                     # Train a linear model (no hidden_sizes specified)
                     model_linear <- train_causal(
                       data_G1 = train_data_G1,
                       data_G2 = train_data_G2,
                       lambda = lambda,
                       target = "Y",
                       verbose = FALSE
                     )
                     
                     # Train a neural network model (with hidden_sizes provided)
                     model_nn <- train_causal(
                       data_G1 = train_data_G1,
                       data_G2 = train_data_G2,
                       lambda = lambda,
                       target = "Y",
                       hidden_sizes = c(3, 3),
                       verbose = FALSE
                     )
                     
                     # Simulate test data (from a different environment)
                     test_data <- selected_system$data_func(
                       n = 100,
                       environment = list(mu_A1 = 3, sigma_A1 = 1, mu_A2 = 3, sigma_A2 = 1)
                     )
                     
                     # Evaluate both models on test data
                     perf_linear <- evaluate_causal_model(model_linear, test_data)
                     perf_nn <- evaluate_causal_model(model_nn, test_data)
                     
                     # Return a small data frame with one row per model type
                     data.frame(
                       Lambda = lambda,
                       RMSE = perf_linear$RMSE,
                       Model = "Linear",
                       Repetition = rep,
                       stringsAsFactors = FALSE
                     ) %>%
                       rbind(data.frame(
                         Lambda = lambda,
                         RMSE = perf_nn$RMSE,
                         Model = "Neural Network",
                         Repetition = rep,
                         stringsAsFactors = FALSE
                       ))
                   }

# Stop the parallel cluster
stopCluster(cl)

# Plot boxplots of RMSE across lambda values for both model types
p <- ggplot(results, aes(x = factor(Lambda), y = RMSE, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(title = "Test RMSE Distribution Across Lambda Values",
       x = "Lambda",
       y = "Test RMSE",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p+theme_bw())
