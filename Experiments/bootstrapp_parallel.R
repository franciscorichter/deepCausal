library(deepCausal)
library(ggplot2)
library(dplyr)
library(foreach)
library(doParallel)

### Experiment Settings
lambdas <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
repetitions <- 100
cv_folds <- 5

# Create a grid of jobs (each row is one experiment instance)
jobs <- expand.grid(Lambda = lambdas, Repetition = 1:repetitions)

# Register a parallel backend using available cores (reserve one for OS)
numCores <- parallel::detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Record overall start time
overall_start <- Sys.time()

# Select System 1 from pilot_systems (the Modified Non-linear System with External Actions)
systems <- pilot_systems()
selected_system <- systems[[2]]  # System 1

# Run experiments in parallel over all jobs
results <- foreach(i = 1:nrow(jobs), .combine = rbind,
                   .packages = c("deepCausal", "dplyr", "ggplot2")) %dopar% {
                     lambda <- jobs$Lambda[i]
                     rep <- jobs$Repetition[i]
                     
                     # Simulate training data for Environment 1 and Environment 2 for System 1
                     train_data_G1 <- selected_system$data_func(
                       n = 100,
                       environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1)
                     )
                     train_data_G2 <- selected_system$data_func(
                       n = 100,
                       environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
                     )
                     
                     # --- Train Linear Model ---
                     start_time_linear <- Sys.time()
                     model_linear <- train_causal(
                       data_G1 = train_data_G1,
                       data_G2 = train_data_G2,
                       lambda = lambda,
                       target = "Y",
                       cv_folds = cv_folds,
                       verbose = FALSE
                     )
                     end_time_linear <- Sys.time()
                     time_linear <- as.numeric(difftime(end_time_linear, start_time_linear, units = "secs"))
                     cv_lin <- model_linear$cv_performance
                     
                     # --- Train Neural Network Model ---
                     start_time_nn <- Sys.time()
                     model_nn <- train_causal(
                       data_G1 = train_data_G1,
                       data_G2 = train_data_G2,
                       lambda = lambda,
                       target = "Y",
                       hidden_sizes = c(3, 3),
                       cv_folds = cv_folds,
                       verbose = FALSE
                     )
                     end_time_nn <- Sys.time()
                     time_nn <- as.numeric(difftime(end_time_nn, start_time_nn, units = "secs"))
                     cv_nn <- model_nn$cv_performance
                     
                     # Simulate test data (from a different environment)
                     test_data <- selected_system$data_func(
                       n = 100,
                       environment = list(mu_A1 = 3, sigma_A1 = 1, mu_A2 = 3, sigma_A2 = 1)
                     )
                     
                     # Evaluate both models on test data (out-of-sample RMSE)
                     perf_linear <- evaluate_causal_model(model_linear, test_data)
                     perf_nn <- evaluate_causal_model(model_nn, test_data)
                     
                     # Create a combined data frame for both models
                     data.frame(
                       Lambda = rep(lambda, 2),
                       Repetition = rep(rep, 2),
                       Model = c("Linear", "Neural Network"),
                       TrainTime = c(time_linear, time_nn),
                       CV_RMSE = c(cv_lin, cv_nn),
                       Test_RMSE = c(perf_linear$RMSE, perf_nn$RMSE),
                       stringsAsFactors = FALSE
                     )
                   }

# Stop the parallel cluster
stopCluster(cl)

# Record overall end time and compute elapsed time
overall_end <- Sys.time()
overall_elapsed <- round(as.numeric(difftime(overall_end, overall_start, units = "secs")), 2)
cat("Overall experiment elapsed time:", overall_elapsed, "seconds\n")

# Compute average training times per model type in a summary table
train_time_summary <- results %>%
  group_by(Model, Lambda) %>%
  summarise(AvgTrainTime = mean(TrainTime), .groups = "drop")
print(train_time_summary)

# Plot 1: In-sample performance (CV RMSE) vs. Lambda
p_cv <- ggplot(results, aes(x = factor(Lambda), y = CV_RMSE, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(title = "In-Sample CV RMSE vs. Lambda",
       x = "Lambda",
       y = "CV RMSE",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Out-of-sample performance (Test RMSE) vs. Lambda
p_test <- ggplot(results, aes(x = factor(Lambda), y = Test_RMSE, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(title = "Out-of-Sample Test RMSE vs. Lambda",
       x = "Lambda",
       y = "Test RMSE",
       fill = "Model Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_cv + theme_bw())
print(p_test + theme_bw())
