set.seed(123)
# library(deepCausal) # Removed to allow devtools::load_all workflow
suppressPackageStartupMessages({
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not installed; using base plotting.")
    use_gg <- FALSE
  } else {
    library(ggplot2)
    use_gg <- TRUE
  }

plot_loss_curve <- function(loss_hist, title, outfile) {
  epochs <- seq_along(loss_hist)
  if (exists("use_gg") && isTRUE(use_gg)) {
    df <- data.frame(Epoch = epochs, Loss = as.numeric(loss_hist))
    p <- ggplot(df, aes(x = Epoch, y = Loss)) +
      geom_line(color = "steelblue", linewidth = 0.7) +
      ggtitle(title) +
      theme_minimal()
    ggsave(outfile, p, width = 5, height = 3.2, dpi = 150)
  } else {
    png(outfile, width = 800, height = 480, res = 120)
    plot(epochs, loss_hist, type = "l", col = "steelblue", lwd = 2,
         main = title, xlab = "Epoch", ylab = "Loss")
    dev.off()
  }
}

plot_residual_hist <- function(residuals, title, outfile) {
  if (exists("use_gg") && isTRUE(use_gg)) {
    df <- data.frame(Residual = residuals)
    p <- ggplot(df, aes(x = Residual)) +
      geom_histogram(bins = 30, fill = "gray50", color = "white") +
      ggtitle(title) +
      theme_minimal()
    ggsave(outfile, p, width = 5, height = 3.2, dpi = 150)
  } else {
    png(outfile, width = 800, height = 480, res = 120)
    hist(residuals, breaks = 30, col = "gray", main = title, xlab = "Residual")
    dev.off()
  }
}
})

dir.create("Reports/figures", recursive = TRUE, showWarnings = FALSE)

plot_pred_vs_actual <- function(y_true, y_pred, title, outfile) {
  if (exists("use_gg") && isTRUE(use_gg)) {
    df <- data.frame(Actual = y_true, Predicted = y_pred)
    p <- ggplot(df, aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.6, size = 1.5) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      ggtitle(title) +
      theme_minimal()
    ggsave(outfile, p, width = 5, height = 4, dpi = 150)
  } else {
    png(outfile, width = 800, height = 640, res = 120)
    plot(y_true, y_pred, main = title, xlab = "Actual", ylab = "Predicted", pch = 16, col = rgb(0,0,0,0.5))
    abline(0, 1, col = "red", lty = 2, lwd = 2)
    dev.off()
  }
}

# System 1
cat("### Using System 1: Modified Nonlinear System with External Actions ###\n")
systems <- pilot_systems()
selected_system1 <- systems[[1]]
# Simulate training data for Environment 1 (no intervention) for System 1
data_env1_sys1 <- selected_system1$data_func(
  n = 1000,
  environment = list(mu_A1 = 0, sigma_A1 = 1, mu_A2 = 0, sigma_A2 = 1)
)

# Simulate training data for Environment 2 (with intervention) for System 1
data_env2_sys1 <- selected_system1$data_func(
  n = 1000,
  environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5)
)

# Train a linear causal model on System 1
model_linear_sys1 <- train_causal(
  data_G1 = data_env1_sys1,
  data_G2 = data_env2_sys1,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train a neural network causal model on System 1
model_nn_sys1 <- train_causal(
  data_G1 = data_env1_sys1,
  data_G2 = data_env2_sys1,
  lambda = 0.6,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  optimizer = "adam",
  epochs = 1000,
  lr = 0.005,
  verbose = TRUE
)

cat("\nSystem 1 - Linear Model CV RMSE:", model_linear_sys1$cv_performance, "\n")
cat("System 1 - Neural Network Model CV RMSE:", model_nn_sys1$cv_performance, "\n\n")

# For System 1: simulate test data and evaluate out-of-sample performance
test_data_sys1 <- selected_system1$data_func(
  n = 100,
  environment = list(mu_A1 = 2, sigma_A1 = 1, mu_A2 = 2, sigma_A2 = 1)
)

performance_linear_sys1 <- evaluate_causal_model(model_linear_sys1, test_data_sys1)
performance_nn_sys1 <- evaluate_causal_model(model_nn_sys1, test_data_sys1)

cat("\nSystem 1 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys1$MSE, "\n")
cat("  RMSE:", performance_linear_sys1$RMSE, "\n")

cat("\nSystem 1 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys1$MSE, "\n")
cat("  RMSE:", performance_nn_sys1$RMSE, "\n")

# Plots for System 1
plot_pred_vs_actual(test_data_sys1$Y, performance_linear_sys1$predictions,
                    sprintf("System 1 - Linear (RMSE=%.3f)", performance_linear_sys1$RMSE),
                    "Reports/figures/sys1_linear_pred_vs_actual.png")
plot_pred_vs_actual(test_data_sys1$Y, performance_nn_sys1$predictions,
                    sprintf("System 1 - NN (RMSE=%.3f)", performance_nn_sys1$RMSE),
                    "Reports/figures/sys1_nn_pred_vs_actual.png")
plot_loss_curve(model_linear_sys1$loss_history, "System 1 - Linear Training Loss", "Reports/figures/sys1_linear_loss.png")
plot_loss_curve(model_nn_sys1$loss_history, "System 1 - NN Training Loss", "Reports/figures/sys1_nn_loss.png")
plot_residual_hist(performance_linear_sys1$predictions - test_data_sys1$Y, "System 1 - Linear Residuals", "Reports/figures/sys1_linear_resid_hist.png")
plot_residual_hist(performance_nn_sys1$predictions - test_data_sys1$Y, "System 1 - NN Residuals", "Reports/figures/sys1_nn_resid_hist.png")


### System 2 Usage (7-Variable System with Interventions)
cat("### Using System 2: 7-Variable System with Interventions ###\n")
selected_system2 <- systems[[2]]

# Simulate training data for Environment 1 (no intervention) for System 2
data_env1_sys2 <- selected_system2$data_func(
  n = 1000,
  environment = list(sd_eps = 1)
)

# Simulate training data for Environment 2 (with interventions) for System 2
data_env2_sys2 <- selected_system2$data_func(
  n = 1000,
  environment = list(sd_eps = 1.5)
)

# Train a linear causal model on System 2
model_linear_sys2 <- train_causal(
  data_G1 = data_env1_sys2,
  data_G2 = data_env2_sys2,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train a neural network causal model on System 2
model_nn_sys2 <- train_causal(
  data_G1 = data_env1_sys2,
  data_G2 = data_env2_sys2,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  optimizer = "adam",
  epochs = 1000,
  lr = 0.005,
  verbose = TRUE
)

cat("\nSystem 2 - Linear Model CV RMSE:", model_linear_sys2$cv_performance, "\n")
cat("System 2 - Neural Network Model CV RMSE:", model_nn_sys2$cv_performance, "\n")

# Optionally, simulate test data and evaluate models for System 2.
test_data_sys2 <- selected_system2$data_func(
  n = 100,
  environment = list(sd_eps = 1)
)
performance_linear_sys2 <- evaluate_causal_model(model_linear_sys2, test_data_sys2)
performance_nn_sys2 <- evaluate_causal_model(model_nn_sys2, test_data_sys2)

cat("\nSystem 2 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys2$MSE, "\n")
cat("  RMSE:", performance_linear_sys2$RMSE, "\n")

cat("\nSystem 2 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys2$MSE, "\n")
cat("  RMSE:", performance_nn_sys2$RMSE, "\n")

# Plots for System 2
plot_pred_vs_actual(test_data_sys2$Y, performance_linear_sys2$predictions,
                    sprintf("System 2 - Linear (RMSE=%.3f)", performance_linear_sys2$RMSE),
                    "Reports/figures/sys2_linear_pred_vs_actual.png")
plot_pred_vs_actual(test_data_sys2$Y, performance_nn_sys2$predictions,
                    sprintf("System 2 - NN (RMSE=%.3f)", performance_nn_sys2$RMSE),
                    "Reports/figures/sys2_nn_pred_vs_actual.png")
plot_loss_curve(model_linear_sys2$loss_history, "System 2 - Linear Training Loss", "Reports/figures/sys2_linear_loss.png")
plot_loss_curve(model_nn_sys2$loss_history, "System 2 - NN Training Loss", "Reports/figures/sys2_nn_loss.png")
plot_residual_hist(performance_linear_sys2$predictions - test_data_sys2$Y, "System 2 - Linear Residuals", "Reports/figures/sys2_linear_resid_hist.png")
plot_residual_hist(performance_nn_sys2$predictions - test_data_sys2$Y, "System 2 - NN Residuals", "Reports/figures/sys2_nn_resid_hist.png")
cat("### Using System 3: Nonlinear Confounded Mediator System ###\n")
selected_system3 <- systems[[3]]

# Env 1
data_env1_sys3 <- selected_system3$data_func(
  n = 1000,
  environment = list(mu_A = 0, sd_A = 1, sd_eps = 1)
)
# Env 2 (shift A and noise)
data_env2_sys3 <- selected_system3$data_func(
  n = 1000,
  environment = list(mu_A = 1, sd_A = 0.5, sd_eps = 1.2)
)

# Train linear
model_linear_sys3 <- train_causal(
  data_G1 = data_env1_sys3,
  data_G2 = data_env2_sys3,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train NN
model_nn_sys3 <- train_causal(
  data_G1 = data_env1_sys3,
  data_G2 = data_env2_sys3,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  optimizer = "adam",
  epochs = 1000,
  lr = 0.005,
  verbose = TRUE
)

cat("\nSystem 3 - Linear Model CV RMSE:", model_linear_sys3$cv_performance, "\n")
cat("System 3 - Neural Network Model CV RMSE:", model_nn_sys3$cv_performance, "\n")

# Test
test_data_sys3 <- selected_system3$data_func(
  n = 100,
  environment = list(mu_A = 0.5, sd_A = 1, sd_eps = 1)
)

performance_linear_sys3 <- evaluate_causal_model(model_linear_sys3, test_data_sys3)
performance_nn_sys3 <- evaluate_causal_model(model_nn_sys3, test_data_sys3)

cat("\nSystem 3 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys3$MSE, "\n")
cat("  RMSE:", performance_linear_sys3$RMSE, "\n")

cat("\nSystem 3 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys3$MSE, "\n")
cat("  RMSE:", performance_nn_sys3$RMSE, "\n")

# Plots for System 3
plot_pred_vs_actual(test_data_sys3$Y, performance_linear_sys3$predictions,
                    sprintf("System 3 - Linear (RMSE=%.3f)", performance_linear_sys3$RMSE),
                    "Reports/figures/sys3_linear_pred_vs_actual.png")
plot_pred_vs_actual(test_data_sys3$Y, performance_nn_sys3$predictions,
                    sprintf("System 3 - NN (RMSE=%.3f)", performance_nn_sys3$RMSE),
                    "Reports/figures/sys3_nn_pred_vs_actual.png")
plot_loss_curve(model_linear_sys3$loss_history, "System 3 - Linear Training Loss", "Reports/figures/sys3_linear_loss.png")
plot_loss_curve(model_nn_sys3$loss_history, "System 3 - NN Training Loss", "Reports/figures/sys3_nn_loss.png")
plot_residual_hist(performance_linear_sys3$predictions - test_data_sys3$Y, "System 3 - Linear Residuals", "Reports/figures/sys3_linear_resid_hist.png")
plot_residual_hist(performance_nn_sys3$predictions - test_data_sys3$Y, "System 3 - NN Residuals", "Reports/figures/sys3_nn_resid_hist.png")


### System 4 Usage (Heteroskedastic Nonlinear Interaction System)
cat("### Using System 4: Heteroskedastic Nonlinear Interaction System ###\n")
selected_system4 <- systems[[4]]

# Env 1
data_env1_sys4 <- selected_system4$data_func(
  n = 1000,
  environment = list(mu_A = 0, sd_A = 1, sd_eps = 1, sdY = 1)
)
# Env 2 (action shift and higher output noise)
data_env2_sys4 <- selected_system4$data_func(
  n = 1000,
  environment = list(mu_A = 1, sd_A = 1, sd_eps = 1, sdY = 2)
)

# Train linear
model_linear_sys4 <- train_causal(
  data_G1 = data_env1_sys4,
  data_G2 = data_env2_sys4,
  lambda = 0.3,
  target = "Y",
  cv_folds = 5,
  verbose = TRUE
)

# Train NN
model_nn_sys4 <- train_causal(
  data_G1 = data_env1_sys4,
  data_G2 = data_env2_sys4,
  lambda = 0.3,
  target = "Y",
  hidden_sizes = c(3, 3),
  cv_folds = 5,
  optimizer = "adam",
  epochs = 1000,
  lr = 0.005,
  verbose = TRUE
)

cat("\nSystem 4 - Linear Model CV RMSE:", model_linear_sys4$cv_performance, "\n")
cat("System 4 - Neural Network Model CV RMSE:", model_nn_sys4$cv_performance, "\n")

# Test
test_data_sys4 <- selected_system4$data_func(
  n = 100,
  environment = list(mu_A = 0.5, sd_A = 1, sd_eps = 1, sdY = 1.5)
)

performance_linear_sys4 <- evaluate_causal_model(model_linear_sys4, test_data_sys4)
performance_nn_sys4 <- evaluate_causal_model(model_nn_sys4, test_data_sys4)

cat("\nSystem 4 Test Performance (Linear Model):\n")
cat("  MSE: ", performance_linear_sys4$MSE, "\n")
cat("  RMSE:", performance_linear_sys4$RMSE, "\n")

cat("\nSystem 4 Test Performance (Neural Network Model):\n")
cat("  MSE: ", performance_nn_sys4$MSE, "\n")
cat("  RMSE:", performance_nn_sys4$RMSE, "\n")

# Plots for System 4
plot_pred_vs_actual(test_data_sys4$Y, performance_linear_sys4$predictions,
                    sprintf("System 4 - Linear (RMSE=%.3f)", performance_linear_sys4$RMSE),
                    "Reports/figures/sys4_linear_pred_vs_actual.png")
plot_pred_vs_actual(test_data_sys4$Y, performance_nn_sys4$predictions,
                    sprintf("System 4 - NN (RMSE=%.3f)", performance_nn_sys4$RMSE),
                    "Reports/figures/sys4_nn_pred_vs_actual.png")
plot_loss_curve(model_linear_sys4$loss_history, "System 4 - Linear Training Loss", "Reports/figures/sys4_linear_loss.png")
plot_loss_curve(model_nn_sys4$loss_history, "System 4 - NN Training Loss", "Reports/figures/sys4_nn_loss.png")
plot_residual_hist(performance_linear_sys4$predictions - test_data_sys4$Y, "System 4 - Linear Residuals", "Reports/figures/sys4_linear_resid_hist.png")
plot_residual_hist(performance_nn_sys4$predictions - test_data_sys4$Y, "System 4 - NN Residuals", "Reports/figures/sys4_nn_resid_hist.png")
