# deepCausal: Non-Linear Structural Equation Modeling

<img src="man/figures/logo3.png" alt="deepCausal Logo" style="border-radius: 50%; width: 800px; height: 800px; object-fit: cover;"/>


deepCausal is an R package designed to extend traditional Structural Equation Models (SEMs) by incorporating neural networks to model non-linear relationships among variables. While conventional SEMs are limited to linear interactions, deepCausal leverages the flexibility of neural networks, combined with Causal Dantzig methodology, to enhance causal inference and prediction accuracy in complex systems.

For details about the structural equation and the underlying methodologies, please refer to the [methodologies document](man/figures/methodologies.pdf).


## Usage Example

Here is an example of how to use `deepCausal` to define systems, train models, and evaluate them:

```r
# Install and load the package
devtools::install_github("franciscorichter/deepCausal")
library(deepCausal)

cat("Starting data simulation...\n")
systems <- define_systems()
selected_system <- systems[[3]]  # Example: Complex Non-linear System with Many Variables

n_train <- 500
n_test <- 500

# Environment parameters
mu_A1_env1 <- 0; sigma_A1_env1 <- 1
mu_A1_env2 <- 1; sigma_A1_env2 <- 1
mu_A1_env3 <- 2; sigma_A1_env3 <- 2

mu_A2_env1 <- 0; sigma_A2_env1 <- 1
mu_A2_env2 <- 1; sigma_A2_env2 <- 1
mu_A2_env3 <- 2; sigma_A2_env3 <- 2

train_data_G1 <- selected_system$data_func(n_train,environment = list(mu_A1=0,sigma_A1=0,mu_A2=0,sigma_A2=0))
train_data_G2 <- selected_system$data_func(n_train, environment = list(mu_A1=1,sigma_A1=1,mu_A2=2,sigma_A2=0.5))
test_data <- selected_system$data_func(n_test, environment = list(mu_A1=2,sigma_A1=0.8,mu_A2=1.5,sigma_A2=1))

cat("Starting model training and evaluation...\n")

mse_results <- data.frame(Lambda = numeric(), MSE = numeric(), Sample = character(), Functional_Form = character())

# Train linear model
lambda = 0.5
linear_params <- train_predictive_models(data_G1 = train_data_G1, data_G2 = train_data_G2, nn_params = nn_params, model_type = "linear", lambda)

# Calculate in-sample MSE (training data)
data_type = "in-sample"
cat("\nEvaluating models (", data_type, ")...\n")
mse_linear_in_sample <- evaluate_models(models = linear_params, data = rbind(train_data_G1, train_data_G2), nn_params = nn_params,model_type =  "linear")

# Calculate out-of-sample MSE (testing data)
data_type = "out-of-sample"
cat("\nEvaluating models (", data_type, ")...\n")
mse_linear_out_sample <- evaluate_models(linear_params, test_data, nn_params, "linear")

# Train neural network model
nn_par = list(hidden_sizes = c(3, 3))
nn_params <- train_predictive_models(train_data_G1, train_data_G2, nn_par, "neural_network", lambda)

# Calculate in-sample MSE (training data)
data_type = "in-sample"
cat("\nEvaluating models (", data_type, ")...\n")
mse_nn_in_sample <- evaluate_models(nn_params, rbind(train_data_G1, train_data_G2), nn_par, "neural_network")

# Calculate out-of-sample MSE (testing data)
data_type = "out-of-sample"
cat("\nEvaluating models (", data_type, ")...\n")
mse_nn_out_sample <- evaluate_models(nn_params, test_data, nn_par, "neural_network")

# Collect results for linear model
mse_results <- rbind(mse_results,
                     data.frame(Lambda = lambda, MSE = mse_linear_in_sample, Sample = "In-sample", Functional_Form = "Linear"),
                     data.frame(Lambda = lambda, MSE = mse_linear_out_sample, Sample = "Out-of-sample", Functional_Form = "Linear"))

# Collect results for neural network model
mse_results <- rbind(mse_results,
                     data.frame(Lambda = lambda, MSE = mse_nn_in_sample, Sample = "In-sample", Functional_Form = "Neural Network"),
                     data.frame(Lambda = lambda, MSE = mse_nn_out_sample, Sample = "Out-of-sample", Functional_Form = "Neural Network"))

cat("Lambda =", lambda, ": In-sample MSE (Linear) =", mse_linear_in_sample, ", Out-of-sample MSE (Linear) =", mse_linear_out_sample, "\n")
cat("Lambda =", lambda, ": In-sample MSE (Neural Network) =", mse_nn_in_sample, ", Out-of-sample MSE (Neural Network) =", mse_nn_out_sample, "\n")


```

## **License**

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## **Contact**

For any questions or feedback, please open an issue on this repository or contact the maintainer at [richtf@usi.ch](mailto:richtf@usi.ch).



