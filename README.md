# deepCausal: Advanced Non-Linear Structural Equation Modeling

<img src="man/figures/logo3.png" alt="deepCausal Logo" style="border-radius: 50%; width: 800px; height: 800px; object-fit: cover;"/>

deepCausal is an R package designed to extend traditional Structural Equation Models (SEMs) by incorporating neural networks to capture non-linear relationships among variables. By combining the flexibility of neural networks with the principles of Causal Dantzig methodology, deepCausal provides robust causal inference and accurate predictions for complex systems, overcoming the limitations of linear models.

## Key Features
- **Non-linear structural equation modeling:** Harness the power of neural networks to model intricate causal relationships.
- **Causal Dantzig integration:** Ensure stable causal structures across environments.
- **Flexible model training:** Customize network architectures, training parameters, and regularization.
- **In-sample and out-of-sample evaluation:** Quantify model performance on training and testing datasets.

For detailed methodology, please see the [methodologies document](man/figures/methodologies.pdf).

---

## Usage Example

Below is an example workflow using `deepCausal`:

```r
# Install and load the package
devtools::install_github("franciscorichter/deepCausal")
library(deepCausal)

# Define systems and select one for analysis
systems <- define_systems()
selected_system <- systems[[1]]

# Simulation parameters
n_train <- 500
n_test <- 500
lambda <- 0.5
nn_params <- list(hidden_sizes = c(3, 3))

# Simulate training and testing datasets
train_data_G1 <- selected_system$data_func(n_train, environment = list(mu_A1 = 0, sigma_A1 = 0.5, mu_A2 = 0, sigma_A2 = 0.5))
train_data_G2 <- selected_system$data_func(n_train, environment = list(mu_A1 = 1, sigma_A1 = 0.8, mu_A2 = 2, sigma_A2 = 0.5))
test_data <- selected_system$data_func(n_test, environment = list(mu_A1 = 2, sigma_A1 = 0.8, mu_A2 = 1.5, sigma_A2 = 1))

# Train models
linear_model <- train_predictive_models(data_G1 = train_data_G1, data_G2 = train_data_G2, nn_params = nn_params, model_type = "linear", lambda = lambda)
neural_network_model <- train_predictive_models(data_G1 = train_data_G1, data_G2 = train_data_G2, nn_params = nn_params, model_type = "neural_network", lambda = lambda)

# Evaluate models
mse_linear_in_sample <- evaluate_models(models = linear_model, data = rbind(train_data_G1, train_data_G2), nn_params = nn_params, model_type = "linear")
mse_linear_out_sample <- evaluate_models(models = linear_model, data = test_data, nn_params = nn_params, model_type = "linear")

mse_nn_in_sample <- evaluate_models(models = neural_network_model, data = rbind(train_data_G1, train_data_G2), nn_params = nn_params, model_type = "neural_network")
mse_nn_out_sample <- evaluate_models(models = neural_network_model, data = test_data, nn_params = nn_params, model_type = "neural_network")

# Print results
cat("Linear Model: In-sample MSE =", mse_linear_in_sample, ", Out-of-sample MSE =", mse_linear_out_sample, "\n")
cat("Neural Network: In-sample MSE =", mse_nn_in_sample, ", Out-of-sample MSE =", mse_nn_out_sample, "\n")
```

---

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

---

## Contact
For questions or feedback, please open an issue on this repository or contact the maintainer at [richtf@usi.ch](mailto:richtf@usi.ch).

