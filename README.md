# deepCausal: Non-Linear Structural Equation Modeling

<img src="man/figures/logo3.png" alt="deepCausal Logo" style="border-radius: 50%; width: 800px; height: 800px; object-fit: cover;"/>


deepCausal is an R package designed to extend traditional Structural Equation Models (SEMs) by incorporating neural networks to model non-linear relationships among variables. While conventional SEMs are limited to linear interactions, deepCausal leverages the flexibility of neural networks, combined with Causal Dantzig methodology, to enhance causal inference and prediction accuracy in complex systems.

### **Key Concepts**

- **Endogenous Variables**: A vector **X** representing the endogenous variables, each potentially influenced by others in the system.
- **Non-Linear Operator**: The operator **B(X)** represents non-linear transformations applied to **X**. This could include polynomials, trigonometric functions, or other non-linear forms.
- **Causal Dantzig**: A methodology that combines the Mean Squared Error (MSE) with a causal discrepancy term to improve causal inference and prediction accuracy across different environments.

### Structural Equation

For details about the structural equation and the underlying methodologies, please refer to the [methodologies document](man/figures/methodologies.pdf).

### **Assumptions**

For the non-linear SEM to be valid and solvable, the following assumptions are made:

- **Non-Linearity**: The operator **B(X)** is non-linear, enabling the model to capture complex, non-linear interactions between variables.
- **Acyclic Constraint**: The system is acyclic, ensuring that there are no circular dependencies among the variables, allowing for a unique and unambiguous solution.

## Usage Example

Here is an example of how to use `deepCausal` to define systems, train models, and evaluate them:

```r
# Install and load the package
devtools::install_github("franciscorichter/deepCausal")
library(deepCausal)

# Define the systems
systems <- define_systems()

# Simulate data for two environments using System 1
data_list <- simulate_data(systems[[1]]$data_func, n = 1000, mu_A1 = c(0, 0.5), sigma_A1 = c(1, 1), mu_A2 = c(0, 0.5), sigma_A2 = c(1, 1))
data_G1 <- data_list[[1]]
data_G2 <- data_list[[2]]

# Define functional forms for linear and neural network models
functional_forms <- define_functional_forms()

# Train a linear model with a fixed lambda
lambda <- 0.5
linear_params <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$linear)

# Train a neural network model with a fixed lambda and specified hidden layers
nn_params <- list(hidden_sizes = c(3, 3))
nn_params_trained <- train_model(data_G1, data_G2, lambda, obj_func, functional_forms$neural_network, parameters = nn_params, nn = TRUE)

# Evaluate the models on a test dataset
test_data <- systems[[1]]$data_func(n = 500)
mse_linear <- evaluate_models(list(combined_params = linear_params), test_data, define_functional_forms, nn_params, model_type = "linear")
mse_nn <- evaluate_models(list(combined_params = nn_params_trained), test_data, define_functional_forms, nn_params, model_type = "neural_network")

# Print the MSE results
cat("MSE for Linear Model:", mse_linear, "\n")
cat("MSE for Neural Network Model:", mse_nn, "\n")

```

## **License**

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## **Contact**

For any questions or feedback, please open an issue on this repository or contact the maintainer at [richtf@usi.ch](mailto:richtf@usi.ch).



