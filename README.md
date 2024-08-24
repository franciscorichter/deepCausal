# **Non-Linear Structural Equation Modeling (SEM) Package**

## **Short Description**

This package extends traditional Structural Equation Models (SEMs) to accommodate non-linear relationships among variables in multivariate data. While SEMs are a powerful tool for modeling complex dependencies between observed and latent variables, they are typically limited to linear interactions. This package addresses this limitation by introducing non-linear operators into the SEM framework, enabling more accurate modeling of real-world systems where non-linearity is prevalent.

## **Mathematical Theory**

Structural Equation Models (SEMs) are a fundamental tool for representing the relationships among multiple variables, particularly in cases where both observed and latent variables are involved. Traditional SEMs assume linear relationships, which may not always capture the true dynamics of the underlying system. This package introduces a non-linear extension to SEMs, allowing for more flexible and realistic modeling of complex systems.

### **Key Concepts**

- **Endogenous Variables**: A vector **X** representing the endogenous variables, each potentially influenced by others in the system.
- **Non-Linear Operator**: The operator **B(X)** represents non-linear transformations applied to **X**. This could include polynomials, trigonometric functions, or other non-linear forms.
- **Error Terms**: A vector **ε** of error terms, typically assumed to follow a normal distribution with a mean of zero and a given variance.

The structural equation governing the system is:

![Structural Equation](https://latex.codecogs.com/png.latex?%5Cmathbf%7BX%7D%20%5Cleftarrow%20%5Cmathbf%7BB%7D%28%5Cmathbf%7BX%7D%29%20%2B%20%5Cepsilon)

### **Assumptions**

For the non-linear SEM to be valid and solvable, the following assumptions are made:

- **Non-Linearity**: The operator **B(X)** is non-linear, enabling the model to capture complex, non-linear interactions between variables.
- **Acyclic Constraint**: The system is acyclic, ensuring that there are no circular dependencies among the variables, allowing for a unique and unambiguous solution.

## **Further Information**

- [Usage and Examples](usage.md): Instructions and code examples for applying the non-linear SEM package to various datasets.
- [Case Studies](case-studies.md): Detailed results and discussions from applying the package to real-world systems.
- [API Documentation](api-documentation.md): Complete documentation of the package’s API, including function definitions and usage.

## **Contributing**

Contributions are encouraged! Please refer to the [Contributing Guidelines](CONTRIBUTING.md) for information on how to contribute to the project, including style guides, code reviews, and how to submit pull requests.

## **License**

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## **Contact**

For any questions or feedback, please open an issue on this repository or contact the maintainer at [your.email@example.com](mailto:your.email@example.com).

