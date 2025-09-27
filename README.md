# deepCausal

Non-Linear Structural Equation Modeling with Causal Dantzig in R.

This package extends traditional Structural Equation Models (SEMs) by incorporating neural networks to model non-linear relationships and uses a two-environment objective that balances pooled least-squares error and environment discrepancy.

## Installation

This is an R package project. After cloning, you can build and install with your usual tooling (e.g., RStudio Build or devtools):

```r
# install.packages("devtools")
devtools::document()  # if you use roxygen2
devtools::install()
```

## Core API

- `train_causal()`: Trains a linear or neural model to minimize the objective.
- `evaluate_causal_model()`: computes MSE/RMSE on a test set.
- `define_functional_forms()`: Returns `linear` and `neural_network` predictors.
- `pilot_systems()`: Returns toy systems for simulation.

  See function documentation (`?train_causal`) for details and examples.
  
  ## Shiny App
 
 A small Shiny prototype lives in `systemsApp.R`. It is excluded from the package build (`.Rbuildignore`) to keep the package lightweight. You can run it directly in an R session:
 
 ```r
 source("systemsApp.R")
 ```
 
 ## Python Experiments
 
 The Python experiment now lives outside this R package, in a sibling folder: `../deepCausal-python/`.
 Run it from there, for example:
 
 ```bash
 cd ../deepCausal-python
 python3 -m venv .venv
 source .venv/bin/activate
 pip install -r requirements.txt
 python study01.py
 ```
 
 The R package does not depend on Python.
 
 ## Development
 
 - Run `R CMD check` or `devtools::check()` to validate.
 - Tests are recommended for `obj_func`, `train_causal`, and `evaluate_causal_model`.
 
 ## License
 
 MIT. See `LICENSE`.
