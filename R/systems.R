define_systems <- function() {
  systems <- list(
    # System 1: Non-linear System with External Actions
    list(
      name = "Non-linear System with External Actions",
      description = "X1 = epsilon1 + A1\nY = sin(X1) + epsilonY\nX2 = Y^2 + A2 + epsilon2",
      data_func = function(n, environment=1, mu_A1=0, sigma_A1=1, mu_A2=0, sigma_A2=1) {
        epsilon1 <- rnorm(n)
        epsilonY <- rnorm(n)
        epsilon2 <- rnorm(n)
        
        A1 <- ifelse(environment == 2, rnorm(n, mean = mu_A1, sd = sigma_A1), 0)
        A2 <- ifelse(environment == 2, rnorm(n, mean = mu_A2, sd = sigma_A2), 0)
        
        X1 <- epsilon1 + A1
        Y <- sin(X1) + epsilonY
        X2 <- Y^2 + A2 + epsilon2
        
        data <- data.frame(X1, Y, X2)
        return(data)
      }
    ),
    
    # System 2: Modified Non-linear System
    list(
      name = "Modified Non-linear System with External Actions",
      description = "X1 = epsilon1 + A1\nX2 = X1 + epsilon2\nX3 = X1 + X2 + epsilon3\nY = sin(5 * X2) + X3^3 + epsilonY\nX4 = X2 + epsilon4\nX5 = Y + A2 + epsilon5\nX6 = Y + epsilon6",
      data_func = function(n, environment=1, mu_A1=0, sigma_A1=1, mu_A2=0, sigma_A2=1) {
        epsilon1 <- rnorm(n)
        epsilon2 <- rnorm(n)
        epsilon3 <- rnorm(n)
        epsilonY <- rnorm(n)
        epsilon4 <- rnorm(n)
        epsilon5 <- rnorm(n)
        epsilon6 <- rnorm(n)
        
        A1 <- ifelse(environment == 2, rnorm(n, mean = mu_A1, sd = sigma_A1), 0)
        A2 <- ifelse(environment == 2, rnorm(n, mean = mu_A2, sd = sigma_A2), 0)
        
        X1 <- epsilon1 + A1
        X2 <- X1 + epsilon2
        X3 <- X1 + X2 + epsilon3
        Y  <- sin(5 * X2) + X3^3 + epsilonY
        X4 <- X2 + epsilon4
        X5 <- Y + A2 + epsilon5
        X6 <- Y + epsilon6
        
        data <- data.frame(X1, X2, X3, X4, X5, X6, Y)
        return(data)
      }
    ),
    
    # System 3: Reduced Complex Non-linear System with 21 Variables
    list(
      name = "Reduced Complex Non-linear System with 21 Variables",
      description = "A medium-dimensional system with fewer variables but still complex interdependencies. Y has three parents and no children.",
      data_func = function(n, environment=1, mu_A1=0, sigma_A1=1, mu_A2=0, sigma_A2=1) {
        set.seed(123)  # For reproducibility
        
        # Generate independent noise variables
        epsilon <- replicate(21, rnorm(n))
        
        A1 <- ifelse(environment == 2, rnorm(n, mean = mu_A1, sd = sigma_A1), 0)
        A2 <- ifelse(environment == 2, rnorm(n, mean = mu_A2, sd = sigma_A2), 0)
        
        # Define the variables
        X1 <- epsilon[, 1] + A1
        X2 <- epsilon[, 2]
        X3 <- epsilon[, 3]
        X4 <- epsilon[, 4]
        X5 <- epsilon[, 5]
        X6 <- epsilon[, 6]
        X7 <- epsilon[, 7]
        X8 <- epsilon[, 8]
        X9 <- epsilon[, 9]
        X10 <- epsilon[, 10]
        X11 <- X1 + X2 + epsilon[, 11]
        X12 <- X2 + X3 + epsilon[, 12]
        X13 <- X3 + X4 + epsilon[, 13]
        X14 <- X4 + X5 + epsilon[, 14]
        X15 <- X5 + X6 + epsilon[, 15]
        X16 <- X6 + X7 + epsilon[, 16]
        X17 <- X7 + X8 + epsilon[, 17]
        X18 <- X8 + X9 + epsilon[, 18]
        X19 <- X9 + X10 + epsilon[, 19]
        X20 <- epsilon[, 20]
        X21 <- X10 + X11 + epsilon[, 21]
        
        # Define Y based on three parents
        Y <- (X10 + X20 + X5)^2 + rnorm(n)
        
        data <- data.frame(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                           X11, X12, X13, X14, X15, X16, X17, X18, X19, X20,
                           X21, Y)
        
        return(data)
      }
    ),
    
    # System 4: Complex Non-linear System with 50 Variables
    list(
      name = "Complex Non-linear System with 50 Variables",
      description = "A high-dimensional system with 50 variables. Y is influenced by a combination of selected variables and non-linear transformations.",
      data_func = function(n, environment=1, mu_A1=0, sigma_A1=1, mu_A2=0, sigma_A2=1) {
        set.seed(123)  # For reproducibility
        
        # Generate independent noise variables
        epsilon <- replicate(50, rnorm(n))
        
        A1 <- ifelse(environment == 2, rnorm(n, mean = mu_A1, sd = sigma_A1), 0)
        A2 <- ifelse(environment == 2, rnorm(n, mean = mu_A2, sd = sigma_A2), 0)
        A3 <- ifelse(environment == 2, rnorm(n, mean = mu_A2, sd = sigma_A2), 0)
        A4 <- ifelse(environment == 2, rnorm(n, mean = mu_A2, sd = sigma_A2), 0)
        A5 <- ifelse(environment == 2, rnorm(n, mean = mu_A2, sd = sigma_A2), 0)
        
        # Define the variables
        X1 <- epsilon[, 1] + A1
        X2 <- epsilon[, 2]
        X3 <- epsilon[, 3]
        X4 <- epsilon[, 4]
        X5 <- epsilon[, 5]
        X6 <- epsilon[, 6]
        X7 <- epsilon[, 7] + A2
        
        X10 <- epsilon[, 10]
        X11 <- X1 + X2 + epsilon[, 11]
        X12 <- X2 + X3 + epsilon[, 12]
        X13 <- X3 + X4 + epsilon[, 13]
        X14 <- X4 + X5 + epsilon[, 14]
        X15 <- X5 + X6 + epsilon[, 15]
        X16 <- X6 + X7 + epsilon[, 16]
        X17 <- X7 + epsilon[, 17]
      
        X20 <- epsilon[, 20]
        X21 <- X10 + X11 + epsilon[, 21]
        X22 <- X11 + X12 + epsilon[, 22]
        X23 <- X12 + X13 + epsilon[, 23]
        X24 <- X13 + X14 + epsilon[, 24]
        X25 <- X14 + X15 + epsilon[, 25]
        X26 <- X15 + X16 + epsilon[, 26]
        X27 <- X16 + X17 + epsilon[, 27]
    
        X30 <- epsilon[, 30]
        X31 <- X20 + X21 + epsilon[, 31]
        X32 <- X21 + X22 + epsilon[, 32]
        X33 <- X22 + X23 + epsilon[, 33]
        X34 <- X23 + X24 + epsilon[, 34]
        X35 <- X24 + X25 + epsilon[, 35]
        X36 <- X25 + X26 + epsilon[, 36]
        X37 <- X26 + X27 + epsilon[, 37]
   
        X40 <- epsilon[, 40]
        X41 <- X30 + X31 + epsilon[, 41]
        X42 <- X31 + X32 + epsilon[, 42]
        X43 <- X32 + X33 + epsilon[, 43]
        X44 <- X33 + X34 + epsilon[, 44]
        X45 <- X34 + X35 + epsilon[, 45]
        X46 <- X35 + X36 + epsilon[, 46]
        X47 <- X36 + X37 + epsilon[, 47] 

        X50 <- epsilon[, 50]
        
        # Define Y based on selected variables
        Y <- (X10 + X20 + X30 + X40 + X50)^2 + sin(X5 + X15 + X25 + X35 + X45) + rnorm(n)
        
        X8 <- Y + epsilon[, 8] + A4
        X9 <- Y + epsilon[, 9] + A3
        
        X18 <- Y + X8 + X9 + epsilon[, 18] + A5
        X19 <- Y + X9 + X10 + epsilon[, 19]
        
        X28 <- X17 + X18 + epsilon[, 28]
        X29 <- X18 + X19 + epsilon[, 29]
        
        X38 <- X27 + X28 + epsilon[, 38]
        X39 <- X28 + X29 + epsilon[, 39]
        
        X48 <- X37 + X38 + epsilon[, 48]
        X49 <- X38 + X39 + epsilon[, 49]
        
        data <- data.frame(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                           X11, X12, X13, X14, X15, X16, X17, X18, X19, X20,
                           X21, X22, X23, X24, X25, X26, X27, X28, X29, X30,
                           X31, X32, X33, X34, X35, X36, X37, X38, X39, X40,
                           X41, X42, X43, X44, X45, X46, X47, X48, X49, X50,
                           Y)
        
        return(data)
      }
    )
  )
  return(systems)
}

