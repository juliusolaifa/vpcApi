library(plumber)
library(vpc)
source("utils.R")

#* @apiTitle vpcApi
#* @apiDescription API to generate GLMM data with flexible inputs


#* @filter CORS
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "http://localhost:5173")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* Generate Random Intercept Design Matrix for Mixed Models
#*
#* @param ns A vector specifying the number of observations per group (comma-separated).
#* @get /random_intercept_matrix
#* @serializer json
function(ns) {
  ns <- parse_numeric_vector(ns)
  Z <- vpc::generateRandomInterceptMatrix(ns)
  return(Z)
}


#* Generate Random Design Matrices for Mixed Models
#*
#* @param ns A comma-separated string of group sizes (e.g., "5,3,2").
#* @param X A comma-separated string of predictor values for X matrix (optional). The length must be equal to the sum of ns
#* @get /random_design_matrices
#* @serializer json
function(ns, X = "") {
  ns <- parse_numeric_vector(ns)
  X <- parse_and_reshape_X(X = X, ns = ns)
  Z_list <- vpc::generateRandomDesignMatrices(ns, X)
  return(Z_list)
}


#* Generate Random Effects Coefficients
#* @param n:int The number of samples to generate.
#* @param sigma A square, comma-separated covariance matrix in a string format.
#* @get /generateRandomEffectsCoefficients
#* @serializer json
function(n, sigma) {
  sigma_vector <- parse_square_matrix(sigma)
  n <- as.integer(n)
  result <- vpc::generateRandomEffectsCoefficients(n, sigma_matrix)
  return(result)
}


#* Compute Fixed Effects Contribution to the Linear Predictor
#*
#* @param X A comma-separated string of design matrix values (optional).
#* @param beta A comma-separated string of fixed effect coefficients.
#* @param add_intercept Logical flag to add intercept (1 or 0).
#* @get /compute_fixed_effects
#* @serializer json
function(X = "", beta, add_intercept = 1) {
  add_intercept <- as.logical(as.numeric(add_intercept))
  beta <- parse_numeric_vector(beta)
  X <- parse_and_reshape_X(X = X, beta = beta, add_intercept = add_intercept)
  result <- vpc::computeFixedEffects(beta = beta, X = X, add_intercept = add_intercept)
  return(result)
}

#* Generate and Compute Random Effects
#*
#* @param X A comma-separated string of covariate values (optional). If a matrix, provide semicolon-separated rows.
#* @param ns A comma-separated string representing sample sizes in each cluster.
#* @param Sigma A square, comma-separated variance-covariance matrix.
#* @get /generate_and_compute_random_effects
#* @serializer json
function(ns, Sigma, X = "") {

  ns <- parse_numeric_vector(ns)
  Sigma <- parse_square_matrix(Sigma)
  X <- parse_and_reshape_X(X = X, ns = ns)
  result <- vpc::generateAndComputeRandomEffects(ns = ns, Sigma = Sigma, X = X)

  return(result)
}



# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JS
        pr_set_serializer(serializer_unboxed_json())
}


