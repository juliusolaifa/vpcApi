library(plumber)
library(vpc)  # Assuming your vpc package contains the singleGLMMData function

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
function(ns) {

  ns <- as.numeric(strsplit(ns, ",")[[1]])
  Z <- vpc::generateRandomInterceptMatrix(ns)
  #
  # Return the matrix
  return(as.data.frame(Z))
}


#* Generate Random Design Matrices for Mixed Models
#*
#* @param ns A comma-separated string of group sizes (e.g., "5,3,2").
#* @param X A comma-separated string of predictor values for X matrix (optional). The length must be equal to the sum of ns
#* @get /random_design_matrices
function(ns, X = NULL) {
  # Convert 'ns' and 'X' from strings to numeric vectors/matrix
  ns <- as.numeric(strsplit(ns, ",")[[1]])

  if (!is.null(X)) {
    X <- as.numeric(strsplit(X, ",")[[1]])
    X <- matrix(X, ncol = length(X)/sum(ns))  # Assume it's a matrix
  }

  # Call the functions from your package
  Z_list <- vpc::generateRandomDesignMatrices(ns, X)

  # Return the result as a list of matrices
  return(lapply(Z_list, as.data.frame))  # Convert matrices to data.frames for JSON serialization
}




# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JS
        pr_set_serializer(serializer_unboxed_json())
}


