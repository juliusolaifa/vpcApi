library(plumber)
library(vpc)  # Assuming your vpc package contains the singleGLMMData function

#* @apiTitle vpcApi
#* @apiDescription API to generate GLMM data with flexible inputs

# Endpoint to generate the random intercept design matrix
#* @param ns A vector specifying the number of observations per group (comma-separated).
#* @get /random_intercept_matrix
function(ns) {
  # Convert the input string to a numeric vector
  ns <- as.numeric(strsplit(ns, ",")[[1]])

  # Generate the random intercept design matrix
  Z <- vpc::generateRandomInterceptMatrix(ns)

  # Return the matrix
  return(as.data.frame(Z))  # Converting to data frame for JSON serialization
}


# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
