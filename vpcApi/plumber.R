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

# Endpoint to generate the random intercept design matrix
#* @param ns A vector specifying the number of observations per group (comma-separated).
#* @get /random_intercept_matrix
function(ns) {

  ns <- as.numeric(strsplit(ns, ",")[[1]])
  Z <- vpc::generateRandomInterceptMatrix(ns)
  #
  # Return the matrix
  return(as.data.frame(Z))
}


# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JS
        pr_set_serializer(serializer_unboxed_json())
}


