# Utility function to convert a comma-separated string into a numeric vector
parse_numeric_vector <- function(string) {
  if (string == "") {
    return(NULL)  # Handle empty strings
  }
  as.numeric(strsplit(string, ",")[[1]])
}

# Utility function to parse and reshape X based on ns or beta
parse_and_reshape_X <- function(X, ns = NULL, beta = NULL, add_intercept = TRUE) {
  if (X == "") {
    return(NULL)  # Handle empty strings
  }
  if (!is.null(X)) {
    # Parse comma-separated vector or semicolon-separated matrix
    if (grepl(";", X)) {
      X <- lapply(strsplit(X, ";")[[1]], function(row) as.numeric(strsplit(row, ",")[[1]]))
      X <- do.call(rbind, X)
    } else {
      X <- as.numeric(strsplit(X, ",")[[1]])
    }

    # Reshape based on ns (cluster sizes) or beta (covariates)
    if (!is.null(ns)) {
      # Reshape using ns (cluster sizes)
      X <- matrix(X, ncol = length(X) / sum(ns))
    } else if (!is.null(beta)) {
      # Reshape using beta (covariates)
      n_covariates <- length(beta) - if (add_intercept) 1 else 0
      X <- matrix(X, nrow = length(X) / n_covariates)
    }
  }

  return(X)
}


# Utility function to convert a comma-separated string into a square matrix
parse_square_matrix <- function(string) {
  numeric_values <- as.numeric(strsplit(string, ",")[[1]])
  matrix_size <- sqrt(length(numeric_values))
  if (floor(matrix_size) != matrix_size) {
    stop("Invalid matrix: must be square.")
  }
  matrix(numeric_values, nrow = matrix_size, ncol = matrix_size)
}
