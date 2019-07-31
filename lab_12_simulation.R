generate_data <- function(n, p) {
  mat <- matrix(0, n, p)
  for (i in 1:p) {
    mat[,i] <- rnorm(n)
  }
  list(covariates = mat, responses = rnorm(n))
}

