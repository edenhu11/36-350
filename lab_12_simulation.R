generate_data <- function(n, p) {
  mat <- matrix(0, n, p)
  for (i in 1:p) {
    mat[,i] <- rnorm(n)
  }
  list(covariates = mat, responses = rnorm(n))
}

model_select <- function(covariates, responses, cutoff) {
  fit1 <- lm(responses ~ covariates)
  pvals <- summary(fit1)$coefficients[,4][-1]
  ind <- which(pvals <= cutoff)
  if (length(ind) == 0) {
    return (vector())
  }
  fit2 <- lm(responses ~ covariates[,ind])
  res <- (summary(fit2)$coefficients[,4][-1])
  res <- as.vector(res)
  res 
}

run_simulation <- function(n_trials, n, p, cutoff) {
  pvalues <- vector()
  for (i in 1:n_trials) {
    res <- generate_data(n, p)
    cov <- res$covariates
    resp <- res$responses
    pval <- model_select(cov, resp, cutoff)
    pvalues <- c(pvalues, pval)
  }
  hist(pvalues)
}

