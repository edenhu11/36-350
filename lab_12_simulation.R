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
    return (as.numeric(c()))
  }
  fit2 <- lm(responses ~ covariates[,ind])
  res <- (summary(fit2)$coefficients[,4][-1])
  res <- as.vector(res)
  res 
}

#it does not seem like it's uniformly distributed
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

n <- c(100, 1000, 10000)
p <- c(10, 20, 50)
n_trials <- 1000
cutoff <- 0.05
for (i in n) {
  for (j in p) {
    run_simulation(n_trials, i, j, cutoff)
  }
}
