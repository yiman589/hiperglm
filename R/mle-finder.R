linear_mle_pseudo_inv <- function(design, outcome){
  MLE <- solve(t(design)%*%design,t(design)%*%outcome)
  return(MLE)
}

linear_log_likelihood <- function(beta, design, outcome, noise_var = 1){
  log_lik <- -1/(2*noise_var)*sum((outcome-design%*%beta)^2)
  return(log_lik)
}

linear_gradient_log_likelihood <- function(beta, design, outcome, noise_var = 1){
  gradient <- -1/noise_var*(t(design)%*%design%*%beta-t(design)%*%outcome)
  return(gradient)
}

linear_mle_bfgs <- function(design, outcome, noise_var = 1){
  p <- ncol(design)
  MLE <- stats::optim(rep(0,p), fn=linear_log_likelihood, gr=linear_gradient_log_likelihood,
                      design=design, outcome=outcome, noise_var=noise_var, method="BFGS",
                      control = list(fnscale = -1))
  return(MLE$par)
}

logit_log_likelihood <- function(beta, design, outcome){
  n <- length(outcome)
  log_lik <- sum(sapply(1:n, function(i){
    outcome[i]*sum(design[i,]*beta)-log(1+exp(sum(design[i,]*beta)))
  }))
  return(log_lik)
}

logit_gradient_log_likelihood <- function(beta, design, outcome){
  pi <- exp(design%*%beta)/(1+exp(design%*%beta))
  gradient <- as.vector(t(outcome-pi) %*% design)
  return(gradient)
}

logit_mle_bfgs <- function(design, outcome){
  p <- ncol(design)
  MLE <- stats::optim(rep(0,p), fn=logit_log_likelihood, gr=logit_gradient_log_likelihood,
                      design=design, outcome=outcome, method="BFGS",
                      control = list(fnscale = -1))
  return(MLE$par)
}

logit_hessian_log_likelihood <- function(beta, design, outcome){
  pi <- as.vector(exp(design%*%beta)/(1+exp(design%*%beta)))
  W <- diag(pi*(1-pi))
  hessian <- t(design)%*%W%*%design
  return(hessian)
}

logit_mle_newton <- function(design, outcome){
  prev_beta <- rep(0, ncol(design))

  t <- 1
  difference <- 1
  epsilon_tol <- ncol(design)/1000

  while(difference>=epsilon_tol & t<=1000){
    pi <- exp(design%*%prev_beta)/(1+exp(design%*%prev_beta))
    next_beta <- prev_beta+solve(logit_hessian_log_likelihood(prev_beta, design, outcome),
                                 t(design)%*%(outcome-pi))
    difference <- abs(logit_log_likelihood(next_beta, design, outcome)-
                        logit_log_likelihood(prev_beta, design, outcome))
    prev_beta <- next_beta
    t <- t+1
  }

  return(prev_beta)
}


