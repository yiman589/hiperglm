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



