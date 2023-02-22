linear_mle_pseudo_inv <- function(design, outcome){
  MLE <- solve(t(design)%*%design,t(design)%*%outcome)
  return(MLE)
}

linear_log_likelihood <- function(MLE, design, outcome, noise_var = 1){
  n <- length(outcome)
  log_lik <- -1/(2*noise_var)*sum((outcome-design%*%MLE)^2)
  return(log_lik)
}

linear_gradient <- function(MLE, design, outcome, noise_var = 1){
  gradient <- -1/noise_var*(t(design)%*%design%*%MLE-t(design)%*%outcome)
  return(gradient)
}

linear_mle_bfgs <- function(design, outcome, noise_var = 1){
  p <- ncol(design)
  MLE <- stats::optim(rep(0,p), fn=linear_log_likelihood, gr=linear_gradient,
                      design=design, outcome=outcome, noise_var=noise_var, method="BFGS",
                      control = list(fnscale = -1))
  return(MLE$par)
}

approx_grad <- function(func, x, design, outcome, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for(i in 1:length(x)){
    delta_x <- rep(0,length(x))
    delta_x[i] <- dx
    numerical_grad[i] <- (func(x+delta_x, design, outcome)-func(x-delta_x, design, outcome))/(2*dx)
  }
  return(numerical_grad)
}

