#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()){
  supported_model <- c("linear")
  if(!(model %in% supported_model)){
    stop("Model not supported yet.")
  }

  if(model == "linear"){
    if(is.null(option$mle_solver)||(option$mle_solver == "pseudo-inverse")){
      MLE <- linear_mle_pseudo_inv(design, outcome)
    } else if(option$mle_solver == "BFGS"){
      MLE <- linear_mle_bfgs(design, outcome)
    } else {
      stop("Method not supported yet.")
    }
  }

  hglm_out <- list(coef = MLE)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}

