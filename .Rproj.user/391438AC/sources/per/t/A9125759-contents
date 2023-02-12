#' @export
hiper_glm <- function(design, outcome, model = "linear"){
  supported_model <- c("linear", "logit")
  if(!(model %in% supported_model)){
    stop(sprintf(""))
  }
  # TODO: find MLE.
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}

