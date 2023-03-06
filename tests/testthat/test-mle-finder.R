test_that("linalg and optim least-sq coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = "linear")
  via_bfgs_out <- hiper_glm(
    design, outcome, model = "linear", option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_linalg_out), coef(via_bfgs_out), abs_tol = 1e-6, rel_tol = 1e-6
  ))
})

test_that("gradient calculation by comparing it against a numerical one (linear)", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_analytical_out <- linear_gradient_log_likelihood(rep(1,n_pred), design, outcome)
  via_numerical_out <- approx_grad(linear_log_likelihood, rep(1,n_pred), design, outcome)
  expect_true(are_all_close(
    via_analytical_out, via_numerical_out, abs_tol = 1e-6, rel_tol = 1e-6
  ))
})

test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "logit", seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = 'logit')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = "logit", option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("gradient calculation by comparing it against a numerical one (logit)", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "logit", seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_analytical_out <- logit_gradient_log_likelihood(rep(1,n_pred), design, outcome)
  via_numerical_out <- approx_grad(logit_log_likelihood, rep(1,n_pred), design, outcome)
  expect_true(are_all_close(
    via_analytical_out, via_numerical_out, abs_tol = 1e-6, rel_tol = 1e-6
  ))
})
