get_independent_ci <- function(theta_hat,
                               S,
                               alpha){
  K <- length(theta_hat)
  gamma = 1-(1-alpha)^(1/K)
  z = qnorm(1-gamma/2)
  ci_lower <- theta_hat - z*S
  ci_upper <- theta_hat + z*S
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_bonferroni_ci <- function(theta_hat,
                              S,
                              alpha){
  K <- length(theta_hat)
  z = qnorm(1-(alpha/K)/2)
  ci_lower <- theta_hat - z*S
  ci_upper <- theta_hat + z*S
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_parametric_ci <- function(B,
                              theta_hat,
                              S,
                              alpha) {
  K <- length(theta_hat)
  # step 1a ====================================
  thetahat_star <- sapply(seq_len(K), function(i) {
    rnorm(B, mean = theta_hat[i], sd = S[i])
  })
  colnames(thetahat_star) <- paste0("thetahat_star", 
                                    sprintf("%02d", 1:K))
  sorted_thetahat_star <- t(apply(thetahat_star, 1, sort))
  colnames(sorted_thetahat_star) <- paste0("sorted_thetahat_star", 
                                           sprintf("%02d", 1:K))
  # step 1b ====================================
  variance_vector <- S^2
  minuend <- thetahat_star^2 + rep(
    variance_vector, each = nrow(thetahat_star))
  sigma_hat_star <- sqrt(
    t(apply(minuend, 1, sort)) - sorted_thetahat_star^2)
  # step 1c ====================================
  sorted_theta_hat <- sort(theta_hat)
  t_star <- apply(
    abs(
      (
        sorted_thetahat_star - rep(
          sorted_theta_hat,
          each = nrow(sorted_thetahat_star)
          )
        )/sigma_hat_star
      ),
    1, 
    max)
  # step 2 =====================================
  t_hat <- quantile(t_star, probs = 1 - alpha)
  # step 3 =====================================
  sigma_hat <- sqrt(
    sort(theta_hat^2 + variance_vector) - sorted_theta_hat^2)
  # step 6 =====================================
  ci_lower <- sorted_theta_hat - t_hat*sigma_hat
  ci_upper <- sorted_theta_hat + t_hat*sigma_hat
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_nonrankbased_ci <- function(B, 
                                theta_hat,
                                alpha, 
                                varcovar_matrix) {
  K <- length(theta_hat)
  # step 1a ===================================
  generate_data <- function(){MASS::mvrnorm(n = 1,
                                      mu = theta_hat,
                                      Sigma = varcovar_matrix)}
  thetahat_star <- t(replicate(B, generate_data()))
  # step 1b ===================================
  t_star <- apply(thetahat_star, 
                  1, 
                  function(x) max(abs((x - theta_hat) / sqrt(
                    diag(varcovar_matrix)))))  
  # step 2 ====================================
  t_hat <- quantile(t_star, probs = 1 - alpha)
  # step 3 ====================================
  ci_lower <- theta_hat - t_hat*sqrt(diag(varcovar_matrix))
  ci_upper <- theta_hat + t_hat*sqrt(diag(varcovar_matrix))
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}