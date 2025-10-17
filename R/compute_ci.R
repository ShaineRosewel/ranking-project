get_ci_independent <- function(theta_hat,
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

get_ci_bonferroni <- function(theta_hat,
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

get_ci_rankbased_asymptotic <- function(B,
                                        theta_hat,
                                        varcovar_matrix,
                                        alpha) {
  print("ci_rankbased_asymptotic ===================================")
  K <- length(theta_hat)
  
  generate_data <- function(){MASS::mvrnorm(n = 1,
                                            mu = theta_hat,
                                            Sigma = varcovar_matrix)}
  thetahat_star <- t(replicate(B, generate_data()))
  print(dim(thetahat_star))
  sorted_thetahat_star <- t(apply(thetahat_star, 1, sort))
  print(dim(sorted_thetahat_star))
  # step 1b ====================================
  print("printing S")
  variance_vector <- diag(varcovar_matrix)
  print(variance_vector)
  minuend <- thetahat_star^2 + matrix(variance_vector, B, K, byrow = TRUE)
    #(matrix(rep(variance_vector, each = B),nrow = B, byrow = FALSE))
  print("minuend")
  print(dim(minuend))
  print("sorted minuend shape")
  print(dim(t(apply(minuend, 1, sort))))
  sigma_hat_star <- sqrt(
    t(apply(minuend, 1, sort)) - sorted_thetahat_star^2)
  # step 1c ====================================
  sorted_theta_hat <- sort(theta_hat)
  print("sorted theta hat")
  print(sorted_theta_hat)
  print((matrix(rep(sorted_theta_hat, each = B),
                nrow = B, byrow = FALSE)))
  t_star <- apply(
    abs(
      (
        sorted_thetahat_star - matrix(sorted_theta_hat, B, K, byrow = TRUE)
        # (matrix(rep(sorted_theta_hat, each = B),
        #                               nrow = B, byrow = FALSE))
        )/sigma_hat_star
      ),
    1,
    max)
  print("sigma_hat_star")
  print(sigma_hat_star)
  
  print("t_star")
  print(t_star)
  
  # step 2 =====================================
  t_hat <- quantile(t_star, probs = 1 - alpha)
  # step 3 =====================================
  sigma_hat <- sqrt(
    sort(theta_hat^2 + variance_vector) - sorted_theta_hat^2)
  # step 6 =====================================
  ci_lower <- sorted_theta_hat - t_hat*sigma_hat
  ci_upper <- sorted_theta_hat + t_hat*sigma_hat
  
  print(ci_lower)
  print(ci_upper)
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_ci_rankbased_level2bs <- function(B,
                                   C,
                                   theta_hat,
                                   varcovar_matrix,
                                   alpha) {
  print("ci_rankbased_level2bs ===================================")
  K <- length(theta_hat)
  sorted_theta_hat <- sort(theta_hat)
  # step 1a ====================================
  generate_data <- function(){MASS::mvrnorm(n = 1,
                                            mu = theta_hat,
                                            Sigma = varcovar_matrix)}
  # B x K matrix
  thetahat_star <- t(replicate(B, generate_data())) # B x K
  sorted_thetahat_star <- t(apply(thetahat_star, 1, sort)) # B x K
  print(dim(thetahat_star))
  print("ci_rankbased_level2bs 1 **********************************")
  # level 2 bs ====================================
  # level 1: B rows with K columns, 
  # for each b row in level 1, generate a K x C matrix, 
  
  # returns one vector of sample
  generate_level2_data <- function(mu){MASS::mvrnorm(n = 1,
                                                     mu,
                                                     Sigma = varcovar_matrix)}
  print(dim(replicate(C, # expected is a K x C matrix
                      generate_level2_data(mu=thetahat_star[1,]))))
  thetahat_double_star <- # list of length B, each a K x C matrix
    apply(thetahat_star,
          1,
          function(thetahat_b) {replicate(C, # expected is a K x C matrix
                                          generate_level2_data(mu=thetahat_b))},
          simplify = FALSE
          )
  print(length(thetahat_double_star))
  print(dim(thetahat_double_star[[1]]))
  print("ci_rankbased_level2bs 2 **********************************")
  sorted_thetahat_double_star <- lapply(thetahat_double_star, 
                                        function(x)apply(x, 2, sort))
  print(dim(sorted_thetahat_double_star[[1]]))
  print("ci_rankbased_level2bs 3 **********************************")
  # for each matrix b and for each row k
  compute_sigma_hat <- function(mat, n){apply(mat, 
                                              1, 
                                              function(row){
                                                sum((row - mean(row))^2)/(n-1)
                                              })}
  
  # output must be B lists, each a vector of length K
  sigma_hat_star <- lapply(sorted_thetahat_double_star, 
                           function(x) compute_sigma_hat(x, C)
                           )
  print(length(sigma_hat_star))
  print(length(sigma_hat_star[[1]]))
  print("ci_rankbased_level2bs 4 **********************************")
  compute_max <- function(b) {
    t_b <- abs(
      (sorted_thetahat_star[b, ] - sorted_theta_hat) /
        sigma_hat_star[[b]]
    )
    max(t_b)
  }
  print("ci_rankbased_level2bs 5 **********************************")
  t_star <- sapply(1:B, compute_max)
  print(t_star)
  print(dim(t_star))
  # step 2 =====================================
  print("ci_rankbased_level2bs 6 **********************************")
  t_hat <- quantile(t_star, probs = 1 - alpha)
  # step 3 =====================================
  sigma_hat <- compute_sigma_hat(t(sorted_thetahat_star),B)
  # step 6 =====================================
  ci_lower <- sorted_theta_hat - t_hat*sigma_hat
  ci_upper <- sorted_theta_hat + t_hat*sigma_hat
  print(ci_lower)
  print(ci_upper)
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_ci_nonrankbased <- function(B, 
                                theta_hat,
                                alpha, 
                                varcovar_matrix) {
  print("ci_nonrankbased ===================================")
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