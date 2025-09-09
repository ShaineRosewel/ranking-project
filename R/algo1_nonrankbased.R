library(MASS)

get_ranks <- function(k, tuple_list){
  Lambda_lk <- which(tuple_list[,2]<=tuple_list[k,1])
  Lambda_lk <- Lambda_lk[Lambda_lk != k]
  Lambda_Ok <- which(tuple_list[,2]>tuple_list[k,1] & tuple_list[k,2] > tuple_list[,1])
  Lambda_Ok <- Lambda_Ok[Lambda_Ok != k]
  
  ranks <- seq(
    length(unique(Lambda_lk)) + 1,
    length(unique(Lambda_lk)) + length(unique(Lambda_Ok)) + 1,
    1
  )
  
  return(list(
    ranks = ranks,
    Lambda_Ok = Lambda_Ok
  ))
}

get_t1 <- function(v) mean(v)

get_t2 <- function(v) prod(v)^(1/length(v))

get_t3 <- function(v) 1 - ((length(v)+sum(v))/(length(v)^2))

run_algorithm1 <- function(B, 
                           theta_hat,
                           alpha = 0.1, 
                           varcovar_matrix) {
  
  K <- length(theta_hat) # number of rows
  # print(K)
 # set.seed(seed)
  
  # step 1a =====================================================================


  generate_data <- function(){mvrnorm(n = 1,
                                      mu = theta_hat,
                                      Sigma = varcovar_matrix)}
  
  
  thetahat_star <- t(replicate(B, generate_data()))
  # print(dim(thetahat_star))
  # step 1b =====================================================================

  t_star <- apply(thetahat_star, 
                  1, 
                  function(x) max(abs((x - theta_hat) / sqrt(
                    diag(varcovar_matrix)))))  
  # t_star <- apply(
  #   abs(sweep(thetahat_star, 2, theta_hat, "-") / sqrt(diag(varcovar_matrix))),
  #   1, max
  # )
  # print(length(t_star))

  # step 2 =====================================================================
  
  t_hat <- quantile(t_star, probs = 1 - alpha)
  # print(length(t_hat))
  
  # step 3 =====================================================================
  
  ci_lower <- theta_hat - t_hat*sqrt(diag(varcovar_matrix))
  # print(length(ci_lower))
  ci_upper <- theta_hat + t_hat*sqrt(diag(varcovar_matrix))
  # print(length(ci_upper))
  
  # # END ------------------------------------------------------------------------
  #
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}