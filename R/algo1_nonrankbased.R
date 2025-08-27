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
                           dataset, 
                           seed = 1469382642, 
                           alpha = 0.1, 
                           varcovar_matrix) {
  
  K <- dim(dataset)[1] # number of rows
  set.seed(seed)
  
  # step 1a =====================================================================
  
  # corr <- 0.1
  # corr_matrix <- (1 - corr) * diag(K) + corr * matrix(1, K, K)
  # variance_vector <- dataset$S^2
  # delta <- diag(variance_vector)
  # 
  # varcovar_matrix <- delta^(1/2) %*% corr_matrix %*% delta^(1/2)
  
  means <- dataset$theta_k
  
  
  library(MASS)
  
  # Generate 100 samples from a bivariate normal distribution
  generate_data <- function(){mvrnorm(n = 1, mu = means, Sigma = varcovar_matrix)}
  
  set.seed(110925)
  thetahat_star <- replicate(B, generate_data(), simplify = "array") %>% t
  
  # step 1b =====================================================================
  
  sd_vector <- dataset$S
  t_star <- apply(
    abs((thetahat_star - rep(means, each = nrow(thetahat_star)))/sd_vector),
    1,
    max)
  
  # step 2 =====================================================================
  
  t_hat <- quantile(t_star, probs = 1 - alpha)
  
  # step 3 =====================================================================
  
  dataset$ci_lower <- means - t_hat*sd_vector
  dataset$ci_upper <- means + t_hat*sd_vector
  
  # # END ------------------------------------------------------------------------
  #
  return(dataset)
}