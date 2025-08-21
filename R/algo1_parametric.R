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

get_t2 <- function(v) prod(v)^(1/length(v))

run_algorithm1 <- function(B, dataset, seed = 1469382642, alpha = 0.1) {
  
  K <- dim(dataset)[1] # number of rows
  set.seed(seed)
  
  # step 1a =====================================================================
  
  thetahat_star <- foreach(i = 1:K, .combine = cbind) %do% {
    foreach(b = 1:B, .combine = c) %do% {
      rnorm(1, mean = dataset[i, 'theta_k'], sd = dataset[i, 'S'])}}
  
  colnames(thetahat_star) <- paste0("thetahat_star", 
                                    sprintf("%02d", 1:K))
  
  sorted_thetahat_star <- t(apply(thetahat_star, 1, sort))
  colnames(sorted_thetahat_star) <- paste0("sorted_thetahat_star", 
                                           sprintf("%02d", 1:K))
  
  # step 1b =====================================================================
  
  variance_vector <- dataset$S^2
  minuend <- thetahat_star^2 + rep(variance_vector, each = nrow(thetahat_star))
  sigma_hat_star <- sqrt(t(apply(minuend, 1, sort)) - sorted_thetahat_star^2)
  
  # step 1c =====================================================================
  
  sorted_theta_hat <- sort(dataset$theta_k)
  
  t_star <- apply(abs((sorted_thetahat_star - rep(sorted_theta_hat, each = nrow(sorted_thetahat_star)))/sigma_hat_star), 1, max)
  
  # step 2 =====================================================================
  
  t_hat <- quantile(t_star, probs = 1 - alpha)
  
  # step 3 =====================================================================
  
  theta_hat <- dataset$theta_k
  
  sigma_hat <- sqrt(sort(theta_hat^2 + variance_vector) - sorted_theta_hat^2)
  
  
  
  sorted_theta_hat + t_hat*sigma_hat
  
  # step 6 =====================================================================
  sorted_indices <- order(dataset$theta_k) # sorted in increasing theta.h
  sorted_dataset <- dataset[sorted_indices, ]
  sorted_dataset$ci_lower <- sorted_theta_hat - t_hat*sigma_hat
  sorted_dataset$ci_upper <- sorted_theta_hat + t_hat*sigma_hat

  
  # # END ------------------------------------------------------------------------
  #
  return(sorted_dataset)
}