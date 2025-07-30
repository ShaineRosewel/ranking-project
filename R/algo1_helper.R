library("GoFKernel")

calculate_variance <- function(moe) return((moe/1.645)^2)

kde_cdf_function <- function(datapoints, h) {
  return(function(x) mean(pnorm((x - datapoints) / h)))
}

kde_pdf_function <- function(datapoints,h) {
  function(x) {
    sapply(x, function(xi) {
      mean(dnorm((xi - datapoints) / h) / h)
    })
  }
}


estimate_PDF <- function(i, r, B){
  S = sd(r[,i])
  IQR = quantile(r[,i], .75) - quantile(r[,i], .25)
  h_i <- 0.9*min(S, IQR/1.34)*(B^(-1/5))
  return(kde_pdf_function(r[,i], h_i))
}


empirical_cdf_function <- function(datapoints) {
  function(x) mean(datapoints <= x)
}

estimate_CDF <- function(i, r, B){
  S = sd(r[,i])
  IQR = quantile(r[,i], .75) - quantile(r[,i], .25)
  h_i <- 0.9*min(S, IQR/1.34)*(B^(-1/5))
  #return(empirical_cdf_function(r[,i]))
  return(kde_cdf_function(r[,i], h_i))
}



get_inner_max <- function(value) return(max(value, 1-value))

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

generate_synthetic_dataset <- function(K = 51, seed = 42) {
  set.seed(seed)
  # true parameter values, evenly spaced
  theta_k <- seq(1, 2*K, length.out = K)
  # fixed small standard errors
  S <- rep(0.1, K)
  k <- 1:K
  data.frame(k = k, theta_k = theta_k, S = S)
}

run_algorithm1 <- function(B, dataset, seed = 4, alpha = 0.1) {

  K <- dim(dataset)[1]
  set.seed(seed)

  # step 1 =====================================================================
 
   set.seed(4)
  thetahat_star <- foreach(i = 1:K, .combine = cbind) %do% {
    foreach(b = 1:B, .combine = c) %do% {
      rnorm(1, 
            mean = dataset[i, 'theta_k'], 
            sd = dataset[i, 'S'])
    }
  }
  colnames(thetahat_star) <- paste0("thetahat_star", 
                                    sprintf("%02d", 1:K))
  theta_hat <- dataset$theta_k
  
  # step 2 =====================================================================
  
  sorted_indices <- order(dataset$theta_k) # index when order is increasing
  sorted_dataset <- dataset[sorted_indices, ]
  sorted_theta_hat <- sorted_dataset$theta_k
  sorted_thetahat_star <- t(apply(thetahat_star, 1, sort))
  colnames(sorted_thetahat_star) <- paste0("sorted_thetahat_star", 
                                           sprintf("%02d", 1:K))
  
  # step 3 =====================================================================
  
  rstar <- sorted_thetahat_star - matrix(sorted_theta_hat,B, K, byrow=TRUE)
  #rstar <- sweep(sorted_thetahat_star, 2, colMeans(sorted_thetahat_star), FUN = "-")
  colnames(rstar) <- paste0("rstar", sprintf("%02d", 1:K))
  
  # step 4 =====================================================================
  
  Fhat <- lapply(1:K, function(x)(estimate_CDF(x, rstar, B)))
  
  # step 5 =====================================================================
  
  Ystar <- foreach(i = 1:K, .combine = cbind) %do% {
    sapply(rstar[,i], Fhat[[i]])
  }
  colnames(Ystar) <- paste0("Ystar", sprintf("%02d", 1:K))
  
  # step 6 =====================================================================
  Ustar <- apply(
    apply(Ystar,
          MARGIN = c(1, 2),
          FUN = get_inner_max),
    1,
    max)
  # Ustar <- apply(Ystar, 1, function(row) {
  #   quantile(sapply(row, get_inner_max), probs = 0.9)
  # })
  
  # step 7 =====================================================================
  
  uhat <- quantile(Ustar, probs = 1 - alpha)
  
  # step 8 =====================================================================
  
  Fhat.inv <- lapply(Fhat, 
                     function(F) inverse(F, lower = -100, upper = 100))
  sorted_dataset$Fhat.inv_u    <- sapply(1:K, 
                                         function(i) Fhat.inv[[i]](uhat))
  sorted_dataset$Fhat.inv_1_u  <- sapply(1:K, 
                                         function(i) Fhat.inv[[i]](1 - uhat))
  
  sorted_dataset$kde_ci_lower <- sorted_theta_hat - sorted_dataset$Fhat.inv_u
  sorted_dataset$kde_ci_upper <- sorted_theta_hat - sorted_dataset$Fhat.inv_1_u
  sorted_dataset$original_index <- sorted_indices
  
  interval_table <- sorted_dataset %>%
    select(k = k,
           theta_k = theta_k,
           kde_ci_lower = kde_ci_lower,
           kde_ci_upper = kde_ci_upper,
           original_index = original_index)

  # END ------------------------------------------------------------------------

  return(list(interval_table = interval_table, 
              Finv_u = sorted_dataset$Fhat.inv_u,
              Finv_1_u = sorted_dataset$Fhat.inv_1_u,
              sorted_thetahat = sorted_theta_hat,
              sorted_indices = sorted_indices,
              Fhat = Fhat,
              uhat = uhat
              ))
}