library("GoFKernel")

calculate_variance <- function(moe) return((moe/1.645)^2)

kde_cdf_function <- function(datapoints, bandwidth) {
  return(function(x) mean(pnorm((x - datapoints) / bandwidth)))
}

estimate_CDF <- function(dataset, i, r, B){
  S = sd(r[,i])
  IQR = quantile(r[,i], .75) - quantile(r[,i], .25)
  h_i <- 0.9*min(S, IQR/1.34)*(B^(-1/5))
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

get_t1 <- function(v){mean(v)}

get_t2 <- function(v){prod(v)^(1/length(v))}

get_t3 <- function(v){1 - ((length(v)+sum(v))/(length(v)^2))}

run_algorithm1 <- function(B, dataset, seed = 4, alpha = 0.1) {
  #dataset['variance'] <- lapply(dataset['moe_k'], calculate_variance)
  K <- dim(dataset)[1]
  set.seed(seed)
  # step 1
  mat1 <- foreach(i = 1:K, .combine = cbind) %do% {
    foreach(b = 1:B, .combine = c) %do% {
      # rnorm(1, mean = dataset[i, 'theta_k'], sd = sqrt(dataset[i, 'variance']))
      rnorm(1, mean = dataset[i, 'theta_k'], sd = dataset[i, 'S'])
    }
  }
  # step 2
  mat1_sorted <- t(apply(mat1, 1, sort))
  # step 3
  r <- mat1_sorted - matrix(sort(t(dataset['theta_k'])),B, K, byrow=TRUE)
  # step 4
  estimated_CDF <- lapply(1:K, function(x)(estimate_CDF(dataset, x, r, B)))
  # step 5
  Y <- foreach(i = 1:K, .combine = cbind) %do% {
    sapply(r[,i], estimated_CDF[[i]])
  }
  # step 6
  U <- apply(apply(Y, MARGIN = c(1, 2), FUN = get_inner_max), 1, max)
  # step 7
  uhat <- quantile(U, probs = 1 - alpha)
  # step 8
  F.inv <- lapply(estimated_CDF, function(F){inverse(F, lower = -100, upper = 100)})
  dataset['F.inv_u'] <- sapply(1:K, function(i){F.inv[[i]](uhat)})
  dataset['F.inv_1-u'] <- sapply(1:K, function(i){F.inv[[i]](1-uhat)})
  dataset['kde_ci_lower'] <- dataset['theta_k'] - dataset['F.inv_u'] 
  dataset['kde_ci_upper'] <- dataset['theta_k'] - dataset['F.inv_1-u']
  return(dataset[, c('rhat_k', 'k', 'theta_k', 'kde_ci_lower', 'kde_ci_upper')])
}