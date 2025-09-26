source("../../R/compute_ci.R")
library("doRNG")

get_ranks <- function(k, tuple_list){
  Lambda_lk <- which(
    tuple_list[,2]<=tuple_list[k,1])
  Lambda_lk <- Lambda_lk[Lambda_lk != k]
  Lambda_Ok <- which(
    tuple_list[,2]>tuple_list[k,1] & tuple_list[k,2] > tuple_list[,1])
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

get_t3 <- function(v) {
  1 - ((length(v)+sum(v))/(length(v)^2))
}

get_coverage <- function(ci_lower,
                         ci_upper,
                         true_theta) {
  return(all(ci_lower<=true_theta) & all(true_theta<=ci_upper))
}

algo2_nonrankbased <- function(
    true_theta,
    K, 
    reps = 5, # step 4
    B=100, 
    alpha= 0.10,
    varcovar_matrix){
  foreach(iter = 1:reps, 
          .combine = rbind,
          .packages = c("foreach", "arrow", "MASS"),
          .export = c("get_nonrankbased_ci", "get_independent_ci",
                      "get_bonferroni_ci", "get_ranks", "get_coverage",
                      "get_t1", "get_t2", "get_t3")
  ) %dorng% {
    
    # step 1 =======
    theta_hat <- mvrnorm(n = 1, 
                         mu = true_theta, 
                         Sigma = varcovar_matrix)
    
    # step 2 =======
    S <- sqrt(diag(varcovar_matrix))
    
    ci_methods <- list(
      nonrankbased = function() get_nonrankbased_ci(B, theta_hat, alpha, 
                                                    varcovar_matrix),
      independent  = function() get_independent_ci(theta_hat, S, alpha),
      bonferroni   = function() get_bonferroni_ci(theta_hat, S, alpha)
    )
    
    ci_results <- lapply(ci_methods, function(f) f())
    
    coverages <- lapply(ci_results, function(res) {
      get_coverage(
        ci_lower   = res$ci_lower,
        ci_upper   = res$ci_upper,
        true_theta = true_theta
      )
    })
    
    process_ci_result <- function(result, K) {
      tuple_list <- t(apply(
        data.frame(
          ci_lower = result$ci_lower,
          ci_upper = result$ci_upper
        ), 
        1, 
        function(row) as.numeric(row)
      ))
      
      rank_range_length <- sapply(1:K, function(x) 
        length(get_ranks(x, tuple_list)$ranks)
      )
      
      list(
        t1 = get_t1(rank_range_length),
        t2 = get_t2(rank_range_length),
        t3 = get_t3(rank_range_length)
      )
    }
    
    processed <- lapply(ci_results, process_ci_result, K = K)

    data.frame(
      t1_nonrankbased = processed$nonrankbased$t1,
      t2_nonrankbased = processed$nonrankbased$t2,
      t3_nonrankbased = processed$nonrankbased$t3,
      coverage_nonrankbased = coverages$nonrankbased,
      t1_independent = processed$independent$t1,
      t2_independent = processed$independent$t2,
      t3_independent = processed$independent$t3,
      coverage_independent = coverages$independent,
      t1_bonferroni = processed$bonferroni$t1,
      t2_bonferroni = processed$bonferroni$t2,
      t3_bonferroni = processed$bonferroni$t3,
      coverage_bonferroni = coverages$bonferroni
      )
  }
}


algo2_parametric <- function(
    true_theta,
    K, 
    reps = 5, # step 4
    B=100, 
    alpha= 0.10,
    S){
  foreach(iter = 1:reps, 
          .combine = rbind,
          .packages = c("foreach", "arrow", "MASS"),
          .export = c("get_parametric_ci","get_ranks", "get_coverage",
                      "get_t1", "get_t2", "get_t3")
  ) %dorng% {
    
    # step 1 =======
    theta_hat <- rnorm(
      n    = K,
      mean = true_theta,
      sd   = S
    )
    
    # step 2 =======
    result <- get_parametric_ci(B,
                                theta_hat,
                                S,
                                alpha)
    
    # step 3 =======
    sorted_true_theta <- sort(true_theta)
    coverage <- get_coverage(ci_lower = result$ci_lower,
                             ci_upper = result$ci_upper,
                             true_theta = sorted_true_theta)
    
    tuple_list <- t(apply(
      data.frame(ci_lower = result$ci_lower,
                 ci_upper = result$ci_upper), 1, function(row) as.numeric(row)))
    rank_range_length <- sapply(1:K, function(x) length(
      get_ranks(x, tuple_list)$ranks))
    t1 <- get_t1(rank_range_length)
    t2 <- get_t2(rank_range_length)
    t3 <- get_t3(rank_range_length)
    
    data.frame(
      t1_parametric = t1,
      t2_parametric = t2,
      t3_parametric = t3,
      coverage_parametric = coverage
    )
  }
}

