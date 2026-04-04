generate_corr_mat <- function(r, appdata){
  if (appdata == 'pulse') {
    # cormat <- block_corr(block_sizes=c(2, 12, 10, 2, 11, 27), rho_within=rep(r, 6), rho_between=0.0)
    # cormat <-  block_corr(block_sizes=c(2, 12, 10, 2, 11, 2, 3, 3, 19), rho_within=rep(r, 9), rho_between=0.0)
    cormat <-  block_corr(block_sizes=c(2, 12, 10, 2, 11, 2, 3, 3, 6), rho_within=rep(r, 9), rho_between=0.0)
  } else if (appdata == 'traveltime') {
    cormat <- block_corr(block_sizes=c(15, 11, 11, 7, 7), rho_within=rep(r, 5), rho_between=0.0)
    # cormat <- block_corr(block_sizes=c(13, 17, 9, 12), rho_within=rep(r, 4), rho_between=0.0)
  }
  return(cormat)
}

get_ci_for_app_data <- function(theta_hat = (dataset_all$`Voting For`)/100,
                                true_sds = dataset_all$se,
                                K = dim(df)[1],
                                alpha = 0.05,
                                B = 500,
                                corr_matrix) {
  
  variance_vector <- true_sds^2
  delta <- diag(variance_vector)
  varcovar_matrix <- delta^(1/2) %*% corr_matrix %*% delta^(1/2)
  S <- sqrt(diag(varcovar_matrix))
  
  set.seed(123974999)
  # set.seed(982096)
  ci_methods <- list(
    nonrankbased = function() get_ci_nonrankbased(B = B,
                                                  theta_hat = theta_hat,
                                                  alpha = alpha,
                                                  varcovar_matrix = varcovar_matrix),
    rankbased_asymptotic = function() get_ci_rankbased_asymptotic(B = B,
                                                                  theta_hat = theta_hat,
                                                                  varcovar_matrix = varcovar_matrix,
                                                                  alpha = alpha),
    rankbased_level2bs = function() get_ci_rankbased_level2bs(B = B,
                                                              C = 300,
                                                              theta_hat = theta_hat,
                                                              varcovar_matrix = varcovar_matrix,
                                                              alpha = alpha),
    independent  = function() get_ci_independent(theta_hat = theta_hat, S = S, alpha = alpha),
    bonferroni   = function() get_ci_bonferroni(theta_hat = theta_hat, S = S, alpha = alpha)
  )
  ci_results <- lapply(ci_methods, function(f) f())
  return(ci_results)
}

get_ci_result <- function(result, K, reverse_ranks = FALSE) {
  tuple_list <- t(apply(
    data.frame(
      ci_lower = result$ci_lower,
      ci_upper = result$ci_upper
    ), 
    1, 
    function(row) as.numeric(row)
  ))
  
  sapply(1:K, function(x) {
    if (reverse_ranks) {
      (K-get_ranks(x, tuple_list)$ranks) + 1
    } else {
      get_ranks(x, tuple_list)$ranks
    }
  }
  )
}


get_tmeasures_for_app_data <- function(ci_results, K){
  processed <- lapply(ci_results, process_ci_result, K = K)
  # print("REACHED 4 ===================================")
  res <- data.frame(
    T1_Nonrank = processed$nonrankbased$t1,
    T2_Nonrank = processed$nonrankbased$t2,
    T3_Nonrank = processed$nonrankbased$t3,
    #coverage_nonrankbased = coverages$nonrankbased,
    T1_Asymptotic = processed$rankbased_asymptotic$t1,
    T2_Asymptotic = processed$rankbased_asymptotic$t2,
    T3_Asymptotic = processed$rankbased_asymptotic$t3,
    # coverage_rankbased_asymptotic = coverages$rankbased_asymptotic,
    T1_Level2bs = processed$rankbased_level2bs$t1,
    T2_Level2bs = processed$rankbased_level2bs$t2,
    T3_Level2bs = processed$rankbased_level2bs$t3,
    #coverage_rankbased_level2bs = coverages$rankbased_level2bs,
    T1_Independent = processed$independent$t1,
    T2_Independent = processed$independent$t2,
    T3_Independent = processed$independent$t3,
    #coverage_independent = coverages$independent,
    T1_Bonferroni = processed$bonferroni$t1,
    T2_Bonferroni = processed$bonferroni$t2,
    T3_Bonferroni = processed$bonferroni$t3
    #coverage_bonferroni = coverages$bonferroni
  )
  
  summ <- as.data.frame(t(res))
  summ$names <- rownames(summ)
  rownames(summ) <- NULL
  return(summ %>% rename(Value = 'V1') %>%
           separate(
             col = names,
             into = c("Measure", "Approach"),
             sep = "_",
             extra = "merge"
           ) %>% arrange(Measure, Value) %>%
           pivot_wider(names_from = Approach, values_from = Value) %>% 
           select(c(Measure, Independent, Bonferroni, Nonrank, Asymptotic, Level2bs))
  )
  
}
