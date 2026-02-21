#3:37PM
print(getwd())
source("R/compute_metrics.R")
library(doParallel)
library(foreach)
library(tictoc)
mean <- 23.8
df <- readRDS("data/mean_travel_time_ranking_2011.rds")
cl=parallel::makeCluster(15)
registerDoParallel(cl)

sds <- c(2, 3.6, 6)
Ks <- c(10)
#corrs <- c(0.1,0.5,0.9)
alphas <- c(0.05)#c(0.05, 0.1, 0.15, 0.2)


blocks <- list(1:3, 4:6, 7:10)          # 3 blocks of sizes 3, 3, 4
within_corrs <- c(0.1, 0.5, 0.9)        # one correlation per block
between_corr <- 0.1                     # correlation between blocks
corr_matrix <- matrix(between_corr, Ks, Ks)
for (i in seq_along(blocks)) {
  block <- blocks[[i]]
  corr_matrix[block, block] <- within_corrs[i]
}
diag(corr_matrix) <- 1


for (sd in sds) {
  for (K in Ks) {
    set.seed(123974)
    true_theta <- rnorm(K, mean, sd)
    SE <- df$S[1:K]
    
    for (alpha in alphas) {
      
      tic()
      # for (corr in corrs) {
        #corr_matrix <- (1 - corr) * diag(K) + corr * matrix(1, K, K)
        variance_vector <- SE^2
        delta <- diag(variance_vector)
        varcovar_matrix <- delta^(1/2) %*% corr_matrix %*% delta^(1/2)
        
        cat("==========================================\n\n")
        cat("SIMULATION SETTINGS ~ K:", K,"corr:", "block","sd:", sd, "corr:", "block", "alpha:", alpha)
        cat("\n")
        cat("SORTED TRUE THETA", sort(true_theta))
        cat("\n")
        case_start <- Sys.time()
        
        tic()
        coverage_output_df <- implement_algorithm2(
          true_theta,
          K, 
          reps = 5000, 
          B = 600, 
          alpha=alpha,
          C = 300,
          varcovar_matrix = varcovar_matrix)
        toc()
        
        # saveRDS(coverage_output_df,  paste0("output/final_coverage_",
        saveRDS(coverage_output_df,  paste0("output/blockcorr_coverage_",
                                            K,"_", sd, "_", "block", "_", 
                                            alpha, ".rds"))
        case_end <- Sys.time()
        case_runtime <- as.numeric(difftime(case_end, case_start, units = "mins"))
        cat("Finished at:", format(case_end, "%I:%M %p"), "\n")
        cat("Runtime for this case:", round(case_runtime, 2), "minutes\n")
        cat("==========================================\n\n")
      # }
    }
  }
}

stopCluster(cl)
# 
# param_grid <- expand.grid(K = Ks, sd = sds, corr = corrs, alpha = alphas)
# 
# results <- do.call(rbind, lapply(seq_len(nrow(param_grid)), function(i) {
#   K <- param_grid$K[i]
#   sd <- param_grid$sd[i]
#   corr <- param_grid$corr[i]
#   alpha <- param_grid$alpha[i]
#   
#   a <- readRDS(paste0("output/TEST_coverage_", 
#                       K, "_", sd, "_", corr, "_", alpha, ".rds"))
#   
#   data.frame(
#     K = K,sd = sd,corr = corr,alpha = alpha,
#     
#     Cov_nonrankbased = mean(a$coverage_nonrankbased), 
#     Cov_rankbased_asymptotic = mean(a$coverage_rankbased_asymptotic),
#     Cov_rankbased_level2bs = mean(a$coverage_rankbased_level2bs),    
#     Cov_independent = mean(a$coverage_independent),
#     Cov_bonferroni = mean(a$coverage_bonferroni),
# 
#     T1_nonrankbased = mean(a$t1_nonrankbased),
#     T1_rankbased_asymptotic = mean(a$t1_rankbased_asymptotic),
#     T1_rankbased_level2bs = mean(a$t1_rankbased_level2bs),
#     T1_independent = mean(a$t1_independent),
#     T1_bonferroni = mean(a$t1_bonferroni),
# 
#     T2_nonrankbased = mean(a$t2_nonrankbased),
#     T2_rankbased_asymptotic = mean(a$t2_rankbased_asymptotic),
#     T2_rankbased_level2bs = mean(a$t2_rankbased_level2bs),
#     T2_independent = mean(a$t2_independent),
#     T2_bonferroni = mean(a$t2_bonferroni),
# 
#     T3_nonrankbased = mean(a$t3_nonrankbased),
#     T3_rankbased_asymptotic = mean(a$t3_rankbased_asymptotic),
#     T3_rankbased_level2bs = mean(a$t3_rankbased_level2bs),
#     T3_independent = mean(a$t3_independent),
#     T3_bonferroni = mean(a$t3_bonferroni)
#   )
# }))
# 
# # param_grid <- expand.grid(K = Ks, sd = sds, alpha = alphas)
# # 
# # results1 <- do.call(rbind, lapply(seq_len(nrow(param_grid)), function(i) {
# #   K <- param_grid$K[i]
# #   sd <- param_grid$sd[i]
# #   alpha <- param_grid$alpha[i]
# #   
# #   a <- readRDS(paste0("output/coverage_parametric_", 
# #                       K, "_", sd, "_", alpha, ".rds"))
# #   
# #   data.frame(
# #     K = K,
# #     sd = sd,
# #     alpha = alpha,
# #     Cov_parametric = mean(a$coverage_parametric),
# #     T1_parametric = mean(a$t1_parametric),
# #     T2_parametric = mean(a$t2_parametric),
# #     T3_parametric = mean(a$t3_parametric)
# #   )
# # }))
# # 
# # save(results, results1, file = "simulation_results.RData")
