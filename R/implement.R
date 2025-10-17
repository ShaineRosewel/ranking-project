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

sds <- c(2)#, 3.6, 6)
Ks <- c(5)#(51, 40, 30, 20, 10, 5)
corrs <- c(0.1,0.5,0.9)
alphas <- c(0.1)#c(0.05, 0.1, 0.15, 0.2)

for (sd in sds) {
  for (K in Ks) {
    set.seed(123974)
    true_theta <- rnorm(K, mean, sd)
    true_sds <- df$S[1:K]
    
    for (alpha in alphas) {
      
      # tic("Running parametric...")
      # coverage_parametric_df <- algo2_parametric(true_theta,
      #                                            K, 
      #                                            reps = 5000,
      #                                            B=500, 
      #                                            alpha= alpha,
      #                                            S=true_sds)
      # toc()
      # saveRDS(coverage_parametric_df,  paste0("output/coverage_parametric_",
      #                                         K,"_", sd, "_", alpha, ".rds"))
      for (corr in corrs) {
        corr_matrix <- (1 - corr) * diag(K) + corr * matrix(1, K, K)
        variance_vector <- true_sds^2
        delta <- diag(variance_vector)
        varcovar_matrix <- delta^(1/2) %*% corr_matrix %*% delta^(1/2)
        
        print("CASE")
        cat("corr:", corr, "\n")
        
        print("SORTED TRUE THETA")
        print(sort(true_theta))
        tic()
        coverage_output_df <- implement_algorithm2(
          true_theta,
          K, 
          reps = 500, 
          B = 1000, 
          alpha=alpha,
          C = 300,
          varcovar_matrix = varcovar_matrix)
        toc()
        
        saveRDS(coverage_output_df,  paste0("output/TEST_coverage_",
                                            K,"_", sd, "_", corr, "_", 
                                            alpha, ".rds"))
      }
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
