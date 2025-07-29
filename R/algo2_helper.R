get_iter_misses <- function(K, file_list, sorted_theta){
  coverage_output_df <- lapply(file_list, function(file) {
    df <- read_feather(file)
    df$theta <- sorted_theta
    df$filename <- basename(file)
    return(df)
  }) %>%
    bind_rows()
  
  subs <- coverage_output_df[,c('k', "theta_k", 'theta',"kde_ci_lower", "kde_ci_upper", "filename")]

  subs$theta_within <- (subs$theta >= subs$kde_ci_lower) & (subs$theta <= subs$kde_ci_upper)
  subs$theta_k_within <- (subs$theta_k >= subs$kde_ci_lower) & (subs$theta_k <= subs$kde_ci_upper)
  return(subs %>% select(c('k', "theta_k", "theta", "kde_ci_lower", "kde_ci_upper", "theta_within", "filename"))# %>% filter(!theta_within))
  )
}