library(arrow)
library(dplyr)
library(ggplot2)

prepare_misses <- function(file_input_path, 
                        K = 10, 
                        feather_pattern = "\\.feather$", 
                        df_path = "../data/mean_travel_time_ranking_2011.rds"
                        ) {

  file_list <- list.files(path = file_input_path, 
                          pattern = feather_pattern, full.names = TRUE)

  df <- readRDS(df_path)
  dataset_or <- df[order(df$k), ][1:K, ]
  

  coverage_output_df <- lapply(file_list, function(file) {
    df_tmp <- read_feather(file)
    colnames(df_tmp)[colnames(df_tmp) == 'theta_k'] <- 'thetahat'
    df_tmp$theta <- dataset_or$theta_k[df_tmp$original_index]
    df_tmp$filename <- basename(file)
    return(df_tmp)
  }) %>% bind_rows()
  

  misses <- coverage_output_df[, c('k', 'theta', "kde_ci_lower", 
                                   "kde_ci_upper", "filename")]
  misses$theta_within <- (
    misses$theta >= misses$kde_ci_lower) & (
      misses$theta <= misses$kde_ci_upper)
  misses$diff1 <- misses$theta - misses$kde_ci_lower
  misses$diff2 <- misses$theta - misses$kde_ci_upper
  misses$closest_diff <- ifelse(abs(misses$diff1) < abs(misses$diff2), 
                                misses$diff1, misses$diff2)
  misses$closest_diff <- ifelse(!misses$theta_within, misses$closest_diff, 0)
  misses <- misses %>% left_join(df[, c('k', 'S')], by = "k")
  return(misses)
}

plot_misses <- function(misses) {
  ggplot(misses, aes(x = reorder(k, theta), y = closest_diff)) +
    geom_point(aes(
      shape = theta_within,
      color = S
    ), size = 2, alpha = 0.7) +
    scale_shape_manual(values = c("FALSE" = 17, "TRUE" = 16)) +
    scale_color_gradient(low = "yellow", high = "red") +
    theme_bw() +
    labs(
      title = 'How often & far does a theta fall outside CI across reps?',
      shape = "On Target?",
      color = "S",
      x = "State Arranged in Increasing Theta",
      y = "Diff from Closest Bound"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
}
