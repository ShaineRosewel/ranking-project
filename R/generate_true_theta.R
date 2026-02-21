generate_true_theta <- function()
  df <- readRDS("/home/realiseshewon/PDev/kde-ranking/data/mean_travel_time_ranking_2011.rds")
  mean <- 23.8
  sds <- c(2, 3.6, 6.0)
  Ks <- c(10, 20, 30, 40, 50)
  # corr <- c(0.1, 0.5, 0.9)
  data_list <- list()
  
  for (sd in sds) {
    for (K in Ks) {
      
      set.seed(123974)
      true_theta <- rnorm(K, mean, sd)
      data_list[[length(data_list) + 1]] <- data.frame(sd = sd, K = K, true_theta = true_theta)
    }}
  
  bplot <- data_list %>% 
    bind_rows() %>% 
    mutate(data_spread = factor(ifelse(sd == 2, "low", 
                                       ifelse(sd == 3.6, "med", "high")),
                                levels =c("low", 'med', 'high'),
                                ordered =TRUE
    ))
  return(bplot)