library(dplyr)


combine_summaries <- function(
    directory_prefix="/home/realiseshewon/PDev/kde-ranking/simulation_results/summaries/summary_k",
    equicorrelation = TRUE){
  if (equicorrelation) {
    combined <- bind_rows(
      lapply(seq(from = 10, to = 50, by = 10),
             function(K) read.csv(
               paste0(directory_prefix, K, ".csv"))))
  } else {
    combined <- bind_rows(
      lapply(list("2_balanced", "2_unbalanced", "3_unbalanced"),
             function(K) {
               dat <- read.csv(paste0(directory_prefix, K, ".csv"))
               dat['blocks'] =  substr(K, start = 1, stop = 1)
               return(dat)
             }
      ))
  }
  combined$X <-NULL
  
  return(combined)
}

preprocess_coverage <- function(dataset, sd_value, equicorrelation = TRUE){
  partial <- dataset %>%
    filter(sd == sd_value, metric == "coverage") %>% 
    select(-c(rankbased_asymptotic, rankbased_level2bs, metric, sd))
  if (equicorrelation) {
    final <- partial %>%
      select(c(K, r, independent, bonferroni, nonrankbased))
  } else {
    final <- partial %>%
      select(c(K, r, blocks ,independent, bonferroni, nonrankbased))
  }
  
  return(
     final
  )
} 


preprocess_tightness_measure <- function(dataset, metric_type){
  return(
  dataset %>% 
    filter(metric == metric_type) %>% 
    mutate(variability = ifelse(sd==2.0, 
                                'low', 
                                ifelse(sd == 3.6, 'med', 'high'))) %>%
    select(c(K, r, variability, independent, bonferroni, nonrankbased)) %>%
    tidyr::pivot_wider(names_from = variability, 
                       values_from = c(independent, bonferroni, nonrankbased), 
                       names_sep = "_") %>%
    select(K, r, 
           independent_low, bonferroni_low, nonrankbased_low,
           independent_med, bonferroni_med, nonrankbased_med,
           independent_high, bonferroni_high, nonrankbased_high)
  )
} 
