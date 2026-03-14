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
      lapply(list("2_balanced", "2_unbalanced", "3_unbalanced-low", "3_unbalanced-high"),
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

preprocess_coverage <- function(dataset, 
                                sd_value, 
                                equicorrelation = TRUE,
                                unordered = TRUE){
  
  if (unordered) {
    deselected_columns <- c('rankbased_asymptotic', 'rankbased_level2bs', 'metric', 'sd')
    selected_columns <- c('independent','bonferroni', 'nonrankbased')
  } else {
    deselected_columns <- c('independent','bonferroni', 'nonrankbased', 'metric', 'sd')
    selected_columns <- c('rankbased_asymptotic', 'rankbased_level2bs')
  }

  partial <- dataset %>%
    filter(sd == sd_value, metric == "coverage") %>% 
    select(!all_of(deselected_columns))
  if (equicorrelation) {
    final <- partial %>%
      select(c(K, r, selected_columns))
  } else {
    final <- partial %>%
      select(c(K, selected_columns))
  }
  
  return(
     final
  )
} 



preprocess_tightness_measure <- function(dataset, metric_type, equicorrelation,
                                         unordered = TRUE){
  if (equicorrelation) {
    vector1 <- c('K', 'r')
  } else {
    vector1 <- c('K', 'r', 'blocks')
  }
  
  levels <- c('low', 'med', "high")
  if (unordered) {
    selected_columns <- c('independent', 'bonferroni', 'nonrankbased')
  } else {
    selected_columns <- c('rankbased_asymptotic', 'rankbased_level2bs')
  }
  
  
  return(
    dataset %>% 
      filter(metric == metric_type) %>% 
      mutate(variability = ifelse(sd==2.0, 
                                  'low', 
                                  ifelse(sd == 3.6, 'med', 'high'))) %>%
      select(
        c(vector1, c(variability, selected_columns)
          )
        ) %>%
      tidyr::pivot_wider(names_from = variability, 
                         values_from = selected_columns, 
                         names_sep = "_") %>%
      select(
        c(vector1, unlist(lapply(levels, 
                                 function(x) {paste(selected_columns, 
                                                    x, 
                                                    sep = '_')}))
          )
        )
  )
} 

