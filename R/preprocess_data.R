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


prepare_plotting_data_for_pulse <- function(ci_results, df){
  
  dataset_all <- df
  
  K <- dim(dataset_all)[1]
  dataset_all$`Rank LB_nonrank` <- sapply(
    get_ci_result(result = ci_results$nonrankbased, 
                  K=K, 
                  reverse_ranks = TRUE), 
    min)
  dataset_all$`Rank UB_nonrank` <- sapply(
    get_ci_result(result = ci_results$nonrankbased, 
                  K=K, 
                  reverse_ranks = TRUE),
    max)
  
  dataset_all$`Rank LB_independent` <- sapply(
    get_ci_result(result = ci_results$independent, 
                  K=K, 
                  reverse_ranks = TRUE), 
    min)
  dataset_all$`Rank UB_independent` <- sapply(
    get_ci_result(result = ci_results$independent, 
                  K=K, 
                  reverse_ranks = TRUE), 
    max)
  
  dataset_all$`Rank LB_bonferroni` <- sapply(
    get_ci_result(result = ci_results$bonferroni, 
                  K=K, 
                  reverse_ranks = TRUE), 
    min)
  dataset_all$`Rank UB_bonferroni` <- sapply(
    get_ci_result(result = ci_results$bonferroni, 
                  K=K, 
                  reverse_ranks = TRUE), 
    max)
  
  dataset_all <- dataset_all %>% arrange(desc(`Voting For`))
  
  # df <- readRDS("../data/mean_travel_time_ranking_2011.rds")
  dataset_all$k <- seq(1,K, 1)
  dataset_all$sample_rank <- seq(1,K, 1)
  dataset_all$order_index <- seq(1,K, 1)
  
  dataset_all <- dataset_all %>% pivot_longer(
    cols = starts_with("Rank"), 
    names_to = "type",   
    values_to = "Ranks"
  )%>%
    separate(
      col = type, 
      into = c("Rank", "Approach"),
      sep = "_",  
      extra = "merge"
    ) %>% pivot_wider(
      names_from = Rank,
      values_from = Ranks
    )
  
  to_string_seq <- function(x, y){paste(seq(x, y, 1), collapse=',')}
  
  dat_to_plot <- dataset_all %>% #filter(Approach == 'nonrank') %>%
    select(c(k, `Rank LB`, `Rank UB`, sample_rank, order_index, 
             Candidate, Approach)) %>%
    mutate(string_ranks = mapply(to_string_seq, `Rank LB`, `Rank UB`)) %>%
    separate_rows(string_ranks, sep=",") %>%
    select(c(k, sample_rank, string_ranks, Candidate, Approach))
  
  
  dat_to_plot$highlight0 <- ifelse(
    dat_to_plot$sample_rank == as.numeric(dat_to_plot$string_ranks), 
    "yes", 
    "no")
  
  dsen <- unique(
    dataset_all[dataset_all$DuterTen==1 & dataset_all$Alyansa==0,]$Candidate
    )
  msen <- unique(
    dataset_all[dataset_all$Alyansa==1 & dataset_all$DuterTen==0,]$Candidate
    )
  ksen <- unique(dataset_all[dataset_all$KiBam==1,]$Candidate)
  mksen <- unique(dataset_all[dataset_all$Makabayan==1,]$Candidate)
  dmsen <- unique(
    dataset_all[dataset_all$DuterTen==1 & dataset_all$Alyansa==1,]$Candidate
    )
  
  
  dat_to_plot$highlight1 <- ifelse(
    dat_to_plot$Candidate %in% dsen,
    "DuterTen",
    ifelse(
      dat_to_plot$Candidate %in% msen,
      "Alyansa",
      ifelse(
        dat_to_plot$Candidate %in% ksen,
        "KiBam",
        ifelse(dat_to_plot$Candidate %in% mksen,
               "Makabayan",
               ifelse(
                 dat_to_plot$Candidate %in% dmsen,
                 "DuterTen-Alyansa","None"
                 )
               )
        )
      )
    )

  return(dat_to_plot)
}