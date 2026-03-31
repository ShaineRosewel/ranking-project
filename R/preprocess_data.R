source("/home/realiseshewon/PDev/kde-ranking/R/CONSTANTS.R")

UNORD <- c(IND$RAWCHAR, BONF$RAWCHAR, NONRANK$RAWCHAR)
ORD <- c(ASYMP$RAWCHAR, BOOT$RAWCHAR)



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
      lapply(list("2_balanced", "2_unbalanced", 
                  "3_unbalanced-low", "3_unbalanced-high"),
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
    deselected_columns <- ORD
    selected_columns <- UNORD
  } else {
    deselected_columns <- UNORD
    selected_columns <- ORD
  }

  deselected_columns <- c(deselected_columns, c('metric', 'sd'))
  partial <- dataset %>%
    filter(sd == sd_value, metric == COVERAGE$CHAR) %>% 
    select(!all_of(deselected_columns)) 
  if (equicorrelation) {
    final <- partial %>%
      select(c(K, r, selected_columns),`Correlation structure`)
  } else {
    final <- partial %>%
      select(c(K, r, selected_columns), `Correlation structure`)
  }
  
  return(
     final %>% mutate(across(c(selected_columns), round, 3))
  )
} 


prepare_coverage_data_plot <- function(eq_res, bl_res, unordered = TRUE){
  
  eq_coverage_ordered <- preprocess_coverage(eq_res, sd_value = 2, equicorrelation = TRUE, unordered = unordered)
  bl_coverage_ordered <- preprocess_coverage(bl_res, sd_value = 2, equicorrelation = FALSE, unordered = unordered)
  
  eq_coverage_orderedm <- preprocess_coverage(eq_res, sd_value = 3.6, equicorrelation = TRUE, unordered = unordered)
  bl_coverage_orderedm <- preprocess_coverage(bl_res, sd_value = 3.6, equicorrelation = FALSE, unordered = unordered)
  
  eq_coverage_orderedh <- preprocess_coverage(eq_res, sd_value = 6, equicorrelation = TRUE, unordered = unordered)
  bl_coverage_orderedh <- preprocess_coverage(bl_res, sd_value = 6, equicorrelation = FALSE, unordered = unordered)
  
  if (unordered) {
    pivot_longer_cols <- c('bonferroni','nonrankbased', 'independent')
    all_rows <- rbind(eq_coverage_ordered, bl_coverage_ordered) %>% mutate(sd = LOW$NAME)
    app_levels <- c(IND$SHORTNAME, BONF$SHORTNAME, NONRANK$SHORTNAME)
  } else {
    pivot_longer_cols <- c('rankbased_asymptotic', 'rankbased_level2bs')
    all_rows <- rbind(rbind(eq_coverage_ordered, bl_coverage_ordered) %>% mutate(sd = LOW$NAME),
                      rbind(eq_coverage_orderedm, bl_coverage_orderedm) %>% mutate(sd = MED$NAME),
                      rbind(eq_coverage_orderedh, bl_coverage_orderedh) %>% mutate(sd = HIGH$NAME))
    app_levels <- c(ASYMP$SHORTNAME, BOOT$SHORTNAME)
  }
  
  return(
    all_rows %>%
      pivot_longer(cols = pivot_longer_cols, 
                   names_to = 'Approach') %>%
      mutate(diff = value - .95,
             sd = factor(sd, 
                         levels = c(LOW$NAME, MED$NAME, HIGH$NAME), 
                         ordered = TRUE),
             `Correlation structure` = factor(`Correlation structure`,
                                              levels = c(EQUICORRELATED$NAME,
                                                         BLOCK_DIAGONAL$NAME), 
                                              ordered = TRUE),
             r = factor(r, levels = c("0.1","0.5", "0.9", 
                                      "B2", "UL3", "U2", "UH3"), ordered =TRUE),
             Approach = case_when(
               Approach == IND$RAWCHAR   ~ IND$SHORTNAME,
               Approach == BONF$RAWCHAR  ~ BONF$SHORTNAME,
               Approach == NONRANK$RAWCHAR ~ NONRANK$SHORTNAME,
               Approach == ASYMP$RAWCHAR ~ ASYMP$SHORTNAME,
               Approach == BOOT$RAWCHAR  ~ BOOT$SHORTNAME,
               TRUE                      ~ Approach # Fallback
             ),
             Approach = factor(Approach, levels = app_levels, ordered = TRUE))
  )
}


prepare_t_facet_plot_data <- function(
    eq_data = eq_res,
    bl_data = bl_res,
    unordered = TRUE){
  
  if (unordered) {
    cols_drop <- ORD
    cols_select <- UNORD
  } else {
    cols_drop <- UNORD
    cols_select <- ORD
  }
  
  t_bl <- bl_res %>% 
    filter(metric != COVERAGE$CHAR) %>% 
    select(-cols_drop) %>%
    select(-c(blocks, sd)) %>% 
    arrange(K, r) %>% 
    pivot_longer(cols =cols_select , 
                 names_to = "Approach", 
                 values_to = "Values") %>% 
    rename(Metric = metric) %>%
    select(c(K,r,Approach,Variance,Values,Metric,`Correlation structure`))
  
  equicorr = TRUE
  t1 <- preprocess_tightness_measure_OG(eq_res, 
                                        T1$CHAR, 
                                        equicorr,
                                        unordered = unordered) %>% 
    pivot_longer(cols = contains("-"),
                 names_to = "Approach", 
                 values_to = "Values") %>% 
    separate(Approach, into = c('Approach','Variance'), sep = "-")
  t1$Metric <- T1$CHAR
  
  t2 <- preprocess_tightness_measure_OG(eq_res, 
                                        T2$CHAR,
                                        equicorr,
                                        unordered = unordered) %>% 
    pivot_longer(cols = contains("-"),
                 names_to = "Approach", 
                 values_to = "Values") %>% 
    separate(Approach, into = c('Approach','Variance'), sep = "-")
  t2$Metric <- T2$CHAR
  
  t3 <- preprocess_tightness_measure_OG(eq_res, 
                                        T3$CHAR, 
                                        equicorr,
                                        unordered = unordered) %>% 
    pivot_longer(cols = contains("-"),
                 names_to = "Approach", 
                 values_to = "Values") %>% 
    separate(Approach, into = c('Approach','Variance'), sep = "-")
  t3$Metric <- T3$CHAR
  
  t_eq <- rbind(t1, t2, t3) #%>% 
  return(rbind(t_eq, t_bl))
}

prepare_appendix_cov_measure_table <- function(partial){
  
  eq_padded_ordered<-partial %>% 
    filter(`Correlation structure` == EQUICORRELATED$NAME) %>% 
    select(-c(`Correlation structure`)) %>%
    group_by(K) %>%
    group_modify(~ add_row(.x)) %>% 
    ungroup() %>% 
    mutate(K = as.character(K)) %>%
    mutate(K = ifelse(duplicated(K), "", K))
  
  bl_test <- partial %>% 
    filter(`Correlation structure` == BLOCK_DIAGONAL$NAME) %>% 
    select(-c(`Correlation structure`, K))
  
  return(cbind(eq_padded_ordered, bl_test))
}

prepare_appendix_t_measure_table <- function(eq_data, bl_data, 
                                             unordered, tmeasure = "t1"){
  partial <- prepare_t_facet_plot_data(
    eq_data = eq_data,
    bl_data = bl_data,
    unordered = unordered) %>% 
    unite("app", Approach, Variance, sep = "_", remove =TRUE) %>%
    pivot_wider(names_from = app,
                values_from = Values) %>% 
    filter(Metric == tmeasure) %>%
    select(-c(Metric))
  
  
  eq_padded_ordered<-partial %>% 
    filter(`Correlation structure` == EQUICORRELATED$NAME) %>% 
    select(-c(`Correlation structure`)) %>%
    group_by(K) %>%
    group_modify(~ add_row(.x)) %>% 
    ungroup() %>% 
    mutate(K = as.character(K)) %>%
    mutate(K = ifelse(duplicated(K), "", K))
  
  bl_test <- partial %>% 
    filter(`Correlation structure` == BLOCK_DIAGONAL$NAME) %>% 
    select(-c(`Correlation structure`, K))

  return(cbind(eq_padded_ordered, bl_test))
}

preprocess_tightness_measure_OG <- function(dataset, metric_type, 
                                            equicorrelation,
                                            unordered = TRUE){
  if (equicorrelation) {
    vector1 <- c('K', 'r')
  } else {
    vector1 <- c('K', 'r', 'blocks')
  }

  levels <- c(LOW$NAME, MED$NAME, HIGH$NAME)
  if (unordered) {
    selected_columns <- UNORD
  } else {
    selected_columns <- ORD
  }


  return(
    dataset %>%
      filter(metric == metric_type) %>%
      select(
        c(vector1, c(Variance, selected_columns,`Correlation structure`)
          )
        ) %>%
      tidyr::pivot_wider(names_from = Variance,
                         values_from = selected_columns,
                         names_sep = "-") %>%
      select(
        c(vector1,`Correlation structure`, unlist(lapply(levels,
                                 function(x) {paste(selected_columns,
                                                    x,
                                                    sep = '-')}))
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
  
  labor <- unique(dataset_all[dataset_all$labor==1,]$Candidate)
  sectoral <- unique(dataset_all[dataset_all$sectoral==1,]$Candidate)
  ind_name_trad <- unique(dataset_all[dataset_all$ind_name_trad==1,]$Candidate)
  ind_name_recall <- unique(dataset_all[dataset_all$ind_name_recall==1,]$Candidate)
  
  dat_to_plot <- dat_to_plot %>%
    mutate(highlight1 = case_when(
      Candidate %in% dmsen           ~ "DuterTen-Alyansa",
      Candidate %in% dsen            ~ "DuterTen",
      Candidate %in% msen            ~ "Alyansa",
      Candidate %in% ksen            ~ "KiBam",
      Candidate %in% mksen           ~ "Makabayan",
      Candidate %in% labor           ~ "labor",
      Candidate %in% sectoral        ~ "sectoral",
      Candidate %in% ind_name_trad   ~ "ind_name_trad",
      Candidate %in% ind_name_recall ~ "ind_name_recall",
      TRUE                           ~ "None"
    ))
  
  # dat_to_plot$highlight1 <- ifelse(
  #   dat_to_plot$Candidate %in% dsen,
  #   "DuterTen",
  #   ifelse(
  #     dat_to_plot$Candidate %in% msen,
  #     "Alyansa",
  #     ifelse(
  #       dat_to_plot$Candidate %in% ksen,
  #       "KiBam",
  #       ifelse(dat_to_plot$Candidate %in% mksen,
  #              "Makabayan",
  #              ifelse(
  #                dat_to_plot$Candidate %in% dmsen,
  #                "DuterTen-Alyansa","None"
  #                )
  #              )
  #       )
  #     )
  #   )

  return(dat_to_plot)
}


prepare_plotting_data_for_traveltime <- function(ci_results, df){
  
  dataset_all <- df
  
  K <- dim(dataset_all)[1]
  dataset_all$`Rank LB_nonrank` <- sapply(
    get_ci_result(result = ci_results$nonrankbased, 
                  K=K, 
                  reverse_ranks = FALSE), 
    min)
  dataset_all$`Rank UB_nonrank` <- sapply(
    get_ci_result(result = ci_results$nonrankbased, 
                  K=K, 
                  reverse_ranks = FALSE),
    max)
  
  dataset_all$`Rank LB_independent` <- sapply(
    get_ci_result(result = ci_results$independent, 
                  K=K, 
                  reverse_ranks = FALSE), 
    min)
  dataset_all$`Rank UB_independent` <- sapply(
    get_ci_result(result = ci_results$independent, 
                  K=K, 
                  reverse_ranks = FALSE), 
    max)
  
  dataset_all$`Rank LB_bonferroni` <- sapply(
    get_ci_result(result = ci_results$bonferroni, 
                  K=K, 
                  reverse_ranks = FALSE), 
    min)
  dataset_all$`Rank UB_bonferroni` <- sapply(
    get_ci_result(result = ci_results$bonferroni, 
                  K=K, 
                  reverse_ranks = FALSE), 
    max)
  
  dataset_all <- dataset_all %>% arrange(desc(theta_k))
  
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
    select(c(iso, `Rank LB`, `Rank UB`, rhat_k, 
             order_index, theta_k, Approach)) %>%
    mutate(string_ranks = mapply(to_string_seq, `Rank LB`, `Rank UB`)) %>%
    separate_rows(string_ranks, sep=",") %>%
    select(c(iso, rhat_k, string_ranks, theta_k, Approach))
  
  
  dat_to_plot$highlight0 <- ifelse(
    dat_to_plot$rhat_k == as.numeric(dat_to_plot$string_ranks), "yes", "no")
  
  
  d1 <- unique(dataset_all[dataset_all$density_group==1,]$theta_k)
  d2 <- unique(dataset_all[dataset_all$density_group==2,]$theta_k)
  d3 <- unique(dataset_all[dataset_all$density_group==3,]$theta_k)
  d4 <- unique(dataset_all[dataset_all$density_group==4,]$theta_k)
  d5 <- unique(dataset_all[dataset_all$density_group==5,]$theta_k)
  
  dat_to_plot$highlight1 <- ifelse(dat_to_plot$theta_k %in% d1, "1", 
                                   ifelse(
                                     dat_to_plot$theta_k %in% d2, "2",
                                     ifelse(dat_to_plot$theta_k %in% d3,
                                            "3",ifelse(
                                              dat_to_plot$theta_k %in% d4,
                                              "4",
                                              ifelse(
                                                dat_to_plot$theta_k %in% d5,
                                                     "5", "None"
                                                        )))
                                     ))
  
  return(dat_to_plot)
}


prepare_block_corrs <- function(df){
  b2 <- df %>% filter(blocks == 2) %>%
    group_modify(~ add_row(.x, .after = 2)) %>% 
    group_modify(~ add_row(.x, .after = 5)) %>% 
    mutate(label = ifelse(duplicated(label), "", label))%>% 
    mutate(blocks = ifelse(duplicated(blocks), "", blocks)) %>% select(-blocks)

  b3 <- df %>% filter(blocks == 3) %>% 
    mutate(label = ifelse(duplicated(label), "", label))%>% 
    mutate(blocks = ifelse(duplicated(blocks), "", blocks)) %>% select(-blocks)

  return(cbind(b2, b3))
}

