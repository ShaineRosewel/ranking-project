library(kableExtra)

FONT_SIZE <- 11

create_basic_results_table <- function(dataset, caption){
  # kable(
  # get_tmeasures_for_app_data(ci_results),
  # "latex",
  # booktabs = TRUE,
  # escape = FALSE,
  # linesep = "",
  # longtable = TRUE,
  # caption = caption) %>%
  # kable_styling(
  #   latex_options = c("striped", "repeat_header", "HOLD_position"),
  #   stripe_color = "gray!15",
  #   font_size=FONT_SIZE
  # )
  dataset %>% kable("latex",
                     booktabs = TRUE,
                     escape = FALSE,
                     linesep = "",
                     longtable = TRUE,
                     align = "c",
                     # col.names = c(vector_1,
                     #               selected_columns),
                     caption = caption) %>%
    collapse_rows(columns = c(1,2),
                  valign = "middle", 
                  latex_hline = "major")%>%
    kable_styling(latex_options = c(
      "repeat_header", 
      "HOLD_position"),
      #stripe_color = "gray!15",
      #stripe_index = striped,
      font_size=FONT_SIZE)
}

create_table_for_true_theta <- function(dataset,
                                        column_names = c("K", "Variance",'$\\boldsymbol{\\theta}$', "R"),
                                        main = TRUE){
  if (main){
  formatted_data <- dataset %>%
    mutate(K = as.character(K)) %>%
    mutate(K = ifelse(duplicated(K), "", K)) %>% 
    mutate(data_spread = case_match(as.character(data_spread),
                                    "low"    ~ "Low",
                                    "med" ~ "Moderate",
                                    "high"   ~ "High",
                                    .default = as.character(data_spread))) %>%
    select(K, data_spread, true_theta, R)
  } else {
    formatted_data <- dataset
  }
  
  partial <- formatted_data %>%
    kable(
      "latex",
      booktabs = TRUE,
      escape = FALSE,
      linesep = "\\addlinespace[0.6em]", 
      longtable = TRUE,
      col.names = column_names,
      align = paste(rep("l", length(column_names), collapse = ""))
    ) %>%
  kable_styling(
    latex_options = c("striped", 
                      "repeat_header"),
    stripe_color = "gray!15",
    font_size = FONT_SIZE
  )
  
  if (main) {
    return(
    partial %>% 
      column_spec(1, width = "1cm") %>%
      column_spec(2, width = "1.5cm") %>%
      column_spec(3, width = "9cm") %>%
      column_spec(4, width = "3.5cm")
    )
  } else {
    return(
    partial %>% 
      column_spec(1, width = "1cm") %>%
      column_spec(2, width = "15cm")
    )
  }

}


create_side_by_side_table_within_main <- function(eq_data,
                                                  bl_data,
                                                  num_variance=1,
                                                  unordered = TRUE,
                                                  metric = "Coverage"
){
  
  padded_eq <- eq_data %>%
    group_by(K) %>%
    group_modify(~ add_row(.x, .after = dim(eq_data %>% filter(K==10))[1])) %>% 
    ungroup() %>% arrange(K, r)
  
  formatted_bl <- bl_data %>% 
    mutate(r = case_when(
      r == "balanced-block-2" ~ "B2",
      r == "unbalanced-block-2" ~ "U2",
      r == "unbalanced-block-3" ~ "UL3",
      r == "unbalanced-block-high-3" ~ "UH3",
      TRUE ~ r)) %>% 
    arrange(K, r)
  
  summary <- cbind(padded_eq %>% select(-c(K, `Correlation structure`)), 
                   formatted_bl %>% select(-c(K, `Correlation structure`))) 
  
  if (unordered) {
    selected_columns <- c("Ind","Bonf","NR")
    colwidth  <- "1cm"
    parameter <- "Unordered"
  } else {
    selected_columns <- c("Asymp","Boot")
    colwidth  <- "1.25cm"
    parameter <- "Ordered"
  }
  num <- length(selected_columns)
  
  return(
    summary %>%
      kable("latex", 
            booktabs = TRUE,
            escape = FALSE,
            align = rep("c", (num_variance*num*2 + 1),collapse=""), 
            linesep = "",
            col.names = rep(c("r", rep(selected_columns, num_variance)),2), 
            caption = paste("Simulation Results for", metric, "of", parameter, "Parameters"),
            na.character = "") %>%
      column_spec(1:(num_variance*num*2 + 1) , width = colwidth) %>%
      add_header_above(c("Equicorrelated" = num_variance*num + 1,
                         "Block diagonal" = num_variance*num + 1), 
                       escape = FALSE) %>%
      pack_rows("K = 10", 1, 4, latex_gap_space = "0.5em") %>%
      pack_rows("K = 20", 5, 8, latex_gap_space = "0.5em") %>%
      pack_rows("K = 30", 9, 12, latex_gap_space = "0.5em") %>%
      pack_rows("K = 40", 13, 16, latex_gap_space = "0.5em") %>%
      pack_rows("K = 50", 17, 20,latex_gap_space = "0.5em") %>%
      kable_styling(latex_options = c("scale_down","HOLD_position"),
                    font_size = 11.5)%>%
      footnote(general_title = "\\\\textit{Note: }", 
               general = "B2: balanced-block-2; U2: unbalanced-block-2; U3: unbalanced-block-3; UH3: unbalanced-block-high-3",
               threeparttable = TRUE,
               escape = FALSE) 
  )
}


# create_table_for_tightness_measure <- function(summary, metric_type,
#                                                equicorrelation){
#   
#   # if (unordered) {
#   unord <- c("Ind","Bonf","NR")
#   ord <- c("Asymp","Boot")
#   colwidth  <- "0.8cm"
#   # } else {
#   #   selected_columns <- c("Asymptotic Var","Bootstrap Est")
#   #   colwidth  <- "3cm"
#   # }
#   
#   num <- length(c(ord,unord))
#   
#   # sd <- c("Low $\\\\boldsymbol{\\\\theta}$ Variance" = 3, 
#   #         "Moderate $\\\\boldsymbol{\\\\theta}$ Variance" = 3, 
#   #         "High $\\\\boldsymbol{\\\\theta}$ Variance" = 3,
#   #         "Low $\\\\boldsymbol{\\\\theta}$ Variance" = 2, 
#   #         "Moderate $\\\\boldsymbol{\\\\theta}$ Variance" = 2, 
#   #         "High $\\\\boldsymbol{\\\\theta}$ Variance" = 2)
#   
#   sd <- c("Low" = 3, 
#           "Moderate" = 3, 
#           "High" = 3,
#           "Low" = 2, 
#           "Moderate" = 2, 
#           "High" = 2)
#   approach <- c(" " = 2,c("Unordered" = 9, "Ordered" = 6))
#   
#   if (equicorrelation) {
#     vector_1 <- c("K", "r")
#     striped <- c(2:4, 8:10, 14:16)#rep(c(0, 6, 12), each = 3) + 1:3
#     headers <- c(" " = 2, sd)
#   } else {
#     vector_1 <- c("K")
#     striped <- -1 #rep(c(0, 10), each = 5) + 1:5
#     headers <- c(" "= 1, sd)
#   }
# 
# # create_table_for_tightness_measure <- function(summary, metric_type,
# #                                                equicorrelation,
# #                                                unordered = TRUE){
# # 
# #   if (unordered) {
# #     selected_columns <- c("Independent","Bonferroni","Nonrank")
# #     colwidth  <- "2cm"
# #   } else {
# #     selected_columns <- c("Asymptotic Var","Bootstrap Est")
# #     colwidth  <- "3cm"
# #   }
# #   num <- length(selected_columns)
# #   if (equicorrelation) {
# #     vector_1 <- c("K", "r")
# #     striped <- rep(c(0, 6, 12), each = 3) + 1:3
# #     headers <- c(" " = 2, 
# #                  "Low $\\\\boldsymbol{\\\\theta}$ Variance" = num, 
# #                  "Moderate $\\\\boldsymbol{\\\\theta}$ Variance" = num, 
# #                  "High $\\\\boldsymbol{\\\\theta}$ Variance" = num)
# #   } else {
# #     vector_1 <- c("K")
# #     striped <- -1 #rep(c(0, 10), each = 5) + 1:5
# #     headers <- c(" "=1,
# #                  "Low $\\\\boldsymbol{\\\\theta}$ Variance" = num, 
# #                  "Moderate $\\\\boldsymbol{\\\\theta}$ Variance" = num,
# #                  "High $\\\\boldsymbol{\\\\theta}$ Variance" = num)
# #   }
# 
#   
#   formatted <- summary %>%
#     kable("latex",
#           booktabs = TRUE,
#           escape = FALSE,
#           linesep = "",
#           longtable = TRUE,
#           col.names = c(vector_1, 
#                         c(rep(unord,3),rep(ord,3))),
#           caption = paste("Simulation Results for Tightness Measure", 
#                           metric_type)) %>%
#     collapse_rows(columns = 1,
#                   valign = "middle", 
#                   latex_hline = "none",
#                   row_group_label_position = "first") %>%
#     add_header_above(headers, escape = FALSE) %>%
#     add_header_above(approach, escape = FALSE) %>%
#     kable_styling(latex_options = c("striped",
#                                     "repeat_header",
#                                     "HOLD_position"),
#                   font_size=FONT_SIZE-1,
#                   stripe_color = "gray!15",
#                   stripe_index = striped)
#                     #rep(c(0, 6, 12), each = 3) + 1:3) %>%
# 
#   if (equicorrelation) {
#     final <- formatted %>% 
#       column_spec(1:2, width = "0.5cm") %>%
#       column_spec(3:(2+3*num), width = colwidth)
#   } else {
#     final <- formatted %>% 
#       column_spec(1:1, width = "1cm") %>%
#       column_spec(2:(1+3*num), width = colwidth) %>%
#       pack_rows("2 balanced blocks", 1, 5,
#                 latex_gap_space = "0.3em") %>%
#       pack_rows("2 unbalanced blocks", 6, 10,
#                 latex_gap_space = "0.3em") %>%
#       pack_rows("3 unbalanced blocks - low", 11, 15,
#                 latex_gap_space = "0.3em") %>%
#       pack_rows("3 unbalanced block - high", 16, 20,
#                 latex_gap_space = "0.3em")
#   }
#   return(
#     final
#   )
# }


create_table_for_tightness_measure <- function(summary, metric_type,
                                               unordered = TRUE){
  
  if (unordered) {
    selected_columns <- c("Ind","Bonf","NR")
    colwidth  <- "0.82cm"
  } else {
    selected_columns <- c("Asymp","Boot")
    colwidth  <- "1cm"
  }
  
  num <- length(selected_columns)
  
  vector_1 <- c("K", "r")
  striped <- rep(c(0, 8, 16), each = 4) + 1:4  #rep(c(0, 6, 12), each = 3) + 1:3
  # headers <- c(" " = 2,
  #              "Low $\\\\boldsymbol{\\\\theta}$ Variance" = num,
  #              "Moderate $\\\\boldsymbol{\\\\theta}$ Variance" = num,
  #              "High $\\\\boldsymbol{\\\\theta}$ Variance" = num,
  #              " " = 1,
  #              "Low $\\\\boldsymbol{\\\\theta}$ Variance" = num,
  #              "Moderate $\\\\boldsymbol{\\\\theta}$ Variance" = num,
  #              "High $\\\\boldsymbol{\\\\theta}$ Variance" = num)
  headers <- c(" " = 2,
               "Low" = num,
               "Moderate" = num,
               "High" = num,
               " " = 1,
               "Low" = num,
               "Moderate" = num,
               "High" = num)
  
  formatted <- summary %>%
    kable("latex",
          booktabs = TRUE,
          escape = FALSE,
          linesep = "",
          longtable = TRUE,
          col.names = c(vector_1, 
                        rep(selected_columns, 3), 
                        'r', 
                        rep(selected_columns, 3)),
          caption = paste("Simulation Results for Tightness Measure", 
                          metric_type),
          na.character = "") %>%
    # collapse_rows(columns = 1,
    #               valign = "middle", 
    #               latex_hline = "none",
    #               row_group_label_position = "first") %>%
    add_header_above(c(" " = 2,
                       "Low" = num, "Moderate" = num, "High" = num,
                       " " = 1, # The 'r' spacer
                       "Low" = num, "Moderate" = num, "High" = num), escape = FALSE) %>%
    add_header_above(c(" " = 2, 
                       "Equicorrelated" = (num * 3), 
                       "Block diagonal" = (num * 3 + 1)), escape = FALSE) %>%
    kable_styling(latex_options = c("striped",
                                    "repeat_header",
                                    "HOLD_position"),
                  font_size=FONT_SIZE-2,
                  stripe_color = "gray!15",
                  stripe_index = striped)
  #rep(c(0, 6, 12), each = 3) + 1:3) %>%
  
  
  final <- formatted #%>% 
  #column_spec(c(1:2,12), width = "0.5cm") %>%
  #column_spec(c(3:11, 13:21), width = colwidth)
  
  return(
    final
  )
}



create_table_for_coverage <- function(summary, footnote, equicorrelation = TRUE,
                                      unordered = TRUE){
  
  if (equicorrelation) {
    vector_1 <- c("K", "r")
    striped <- rep(c(0, 6, 12), each = 3) + 1:3
  } else {
    vector_1 <- c("K")
    striped <- -1
  }
  
  if (unordered) {
    selected_columns <- c("Independent","Bonferroni","Nonrank")
    colwidth  <- "3cm"
  } else {
    selected_columns <- c("Asymptotic Variance","Bootstrap Estimate")
    colwidth  <- "4cm"
  }
  num <- length(selected_columns)
  formatted <- summary %>%
    kable("latex",
          booktabs = TRUE,
          escape = FALSE,
          linesep = "",
          longtable = TRUE,
          align = "c",
          col.names = c(vector_1,
                        selected_columns),
          caption = "Simulation Results for Coverage Probabilities")

  if (equicorrelation) {
    final <- formatted %>% 
      column_spec(1:2, width = "1cm") %>%
      column_spec(3:(2+num), width = colwidth)
  } else {
    final <- formatted %>% 
      column_spec(1:1, width = "1cm") %>%
      column_spec(2:(1+num), width = colwidth) %>%
      pack_rows("2 balanced blocks", 1, 5) %>%
      pack_rows("2 unbalanced blocks", 6, 10) %>%
      pack_rows("3 unbalanced blocks - low", 11, 15) %>%
      pack_rows("3 unbalanced block - high", 16, 20) 
  }
  
  return(
    final %>% 
      collapse_rows(columns = 1,
                    valign = "middle", 
                    latex_hline = "none",
                    row_group_label_position = "first") %>%
      kable_styling(latex_options = c("striped", 
                                      "repeat_header", 
                                      "HOLD_position"),
                    stripe_color = "gray!15",
                    stripe_index = striped,
                    font_size=FONT_SIZE) %>%
    footnote(general_title = "\\\\footnotesize\\\\textit{Note: }", 
             footnote_as_chunk = TRUE, # using the "general" footnote option "Note." and the "Footnote...." text start on the same line
             threeparttable = TRUE,
             general = paste0("\\\\footnotesize{",footnote,"}"),
             escape =FALSE)
  )
}

# https://stackoverflow.com/questions/66472411/how-to-reduce-size-of-footnote-in-kableextra-latex


