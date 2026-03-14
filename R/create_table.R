library(kableExtra)

FONT_SIZE <- 11.75

create_table_for_true_theta <- function(dataset){
  return(kable(
    dataset %>% 
      select(K, data_spread, true_theta), 
    "latex",
    booktabs = TRUE,
    escape = FALSE,
    linesep = "",
    longtable = TRUE,
    col.names = c("K", 'Variability', '$\\boldsymbol{\\theta}$')) %>%
      column_spec(1, width = "1cm") %>%
      column_spec(2, width = "2cm") %>%
      column_spec(3, width = "12cm") %>%
      kable_styling(
        latex_options = c("striped", "repeat_header", "HOLD_position"),
        stripe_color = "gray!15",
        font_size=FONT_SIZE
    )
  )
}


create_table_for_tightness_measure <- function(summary, metric_type,
                                               equicorrelation,
                                               unordered = TRUE){

  if (unordered) {
    selected_columns <- c("Independent","Bonferroni","Nonrank-based")
  } else {
    selected_columns <- c("Asymptotic Variance","Bootstrap Estimate")
  }
  
  numcols <- length(selected_columns)
  
  if (equicorrelation) {
    vector_1 <- c("K", "r")
    striped <- rep(c(0, 6, 12), each = 3) + 1:3
    headers <- c(" " = 2, 
                 "Low $\\\\boldsymbol{\\\\theta}$ Variability" = numcols, 
                 "Medium $\\\\boldsymbol{\\\\theta}$ Variability" = numcols, 
                 "High $\\\\boldsymbol{\\\\theta}$ Variability" = numcols)
  } else {
    vector_1 <- c("K")
    striped <- -1 #rep(c(0, 10), each = 5) + 1:5
    headers <- c(" "=1,
                 "Low $\\\\boldsymbol{\\\\theta}$ Variability" = numcols, 
                 "Medium $\\\\boldsymbol{\\\\theta}$ Variability" = numcols,
                 "High $\\\\boldsymbol{\\\\theta}$ Variability" = numcols)
  }

  
  formatted <- summary %>%
    kable("latex",
          booktabs = TRUE,
          escape = FALSE,
          linesep = "",
          longtable = TRUE,
          col.names = c(vector_1, 
                        rep(selected_columns,3)),
          caption = paste("Simulation Results for Tightness Measure", 
                          metric_type)) %>%
    collapse_rows(columns = 1,
                  valign = "middle", 
                  latex_hline = "none",
                  row_group_label_position = "first") %>%
    add_header_above(headers, escape = FALSE) %>%
    kable_styling(latex_options = c("striped",
                                    "repeat_header",
                                    "HOLD_position"),
                  font_size=FONT_SIZE,
                  stripe_color = "gray!15",
                  stripe_index = striped)
                    #rep(c(0, 6, 12), each = 3) + 1:3) %>%

  if (equicorrelation) {
    final <- formatted
  } else {
    final <- formatted %>% 
      pack_rows("2 balanced blocks", 1, 5,
                latex_gap_space = "0.7em") %>%
      pack_rows("2 unbalanced blocks", 6, 10,
                latex_gap_space = "0.7em") %>%
      pack_rows("3 unbalanced blocks - low", 11, 15,
                latex_gap_space = "0.7em") %>%
      pack_rows("3 unbalanced block - high", 16, 20,
                latex_gap_space = "0.7em")
  }
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
    selected_columns <- c("Independent","Bonferroni","Nonrank-based")
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


