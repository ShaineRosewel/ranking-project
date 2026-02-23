library(kableExtra)

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
        stripe_color = "gray!15"
    )
  )
}


create_table_for_tightness_measure <- function(summary, metric_type,
                                               equicorrelation){
  
  if (equicorrelation) {
    vector_1 <- c("K", "r")
    striped <- rep(c(0, 6, 12), each = 3) + 1:3
  } else {
    vector_1 <- c("K", "r", "blocks")
    striped <- rep(c(0, 10), each = 5) + 1:5
  }

  return(summary  %>%
    kable("latex",
          booktabs = TRUE,
          escape = FALSE,
          linesep = "",
          longtable = TRUE,
          col.names = c(vector_1, 
                        rep(c("Independent","Bonferroni","Nonrank-based"),3)),
          caption = paste("Simulation Results for Tightness Measure", 
                          metric_type)) %>%
    kable_styling(latex_options = c("striped",
                                    "repeat_header",
                                    "HOLD_position"),
                  font_size=11.5,
                  stripe_color = "gray!15",
                  stripe_index = rep(c(0, 6, 12), each = 3) + 1:3) %>%
    add_header_above(c(" "=2,"Low" = 3, "Med" = 3, "High" = 3))
  )
}


create_table_for_coverage <- function(summary, equicorrelation = TRUE){
  
  if (equicorrelation) {
    column_names <- c("K", "r", "Independent", "Bonferroni", "Nonrank-based")
    striped <- rep(c(0, 6, 12), each = 3) + 1:3
  } else {
    column_names <- c("K", "r", "blocks","Independent", "Bonferroni", "Nonrank-based")
    striped <- rep(c(0, 10), each = 5) + 1:5
  }
  
  
  return(
    summary %>%
      kable("latex",
            booktabs = TRUE, 
            escape = FALSE,
            linesep = "",
            longtable = TRUE,
            col.names = column_names,
            caption = "Simulation Results for Coverage Probabilities") %>%
      collapse_rows(columns = 1,
                    valign = "middle", 
                    latex_hline = "none") %>%
      kable_styling(latex_options = c("striped", 
                                      "repeat_header", 
                                      "HOLD_position"),
                    stripe_color = "gray!15",
                    stripe_index = striped)
  )
}


