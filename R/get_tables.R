library(kableExtra)

get_table_for_true_theta <- function(dataset){
  return(kable(dataset %>% 
                 select(K, data_spread, true_theta), 
               "latex", 
               booktabs = TRUE, 
               escape = FALSE,
               linesep = "",
               longtable = TRUE,
               col.names = c("K", 'Variability', '$\\boldsymbol{\\theta}$')) %>%
           column_spec(1, width = "1cm") %>%    # wrap column 1
           column_spec(2, width = "2cm") %>%
           column_spec(3, width = "12cm") %>%
           kable_styling(
             latex_options = c("striped", "repeat_header", "HOLD_position"),
             stripe_color = "gray!15"
    )
  )
  }