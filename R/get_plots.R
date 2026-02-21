library("ggplot2")
library("dplyr")

get_boxplot_for_true_theta <- function(dataset){
  return(
    dataset %>%
      ggplot(.,
             aes(x = data_spread,
                 y = true_theta)) +
      facet_wrap(~K, ncol=5) +
      labs(title = expression(True~theta~Values~Used~'in'~Simulation),
           x = expression(Variability),
           y = expression(theta)) +
      geom_boxplot() +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  )
}
