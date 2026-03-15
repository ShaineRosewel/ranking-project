library(ggplot2)
library(dplyr)
library(tidyr)

create_boxplot_for_true_theta <- function(dataset){
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

create_plot_for_app_data <- function(dat_to_plot, elements, order_identifier, reverse = FALSE,
                                     title ='95% Joint Confidence Region for Candidates with at least 1% Votes', 
                                     xlab = 'Candidate',
                                     ylab = 'Sample rank',
                                     color_map = c("DuterTen" = "limegreen", "Alyansa" = "red", 
                                                   "KiBam" = "gold", "Makabayan" = "blue",
                                                   "DuterTen-Alyansa" = "black")){
  
  data_to_plot <- dat_to_plot %>% filter(Approach != 'pulse') #%>% filter(Approach != 'independent')
  data_to_plot$Approach <- factor(data_to_plot$Approach, levels = c('independent', 'bonferroni', 'nonrank'))
  
  cap_first <- function(x) {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  }
  
  order_mult <- if(reverse) -1 else 1
  
  
  p <- ggplot(data = data_to_plot, aes(x = reorder({{ elements }}, order_mult * {{ order_identifier }}),
                                       y = as.numeric(string_ranks))) + 
    geom_point(data = subset(data_to_plot, highlight0 == "yes"), shape = 16, size = 1, color = "black") +
    geom_point(data = subset(data_to_plot, highlight0 == "no"), shape = 4, size = 0.25, aes(color = highlight1)) + 
    scale_color_manual(values = color_map) +
    scale_y_continuous(breaks = seq(1, length(dat_to_plot %>% pull({{ elements }}) %>% unique()), by = 4)) +
    labs(title=title) + xlab(xlab) + ylab(ylab) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 5),
          axis.text.y = element_text(size=5)
    ) +
    guides(colour="none") +
    theme(axis.line = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          text = element_text(family = "serif")) +
    facet_wrap(~Approach, ncol=length(unique(data_to_plot$Approach)), 
               labeller = as_labeller(cap_first)) + coord_flip()
  return(p)
}

# create_plot_for_pulse <- function(dat_to_plot){
#   
#   data_to_plot <- dat_to_plot %>% filter(Approach != 'pulse') #%>% filter(Approach != 'independent')
#   data_to_plot$Approach <- factor(data_to_plot$Approach, levels = c('independent', 'bonferroni', 'nonrank'))
#   
#   cap_first <- function(x) {
#     paste0(toupper(substring(x, 1, 1)), substring(x, 2))
#   }
#   
#   
#   p <- ggplot(data = data_to_plot, aes(x = reorder(Candidate, -k),
#                                        y = as.numeric(string_ranks))) + 
#     geom_point(data = subset(data_to_plot, highlight0 == "yes"), shape = 16, size = 1, color = "black") +
#     geom_point(data = subset(data_to_plot, highlight0 == "no"), shape = 4, size = 0.25, aes(color = highlight1)) + 
#     scale_color_manual(values = c("DuterTen" = "limegreen", "Alyansa" = "red", "KiBam" = "gold", "Makabayan" = "blue","DuterTen-Alyansa" = "black")) +
#     scale_y_continuous(breaks = seq(1, max(prepared_data$k), by = 4)) +
#     labs(title='95% Joint Confidence Region for Candidates with at least 1% Votes') + xlab('Candidate') + ylab('Sample rank') +
#     theme_bw() +
#     theme(axis.text.x = element_text(hjust = 1, size = 5),
#           axis.text.y = element_text(size=5)
#     ) +
#     guides(colour="none") +
#     theme(axis.line = element_line(colour = "gray"),
#           panel.grid.minor = element_blank(),
#           panel.grid.major.x = element_blank(),
#           text = element_text(family = "serif")) +
#     facet_wrap(~Approach, ncol=length(unique(data_to_plot$Approach)), 
#                labeller = as_labeller(cap_first)) + coord_flip()
#   return(p)
# }
