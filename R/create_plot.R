library(ggplot2)
library(dplyr)
library(tidyr)

COMMON_THEME <- theme(axis.line = element_line(colour = "gray"),
                          panel.grid.minor = element_blank(),
                          panel.grid.major.x = element_blank(),
                          text = element_text(family = "serif"),
                          legend.position = "right",
                          #legend.box = "vertical",
                          #legend.position = c(1, 1), # Coordinates (bottom-left is 0,0; top-right is 1,1)
                          #legend.justification = c(1, 1),
                          legend.spacing.y = unit(0.05, "cm"), 
                          legend.box.just = "left",
                          # legend.margin = margin(t=.15, b=0.15,l=0.5, r=0.5, "cm"),
                          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"),
                          legend.key.width = unit(0.5, "cm"),  # Reduce key width
                          legend.spacing.x = unit(0.1, "cm"),
                          legend.background = element_blank()#,
                          # legend.box.background = element_rect(colour = "black"
)

create_boxplot_for_true_theta <- function(dataset){
  return(
    dataset %>%
      ggplot(.,
             aes(x = data_spread,
                 y = true_theta)) +
      facet_wrap(~K, ncol=5) +
      labs(title = expression(True~theta~Values~Used~'in'~Simulation),
           x = expression(Variance),
           y = expression(theta)) +
      geom_boxplot(outlier.shape = NA) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  )
}

create_plot_for_app_data <- function(dat_to_plot, elements, order_identifier, reverse = FALSE,
                                     title ='95% Joint Confidence Region for Candidates with at least 1% Votes', 
                                     xlab = 'Candidate',
                                     ylab = 'Sample rank',
                                     shape_legend_title = 'Alliance',
                                     shape_labels = "",
                                     shape_map = c("DuterTen" = 1, "Alyansa" = 2, 
                                                   "KiBam" = 3, "Makabayan" = 4,
                                                   "DuterTen-Alyansa" = 5)){
  
  data_to_plot <- dat_to_plot %>% filter(Approach != 'pulse') #%>% filter(Approach != 'independent')
  data_to_plot$Approach <- factor(data_to_plot$Approach, levels = c('independent', 'bonferroni', 'nonrank'))
  
  if (length(shape_labels)==1){
    shape_labels = sort(unique(data_to_plot$highlight1))
  } else {
    shape_labels = shape_labels#c("Label A", "Label B", "Label C")
  }
  
  cap_first <- function(x) {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  }
  
  order_mult <- if(reverse) -1 else 1
  
  
  p <- ggplot(data = data_to_plot, aes(x = reorder({{ elements }}, order_mult * {{ order_identifier }}),
                                       y = as.numeric(string_ranks))) + 
    geom_point(data = subset(data_to_plot, highlight0 == "yes"), shape = 16, size = 1, color = "black") +
    geom_point(data = subset(data_to_plot, highlight0 == "no"), size = 0.5, 
               aes(shape = highlight1), 
               stroke = 0.2) + 
    scale_shape_manual(values = shape_map, name = shape_legend_title, labels = shape_labels) +
    scale_y_continuous(breaks = seq(1, length(dat_to_plot %>% pull({{ elements }}) %>% unique()), by = 4)) +
    labs(title=title) + xlab(xlab) + ylab(ylab) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 5),
          axis.text.y = element_text(size=5)
    ) +
    #guides(shape = guide_legend(title = str_wrap(shape_legend_title, width = 20))) +
    # theme(axis.line = element_line(colour = "gray"),
    #       panel.grid.minor = element_blank(),
    #       panel.grid.major.x = element_blank(),
    #       text = element_text(family = "serif"),
    #       legend.position = "top") +
    COMMON_THEME +
    facet_wrap(~Approach, ncol=length(unique(data_to_plot$Approach)), 
               labeller = as_labeller(cap_first)) + coord_flip()
  return(p)
}


create_plot_for_t <- function(prepared_data, unordered = TRUE){
  
  if (unordered) {
    app_levels = c("independent", "bonferroni", "nonrankbased")
    app_labels = c("Independent", 'Bonferroni', 'Nonrank')
    x = "Approach"
    facet_str <- "`Correlation structure` + Metric ~ K"
  } else {
    app_levels = c("rankbased_asymptotic","rankbased_level2bs")
    app_labels = c("Asymptotic", "Bootsrap")
    x = "Correlation structure"
    facet_str <- "Approach + Metric ~ K"
  }
  
  p <- prepared_data %>%
    mutate(Variance = factor(Variance, 
                                levels = c("low", "med", "high"), 
                                labels = c("Low", "Moderate", "High"),
                                ordered = TRUE),
           # Variance_size = as.numeric(Variance,
           #                                   levels = c("low", "med", "high"),
           #                                   ordered = TRUE),
           `Correlation structure` = factor(`Correlation structure`,
                                            levels =c("Equicorrelated", "Block diagonal"),
                                            labels =c("Equicorr", "Block diag")),
           Approach = factor(Approach,
                             levels = app_levels,
                             labels = app_labels,
                             ordered = TRUE),
           K = factor(K, 
                      levels = c('10','20','30','40',"50"), 
                      labels = paste("K =",c('10','20','30','40',"50")),
                      ordered = TRUE),
           Metric = factor(Metric,
                           levels = c('t1', 't2', 't3'),
                           labels = c("T[1]", "T[2]", "T[3]"),
                           ordered = TRUE)) %>%
    ggplot(aes(x = .data[[x]], 
               y = Values, 
               shape = factor(r), 
               #linetype = `Correlation structure`,
               size = Variance,
               group = interaction(Variance,r))) +
    geom_point(alpha = 0.35) + 
    scale_size_ordinal(range = c(1, 2)) +
    #================================
  facet_grid(as.formula(facet_str),
             scales = "free",
             labeller = labeller(Metric = label_parsed)) +
    #================================
  theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 7, angle = 90),
          axis.text.y = element_text(size=7)
    ) +
    guides(color = guide_legend(title = "r", ncol = 7)) +
    scale_shape_manual(
      name = "r",
      # labels = c(""), # Assign custom labels
      values = c(15, 16, 17, 15, 16, 17, 18)
    ) + 
    COMMON_THEME
  
  if (unordered) {
    p <-p + geom_line(color = 'gray33', lwd = 0.1)
  }
  return(p)
}

