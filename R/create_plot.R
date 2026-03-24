source("/home/realiseshewon/PDev/kde-ranking/R/CONSTANTS.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)

COMMON_THEME <- theme(axis.line = element_line(colour = "gray"),
                          panel.grid.minor = element_blank(),
                          panel.grid.major.x = element_blank(),
                          text = element_text(family = "serif"),
                          legend.position = "top",
                          legend.spacing.y = unit(0.5, "cm"), 
                          legend.box.just = "left",
                          legend.margin = margin(t = 0, r = 0, b = 0, 
                                                 l = 0, unit = "mm"),
                          legend.key.width = unit(0.5, "cm"),
                          legend.spacing.x = unit(0.5, "cm"),
                          legend.background = element_blank()
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

create_plot_for_app_data <- function(dat_to_plot,
                                     elements, 
                                     title,
                                     order_identifier, 
                                     reverse = FALSE,
                                     xlab = 'Candidate',
                                     ylab = 'Sample rank',
                                     shape_legend_title = 'Alliance',
                                     shape_labels = "",
                                     shape_map = c("DuterTen" = 1, 
                                                   "Alyansa" = 2, 
                                                   "KiBam" = 3, 
                                                   "Makabayan" = 4,
                                                   "DuterTen-Alyansa" = 5)){
  
  if ('pulse' %in% dat_to_plot$Approach) {
    size <- 7
    legendloc <- "top"
  } else {
    size <- 5
    legendloc <- "right"
  }
  
  data_to_plot <- dat_to_plot %>% filter(Approach != 'pulse')
  
  all_potential_levels <- c(IND$RAWCHAR, BONF$RAWCHAR, NONRANK$RAWCHAR)
  existing_levels <- all_potential_levels[
    all_potential_levels %in% data_to_plot$Approach]
  data_to_plot$Approach <- factor(data_to_plot$Approach, 
                                  levels = existing_levels)
  
  if (length(shape_labels)==1){
    shape_labels = sort(unique(data_to_plot$highlight1))
  } else {
    shape_labels = shape_labels#c("Label A", "Label B", "Label C")
  }
  
  cap_first <- function(x) {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  }
  
  order_mult <- if(reverse) -1 else 1
  
  
  p <- ggplot(data = data_to_plot, 
              aes(x = reorder({{ elements }}, 
                              order_mult * {{ order_identifier }}),
                  y = as.numeric(string_ranks))) + 
    geom_point(data = subset(data_to_plot, highlight0 == "yes"), 
               shape = 16, 
               size = 1.25, 
               color = "black") +
    geom_point(data = subset(data_to_plot, highlight0 == "no"), 
               size = 1, 
               aes(shape = highlight1), 
               stroke = 0.2,
               color = "gray44") + 
    scale_shape_manual(values = shape_map, 
                       name = shape_legend_title, 
                       labels = shape_labels) +
    scale_y_continuous(breaks = seq(1, length(dat_to_plot %>% 
                                                pull({{ elements }}) %>% 
                                                unique()), by = 4)) +
    labs(title=title) + xlab(xlab) + ylab(ylab) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 8),
          axis.text.y = element_text(size=size)
    ) +
    guides(shape = guide_legend(override.aes = list(size = 1.25))) +
    COMMON_THEME + 
    theme(legend.position = legendloc) +
    facet_wrap(~Approach, ncol=length(unique(data_to_plot$Approach)), 
               labeller = as_labeller(cap_first)) + coord_flip()
  return(p)
}


create_plot_for_t <- function(prepared_data, unordered = TRUE){
  
  if (unordered) {
    app_levels = c(IND$RAWCHAR, BONF$RAWCHAR, NONRANK$RAWCHAR) 
    app_labels = c(IND$SHORTNAME, BONF$SHORTNAME, NONRANK$SHORTNAME)
    x = "Approach"
    label_parsedx <- function(x) x
    rot <- 90
    facet_str <- "`Correlation structure` + Metric ~ K"
  } else {
    app_levels = c(ASYMP$RAWCHAR,BOOT$RAWCHAR)
    app_labels = c(ASYMP$SHORTNAME, BOOT$SHORTNAME)
    x = "Correlation structure"
    facet_str <- "Approach + Metric ~ K"
    label_parsedx <- function(x) parse(text = as.character(x))
    rot <- 0
  }
  
  var_levels = c(LOW$NAME, MED$NAME, HIGH$NAME)
  
  p <- prepared_data %>%
    mutate(
      `Correlation structure` = factor(`Correlation structure`,
                                       levels =c(EQUICORRELATED$NAME, 
                                                 BLOCK_DIAGONAL$NAME),
                                       labels = c(TeX(EQUICORRELATED$GGNAME), 
                                                  TeX(BLOCK_DIAGONAL$GGNAME))),
      r = ifelse(r == 0.1, "$\\rho = 0.1$", 
                 ifelse(r == 0.5, "$\\rho = 0.5", 
                        ifelse(r == 0.9,"$\\rho = 0.9", r))),
      Approach = factor(Approach,
                        levels = app_levels, 
                        labels = app_labels,
                        ordered = TRUE),
      Variance = factor(Variance, 
                        levels = var_levels,
                        labels = var_levels,
                        ordered = TRUE),
      K = factor(K,
                 levels = c('10','20','30','40',"50"),
                 labels = paste("K =",c('10','20','30','40',"50")),
                 ordered = TRUE),
      Metric = factor(Metric,
                      levels = c(T1$CHAR, T2$CHAR, T3$CHAR),
                      labels = c(TeX(T1$MATHNAME), 
                                 TeX(T2$MATHNAME), 
                                 TeX(T3$MATHNAME)),
                      ordered = TRUE)) %>%
    ggplot(aes(x = .data[[x]], 
               y = Values, 
               shape = factor(r), 
               size = Variance,
               group = interaction(Variance,r))) +
    geom_point(alpha = 0.25) + 
    scale_size_ordinal(range = c(1, 2.25)) +
    scale_x_discrete(labels = label_parsedx) +
  facet_grid(as.formula(facet_str),
             scales = "free",
             labeller = labeller(Metric = label_parsed,
                                 `Correlation structure` = label_parsed)) +

  theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 9, angle = rot),
          axis.text.y = element_text(size=7)
    ) +
    guides(color = guide_legend(title = TeX(CORR_MATRIX$MATHNAME), ncol = 7)) + 
    scale_shape_manual(
      name = TeX(CORR_MATRIX$MATHNAME),
      values = c(15, 16, 17, 21, 22, 23, 24), 
      labels = c(TeX("$\\rho = 0.1$"), 
                 TeX("$\\rho = 0.5$"), 
                 TeX("$\\rho = 0.9$"),
                 "B2", "U2", "UL3", "UH3")
    ) +
    # scale_fill_manual(values = c("B2" = "blue", "U2" = "blue", "UL3" = "blue", "UH3" = "blue")) +
    COMMON_THEME
  
  if (unordered) {
    p <-p + geom_line(color = 'gray33', lwd = 0.1)
  }
  return(p)
}

