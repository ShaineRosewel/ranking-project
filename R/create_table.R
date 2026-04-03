library(kableExtra)
source("/home/realiseshewon/PDev/kde-ranking/R/CONSTANTS.R")

FONT_SIZE <- 12

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

create_t_table_for_ordered_application <- function(ordered_t_dataset){
  headers <- setNames(
    c(1, 2, 2, 2), 
    c(" ", T1$MATHNAME, T2$MATHNAME, T3$MATHNAME)
  )
  
  return(ordered_t_dataset %>%
           kable("latex",
                 col.names = c(CORR_COEFF$MATHNAME, rep(c(ASYMP$MATHNAME, BOOT$MATHNAME), 3)), 
                 booktabs = TRUE,
                 escape = FALSE,
                 linesep = "",
                 longtable = TRUE,
                 align = "llcc",
                 caption = "Tightness Measures for Ordered Parameters") %>%
           # column_spec(1:(num_variance*num*2 + 1) , width = colwidth) %>%
           add_header_above(headers, 
                            escape = FALSE) %>%
           kable_styling(latex_options = c("hold_position"), 
                         full_width = FALSE, 
                         font_size = 12)
  )
}

create_basic_results_table <- function(dataset, caption){
  
  columnnames <- c(CORR_COEFF$MATHNAME,
                         "Approach", 
                         T1$MATHNAME, 
                         T2$MATHNAME,
                         T3$MATHNAME)
  
  dataset %>% 
    mutate(Approach = ifelse(Approach == NONRANK$RAWCHAR, 
                             NONRANK$SHORTNAME, 
                             ifelse(Approach == IND$RAWCHAR, 
                                    IND$SHORTNAME, 
                                    ifelse(Approach == BONF$RAWCHAR, 
                                           BONF$SHORTNAME, 
                                           "")
    ))) %>% 
    kable("latex",
          col.names = columnnames,
          booktabs = TRUE,
          escape = FALSE,
          linesep = "",
          longtable = TRUE,
          align = "llcc",
          caption = caption) %>%
    collapse_rows(columns = c(1,2),
                  valign = "middle", 
                  latex_hline = "major")%>%
    kable_styling(latex_options = c(
      "repeat_header", 
      "HOLD_position"),
      font_size=FONT_SIZE)
}

create_table_for_true_theta <- function(dataset,
                                        column_names = c(
                                          "K", 
                                          "Variance",
                                          '$\\boldsymbol{\\theta}$',
                                          CORR_MATRIX$MATHNAME),
                                        main = TRUE){
  if (main){
  formatted_data <- dataset %>%
    mutate(K = as.character(K)) %>%
    mutate(K = ifelse(duplicated(K), "", K)) %>% 
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
    latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15",
    font_size = FONT_SIZE
  )
  
  if (main) {
    return(
    partial %>% 
      column_spec(1, width = "0.5cm") %>%
      column_spec(2, width = "1.5cm") %>%
      column_spec(3, width = "9cm") %>%
      column_spec(4, width = "4cm")
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
                                                  metric = COVERAGE$NAME
){
  
  padded_eq <- eq_data %>%
    #mutate(r = paste("$\\rho =", r, "$")) %>%
    group_by(K) %>%
    group_modify(~ add_row(.x, .after = dim(eq_data %>% filter(K==10))[1])) %>% 
    ungroup() %>% arrange(K, r)
  
  formatted_bl <- bl_data %>% 
    arrange(K, r)
  
  summary <- cbind(padded_eq %>% select(-c(K, `Correlation structure`)), 
                   formatted_bl %>% select(-c(K, `Correlation structure`))) 
  
  if (unordered) {
    selected_columns <- c(IND$MATHNAME,BONF$MATHNAME,NONRANK$MATHNAME)
    colwidth  <- "1cm"
    parameter <- UNORDERED$NAME
  } else {
    selected_columns <- c(ASYMP$MATHNAME,BOOT$MATHNAME)
    colwidth  <- "1.25cm"
    parameter <- ORDERED$NAME
  }
  num <- length(selected_columns)
  
  corrstruc_headers <- setNames(
      c(num_variance * num + 1, num_variance * num + 1), 
      c(EQUICORRELATED$NAME, BLOCK_DIAGONAL$NAME)
    )
  
  r1 <- CORR_COEFF$MATHNAME
  r2 <- " "#BLOCK_DIAGONAL$MATHNAME
  
  return(
    summary %>%
      kable("latex", 
            booktabs = TRUE,
            escape = FALSE,
            align = paste(rep(c("c", rep("c", num_variance*num,collapse="")), 
                              num), collapse = ''), 
            linesep = "",
            col.names = c(r1, selected_columns, r2, selected_columns), 
            caption = paste("Simulation Results for",
                            metric, 
                            "of", 
                            parameter, 
                            "Parameters"),
            na.character = "") %>%
      column_spec(1:(num_variance*num*2 + 1) , width = colwidth) %>%
      add_header_above(corrstruc_headers, 
                       escape = FALSE) %>%
      pack_rows("K = 10", 1, 4, latex_gap_space = "0.5em") %>% 
      pack_rows("K = 20", 5, 8, latex_gap_space = "0.5em") %>%
      pack_rows("K = 30", 9, 12, latex_gap_space = "0.5em") %>%
      pack_rows("K = 40", 13, 16, latex_gap_space = "0.5em") %>%
      pack_rows("K = 50", 17, 20,latex_gap_space = "0.5em") %>%
      kable_styling(latex_options = c("scale_down","HOLD_position"),
                    font_size = FONT_SIZE)
  )
}


create_table_for_tightness_measure <- function(summary, metric_type,
                                               unordered = TRUE){
  
  if (unordered) {
    selected_columns <-c(IND$MATHNAME,BONF$MATHNAME,NONRANK$MATHNAME)
    colwidth  <- "0.83cm"
    colwidth1 <- "0.65cm"
    parameter <- UNORDERED$NAME
    colcount1 <- 1:2
    colcount <- 3:21
  } else { 
    selected_columns <- c(ASYMP$MATHNAME,BOOT$MATHNAME)
    colwidth  <- "1.3cm"
    parameter <- ORDERED$NAME
    colcount <- 1:15
  }
  
  if (metric_type == T1$CHAR) {
    metric_title = T1$MATHNAME 
  } else if (metric_type == T2$CHAR) {
    metric_title = T2$MATHNAME 
  } else if (metric_type == T3$CHAR) {
    metric_title = T3$MATHNAME 
  } else if (metric_type == COVERAGE$CHAR) {
    metric_title = COVERAGE$NAME
  }
    
  num <- length(selected_columns)
  
  vector_1 <- c("K", CORR_COEFF$MATHNAME)
  vector_2 <- " " #BLOCK_DIAGONAL$MATHNAME
  striped <- rep(c(0, 8, 16), each = 4) + 1:4
  
  var_headers <- setNames(
    c(2, num, num, num, 1, num, num, num),
    rep(c(" ", LOW$NAME, MED$NAME, HIGH$NAME),2)
  )
  
  corrstruc_headers <- setNames(
    c(2, num * 3, num * 3 + 1),
    c(" ", EQUICORRELATED$NAME, BLOCK_DIAGONAL$NAME)
  )
  
  formatted <- summary %>%
    kable("latex",
          booktabs = TRUE,
          escape = FALSE,
          linesep = "",
          longtable = TRUE,
          col.names = c(vector_1, 
                        rep(selected_columns, 3), 
                        vector_2, 
                        rep(selected_columns, 3)),
          caption = paste("Simulation Results for", 
                          metric_title, 
                          "of", 
                          parameter, 
                          "Parameters"),,
          na.character = "") %>%
    add_header_above(var_headers, escape = FALSE) %>%
    add_header_above(corrstruc_headers, escape = FALSE) %>%
    kable_styling(latex_options = c("striped",
                                    "repeat_header",
                                    "HOLD_position"),
                  font_size=FONT_SIZE-1,
                  stripe_color = "gray!15",
                  stripe_index = striped)
  
  final <- formatted %>% 
    column_spec(colcount, width = colwidth)
  
  if (unordered){
    final <- final %>% column_spec(colcount1, width = colwidth1)
  }
  
  return(
    final
  )
}


create_block_corr_table <- function(df){
  df %>% kable("latex", 
        booktabs = TRUE,
        escape = FALSE,
        linesep = c("", "", "\\addlinespace[0.6em]", 
                    "", "", "\\addlinespace[0.6em]"),
        align = "lclllcll",
        caption = paste("Configuration for", 
                        BLOCK_DIAGONAL$MATHNAME),
        col.names = rep(c(BLOCK_DIAGONAL$MATHNAME, 
                          "Block No.", 
                          "Size", 
                          CORR_COEFF$MATHNAME),2),
        na.character = "")%>%
    add_header_above(c("2 Blocks" = 4,"3 Blocks" = 4))  %>%
    kable_styling(latex_options = c("striped",
                                    "repeat_header",
                                    "HOLD_position"),
                  stripe_color = "gray!15",
                  stripe_index = 1:3,
                  font_size=FONT_SIZE
                  ) 
}
