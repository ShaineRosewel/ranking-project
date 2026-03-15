block_corr <- function(block_sizes, rho_within, rho_between) {
  B <- length(block_sizes)
  N <- sum(block_sizes)
  
  if (length(rho_within) == 1) {
    rho_within <- rep(rho_within, B)
  }
  
  Sigma <- matrix(rho_between, nrow = N, ncol = N)
  diag(Sigma) <- 1
  
  idx <- 1
  for (b in seq_len(B)) {
    block_idx <- idx:(idx + block_sizes[b] - 1)
    Sigma[block_idx, block_idx] <- rho_within[b]
    diag(Sigma[block_idx, block_idx]) <- 1
    idx <- idx + block_sizes[b]
  }
  
  return(Sigma)
}


generate_true_theta <- function()
  df <- readRDS("/home/realiseshewon/PDev/kde-ranking/data/mean_travel_time_ranking_2011.rds")
  mean <- 23.8
  sds <- c(2, 3.6, 6.0)
  Ks <- c(10, 20, 30, 40, 50)
  # corr <- c(0.1, 0.5, 0.9)
  data_list <- list()
  
  for (sd in sds) {
    for (K in Ks) {
      
      set.seed(123974)
      true_theta <- rnorm(K, mean, sd)
      data_list[[length(data_list) + 1]] <- data.frame(sd = sd, K = K, true_theta = true_theta)
    }}
  
  bplot <- data_list %>% 
    bind_rows() %>% 
    mutate(data_spread = factor(ifelse(sd == 2, "low", 
                                       ifelse(sd == 3.6, "med", "high")),
                                levels =c("low", 'med', 'high'),
                                ordered =TRUE
    ))
  return(bplot)
  
  
generate_true_theta_rd <- function(persist_df = TRUE) {
  long_vector <- c(
      "Go, Bong Go", "PDPLBN", 59.5, 1, 1, 
      "Tulfo, Erwin", "LAKAS", 48.7, 2,4,  
      "Sotto, Tito", "NPC", 44.1, 2, 4,
      "Dela Rosa, Bato", "PDPLBN", 42.9, 2,5,
      "Lapid, Lito", "NPC", 38, 4, 8,
      "Cayetano, Pia", "NP", 34.3, 5,12,
      "Lacson, Ping", "IND", 33.1, 5,13,
      "Aquino, Bam", "KNP", 32.8, 5, 15,
      "Bong Revilla, Ramon, Jr.", "LAKAS", 32.3, 6,15,
      "Binay, Abby", "NPC", 30.5, 6, 16,
      "Marcoleta, Rodante", "IND", 28.9, 6, 18,
      "Tulfo, Ben Bitag", "IND", 28.9, 6, 19,
      "Pacquiao, Manny Pacman", "PFP", 28.4, 7, 19,
      "Marcos, Imee R.", "NP", 27.6, 8, 19,
      "Villar, Camille", "NP", 27.6, 8, 19,
      "Salvador, Phillip Ipe", "PDPLBN", 26.5, 10, 19,
      "Pangilinan, Kiko", "LP", 25.1, 11, 20,
      "Revillame, Willie Wil", "IND", 24.1, 11,20,
      "Abalos, Benhur", "PFP", 23.7, 12, 20,
      "Bondoc, Jimmy", "PDPLBN", 20.5, 17, 21,
      "Tolentino, Francis Tol","PFP" ,18.7, 20, 23,
      "Bosita, Colonel", "IND", 15.5, 21, 27,
      "Lambino, Raul", "PDPLBN", 14.7, 21, 27,
      "Honasan, Gringo", "RP", 13.4, 22, 27,
      
      "Hinlo, Jayvee", "PDPLBN", 13, 22, 27,
      "Quiboloy, Apollo", "IND", 12.4, 22, 27,  
      "Rodriguez, Atty. Vic", "IND", 12.4, 22, 27,
      "Querubin, Ariel Porfirio", "NP", 7.5, 28, 30,
      "Mata, Doc Marites", "IND", 7.4, 28, 30,
      "Mendoza, Heidi", "IND", 5.8, 28, 33,
      
      "Espiritu, Luke", "PLM", 4.4, 30, 36,
      "De Guzman, Ka Leody", "PLM", 3.6, 30, 41,
      "Castro, Teacher France", "MKBYN", 3.4, 30, 43,
      "Casino, Teddy", "MKBYN", 3.2, 31, 44,
      "Matula, Atty. Sonny", "WPP", 2.5, 31, 51,
      "Arambulo, Ronnel", "MKBYN", 2.4, 31, 52,
      "Ramos, Danilo", "MKBYN", 2.2, 32, 54,
      "Brosas, Arlene", "MKBYN", 2, 32, 57,
      "Amad, Wilson", "IND", 2, 32, 57,
      "Gonzales, Norberto", "PDSP", 1.9, 32, 58,
      "Doringo, Nanay Mimi", "MKBYN", 1.9, 32, 58,
      "Marquez, Norman", "IND", 1.6, 33, 61,
      "Martinez, Eric", "IND", 1.6, 33, 61,
      "De Alban, Atty. Angelo", "IND", 1.5, 34, 62,
      "Ballon, Roberto", "IND", 1.3, 35, 63,
      "Gamboa, Mark Louie", "IND", 1.3, 35, 63,
      "Escobal, Arnel", "PM", 1.3, 35, 64,
      "Sahidulla, Nur-Ana", "IND", 1.2, 35, 64,
      "Maza, Liza", "MKBYN", 1.2, 35, 64,
      "Arellano, Ernesto", "KTPNAN", 1.1, 35, 64,
      "Montemayor, Joey", "IND", 1, 35, 64,
      "Lidasan, Amira", "MKBYN", 0.9, 36, 64,
      "Valbuena, Mar Manibela", "IND", 0.9, 37,64,
      "Tapado, Michael BongBong", "PM", 0.8, 37, 64,
      "Floranda, Mody Piston", "MKBYN", 0.7, 38, 64,
      "D'Angelo, David", "BUNYOG", 0.7, 38, 64,
      "Mustapha, Subair", "WPP", 0.7, 38, 64,
      "Olivar, Jose Jessie", "IND", 0.6, 40, 64,
      "Adonis, Jerome", "MKBYN", 0.6, 42, 64,  
      "Jose, Relly Jr.", "KBL", 0.5, 42, 64,
      
      
      "Capuyan, Allen", "PPP", 0.5, 42, 64,
      "Andamo, Nars Alyn", "MKBYN", 0.5, 44, 64,
      "Cabonegro, Roy", "DPP", 0.4,45, 64,
      "Verceles, Leandro", "IND", 0.3, 47, 64
    )
  n_cols <- 5
  
  data_matrix <- matrix(
    long_vector,
    ncol = n_cols,
    byrow = TRUE
  )
  
  df <- as.data.frame(data_matrix)
  
  colnames(df) <- c("Candidate", "Party","Voting For", "Rank LB", "Rank UB")
  
  df$`Voting For` <- as.numeric(df$`Voting For`)
  df$`Rank LB` <- as.integer(df$`Rank LB`)
  df$`Rank UB` <- as.integer(df$`Rank UB`)
  
  # reference: https://www.gmanetwork.com/news/topstories/nation/943845/duterte-completes-2025-senate-slate-by-adding-querubin-honasan/story/
  df$DuterTen <- c(
    1, 0, 0, 1,
    0, 0, 0, 0,
    0, 0, 1, 0,
    0, 1, 1, 1,
    0, 0, 0, 1,
    0, 0, 1, 1,
    1, 1, 1, 1,
    1, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
  )
  
  df$Alyansa <- c(
    0, 1, 1, 0,
    1, 1, 1, 0,
    1, 1, 0, 0,
    1, 1, 1, 0,
    0, 0, 1, 0,
    1, 0, 0, 0,
    0, 0, 0, 0, 
    0, 0, 0, 0, 
    0, 0, 0, 0,  
    0, 0, 0, 0, 
    0, 0, 0, 0, 
    0, 0, 0, 0,  
    0, 0, 0, 0, 
    0, 0, 0, 0, 
    0, 0, 0, 0,  
    0, 0, 0, 0
  )
  
  df$KiBam <- c(
    0, 0, 0, 0, 
    0, 0, 0, 1,  
    0, 0, 0, 0, 
    0, 0, 0, 0, 
    1, 0, 0, 0,  
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0, 
    0, 0, 0, 0, 
    0, 0, 0, 0,  
    0, 0, 0, 0, 
    0, 0, 0, 0, 
    0, 0, 0, 0,  
    0, 0, 0, 0, 
    0, 0, 0, 0, 
    0, 0, 0, 0
  )
  
  if (persist_df) {
    saveRDS(df, file = "application_data.rds")
  }
  return(df)
}