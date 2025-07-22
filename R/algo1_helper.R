kde_cdf_function <- function(datapoints, bandwidth) {
  return(function(x) mean(pnorm((x - datapoints) / bandwidth)))
}

estimate_CDF <- function(dataset, i){
  S = sqrt(dataset[i,'variance'])#sd(r[,i])
  IQR = quantile(r[,i], .75) - quantile(r[,i], .25)
  h_i <- 0.9*min(S, IQR/1.34)*(B^(1/5))
  return(kde_cdf_function(r[,i], h_i))
}

get_inner_max <- function(value) return(max(value, 1-value))