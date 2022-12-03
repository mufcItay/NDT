library(BayesFactor)
library(MCMCpack)
# source quid function (Rouder & Haaf, 2021; 
# Are There Reliable Qualitative Individual Difference in Cognition?)
source("https://bit.ly/2ZqGOik")

run_quid <- function(data) {
  # run the function only on two conditions, continuous variable datasets
  is_interaction_effect <- length(unique(data$iv2)) > 1 
  is_accuracy_efect <- setequal(unique(data$dv),c(0,1)) 
  if(is_interaction_effect | is_accuracy_efect) { return (list(pos_bf = -99999))}
  # run quid solution
  data$idv <- as.numeric(factor(data$idv))
  data$iv <- as.numeric(factor(data$iv))
  res <- quid(id = data$idv, condition = data$iv, rt = data$dv)
  # indicating stronger credibility for the null model,
  # over the random effects model
  null_bf <- res$bfs['bf.0u']
  return (list(pos_bf = null_bf))
}