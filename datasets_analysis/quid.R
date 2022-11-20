library(BayesFactor)
library(MCMCpack)
# source quid function (Rouder & Haaf, 2021; Are There Reliable Qualitative Individual Difference in Cognition?)
source("https://bit.ly/2ZqGOik")

run_quid <- function(data) {
  # run the function only on two conditions, continuous variable datasets
  if((length(unique(data$iv2)) > 1) | 
     (setequal(unique(data$dv),c(0,1)))) {
    return (list(pos_bf = -99999))
  }
  conditions <- unique(data$iv)
  data$idv <- as.numeric(factor(data$idv))
  data$iv <- as.numeric(factor(data$iv))
  res <- quid(id = data$idv, condition = data$iv, rt = data$dv)
  # indicating how much evidence we see for qualitative differences between individuals
  return (list(pos_bf = res$bfs['bf.pu']))
}