library(BayesFactor)
library(MCMCpack)
source(paste('datasets_analysis', 'definitions.R',
             sep = .Platform$file.sep))

# source quid function (Rouder & Haaf, 2021; 
# Are There Reliable Qualitative Individual Difference in Cognition?)
# downloaded from https://bit.ly/2ZqGOik:
source(paste('datasets_analysis', 'quid_imp.R',
              sep = .Platform$file.sep))

#' run_quid
#' The function runs the QUID solution on the given dataset.
#' @param data a dataframe with the shape (#Participants X #Trials) X (idv, iv, dv),
#' where idv is the identifier of each participant, iv is the experimental condition, 
#' and dv is the dependent measure (reaction time)
#' @return returns the result of a model comparison between a null effects model and the
#' random effects model ('unconstrained model').
#' An invalid value code (see definitions.R) is returned if the dataset includes another
#' indepdent variable (iv2) or if the depdent measure includes only two possible values (as in
#' accuracy analysis). 
run_quid <- function(data) {
  # run the function only on two conditions, continuous variable datasets
  is_interaction_effect <- length(unique(data$iv2)) > 1 
  is_accuracy_efect <- setequal(unique(data$dv),c(0,1)) 
  if(is_interaction_effect | is_accuracy_efect) { 
    return (list(quid_bf = INVALID_VALUE_CODE))
  }
  # run quid solution
  data$idv <- as.numeric(factor(data$idv))
  data$iv <- as.numeric(factor(data$iv))
  res <- quid(id = data$idv, condition = data$iv, rt = data$dv)
  # indicating stronger credibility for the null model,
  # over the random effects model
  null_bf <- res$bfs['bf.0u']
  return (list(quid_bf = null_bf))
}