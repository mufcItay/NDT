library(dplyr)
# analyze results
# read the results of all tests
results_folder <- 'results'
results_fns <- list.files(results_folder, pattern = '.csv', full.names = TRUE)
read_res_df <- function(fn) {
  df <- read.csv(fn) %>%
    mutate(type = gsub(basename(fn), pattern="_Results.csv$", replacement=""))
}
results <- do.call(rbind, lapply(results_fns, read_res_df))

bf_criteria <- 3
alpha_effect <- .05
results$quid_bf <- ifelse(results$quid_bf == INVALID_VALUE_CODE,
                          INVALID_VALUE_CODE, 1/results$quid_bf)

#' The function summarizes the results according to whether 
#' an effect was found according to the directional test
#' @param dir_effect_included a Boolean controlling whether to filter
#' experiments for which an effect was found or n.s results 
#'
#' @return a summary data frame with the number of experiments
#' under each analysis type and the respective number of
#' significant results
sum_res <- function(dir_effect_included = FALSE) {
  results %>% 
    mutate(effect_pbt = pbt.low>0, 
           effect_quid = quid_bf>bf_criteria,
           effect_dir = directional_effect.p<alpha_effect,
           effect_non.dir = non_directional.p<alpha_effect) %>%
    filter(effect_dir == dir_effect_included) %>%
    group_by(type) %>%
    summarise(N = n(), 
              pbt = sum(effect_pbt)/n(),
              quid = sum(effect_quid)/n(),
              dir = sum(effect_dir)/n(),
              non.dir = sum(effect_non.dir)/n())  
}
# results fo experiments with n.s directional test results
sum_res(FALSE)
# results fo experiments with significant directional test results
sum_res(TRUE)