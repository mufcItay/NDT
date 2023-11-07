#' ttest test function
#'
#' @description The function returns the within-participant t-test p-value, for
#' the given data.
#'
#' @param dataa dataframe with an independent variable column ('iv'),
#' and a dependent variable column ('dv') 
#'
#' @return the t-test within participant p-value 
ttest_tf <- function(data) {
  # run a t-test to compare the dv between the two-levels of the iv.
  ttest_res <- t.test(data$dv[data$iv == first(data$iv)],
                      data$dv[data$iv != first(data$iv)], 
                      paired = F)
  return(ttest_res$p.value)
}

#' prevalence test
#'
#' @description The function tests an hypothesis on prevalence of effects (gamma), 
#' see Donhauser et al., 2018.
#' 
#' @param data a dataframe of trial-by-trial data for all participants.
#' expects a dataframe with an ID column ('idv'), an
#' independent variable column ('iv'), and a dependent variable column ('dv') 
#' @param H0 the null hypothesis prevalence parameter (gamma; values in (0-1))
#' @param alpha the alpha level for each within-individual test and CI.
#' @param test_function the function to use for obtaining p-values for 
#' within-participant significance testing
#'
#' @return the function returns a list of the test results.
#' \itemize{
#'   \item p - the p-value for the binomial test on prevalence.
#'   \item stat - the obtained prevalence statistic.
#'   \item ci_low - the lower bound of a confidence interval (CI) on prevalence under H0.
#'   \item ci_high - the higher bound of a confidence interval (CI) on prevalence under H0.
#' }
prevalence_test <- function(data, H0 = 0, alpha = .05, test_function = ttest_tf) {
  sig_rate <- H0 + (1-H0) * alpha
  prev_results <- data %>%
    mutate(iv = factor(iv)) %>%
    group_by(idv) %>%
    do(summarise(., sig = summary_function(.) < alpha)) %>%
    summarise(S = sum(sig), N = n()) %>%
    select(S,N) %>%
    summarise_all(sum) %>%
    summarise(pval = 1- pbinom(S, N, sig_rate),
              stat = S/N,
              ci_low = qbinom(alpha/2, N, sig_rate),
              ci_high = qbinom(1 - alpha/2, N, sig_rate))
  return(list(p = prev_results$pval, stat = prev_results$stat,
              ci_low = prev_results$ci_low, ci_high = prev_results$ci_high))
}


#' run Global Null Test (GNT)
#'
#' @description The function tests the global null hypothesis (H0: gamma = 0). 
#'
#' @param data a dataframe of trial-by-trial data for all participants.
#' expects a dataframe with an ID column ('idv'), an
#' independent variable column ('iv'), and a dependent variable column ('dv') 
#' @param alpha the alpha level for each within-individual test and CI.
#' @param test_function the function to use for obtaining p-values for 
#' within-participant significance testing
#'
#' @return the function returns a list of the test results.
#' \itemize{
#'   \item p - the p-value for the binomial test on prevalence.
#'   \item stat - the obtained prevalence statistic.
#'   \item ci_low - the lower bound of a confidence interval (CI) on prevalence under H0.
#'   \item ci_high - the higher bound of a confidence interval (CI) on prevalence under H0.
#' }
run_gnt <- function(data, alpha = .05, summary_function = ttest_tf)  {
  # run the prevalence test, setting H0 to zero.
  return(prevalence_test(data, H0 = 0, alpha = alpha, summary_function = summary_function))
}
