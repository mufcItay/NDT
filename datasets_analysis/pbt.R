library(dplyr)
source('datasets_analysis\\bayesprev.R')
# Assumptions: 
# 1. variables are known in advance
# 2. one-sided test
run_pbt <- function(data, test_function, alpha = .05) {
  # run the function only on two conditions, continuous variable datasets
  if(length(unique(data$iv2)) > 1 | (setequal(unique(data$dv),c(0,1)))) {
    return (list(low = -1, high = -1))
  }
  res_dir <- data %>% 
    group_by(idv) %>%
    summarise(p= test_function(dv[iv==unique(iv)[1]],dv[iv==unique(iv)[2]])$p.value) %>%
    mutate(sig=p<alpha)
  # count the prevalence of significant results in each dataset and compare to chance
  N=sum(!is.na(res_dir$p))
  Nsig = sum(res_dir$p < .05, na.rm=T)
  resolution <- 10^2
  prevalence_pdf = bayesprev_posterior(seq(0, 1, 1 / resolution), Nsig, N)
  prevalence_hdpi = bayesprev_hpdi(.96, Nsig, N)
  prev_above_zero <- 0 < prevalence_hdpi[1]
  prevalence_est = bayesprev_map(Nsig, N)
  prevalence_est_prob = bayesprev_posterior(prevalence_est, Nsig, N)
  return (list(low = prevalence_hdpi[1], high = prevalence_hdpi[2]))
}