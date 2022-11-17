library(dplyr)
# Assumptions: 
# 1. variables are known in advance
# 2. one-sided test
run_pbt <- function(data, test_function) {
  res_dir <- data %>% 
    group_by(idv) %>%
    summarise(p= test_function(dv[iv==unique(iv)[1]],dv[iv==unique(iv)[2]])$p.value) %>%
    mutate(sig=p<0.05)
  # count the prevalence of significant results in each dataset and compare to chance
  N=sum(!is.na(res_dir$p))
  Nsig = sum(res_dir$p < .05, na.rm=T)
  resolution <- 10^2
  prevalence_pdf = bayesprev_posterior(seq(0, 1, 1 / resolution), Nsig, N)
  prevalence_hdpi = bayesprev_hpdi(.96, Nsig, N)
  prev_above_zero <- 0 < prevalence_hdpi[1]
  prealence_est = bayesprev_map(Nsig, N)
  prealence_est_prob = bayesprev_posterior(prealence_est, Nsig, N)
  return (prealence_est)
}