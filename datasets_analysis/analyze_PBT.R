library(dplyr)
library(weaknull)
library(tidyr)
dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_path)
source('utils.R')

set.seed(101)

Ns <- c(10, 30)
TrialNs <- c(100,200)
bSDs = c(.5,1)
#bSDs = c(2)
iter <- 1:100
params <- as.matrix(crossing(Ns, TrialNs, bSDs,iter))
params_unique <- as.matrix(crossing(Ns, TrialNs, bSDs))
datasets <- do.call(rbind, lapply(1:nrow(params), function(rowInd) 
  create_sample_data(p_mean = 0, p_sd = params[rowInd,'bSDs'], seed = params[rowInd, 'iter'], N = params[rowInd,'Ns'], 
                     trials_per_cnd = params[rowInd,'TrialNs'], wSEsd = 10) %>% 
    rename(iv = condition, dv1 = var) %>%
    mutate(exp = rowInd, NSuj = params[rowInd,'Ns'], NTrials = params[rowInd,'Ns'], bSD = params[rowInd,'bSDs'], param_type = ceiling(rowInd / max(iter)))))

prevalence <-datasets %>% group_by(param_type, exp,id) %>%
  # run a Wilcoxon test between the two values of the iv for each id
  summarise(p=wilcox.test(dv1[iv==unique(iv)[1]],dv1[iv==unique(iv)[2]])$p.value) %>%
  mutate(sig=p<0.05)%>%
  group_by(param_type, exp) %>%
  # count the prevalence of significant results in each dataset and compare to chance
  summarise(N=sum(!is.na(p)),
            Nsig = sum(sig, na.rm=T),
            prevalence.p=binom.test(Nsig,N,0.05)$p.value)
sign_consistency <- datasets %>%
  group_by(exp) %>%
  group_modify(~data.frame(non_directional = test_sign_consistency(.x,'id', 'dv1', 'iv', null_dist_samples = 10000)[c('statistic','p')]))


write.csv(prevalence.UC, 'results\\WilcoxPrevalencePrimingDB.csv')
###########################################

############# CONFIDENCE ##################
# write the final database to file - these are parameters and will change for each analysis
input_fld <- 'data\\ConfDB'
output_fn <- 'results\\ConfidenceDB.csv'

# write the full aggregated and preprocessed database to a file
confidence.dfs <- writeDBToFile(input_fld, output_fn, preprocess_dfs_confidence)

prevalence.confidence <-confidence.dfs %>% group_by(exp,id) %>%
  # run a t test between the two values of the iv for each id
  summarise(
    var1 = var(dv[iv==unique(iv)[1]]),
    var2 = var(dv[iv==unique(iv)[2]]),
    p=ifelse(min(var1,var2)>0, t.test(dv[iv==unique(iv)[1]],dv[iv==unique(iv)[2]])$p.value, NA)) %>%
  mutate(sig=p<0.05)%>%
  group_by(exp) %>%
  # count the prevalence of significant results in each dataset and compare to chance
  summarise(N=sum(!is.na(p)),
            Nsig = sum(sig, na.rm=T),
            prevalence.p=binom.test(Nsig,N,0.05)$p.value);

write.csv(prevalence.confidence, 'results\\TPrevalenceConfidenceDB.csv')
###########################################


prevalence <- rbind(prevalence.confidence,prevalence.UC);
all_effects_res <- read.csv('results\\all_effects_res.csv')
all_effects_res_with_prevalence <- merge(all_effects_res,prevalence);

write.csv(write.csv(all_effects_res_with_prevalence, 'results\\all_effects_res_with_prevalence.csv'))
