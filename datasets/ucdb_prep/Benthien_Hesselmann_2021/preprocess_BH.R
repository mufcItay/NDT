library(signcon)
library(dplyr)
library(tidyr)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)


#create function to exclude observations
#d takes a data frame 
#subj are the subject to exclude - integer values
exclude_obs <- function(d, subj){
  return(d[!(d$subj %in% subj),])}

# IQR outlier
identifyOutliers <-  function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm=TRUE, type = 5)
  H <- 1.5 * (quantile(x, .75, na.rm=TRUE, type = 5) - quantile(x, .25, na.rm=TRUE, type = 5))
  outlier <- ((x) < (qnt[1] - H)) | ((x) > qnt[2] + H) | x < .10
  outlier
}

#conditions (cond)
#visible certain (1), visible uncertain (2), invisible certain (3), invisible uncertain (4)
# certain = centrally presented primes
# uncertain = (random) peripherally presented primes

#read the data set 
data <- read.csv("CFSlocation_data.csv", header=F, sep=",", dec=".")
#column names
colnames(data) <- c("exp","subj","trial","prime","cond","pos","probe","alpha","RT","resp1","pre","RT2","resp2")

#split
data_main <- subset(data, select=c(exp, subj, trial, prime, cond, pos, probe, alpha, RT, resp1)) #without task2
data_main <- filter(data_main, exp ==1)

#main: remove training trials (first 16 of each block)
data_main$train <- ifelse ((data_main$trial>0 & data_main$trial<17) | (data_main$trial>144 & data_main$trial<161) |
                             (data_main$trial>288 & data_main$trial<305) | (data_main$trial>432 & data_main$trial<449), 1, 0)
data_main <- filter(data_main, train == 0)

#exclude subj 18, 19, 29, 30, 31 (due to slopes and discrimination performance) from further analyses
data_main <- exclude_obs(d = data_main, subj =  c(18, 19, 29, 30, 31))   # valid measures n = 26 participants
data_main$correct <- ifelse((data_main$probe<5 & data_main$resp1==1) | (data_main$probe>5 & data_main$resp1==2), 1, 0)
data_main = filter(data_main, correct == 1)
#identify anticipatory responses (RT < 100ms, none)
data_main$toofast <- ifelse(data_main$RT < 0.1, 1, 0)
data_main = filter(data_main, toofast ==0)

#data_main_cfs <- filter(data_main, cond>2)
#identify outliers (Tukey, IQR), per participant
data_main <- data_main %>%
  group_by(subj) %>%
  mutate(outlier = identifyOutliers(RT))
data_main <- data_main %>% mutate(outlier = as.numeric(outlier)) # 1 = outlier, 0 = non-outlier
#remove outliers
data_main = filter(data_main, outlier == 0)

#create variable for congruency, visibility and location un/certainty
data_main$congruency <- ifelse ((data_main$prime<5 & data_main$probe<5) | (data_main$prime>5 & data_main$probe>5), 1, 0)
data_main$visibility <- ifelse ((data_main$cond==1) | (data_main$cond==2), 1, 0)
data_main$certainty <- ifelse ((data_main$cond==1) | (data_main$cond==3), 1, 0)
study_name <- 'Benthien & Hesselmann_2021'

data <- data_main %>% 
  filter(visibility==0) %>%
  rename(idv = subj, iv = congruency, iv2 = certainty, dv = RT) %>%
  mutate(exp = study_name, dv = dv * 1000) %>% 
  select(idv, iv, iv2, exp, dv)


write.csv(data, 'Benthien & Hesselmann_2021.csv')


f_int_rt <- function(mat) {
  res <- stats::median(mat[mat[,'iv2'] == 0, 'dv']) - 
    stats::median(mat[mat[,'iv2'] == 1, 'dv'])
  return (res)
}

# nondir_tst <- test_sign_consistency(data, idv = 'idv', dv = c('dv','iv2'), iv = 'iv', 
#                                     null_dist_samples = 10^5, 
#                                     summary_function = f_int_rt)

# validate against Table. 1 in the paper
data %>% group_by(idv, iv2, iv) %>% summarise(m = mean(dv)) %>%
  group_by(iv2, iv) %>% summarise(m = mean(m))

########################################### UTILITY
calc_effect <- function(mat, args = list(summary_f = mean, iv = 'iv', dv = 'dv')) {
  return (as.data.frame(mat) %>% 
    group_by(!!dplyr::sym(args$iv)) %>% 
    summarise(val = args$summary_f(!!dplyr::sym(args$dv))) %>% 
    summarise(effect = diff(val)) %>% 
    pull(effect))
}  

perm_test_subject <- function(mat, obs, summary_f, summary_f_args = list(iv = 'iv', dv = 'dv'), 
                              n_perm = 10^4, two.sided = TRUE) {
  inner_perm <- function(iteration, mat, summary_f, summary_f_args) {
    n_trials <- nrow(mat)
    mat[,summary_f_args$dv] <- mat[sample(n_trials),summary_f_args$dv]
    return (summary_f(mat, summary_f_args))
  }
  if('iv2' %in% summary_f_args) {
    resamp_f_args <- summary_f_args
    resamp_f_args$iv = resamp_f_args$iv2 
    resample_f <- function(iteration, mat) {
      summary_f(mat[sample(nrow(mat), replace = TRUE), ], resamp_f_args)
    }
    conds <- unique(mat[,summary_f_args$iv])
    iv1 <- sapply (1:n_perm, resample_f, mat=mat[mat[,summary_f_args$iv] == conds[1],])
    iv2 <- sapply (1:n_perm, resample_f, mat=mat[mat[,summary_f_args$iv] == conds[1],])
    null_dist <- sample(c(-1,1), n_perm, replace = TRUE) * (iv1 - iv2)
  } else {
    null_dist <- sapply(1:n_perm, inner_perm, mat = mat, 
                        summary_f = summary_f, summary_f_args = summary_f_args)
    
  }
  p_value <- mean(obs < null_dist, na.rm=TRUE)
  if(two.sided) {p_value <- 2 * min(p_value, 1 - p_value)}
  return (p_value)
}

summary_f <- function(mat) {
  benthien_hesselmann_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', summary_f = mean)
  return (calc_effect(mat, args = benthien_hesselmann_args))
}
test_f <- function(mat) {
  benthien_hesselmann_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', summary_f = mean)
  obs <- calc_effect(mat, args = benthien_hesselmann_args)
  return(perm_test_subject(as.matrix(mat), obs, 
                           summary_f = calc_effect, summary_f_args = benthien_hesselmann_args))
}

########################################### UTILITY

summary_f(data[data$idv == 1,])
res <- data %>% select(idv, iv, iv2, dv) %>% group_by(idv) %>% 
   group_modify(~data.frame(p = test_f(.x)))
