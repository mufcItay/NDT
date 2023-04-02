library(dplyr)

#main effect
# full model:
# y = mu = A + S + A:S + e
main_effect_model <- formula('dv ~ iv + idv + iv:idv')
# null model:
# y = mu + S + e
main_effect_null_model <- formula('dv ~ idv')

# interaction
# full model:
# y = mu = A + B + S + AB + SA + SB + SAB + e
interaction_model <- formula('dv ~ iv + iv2 + idv + iv:iv2 + iv:idv + iv2:idv + idv:iv:iv2')
# null model:
# y = mu + A + B + S + SA + SB + e
interaction_null_model <- formula('dv ~ iv + iv2 + idv + iv:idv + iv2:idv')

run_oanova_test <- function(data) {
  data$idv <- factor(data$idv)
  data$iv <- factor(data$iv)
  
  # run the function only on two conditions, continuous variable datasets
  is_accuracy_efect <- setequal(unique(data$dv),c(0,1)) 
  if(is_accuracy_efect) { 
    return (list(F = INVALID_VALUE_CODE, p = INVALID_VALUE_CODE))
  }
  
  is_interaction_effect <- length(unique(data$iv2)) > 1 
  if(is_interaction_effect) {
    data$iv2 <- factor(data$iv2)
    fm <- interaction_model
    nm <- interaction_null_model
  } else {
    fm <- main_effect_model
    nm <- main_effect_null_model
  }
  return(base_oanova_test(data,full_model = fm, null_model = nm))
}

base_oanova_test <- function(data, full_model, null_model) {
  # get ss and df for E* under SoW1
  # based on Appendix C - Miller & Schwarz, 2018 (E. 9)
  null_model_table <- summary(data %>% aov(formula = null_model))[[1]]
  # last row in the table is the Residuals
  SSE_star <- null_model_table$`Sum Sq`[[nrow(null_model_table)]]
  df_E_star <- null_model_table$Df[[nrow(null_model_table)]]
  
  # get ss and df for E, according to the full model
  model_table <- summary(data %>% aov(formula = full_model))[[1]]
  # last row in the table is the Residuals
  SSE <- model_table$`Sum Sq`[[nrow(model_table)]]
  df_E <- model_table$Df[[nrow(model_table)]]
  # calculate F value
  # from Appendix C - Miller & Schwarz, 2018 (E. 11)
  f_value <- ((SSE_star - SSE) / (df_E_star - df_E)) / (SSE / df_E)
  df_numerator <- df_E_star - df_E
  # calculate p-value
  p <- 1- pf(f_value,df_numerator, df_E)
  return(list(F = f_value, p = p, 
              df_num = df_numerator, df_denom = df_E))
}