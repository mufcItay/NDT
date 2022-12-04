preprocess_dfs_cogdb  <- function(df, ds_name) {
  #exclude subjects from experiments if the have too few trials in each cell
  df <- df %>% 
    mutate(iv = factor(iv))
  if(!'iv2' %in% names(df)) { df$iv2 = NA }
  exclusions <- df %>%
    group_by(idv, iv, iv2) %>% 
    count(idv, name = "n", .drop = F) %>% 
    filter (n < 2) %>% 
    pull(idv)
  df <- df %>% 
    dplyr::select(idv,iv,iv2,dv,exp)
  if(length(exclusions)) { df <- df %>% filter(! idv %in% exclusions) }
  
  return (df)
}

# retrieves the database to analyze (including all individual experiments)
get_sum_fs_cogdb <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    if(startsWith(exp_name,'jasifi')) {
      jasifi_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', 
                          summary_f = analysis_conf@summary_f)
      
      summary_f <- function(mat) {
        conds <- sort(unique(mat[,'iv2']))
        res <- mean(mat[mat[,'iv2'] == conds[2], 'dv']) - 
          mean(mat[mat[,'iv2'] == conds[1], 'dv'])
        return (res)
      }
      test_f <- function(mat) {
        args <- jasifi_args
        args$iv = 'iv2'
        conds <- sort(unique(mat$iv))
        obs <- get_diff_effect(mat[mat$iv == conds[1],], args = args) -
          get_diff_effect(mat[mat$iv == conds[2],], args = args)
        return(perm_test_subject(as.data.frame(mat), obs, summary_f = get_diff_effect, 
                          summary_f_args = jasifi_args))
        }
      } else {
        summary_f <- function(mat) {
          # if the data is not arranged as a matrix, rearrange and apply summary f
          if(is.null(dimnames(mat))) {
            return(analysis_conf@summary_f(t(mat)[,'dv']))
          }
          return (analysis_conf@summary_f(mat[,'dv']))
        }
        test_f <- function(mat) {
          mat <- as.data.frame(mat)

          return(wilcox.test(mat[mat$iv==unique(mat$iv)[1],]$dv,
                             mat[mat$iv==unique(mat$iv)[2],]$dv)$p.value)
        }
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

