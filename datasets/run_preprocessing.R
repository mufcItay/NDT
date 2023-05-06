library(groundhog)
groundhog.library(c(
  'dplyr',
  'R.matlab',
  'readxl',
  'tidyverse',
  'data.table',
  'tidyr',
  'reshape2',
  'foreign',
  'readstata13',
  'doBy',
  'nleqslv'), '2023-04-10')

datasets_dir <- 'datasets'
datasets_analysis_dir <- 'datasets_analysis' 
prep_prefix <- 'prep'

source(paste(datasets_analysis_dir, 'definitions.R',sep = .Platform$file.sep))

#' preprocess_datasets
#' the function calls all preprocessing scripts and transfers the resulting csvs
#' of each dataset into the relevant directory under 'datasets'
#'
#' @param analysis_type the type of analysis for which preprocessing is ran
preprocess_datasets <- function(analysis_type) {
  db_prep_dir <- paste(datasets_dir, 
                        paste0(analysis_type, '_', prep_prefix), sep = .Platform$file.sep)
  orig_fld <- rprojroot::find_rstudio_root_file()
  # get all preprocessing scripts
  prep_scripts <- list.files(db_prep_dir,pattern = paste0(prep_prefix,'*'),
                            recursive = TRUE, full.names = TRUE)
  save_preprocessed_csv <- function(script_path) {
    setwd(paste(orig_fld, dirname(script_path), sep = .Platform$file.sep))
    print(basename(script_path))
    source(basename(script_path))
  }
  # run all preprocessing scripts
  sapply(prep_scripts, save_preprocessed_csv)
  setwd(orig_fld)
  # we want all new csv files (one per script)
  csvs_info <- file.info(list.files(db_prep_dir,pattern = '.csv', recursive = TRUE, 
                             full.names = TRUE))
  dataset_paths <- rownames(csvs_info[order(csvs_info$mtime, decreasing = TRUE),]
                            [1:length(prep_scripts),])
  # transfer the new csv files into the respective directory
  file.copy(from = paste(orig_fld, dataset_paths, sep = .Platform$file.sep),
            to = paste(orig_fld, datasets_dir, analysis_type,
                       sapply(dataset_paths, basename), sep = .Platform$file.sep))
}

# preprocess the unconscious processing datasets
preprocess_datasets(Unconscious_Processing_analysis_lbl)
# preprocess the cognitive psychology datasets
preprocess_datasets(Cognitive_Psychology_analysis_lbl)
# preprocess the confidence database datasets
source(paste(datasets_dir, Confidence_analysis_lbl, 
             'preprocess_CDB.R',sep = .Platform$file.sep))