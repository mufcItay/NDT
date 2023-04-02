
#' reset_wd
#' the function sets the working directory to the folder of the relevant script,
#' and returns the original folder (the working directory before running the script)
#'
#' @return the original folder (the working directory before running the script)
reset_wd <- function() {
  orig_fld <- getwd()
  cur_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(cur_dir)
  
  return(orig_fld)
}

save_data_wd <- function() {
  orig_fld <- getwd()
  cur_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(cur_dir)
  
  return(orig_fld)
}