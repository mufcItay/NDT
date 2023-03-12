library(dplyr)
library(readstata13) # read dta Stata 13 file
library(doBy)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# convert Stata 13 file
dat<-read.dta13(list.files(pattern = '.dta'))  
dat$subject <- factor(dat$subject)

# from here on, we follow S Forti, GW Humphreys_Analysis.do as a guide
# code targetPosition variable based on screen position
dat$targetPosition <- NA
dat[which(dat$position == 5),]$targetPosition <- 1
dat[which(dat$position == 4),]$targetPosition <- 2
dat[which(dat$position == 3),]$targetPosition <- 3
dat[which(dat$position == 2),]$targetPosition <- 4
dat[which(dat$position == 1),]$targetPosition <- 5
dat[which(dat$position == 8),]$targetPosition <- 6
dat[which(dat$position == 7),]$targetPosition <- 7
dat[which(dat$position == 6),]$targetPosition <- 8

# drop trials missing trigger and those where trigger is less than 4000 (i.e., blank screens & fixation cross slides)
datClean <- subset(dat, !is.na(trigger) & trigger >= 4000)
nrow(datClean)

# keep only correct trials in the analyses, i.e., those on which the target was eventually fixated upon
datClean$AOITarget <- FALSE
datClean[which(datClean$targetPosition == datClean$AOI),]$AOITarget <- 1

# exclude all trials where subject did not fixate on target at least once
didFixate <- summaryBy(AOITarget ~ subject + trigger, FUN=c(max, length), data=datClean)

# the following function accomplishes the above; note this takes a while, so we include a progress bar...
datClean$exclusion <- FALSE

computeExclusion <- function (didFixate, datClean) {
  
  pb <- txtProgressBar(style=3)
  for (i in 1:nrow(didFixate)) {
    if (didFixate[i,]$AOITarget.max != 1)
      datClean[which(datClean$subject == didFixate$subject[i] & datClean$trigger == didFixate$trigger[i]),]$exclusion <- TRUE
    
    setTxtProgressBar(pb, i/nrow(didFixate))
  }
  
  return(datClean)
}

datCleanExclusion <- computeExclusion(didFixate, datClean)

# author notes the number of dropped trials for targetPosition 3 and 7 (middle of screen, left and right) to be 2158, so I check this
nrow(subset(summaryBy(exclusion ~ subject + trigger + targetPosition, FUN=mean, data=datCleanExclusion), targetPosition == 3 | targetPosition == 7)) # 2158

# calculate amount of data removed (noted in author script as 23.66%)
nrow(subset(summaryBy(exclusion ~ subject + trigger + targetPosition, FUN=mean, data=datCleanExclusion), targetPosition == 3 | targetPosition == 7))/nrow(summaryBy(exclusion ~ subject + trigger + targetPosition, FUN=mean, data=datCleanExclusion)) # 23.66% matches up

# drop trials within the center left/right from the analysis
datCleanExclusionDrop <- subset(datCleanExclusion, targetPosition != 3 & targetPosition != 7)

# author notes 624 trials should be dropped by this action, so we check this
table(summaryBy(exclusion ~ subject + trigger + targetPosition, data = datCleanExclusionDrop)$exclusion.mean) # verified

# drop excluded trials
datCleanExclusionDropMore <- subset(datCleanExclusionDrop, exclusion != TRUE)

# drop trials where AOI equals 0
datCleanExclusionDropMoreAOI <- subset(datCleanExclusionDropMore, AOI != 0)

# sort data
datCleanExclusionDropMoreAOISorted <- datCleanExclusionDropMoreAOI[order(datCleanExclusionDropMoreAOI$subject, datCleanExclusionDropMoreAOI$trigger, datCleanExclusionDropMoreAOI$gazetime),]


# code trials by first fixation, using sort
datCleanExclusionDropMoreAOISorted$firstfix <- NA
firstfix <- plyr::ddply(datCleanExclusionDropMoreAOISorted,plyr::.(subject, trigger), .fun = function(x){
  x$firstfix <- 1:nrow(x)
  return(x)})

# keep only first fixation trials
firstfixOnly <- subset(firstfix, firstfix == 1)

# create coding for up factor
firstfixOnly$up <- 0
firstfixOnly[which(firstfixOnly$targetPosition == 1 | firstfixOnly$targetPosition == 2 | firstfixOnly$targetPosition == 8),]$up <- 1

# create coding for action factor
firstfixOnly$action <- 0
firstfixOnly[which(firstfixOnly$block == 1),]$action <- 1

# create coding for prototypical (proto) factor
firstfixOnly$prototypical <- firstfixOnly$target %% 2

data <- firstfixOnly %>% 
  dplyr::rename(idv = subject, iv2 = prototypical, iv = up, dv = AOITarget) %>% 
  dplyr::filter(idv != 8) %>%
  dplyr::mutate(exp = 'Forti_Humphreys_2008') %>%
  dplyr::select(idv,iv, iv2, dv,exp)

write.csv(data, 'Forti_Humphreys_2008.csv')

library(signcon)
get_effect_f <- function(mat, args = list(summary_f = mean, iv = 'iv2', dv = 'dv')) {
  mat <- as.data.frame(mat)
  mat$dv <- as.numeric(mat$dv)
  values <- mat %>% pull(dplyr::sym(args$iv))
  conds <- sort(unique(values))
  res <- args$summary_f(mat[values == conds[2],]$dv) - 
    args$summary_f(mat[values == conds[1],]$dv)
  return(res)
}

p <- test_directional_effect(data, idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                        summary_function = get_effect_f)$p
2 * min(p,1-p)
test_sign_consistency(data, idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                      summary_function = get_effect_f)$p
