######################################################################
## Zerweck et al., 2020
## Analysis Experiment 3 - prime Durations 40 ms and 50 ms:
##  
##   for the indirect task
##  - read data and exclude erroneous + too fast/slow RT trials
##  - average across trials per participant
##  - perform classification based on median split
##  - calculate dprime based on median classifier
##  - perform paired t-test on RTdiff and dprime 
##
##   for the direct task (discrimination direct task)
##  - read data and exclude too fast/slow RT trials
##  - average across trials for each participant 
##  - calculate PCorr and dprime
##  - perform paired t-test on PCorr and dprime 
##
##   for the sensitivity analysis (Test for ITA)
##  - calculate statistics for dprime direct vs indirect (mean,sd,etc)
##  - perform paired t-test on dprime direct vs. indirect
######################################################################

## Startup:
cat("Clear workspace\n")
rm(list=ls()) 
cat("Loading local-lib.R\n")
source("local-lib_Exp3.R")

## Settings:
myDebug <- F

## specify condition
PrimeDuration = 40  #change here to load and analyse data with different prime duration


##################################################################################
##################################################################################
#### Analysis indirect task (rttask) ####
##################################################################################
##################################################################################

## Read data:
# indirect task
echo1("----------------------------------------------------------------------")
echo3("Read data of Experiment 3 / Indirect Task at", PrimeDuration, "ms: \n")
rttask <- read.table(paste("data_Exp3/om1203_rttask_",PrimeDuration,".dat", sep=""),header=T)
  
## Print structure of data for checking (only when debugging)
if(myDebug){
  cat("Structure of data:\n")
  str(paste("rttask_",PrimeDuration))
}

##Exclude practice trials:
echo1("----------------------------------------------------------------------")
echo1("Excluding practice trials:")
echo4("   Total no. trials at",PrimeDuration,"ms:",length(rttask$block_type))
echo4("   Practice trials  at",PrimeDuration,"ms:",
      length(rttask[grep(".*Prac.*",rttask$block_type),]$block_type))
rttask <- rttask[grep(".*Exp.*",rttask$block_type),]
  
##Remove too fast/slow responses:
echo1("----------------------------------------------------------------------")
echo1("Excluding too fast/slow responses:")
echo4("   Total no. trials at",PrimeDuration,"ms:",length(rttask$time_error))
echo4("   Too early trials at",PrimeDuration,"ms:",sum(rttask$time_error=="Early"))
echo4("   Too late  trials at",PrimeDuration,"ms:",sum(rttask$time_error=="Late"))
rttask <- rttask[rttask$time_error=="Normal",]
  
##Remove trials with wrong response:
echo1("----------------------------------------------------------------------")
echo1("Excluding trials with wrong response:")
echo4("   Total no. trials at",PrimeDuration,"ms:",length(rttask$key_error))
echo4("   Correct   trials at",PrimeDuration,"ms:",sum(rttask$key_error=="correct"))
echo4("   Error     trials at",PrimeDuration,"ms:",sum(rttask$key_error=="error"))
rttask <- rttask[rttask$key_error=="correct",]
echo1("----------------------------------------------------------------------")

## perform analysis for each participant
echo3("Analysis indirect task at",PrimeDuration,"ms")
indResults = data.frame()

for(part in unique(rttask$sub))
{
  echo2("Working on participant:",part)
  ##Select this participant's data:
  partData <- rttask[rttask$sub==part,]
  
  ##Perform standard analyses on this participant:
  partCongrRTs            <- partData[partData$congruency==
                                        "Congruent","RT"]
  partInconRTs            <- partData[partData$congruency==
                                        "Incongruent","RT"]
  partConN                <- length(partCongrRTs)
  partInconN              <- length(partInconRTs)
  partConRTmean           <- mean(partCongrRTs)
  partInconRTmean         <- mean(partInconRTs)
  partConRTsd             <- sd(partCongrRTs)
  partInconRTsd           <- sd(partInconRTs)
  
  ##Classifier:
  ## - First recode data to be compatible with our library-function
  ##   which is based on the coding:
  ##   0=congruent / 1=incongruent
  partData$congruencyTBCoded <- partData$congruency=="Incongruent"
  ## - then perform classification and calculate dprime:
  partDprimeRT = medianClassifierDprime(partData$RT, 
                                        partData$congruencyTBCoded)
  
  ##Collect results:
  indResults <- rbind(indResults,
                         data.frame(part          = part,
                                    conN          = partConN,
                                    inconN        = partInconN,
                                    conRTmean     = partConRTmean,
                                    inconRTmean   = partInconRTmean,
                                    conRTsd       = partConRTsd,
                                    inconRTsd     = partInconRTsd,
                                    RTdiff        = partInconRTmean-partConRTmean,
                                    dprime        = partDprimeRT))
}



## some additional statistics
# t-tests for RTdiff and dprime 
tRTdiff  <- t.test(indResults$RTdiff)
tdprime  <- t.test(indResults$dprime) 
# Number of participants
Nparticipants <- length(indResults$RTdiff)


##Print results for the indirect task:
echo1("----------------------------------------------------------------------")
echo3("Indirect task results for prime duration of",PrimeDuration,"ms")
echo1("----------------------------------------------------------------------")
echo2("...number of participants:",Nparticipants)
echo1("----------------------------------------------------------------------")
echo1("RT-difference betw. congruent and incongruent conditions:")
echo1("----------------------------------------------------------------------")
echo3("Mean   of RTdiff:                     ", mean(indResults$RTdiff),"ms")
echo3("SD     of RTdiff:                     ", sd(indResults$RTdiff)  ,"ms")
echo3("SEM    of RTdiff:                     ", mySEM(indResults$RTdiff),"ms")
echot("t-test of RTdiff against zero:        ", tRTdiff)
echo1("----------------------------------------------------------------------")

echo1("Sensitivity dprime (indirect task):")
echo1("----------------------------------------------------------------------")
echo2("Mean    dprime:                       ", mean(indResults$dprime) )
echo2("SD      dprime:                       ", sd(indResults$dprime) )
echo2("SEM     dprime:                       ", mySEM(indResults$dprime) )
echot("t-test  dprime against 0:             ", tdprime)
echo1("----------------------------------------------------------------------")


##################################################################################
##################################################################################
#### Analysis direct task (perctask) ####
##################################################################################
##################################################################################

## Read data:
# direct task
echo1("----------------------------------------------------------------------")
echo3("Read data of Experiment 3 / Direct Task at",PrimeDuration,"ms\n")
perc <- read.table(paste("data_Exp3/om1203_percI_",PrimeDuration,".dat", sep=""),header=T)

##Exclude practice trials:
echo1("----------------------------------------------------------------------")
echo1("Excluding practice trials:")
echo4("   Total no. trials at",PrimeDuration,"ms:",length(perc$block_type))
echo4("   Practice trials  at",PrimeDuration,"ms:",
      length(perc[grep(".*Prac.*",perc$block_type),]$block_type))
perc <- perc[grep(".*Exp.*",perc$block_type),]


##Remove too fast/slow responses:
echo1("----------------------------------------------------------------------")
echo1("Excluding too fast/slow responses:")
echo4("   Total no. trials at",PrimeDuration,"ms:",length(perc$time_error))
echo4("   Too early trials at",PrimeDuration,"ms:",sum(perc$time_error=="Early"))
echo4("   Too late  trials at",PrimeDuration,"ms:",sum(perc$time_error=="Late"))
perc <- perc[perc$time_error=="Normal",]
echo1("----------------------------------------------------------------------")

## perform analysis for each participant
echo3("Analysis direct task at",PrimeDuration,"ms")
dirResults = data.frame()

for(part in unique(perc$sub))
{
  echo2("Working on participant:",part)
  ##Select this participant's data:
  partData <- perc[perc$sub==part,]
  
  ##Calculate percent correct for this participant:
  partNtrials <- length(partData$key_error)
  partPCorr   <- mean(partData$key_error=="correct")*100
  
  ## calculate dprime for this participant:
  HR <- mean((partData$key_error=="correct")
             [grepl("[6,9]", partData$prime)])
  FA <- 1 - mean((partData$key_error=="correct")
                 [grepl("[1,4]", partData$prime)])
  
  partdprime <- qnorm(HR) - qnorm(FA)
  
  ##Collect results:
  dirResults <- rbind(dirResults,
                         data.frame(part        = part,
                                    Ntrials     = partNtrials,
                                    PCorr       = partPCorr,
                                    dprime      = partdprime))
}


## some additional statistics 
Nparticipants <- length(dirResults$PCorr)
PCorr         <- dirResults$PCorr
# t-tests for Percent Correct and dprime 
tPCorr      <- t.test(dirResults$PCorr, mu=50)
tdprime     <- t.test(dirResults$dprime) 

##Print results for the direct task:
echo1("----------------------------------------------------------------------")
echo3("Direct task results for prime duration of",PrimeDuration,"ms")
echo1("----------------------------------------------------------------------")
echo4("...number of participants, direct task at",PrimeDuration,"ms: ",Nparticipants)
echo1("----------------------------------------------------------------------")
echo1("Percent-correct (direct task):")
echo1("----------------------------------------------------------------------")
echo3("Mean   of PCorr:                ", mean(dirResults$PCorr),"ms")
echo3("SD     of PCorr:                ", sd(dirResults$PCorr)  ,"ms")
echo3("SEM    of PCorr:                ", mySEM(dirResults$PCorr),"ms")
echot("t-test of PCorr against 50%:    ", tPCorr)
echo1("----------------------------------------------------------------------")

echo1("Sensitivity dprime (direct task):")
echo1("----------------------------------------------------------------------")
echo2("Mean    dprime:                       ", mean(dirResults$dprime) )
echo2("SD      dprime:                       ", sd(dirResults$dprime) )
echo2("SEM     dprime:                       ", mySEM(dirResults$dprime) )
echot("t-test  dprime against 0:             ", tdprime)
echo1("----------------------------------------------------------------------")


##################################################################################
##################################################################################
#### Sensitivity analysis (direct vs indirect; ITA) ####
##################################################################################
##################################################################################

echo1("----------------------------------------------------------------------")
echo3("Perform sensitivity analysis comparing both tasks (ITA check) at",PrimeDuration,"ms:")
echo1("----------------------------------------------------------------------")

# calculate difference between indirect and direct task dprime
dprimeInd  <- indResults$dprime
dprimeDir  <- dirResults$dprime
dprimeDiff <- dprimeInd-dprimeDir

# calculate t-test of dprimeDiff against zero (Test for an ITA)
tdprimeDiff <- t.test(dprimeDiff)

# Print results for the sensitivity analysis (ITA Test)
echo1("----------------------------------------------------------------------")
echo3("ITA results for prime duration of",PrimeDuration,"ms")
echo1("----------------------------------------------------------------------")
echo1("----------------------------------------------------------------------")
echo1("difference in dprime (indirect-direct):")
echo1("----------------------------------------------------------------------")
echo2("Mean of   dprime      indirect task:           ", mean(dprimeInd))
echo2("Mean of   dprime      direct task:             ", mean(dprimeDir))
echo2("SD of     dprime      indirect task:           ", sd(dprimeInd))
echo2("SD of     dprime      direct task:             ", sd(dprimeDir))
echo2("mean of   dprimeDiff (indirect-direct):        ", mean(dprimeDiff))
echo2("SD of     dprimeDiff  indirect-direct:         ", sd(dprimeDiff))
echot("t-test of dprimeDiff  against zero             ", tdprimeDiff)
echo1("----------------------------------------------------------------------")
