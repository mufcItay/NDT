######################################################################
## Zerweck et al., 2020
## library for Analysis Experiment 2
##  
##   this local lib contains
##  - add column with contrast in cd/m²
##  - median Classifier on RTs (indirect task) to get percent correct
##  - median Classifier to get dprime
##  - function to calculate simple standard error of the mean
##  - convenience functions for printing results
######################################################################

# add a variable that converts contrast from an rgb-brightness to cd/m²
addColumnWithContrastAsCandela = function(dat)
{
  lines <- readLines('Contrast-rgb-in-candela_Exp2.txt', warn = F)
  lines <- lines[12:19]
  lines <- gsub(',', '.', lines)
  lines[5] <- gsub('[(,),a-z]', '', lines[5])
  lines <- strsplit(lines, '\t\t')
  conversionTable <- t(sapply(1:length(lines),
                             function (i) { as.numeric(lines[[i]]) }))
  conversionFunction <- approxfun(conversionTable[,1], conversionTable[,2])
  
  dat$contrastInCandela <- conversionFunction(dat$Contrast)
  dat
}

## median Classifier for indirect task - returns percent correct
medianClassifier <- function(input,labels){
  ##Step function classification based on median split.
  ##   input  = data to be classified (here: RTs, logRTs)
  ##   labels = labels (here: 0=congruent / 1=incongruent)
  ##Returns: Percent correct
  currCriterion <- median(input)
  classifiedAsCongruent <- (input <= currCriterion)
  PCorr <- evaluateClassification(labels,classifiedAsCongruent)
  return(PCorr)
}

evaluateClassification <- function(labels,classifiedAsCongruent){
  ##Correct is a trial if it is:
  ##  (a) congruent and classified as congruent
  ##  (b) incongruent and classified as incongruent
  ##      (with: 0=congruent / 1=incongruent)
  PCorr <- 100 * mean(( (labels==0)  &  classifiedAsCongruent) |
                        ( (labels==1)  & !classifiedAsCongruent))
  return(PCorr)
}


## median Classifier for indirect task - returns dprime

medianClassifierDprime <- function(input, labels)
{
  ##Step function classification based on median split.
  ##   input  = data to be classified (here: RTs, logRTs)
  ##   labels = labels (here: 0=congruent / 1=incongruent)
  ##   Hit rate (HR) and false alarm rate (FA) computed for d'
  ##Returns: Sensitivity d'
  currCriterion <- median(input)
  classifiedAsCongruent <- (input <= currCriterion)
  HR <- evaluateClassification(labels[labels == 1],
                               classifiedAsCongruent[labels == 1])
  FA <- 100 - evaluateClassification(labels[labels == 0],
                                     classifiedAsCongruent[labels == 0])
  dprime = qnorm(HR/100) - qnorm(FA/100)
  return(dprime)
}

##Calculate simple standard error of the mean from input vector
mySEM <- function(input){
  stopifnot(is.vector(input))
  return(sqrt(var(input)/length(input)))
}


## Quick convenience functions for printing:

echo0 <- function(){
  cat("\n")
}
echo1 <- function(string1){
  cat(string1,"\n")
}
echo2 <- function(string1,float1){
  cat(string1,formatC(float1,digits=5,width=8),"\n")
}
echo3 <- function(string1,float1,string2){
  cat(string1,formatC(float1,digits=5,width=8),string2,"\n")
}
echo4 <- function(string1,float1,string2,float2){
  cat(string1,formatC(float1,digits=5,width=8),
      string2,formatC(float2,digits=5,width=8),"\n")
}
echot <- function(string1,tTestRes){
  #function for printing t-test results
  cat(string1)
  cat("  t(",tTestRes$parameter,") = ",
      formatC(tTestRes$statistic,digits=5,width=5),sep="")
  cat("  p = ",formatC(tTestRes$p.value,digits=5,width=5),"\n")
}
echoezMain <- function(string1,ezRes){
  #function for printing anova results main effect
  cat(string1)
  cat("  F(",ezRes$ANOVA$DFn[1],",",ezRes$ANOVA$DFd[1],") = ",
      formatC(ezRes$ANOVA$F[1],digits=5,width=5),sep="")
  cat("  p = ",formatC(ezRes$ANOVA$p[1],digits=5,width=5),"\n")
}
echoezInt <- function(string1,ezRes){
  #function for printing anova results interaction effect
  cat(string1)
  cat("  F(",ezRes$ANOVA$DFn[3],",",ezRes$ANOVA$DFd[3],") = ",
      formatC(ezRes$ANOVA$F[3],digits=5,width=5),sep="")
  cat("  p = ",formatC(ezRes$ANOVA$p[3],digits=5,width=5),"\n")
}

