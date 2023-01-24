rm(list = ls())
library("data.table")


# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

data = read.table("data\\TMS\\chromaticRTE_TMS_data.txt", header = T, sep = '\t')
data$Number.LogLevel5.<-factor(data$Number.LogLevel5.)
data$Subject<-factor(data$Subject)
data$virhe<-ifelse(data$Number.LogLevel5. == 0&(data$Reaction.RT.LogLevel5.!=0), 1, 0)
data$expTrial<-ifelse(data$ColorTMS.LogLevel5. == 0 & data$GrayTMS.LogLevel5.==0, 0, 1)
data$virhe.edellinen <-shift(data$virhe, n=1, fill=0, type="lag") #siirret??n siten ett? n?ytt?? 1 mik?li edellisess? trialissa virhe
data$virhe.edellinen<-ifelse(data$LogLevel5 == 1, NA, data$virhe.edellinen)
data$StimulusColor.SubTrial. <-relevel(data$StimulusColor.SubTrial., "Blue") #Blue as intercept

data= subset(data, TMS!=0)

data= subset(data, Subject!=15 &Subject!=6&Subject!=16&Subject!=3) #acc <.8 in one of the colors

#REMOVE OUTLIERS
keskireaktiot <- aggregate(correctRT.LogLevel5.~ Subject, FUN =mean, data)
sdreaktiot <- aggregate(correctRT.LogLevel5.~ Subject, FUN =sd, data)
outliers = merge(keskireaktiot, sdreaktiot, by = "Subject")
outliers$alaraja = outliers$correctRT.LogLevel5..x-2*outliers$correctRT.LogLevel5..y #kerroin kertoo kuinka monen keskihajonnan p??ss? olevat poistetaan
outliers$ylaraja = outliers$correctRT.LogLevel5..x+2*outliers$correctRT.LogLevel5..y
data2 = subset(data, correctRT.LogLevel5. != 0) # vain sellaiset joissa on response
for(i in 1:100){
  if(is.element(i,data2$Subject)){
    data2<-data2[!(data2$Subject==i & data2$correctRT.LogLevel5.<subset(outliers, Subject == i)$alaraja),]
    data2<-data2[!(data2$Subject==i & data2$correctRT.LogLevel5.>subset(outliers, Subject == i)$ylaraja),]
    
  }
}

# TMS RTE
lmedata = subset(data2, (TMS ==1 & ColorTMS.LogLevel5. == 1 & Number.LogLevel5. == 2 & Confidence.RESP.LogLevel5. == 4 & NumberResponse.LogLevel5. == 1) |(TMS ==1 & GrayTMS.LogLevel5. == 1 & Number.LogLevel5. == 1 & Confidence.RESP.LogLevel5. == 4 & NumberResponse.LogLevel5. == 1))
write.csv(lmedata, 'TMS_RTE.csv')
