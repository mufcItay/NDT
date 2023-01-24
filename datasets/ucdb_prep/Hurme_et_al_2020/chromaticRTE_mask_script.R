library("data.table")

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

data = read.table("data\\Masking\\chromaticRTE_mask_data.txt", header = T, sep = '\t')
data$Number<-factor(data$Number)
data$Subject<-factor(data$Subject)
data$virhe<-ifelse(data$Number == 0&(data$correctRT!=0), 1, 0)
data$virhe.edellinen <-shift(data$virhe, n=1, fill=0, type="lag") #siirret??n siten ett? n?ytt?? 1 mik?li edellisess? trialissa virhe
data$virhe.edellinen<-ifelse(data$SubTrial == 1, NA, data$virhe.edellinen)
data$StimulusColor <-relevel(data$StimulusColor, "Blue") #Red as intercept

data = subset(data, Subject!= 7 & Subject!= 13 & Subject!= 15) #acc <.8 in one of the colors

#REMOVE OUTLIERS
keskireaktiot <- aggregate(correctRT~ Subject, FUN =mean, data)
sdreaktiot <- aggregate(correctRT~ Subject, FUN =sd, data)
outliers = merge(keskireaktiot, sdreaktiot, by = "Subject")
outliers$alaraja = outliers$correctRT.x-2*outliers$correctRT.y #kerroin kertoo kuinka monen keskihajonnan p??ss? olevat poistetaan
outliers$ylaraja = outliers$correctRT.x+2*outliers$correctRT.y
data2 = subset(data, correctRT != 0) # vain sellaiset joissa on response
for(i in 1:130){
  if(is.element(i,data2$Subject)){
    data2<-data2[!(data2$Subject==i & data2$correctRT<subset(outliers, Subject == i)$alaraja),]
    data2<-data2[!(data2$Subject==i & data2$correctRT>subset(outliers, Subject == i)$ylaraja),]
    
  }
}

#Mask RTE
lmedata = subset(data2, (GrayMasked == 1 & Number == 1 &  NumberResponse== 1 & Confidence.RESP ==4)|(ColorMasked == 1 & Number == 2 &  NumberResponse== 1& Confidence.RESP ==4))

write.csv(lmedata, 'Masking_RTE.csv')
