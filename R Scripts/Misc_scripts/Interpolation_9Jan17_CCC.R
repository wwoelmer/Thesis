setwd("/Users/cayelan/Dropbox/ComputerFiles/Virginia_Tech/Falling Creek/GLM")
data<-read.csv("MolarChemData_9Jan17.csv", header=TRUE)

data2<-read.csv("GLM2015_timeflowtemp.csv", header=TRUE)

data2$time<-as.POSIXct(strptime(data2$time, "%Y-%m-%d", tz="EST"))
data$time<-as.POSIXct(strptime(data$time, "%Y-%m-%d", tz="EST"))

mergeddata<-merge(data, data2, by="time", all.x=TRUE, all.y=TRUE)

#mergeddata<-mergeddata[1:219,1:2]

library(zoo)

mergeddata$OGM_doc <-na.approx(mergeddata$OGM_doc, na.rm=FALSE)
mergeddata$NIT_amm <-na.approx(mergeddata$NIT_amm, na.rm=FALSE)
mergeddata$NIT_nit <-na.approx(mergeddata$NIT_nit, na.rm=FALSE)
mergeddata$PHS_frp <-na.approx(mergeddata$PHS_frp, na.rm=FALSE)
mergeddata$TOT_tn <-na.approx(mergeddata$TOT_tn, na.rm=FALSE)
mergeddata$TOT_tp <-na.approx(mergeddata$TOT_tp, na.rm=FALSE)
mergeddata$salt <-na.approx(mergeddata$salt, na.rm=FALSE)


write.csv(mergeddata, "FCR_GLM_iof_2015_modified_9Jan17.csv")