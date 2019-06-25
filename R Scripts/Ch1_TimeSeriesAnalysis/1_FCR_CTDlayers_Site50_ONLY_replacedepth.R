#FCR site 30 data processing

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

#setwd("C:/Users/wwoel/Dropbox/FCR_TimeSeries")
ctd <- read.csv("./CTD/CTD_FCR.csv")


# select CTD data for site 50 only
site50 <- ctd[which(ctd$Site == 50), ]

for(i in 1:length(site50$Depth_m)){
  if(site50$Depth_m[i]<0){
    site50$Temp_C[i] = NA
  }
  if(site50$DO_mgL[i]<0){
    site50$Temp_C[i] = NA
  }
  if(site50$ORP_mV[i]<0){
    site50$Temp_C[i] = NA
  }
  if(site50$Cond_uScm[i]<0){
    site50$Temp_C[i] = NA
  }
}

# site50 = na.omit(site50)

layer = data.frame(site50)

layer1 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer2 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))
layer3 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
layer4 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.8)))
layer5 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
layer6 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.0)))
layer7 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.2)))
layer8 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
layer9 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.8)))
layer10 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.0)))
layer11 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
layer12 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.0)))
layer13 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.0)))
layer14 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.3)))

## replace name of depth with the selected layer depths for ease of merging later
layer1$Depth_m <- 0.1 
layer2$Depth_m <- 1.0
layer3$Depth_m <- 1.6 
layer4$Depth_m <- 2.8 
layer5$Depth_m <- 3.8 
layer6$Depth_m <- 5.0 
layer7$Depth_m <- 5.2 
layer8$Depth_m <- 5.5 
layer9$Depth_m <- 5.8 
layer10$Depth_m <- 6.0 
layer11$Depth_m <- 6.2 
layer12$Depth_m <- 8.0 
layer13$Depth_m <- 9.0 
layer14$Depth_m <- 9.3 




#save layer 1 as csv
write.csv(layer1, "FCR_CTD_50surf_binned.csv", row.names=FALSE)


#put all layers together for all layers at FCR 50
df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,
                 layer11,layer12,layer13,layer14)

#as.Date(df.final$Date)
fcr50_layers_binned <- df.final[order(df.final$Date, df.final$Depth_m), ]
 # arrange(df.final, Date)

write.csv(fcr50_layers_binned, "FCR_CTD_50_binned.csv", row.names=FALSE)
