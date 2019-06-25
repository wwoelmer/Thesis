
library(tidyverse)
# make plot of fluoro chl from BVR and FCR over time
fluora <- read.csv("./Fluoroprobe/FluoroProbe.csv")
fluora <- fluora[fluora$Reservoir==c("FCR", "BVR"),]
fluora <- fluora[fluora$Site==50,]
#make data only column, drop time
fluora$Date <- as.Date(fluora$DateTime, format = "%Y-%m-%d")
ggplot(data = fluora, aes(x = Date, y =TotalConc_ugL, color = Reservoir )) + geom_point()
ggplot(data = fluora, aes(x = Date, y =TotalConc_ugL, color = Depth_m )) + geom_point(aes(shape = Reservoir))
ggplot(data = fluora, aes(x = Date, y = GreenAlgae_ugL, color = Reservoir)) + geom_point()
ggplot(data = fluora, aes(x = Date, y = BrownAlgae_ugL, color = Reservoir)) + geom_point()
ggplot(data = fluora, aes(x = Date, y = Bluegreens_ugL, color = Reservoir)) + geom_point()
ggplot(data = fluora, aes(x = Date, y = MixedAlgae_ugL, color = Reservoir)) + geom_point()


bvr <- fluora[fluora$Reservoir=="BVR",]
fcr <- fluora[fluora$Reservoir=="FCR",]

# select the measurement closest to 1m
# create individual dataframe for each reservoir
layer_bvr <- bvr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))
layer_fcr <- fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))

# and one wiht both reservoirs
layer_all <- fluora %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))
ggplot(data = layer_all, aes(x = Date, y =TotalConc_ugL, color = Reservoir )) + geom_point()
# not a huge difference between these two at the surface


layer = data.frame(fluora)

layer1 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer2 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))
layer3 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.0)))
layer4 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.0)))
layer5 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.0)))
layer6 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.0)))
layer7 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.0)))
layer8 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.0)))
layer9 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.0)))
layer10 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.0)))
layer11 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.0)))
layer12 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 11.0)))
layer13 <- layer %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - 12.0)))


## replace name of depth with the selected layer depths for ease of merging later
layer1$Depth_m <- 0.1 
layer2$Depth_m <- 1.0
layer3$Depth_m <- 2.0 
layer4$Depth_m <- 3.0 
layer5$Depth_m <- 4.0 
layer6$Depth_m <- 5.0 
layer7$Depth_m <- 6.0 
layer8$Depth_m <- 7.0 
layer9$Depth_m <- 8.0 
layer10$Depth_m <- 9.0 
layer11$Depth_m <- 10.0 
layer12$Depth_m <- 11.0
layer13$Depth_m <- 12.0

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,
                 layer11,layer12,layer13)
layers_binned <- df.final[order(df.final$Date, df.final$Depth_m), ]

ggplot(data = layers_binned, aes(x = Date, y =TotalConc_ugL, color = Reservoir )) + geom_point() + 
  facet_wrap(~Depth_m)




# make plot of BVR, inflow, and FCR nut chemistry over time
chem <- read.csv("./water_chemistry/chemistry.csv")
chem$Reservoir <- as.character(chem$Reservoir)
chem <- chem %>% mutate(location = ifelse(Site==100, "Inf", chem$Reservoir ))
chem <- chem[chem$Reservoir==c("FCR", "BVR"),]
surf <- chem[chem$Depth_m==0.1,]
surf$DateTime <- as.Date(surf$DateTime, format = "%Y-%m-%d")
surf <- surf %>% mutate(TN_TP = TN_ugL/TP_ugL)

ggplot(data = chem, aes(x = DateTime, y =TN_ugL, color = location )) + geom_point() + 
  facet_wrap(~Depth_m)
ggplot(data = chem, aes(x = DateTime, y =TP_ugL, color = Reservoir )) + geom_point() + 
  facet_wrap(~Depth_m)
ggplot(data = res, aes(x = DateTime, y =SRP_ugL, color = Reservoir )) + geom_point() + 
  facet_wrap(~Depth_m)
ggplot(data = surf, aes(x = DateTime, y =TN_ugL, color = location )) + geom_point() 
ggplot(data = surf, aes(x = DateTime, y =TP_ugL, color = location )) + geom_point() 
ggplot(data = surf, aes(x = DateTime, y =SRP_ugL, color = location )) + geom_point() 
ggplot(data = surf[surf$NH4_ugL<50,], aes(x = DateTime, y =NH4_ugL, color = location )) + geom_point() 
ggplot(data = surf, aes(x = DateTime, y =NO3NO2_ugL, color = location )) + geom_point() 
# woah inflow values are periodically super high--maybe because of precip events?
ggplot(data = surf[surf$location==c("BVR", "FCR"),], aes(x = DateTime, y =NO3NO2_ugL, color = location )) + 
  geom_point()  

# want to get rid of that BVR outlier to plot
nplot <- surf[surf$NO3NO2_ugL<20,]
ggplot(data = nplot[nplot$location==c("BVR", "FCR"),], aes(x = DateTime, y =NO3NO2_ugL, color = location, group = location )) + 
  geom_point()  + geom_line()

ggplot(data = surf, aes(x = DateTime, y =DOC_mgL, color = location )) + geom_point() 
ggplot(data = surf, aes(x = DateTime, y =TN_TP, color = location )) + geom_point() 

# 
inflow <- read.csv("./Inflow/inflowcalcs_FCR.csv")
inflow$Date <- as.Date(inflow$Date)
plot(inflow$Date, inflow$mean_flow, type = 'l')
plot(inflow$Date, inflow$flow_max)
