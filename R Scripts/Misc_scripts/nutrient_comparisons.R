# look at 0.1m vs. 1.6m nutrient data
# how well do they compare?

library(tidyverse)

chem <- read.csv('./water_chemistry/FCR_chemistry.csv')
chem <- chem[chem$Site==50,]
chem_surf <- chem[chem$Depth_m<2.0,]
chem_surf <- chem_surf[!chem_surf$Depth_m==0.8,]

ggplot(data = chem_surf, aes(x = DateTime, y = TP_ugL, col = Depth_m)) + geom_point()

chem_0.1 <- chem[chem$Depth_m==0.1,]
chem_1.6 <- chem[chem$Depth_m==1.6,]

dates <- match(chem_0.1$DateTime, chem_1.6$DateTime)

chem_0.1 <- chem_0.1[chem_0.1$DateTime %in% chem_1.6$DateTime,]
# now they are the same length so nutrient variables can be plotted against each other


par(mfrow=c(2,3))
plot(chem_0.1$TP_ugL, chem_1.6$TP_ugL, xlim = c(0,80))
abline(0,1)
plot(chem_0.1$TN_ugL, chem_1.6$TN_ugL, ylim = c(0,800))
abline(0,1)
plot(chem_0.1$NH4_ugL, chem_1.6$NH4_ugL, ylim = c(0,150), xlim = c(0,150))
abline(0,1)
plot(chem_0.1$NO3NO2_ugL, chem_1.6$NO3NO2_ugL, xlim = c(0,30))
abline(0,1)
plot(chem_0.1$SRP_ugL, chem_1.6$SRP_ugL)
abline(0,1)
plot(chem_0.1$DOC_mgL, chem_1.6$DOC_mgL)
abline(0,1)

par(mfrow=c(1,1))
plot(chem_0.1$NH4_ugL, chem_1.6$NH4_ugL, ylim = c(0,100), xlim = c(0,100))
abline(0,1)
