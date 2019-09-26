# script to look at how the new diana pressure transducer and the old wvwa pressure transducer compare

library(tidyverse)
library(lubridate)

# download the latest diana weir file
download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./Data/Inflow/FCRweir.csv')

dianaheader<-read.csv("./Data/Inflow/FCRweir.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
dianadata<-read.csv("./Data/Inflow/FCRweir.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(dianadata)<-names(dianaheader) #combine the names to deal with Campbell logger formatting
dianadata$TIMESTAMP <- as.POSIXct(dianadata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
colnames(dianadata)[colnames(dianadata)=="Lvl_psi"] <- "diana_psi_corr"
dianadata <- dianadata %>% select("TIMESTAMP", "diana_psi_corr")

# download the latest wvwa weir file
#check Reservoirs to see the latest weir data upload
wvwadata1 <- read.csv("./Data/Inflow/FCR_15min_Inf_20190527.csv", skip = 28, header = T)
colnames(wvwadata1)[colnames(wvwadata1)=="Date.Time"] <- "TIMESTAMP"
wvwadata1 <- wvwadata1 %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwadata1$TIMESTAMP <- floor_date(wvwadata1$TIMESTAMP, "15 minutes")
colnames(wvwadata1)[colnames(wvwadata1)=="Pressure.psi."] <- "wvwa_psi_uncorr"
wvwadata1 <- wvwadata1 %>% select("TIMESTAMP", "wvwa_psi_uncorr")
# also load in older wvwa pressure data
wvwadata2 <- read.csv("./Data/Inflow/FCR_15min_Inf_20190506.csv", skip = 28, header = T)
colnames(wvwadata2)[colnames(wvwadata2)=="Date.Time"] <- "TIMESTAMP"
wvwadata2 <- wvwadata2 %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwadata2$TIMESTAMP <- floor_date(wvwadata2$TIMESTAMP, "15 minutes")
colnames(wvwadata2)[colnames(wvwadata2)=="Pressure.psi."] <- "wvwa_psi_uncorr"
wvwadata2 <- wvwadata2 %>% select("TIMESTAMP", "wvwa_psi_uncorr")
# and other wvwa weir data
wvwadata3 <- read.csv("./Data/Inflow/FCR_15min_Inf_20190617.csv", skip = 28, header = T)
colnames(wvwadata3)[colnames(wvwadata3)=="Date.Time"] <- "TIMESTAMP"
wvwadata3 <- wvwadata3 %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwadata3$TIMESTAMP <- floor_date(wvwadata3$TIMESTAMP, "15 minutes")
colnames(wvwadata3)[colnames(wvwadata3)=="Pressure.psi."] <- "wvwa_psi_uncorr"
wvwadata3 <- wvwadata3 %>% select("TIMESTAMP", "wvwa_psi_uncorr")
# and more data
wvwadata4 <- read.csv("./Data/Inflow/FCR_15min_Inf_20190415.csv", skip = 28, header = T)
colnames(wvwadata4)[colnames(wvwadata4)=="Date.Time"] <- "TIMESTAMP"
wvwadata4 <- wvwadata4 %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwadata4$TIMESTAMP <- floor_date(wvwadata4$TIMESTAMP, "15 minutes")
colnames(wvwadata4)[colnames(wvwadata4)=="Pressure.psi."] <- "wvwa_psi_uncorr"
wvwadata4 <- wvwadata4 %>% select("TIMESTAMP", "wvwa_psi_uncorr")

wvwadata <- rbind(wvwadata2, wvwadata1, wvwadata3, wvwadata4)

# wvwa psi needs to be corrected for barometric pressure, so pull in WVWA DO sonde barometric pressure data
# check on Reservoirs for latest WVWA BP file
wvwa_bp <- read.csv("./Data/Inflow/FCR_WVWA_BarometricPressure_20190603.csv", skip = 28, header =T)
colnames(wvwa_bp)[colnames(wvwa_bp)=="Date.Time"] <- "TIMESTAMP"
wvwa_bp <- wvwa_bp %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwa_bp$TIMESTAMP <- floor_date(wvwa_bp$TIMESTAMP, "15 minutes")
colnames(wvwa_bp)[colnames(wvwa_bp)=="Pressure.psi."] <- "wvwa_bp_psi"
wvwa_bp <- wvwa_bp %>% select("TIMESTAMP", "wvwa_bp_psi")
wvwa_bp_2 <- read.csv("./Data/Inflow/FCR_BV_20190415.csv", skip = 28, header =T)
colnames(wvwa_bp_2)[colnames(wvwa_bp_2)=="Date.Time"] <- "TIMESTAMP"
wvwa_bp_2 <- wvwa_bp_2 %>% mutate(TIMESTAMP = parse_date_time(TIMESTAMP, 'dmy HMS',tz = "EST"))
wvwa_bp_2$TIMESTAMP <- floor_date(wvwa_bp_2$TIMESTAMP, "15 minutes")
colnames(wvwa_bp_2)[colnames(wvwa_bp_2)=="Pressure.psi."] <- "wvwa_bp_psi"
wvwa_bp_2 <- wvwa_bp_2 %>% select("TIMESTAMP", "wvwa_bp_psi")

wvwa_bp <- rbind(wvwa_bp_2, wvwa_bp)

# a check to see if the met station barometric pressure is comparable to the wvwa sonde barometric pressure
download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','./Data/MET/FCRmet.csv')
metheader<-read.csv("./Data/MET/FCRmet.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
metdata<-read.csv("./Data/MET/FCRmet.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(metdata)<-names(metheader) #combine the names to deal with Campbell logger formatting
metdata <- metdata %>% mutate(met_bp_psi = BP_kPa_Avg*0.145038)
metdata <- metdata %>% select("TIMESTAMP", "met_bp_psi")
metdata$TIMESTAMP <- as.POSIXct(metdata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")


# join the three together based on TIMESTAMP
baro_correct <- left_join(wvwadata, metdata)
baro_correct <- left_join(baro_correct, wvwa_bp)
baro_correct <- baro_correct %>% mutate(wvwa_psi_corr_met = wvwa_psi_uncorr - met_bp_psi) %>% 
  mutate(wvwa_psi_corr_sonde = wvwa_psi_uncorr - wvwa_bp_psi)


weir <- left_join(baro_correct, dianadata)
weir <- na.omit(weir)

# visualize the data
plot(weir$TIMESTAMP, weir$met_bp_psi, type = 'l', ylim = c(13.7, 14.9), main = 'Figure 1')
points(weir$TIMESTAMP, weir$wvwa_bp_psi, type  = 'l', col = 'red')
legend('center', c('met BP', 'wvwa BP'), col = c('black', 'red'), lty = c(1,1), bty = 'n')

plot(weir$met_bp_psi, weir$wvwa_bp_psi, main = 'Figure 2')
abline(a=0,b=1)
lm(weir$wvwa_bp_psi~weir$met_bp_psi)

plot(weir$diana_psi_corr, weir$wvwa_psi_corr_sonde, type = 'l', xlab = 'Diana psi', ylab = 'WVWA psi, BP correction from WVWA sonde')
plot(weir$diana_psi_corr, weir$wvwa_psi_corr_met, type = 'l', xlab = 'Diana psi', ylab = 'WVWA psi, BP correction from met station')
plot(weir$TIMESTAMP, weir$diana_psi_corr, type = 'l', ylim = c(0.34, 0.52))
points(weir$TIMESTAMP, weir$wvwa_psi_corr_sonde, col = 'blue', type = 'l')
legend('center', c('Diana', 'WVWA'), col = c('black', 'blue'), lty = c(1,1), bty = 'n')
plot(weir$TIMESTAMP, weir$wvwa_psi_corr_met, col = 'blue', type = 'l')

plot(weir$wvwa_psi_corr_met, weir$wvwa_psi_corr_sonde)

# calculate discharge from psi
# using numbers from MEL's inflow aggregation script
weir <- weir %>% mutate(diana_flow1 = (diana_psi_corr )*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(diana_flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (diana_flow1 ^ 1.5) * 35.3147)) %>% 
  mutate(diana_flow_cms = diana_flow_cfs*0.028316847   )

weir <- weir %>% mutate(wvwa_flow1 = wvwa_psi_corr_sonde*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(wvwa_flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (wvwa_flow1 ^ 1.5) * 35.3147)) %>% 
  mutate(wvwa_flow_cms = wvwa_flow_cfs*0.028316847)

plot(weir$TIMESTAMP, weir$diana_flow_cms, type = 'l', ylim= c(0.05, 0.24))
points(weir$TIMESTAMP, weir$wvwa_flow_cms, type = 'l', col = 'red')

plot(weir$diana_flow_cms, weir$wvwa_flow_cms)
summary(lm(weir$diana_flow_cms~weir$wvwa_flow_cms))

#calculate daily means of both discharge estimates
weir <- weir %>% mutate(date = date(TIMESTAMP))
daily <- weir %>% group_by(date) %>% mutate(mean_cms_diana = mean(diana_flow_cms)) %>% mutate(mean_cms_wvwa = mean(wvwa_flow_cms))
daily <- daily %>% select(date, mean_cms_diana, mean_cms_wvwa)
plot(daily$mean_cms_diana, daily$mean_cms_wvwa)
abline(lm(daily$mean_cms_wvwa~daily$mean_cms_diana))
summary(lm(daily$mean_cms_wvwa~daily$mean_cms_diana))
plot(daily$date, daily$mean_cms_diana, col = 'red', ylim = c(0.06, 0.217))
points(daily$date, daily$mean_cms_wvwa)
legend('topright', bty = 'n', lty = c(1,1), c('diana', 'wvwa'), col = c('red', 'black'))
par(mfrow = c(1,1))
plot(daily$date, daily$mean_cms_diana, col = 'red')
plot(daily$date, daily$mean_cms_wvwa)


# calculate discharge for the week before diana was deployed
wvwa_discharge <- baro_correct %>% select(TIMESTAMP, wvwa_psi_corr_sonde) %>% 
mutate(wvwa_flow1 = (wvwa_psi_corr_sonde)*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(wvwa_flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (wvwa_flow1 ^ 1.5) * 35.3147)) %>% 
  mutate(wvwa_flow_cms = wvwa_flow_cfs*0.028316847   )

wvwa_daily <- wvwa_discharge %>% mutate(date = date(TIMESTAMP)) %>% 
  group_by(date) %>% 
  mutate(mean_wvwa_cms = mean(wvwa_flow_cms))
plot(wvwa_daily$date, wvwa_daily$wvwa_flow_cms)
# clean up the dataframe
wvwa_daily <- wvwa_daily %>% select(date, mean_wvwa_cms)
wvwa_daily <- wvwa_daily[!duplicated(wvwa_daily[1:2]),]
wvwa_daily <- wvwa_daily[order(wvwa_daily$date),]
# there are NAs from june 3 on because bp data hasn't been downloaded yet
# remove these 
wvwa_daily <- na.omit(wvwa_daily)
write.csv(wvwa_daily, './DataAnalysis/Data/Inflow/WVWA_discharge_03182019_06022019.csv', row.names = FALSE)

# what is the magnitude of past inflow data? bring in older data file
hist_inf <- read.csv("./DataAnalysis/Data/Inflow/inflow_2013_2018.csv")
hist_inf$DateTime <- as.POSIXct(hist_inf$DateTime, format = "%Y-%m-%d %H:%M:%S")
mean(na.omit(hist_inf$Flow_cms))
plot(hist_inf$DateTime, hist_inf$Flow_cms)
wvwa_baro <- hist_inf %>% select("DateTime", "Baro_pressure_psi")
colnames(wvwa_baro)[colnames(wvwa_baro)=="DateTime"] <- "TIMESTAMP"

# compare barometric pressure
baromet <- left_join(catdata, wvwa_baro )
par(mfrow = c(1,2))
plot(baromet$TIMESTAMP, baromet$Baro_pressure_psi, col= 'red')
plot(baromet$TIMESTAMP, baromet$cat_baro_psi)
