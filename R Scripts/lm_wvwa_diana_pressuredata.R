# building a linear model between the wvwa pressure transducer and diana pressure transducer

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

# the old weir equations are taken directly from MEL's Inlow Aggregation script
dianadata_pre <- dianadata[dianadata$TIMESTAMP< as.POSIXct('2019-06-06 09:30:00'),]
dianadata_pre <- dianadata_pre %>% mutate(diana_flow1 = (diana_psi_corr )*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(diana_flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (diana_flow1 ^ 1.5) * 35.3147)) %>% 
  mutate(flow_cms = diana_flow_cfs*0.028316847   )%>% 
  select(TIMESTAMP, diana_psi_corr, flow_cms)

# q = 2.391 * H^2.5
# where H = head in meters above the notch
# the head was 14.8 cm on June 24 at ~13:30
#14.8 cm is 0.148 m 
#14.9cm on Jun 27 at 3:49PM
dianadata_post <- dianadata[dianadata$TIMESTAMP > as.POSIXct('2019-06-07 00:00:00'),]
dianadata_post <- dianadata_post %>%  mutate(head = (0.149*diana_psi_corr)/0.293) %>% 
  mutate(flow_cms = 2.391* (head^2.5)) %>% 
  select(TIMESTAMP, diana_psi_corr, flow_cms)

dianadata <- rbind(dianadata_pre, dianadata_post)                                                   
plot(dianadata$TIMESTAMP, dianadata$flow_cms, xlim = c(as.POSIXct('2019-06-01 00:00:00'), as.POSIXct('2019-06-10 00:00:00')))                                                    
plot(dianadata$TIMESTAMP, dianadata$flow_cms)                                                    


wvwadata <- read.csv('./Data/Inflow/FCR_inflow_WVWA_2013_2019.csv')
wvwadata <- wvwadata %>% select(DateTime, Flow_cms)
colnames(wvwadata) <- c('TIMESTAMP', 'flow_cms_wvwa')
wvwadata$TIMESTAMP <- as.POSIXct(wvwadata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

discharge <- left_join(dianadata, wvwadata)
plot(discharge$TIMESTAMP, discharge$flow_cms)
points(discharge$TIMESTAMP, discharge$flow_cms_wvwa, col = 'red')
plot(discharge$flow_cms_wvwa, discharge$flow_cms)
summary(lm(discharge$flow_cms_wvwa~discharge$flow_cms))
mod <- lm(discharge$flow_cms_wvwa~discharge$flow_cms)
res <- resid(mod)
sd(res) # use this as the sd for observational uncertainty in dishcarge drive data for forecasts
# dianaflow = 0.0200487 + wvwaflow*1.2035001
# wvwaflow = -0.0160121 + 0.8233599*dianaflow

# different for daily means?
discharge_daily <- discharge %>% mutate(Date = date(TIMESTAMP)) %>% 
  group_by(Date) %>% 
  select(-TIMESTAMP) %>% 
  summarize_all('mean')
plot(discharge_daily$Date, discharge_daily$flow_cms)
points(discharge_daily$Date, discharge_daily$flow_cms_wvwa, col = 'red')
plot(discharge_daily$flow_cms_wvwa, discharge_daily$flow_cms)
summary(lm(discharge_daily$flow_cms_wvwa~discharge_daily$flow_cms))


