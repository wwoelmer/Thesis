# new equation for pressure transducer based on v-notch weir installed on june 6 2019


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

# separate into pre and post vnotch weir to apply different equations and get rid of some data surrounding the time when the weir was being replaced

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

                                  