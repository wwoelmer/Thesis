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

# q = 2.391 * H^2.5
# where H = head in meters above the notch
# the head was 14.8 cm on June 24 at ~13:30
#14.8 cm is 0.148 m 
dianadata <- dianadata %>% 
  mutate(head = (0.148*diana_psi_corr)/0.295) %>% 
  mutate(flow_cms = 2.391* (head^2.5))

plot(dianadata$TIMESTAMP, dianadata$flow_cms, type = 'l')
plot(dianadata$TIMESTAMP, dianadata$head, type = 'l')
