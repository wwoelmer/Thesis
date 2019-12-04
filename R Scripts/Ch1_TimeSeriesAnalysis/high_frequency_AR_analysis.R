# gather met, inflow, and exo data to fit high-frequency autoregressive time series of exo chl data

library(tidyverse)
library(lubridate)

download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','./Data/Catwalk/Catwalk.csv')
download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','./Data/MET/FCRmet.csv')
download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./Data/Inflow/FCRweir.csv')

folder <- "C:/Users/wwoel/Dropbox/Thesis"
data_location <-  "C:/Users/wwoel/Dropbox/Thesis/Data"


source(paste0('C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3',"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) #this is the original file modified to take a daily avg rather than the midnight reading

temperature_location <- paste0(data_location, "/", "Catwalk")

temp_obs_fname <- "Catwalk.csv"
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
setwd(folder)
#download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','./SCCData/mia-data/Catwalk.csv')


observed_depths_chla_fdom <- 1
reference_tzone <- "EST"
full_time = seq(as.Date('2018-08-15'), as.Date(Sys.time()) , by = '1 day')

# gather chl data from exo sonde up to the current day
chl_hist <- extract_chla_chain_dailyavg(fname = temp_obs_fname_wdir,
                                        full_time = full_time,
                                        depths = 1.0,
                                        observed_depths_chla_fdom = observed_depths_chla_fdom,
                                        input_tz = "EST5EDT", 
                                        output_tz = reference_tzone)

date_vector <- data.frame(full_time)
chl_update <- data.frame(cbind(date_vector, chl_hist[[1]][,1])  )
colnames(chl_update) <- c('Date', 'Chla_EXO')
chl_update <- chl_update %>% mutate(Chla_sqrt = sqrt(Chla_EXO*0.55 - 0.0308)) %>%  #convert into CTD units and then take the square root
  select(-Chla_EXO) %>% 
  mutate(Chla_ARlag1_sqrt = 0) # put in zero here because will calculate the lag later so top most entry takes it lagged value from the earlier dataset
chl_update$Date <- as.Date(chl_update$Date)

#######################################################################################################################################################################################
########### gather FCR met data up to the current day  
#######################################################################################################################################################################################

met_obs_fname <- "FCRmet.csv"
met_station_location <- paste0(data_location, "/", "MET")
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_glm <- paste0(folder, "/", "GLM_working")  
met_update_outfile <- paste0(met_station_location, "/", "update_met.csv")
#download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','./SCCData/carina-data/FCRmet.csv')


full_time_hour_obs <- seq(  as.POSIXct('2018-08-15 00:00:00'), 
                            as.POSIXct(Sys.time()),
                            by = "1 hour")

source(paste0('C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3',"/","Rscripts/create_obs_met_input_DA.R"))

create_obs_met_input_DA(fname = met_obs_fname_wdir,
                        outfile= met_update_outfile,
                        full_time_hour_obs = full_time_hour_obs, 
                        input_tz = "EST5EDT", 
                        output_tz = 'EST')
# read in the hourly data that was created and summarize to daily mean
setwd(folder)
sw_hist <- read.csv('./Data/MET/update_met.csv')


# and do the same for the legacy data before 2019
create_obs_met_input_DA(fname = paste0(met_station_location, '/', 'FCRmet_legacy01.csv'),
                        outfile= paste0(met_station_location, "/", "update_met_legacy.csv") ,
                        full_time_hour_obs = full_time_hour_obs, 
                        input_tz = "EST5EDT", 
                        output_tz = 'EST')
sw_legacy<- read.csv('./Data/MET/update_met_legacy.csv')

met_all <- rbind(sw_legacy, sw_hist)
met_all$time <- as.POSIXct(met_all$time)

met_summary <- met_all %>%                                                 # start with the raw data
  mutate(Date = as.Date(time, format="%Y-%m-%d")) %>% # create a column of just the date
  select(-time) %>%                                # drop the datetime column
  group_by(Date) %>%                                           # group by date (for daily statistics)
  summarise_all(funs(mean, median, max, sum), na.rm = TRUE) %>% 
  select(-(ShortWave_sum:WindSpeed_sum), -Rain_median, -Rain_max) %>% # drop calcs you don't want
  select(Date, noquote(order(colnames(.))))                    # arrange columns alphabetically after Date



#######################################################################################################################################################################################
####### gather discharge driver data #######################################################
##########################################################################################################################################################################
# two data sources for discharge data
# for 2018-07-05 through 2019-06-03 (this is the most recent file for wvwa flow data)
# after 2019-06-03, use diana data but convert to wvwa units


# download the latest diana weir file
#download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./SCCData/FCRweir.csv')
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

# calculate daily values
discharge_diana_daily <- dianadata %>% 
  select(TIMESTAMP, flow_cms) %>% 
  mutate(Date = date(TIMESTAMP))%>% 
  group_by(Date) %>% 
  summarise_all('mean') %>% 
  select(-TIMESTAMP)

# convert diana into wvwa units (equation taken from 'thesis/r scripts/lm_wvwa_diana_pressuredata.R' on 10-08-2019)
discharge_diana_daily_wvwaunits <- discharge_diana_daily %>% 
  mutate(flow_cms_diana_wvwaunits = (flow_cms*0.713454 + (-0.004732)))

# throw out data 2019-06-04 through 2019-06-06 because the plug was removed from the weir in prep for replacing weir face so numbers are artificially low
discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits[discharge_diana_daily_wvwaunits$Date!=as.Date('2019-06-04'),]
discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits[discharge_diana_daily_wvwaunits$Date!='2019-06-05',]
discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits[discharge_diana_daily_wvwaunits$Date!='2019-06-06',]

discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits %>% select(-flow_cms)
colnames(discharge_diana_daily_wvwaunits) <- c('Date', 'mean_flow')


# gather wvwa inflow data before april 22 2019
inf_old <- read.csv('C:/Users/wwoel/Desktop/Reservoirs_2/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow_working.csv')
inf_old$Date <- as.Date(inf_old$DateTime)
inf_old <- inf_old[inf_old$Date>as.Date('2018-08-14'),]

inf_old <- inf_old %>% group_by(Date) %>%
  mutate(mean_flow = mean(Flow_cms, na.rm = TRUE)) %>% 
  select(Date, mean_flow)
inf_old <- inf_old[!duplicated(inf_old$Date),]
inf_old <- as.data.frame(inf_old)

# subset diana data to time period where wvwa is unavailable
inf_diana <- discharge_diana_daily_wvwaunits[discharge_diana_daily_wvwaunits$Date>as.Date('2019-09-20'),]

inf_all <- rbind(inf_old, inf_diana)

update <- left_join(chl_update, inf_all)
update <- left_join(update, met_summary)


# create the lag for new datapoints
update <- update %>% mutate(Chla_ARlag1_sqrt = ifelse(Chla_ARlag1_sqrt>0, Chla_ARlag1_sqrt, lag(Chla_sqrt, n = 1L)))


#get rid of na's
update <- na.omit(update)
update <- update %>% 
  select(-Snow_max:-Snow_sum, -Rain_mean)

# save the dataset as a data file
write.csv(update, './Data/ARIMA_data/EXO_plusdrivers_AR.csv', row.names = FALSE)


library(MuMIn)
all_max <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow + AirTemp_max + LongWave_max + ShortWave_max + Rain_sum + RelHum_max + WindSpeed_max, 
           data = update, family = gaussian, na.action = 'na.fail')
glm_max <- dredge(all_max, rank = 'AICc', fixed = 'Chla_ARlag1_sqrt')
select_max <- subset(glm_max, delta<2)

all_mean <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow + AirTemp_mean + LongWave_mean + ShortWave_mean + Rain_sum + RelHum_mean + 
                  WindSpeed_mean, 
               data = update, family = gaussian, na.action = 'na.fail')
glm_mean <- dredge(all_mean, rank = 'AICc', fixed = 'Chla_ARlag1_sqrt')
select_mean <- subset(glm_mean, delta<2)

all_median <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow + AirTemp_median + LongWave_median + ShortWave_median + Rain_sum + RelHum_median + 
                  WindSpeed_median, 
                data = update, family = gaussian, na.action = 'na.fail')
glm_median <- dredge(all_median, rank = 'AICc', fixed = 'Chla_ARlag1_sqrt')
select_median <- subset(glm_median, delta<2)



# select most parsimonious model from each subset
mod_max <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + ShortWave_max, data = update, family = gaussian, na.action = 'na.fail')
pred_max <- predict(mod_max, newdata = update)

mod_mean <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + ShortWave_mean, data = update, family = gaussian, na.action = 'na.fail')
pred_mean <- predict(mod_mean, newdata = update)

mod_median <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + AirTemp_median + LongWave_median, data = update, family = gaussian, na.action = 'na.fail')
pred_median <- predict(mod_median, newdata = update)

mod_AR <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt, data = update, family = gaussian, na.action = 'na.fail')
pred_AR <- predict(mod_AR, newdata = update)

plot(update$Date, update$Chla_sqrt)
points(update$Date, pred_max, col = 'red', type = 'l')

source(paste0('C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3',"/","Rscripts/model_assessment.R"))
max_metrics <- model_metrics(pred_max, update$Chla_sqrt)
mean_metrics <- model_metrics(pred_mean, update$Chla_sqrt)
median_metrics <- model_metrics(pred_median, update$Chla_sqrt)
AR_metrics <- model_metrics(pred_AR, update$Chla_sqrt)
