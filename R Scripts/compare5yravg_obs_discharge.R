# compare the 5-year average of discharge at FCR inflow site to the obs inflow in 2019
# the 5-year average is what is currently being used to forecast discharge in the empirical model

library(lubridate)
library(tidyverse)

# use the function that FLARE uses to make the 5year avg
folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"

source(paste0(folder,"/","Rscripts/create_inflow_outflow_file_old.R"))
full_time <- seq(as.Date("2019-01-01"), as.Date(Sys.Date()), by = '1 day')
reference_tzone <- "GMT"
working_glm <- working_glm <- paste0(folder, "/", "GLM_working")  


create_inflow_outflow_file(full_time ,
                           working_glm = working_glm, 
                           input_tz = "EST5EDT",
                           output_tz = reference_tzone)

fiveyear <- read.csv(paste0(folder,"/", "GLM_working/FCR_inflow.csv"))
colnames(fiveyear) <- c('Date', 'Flow_5year', 'SD')
fiveyear$Date <- as.Date(fiveyear$Date)

# compare this to observed discharge--use the wvwa data because this is where the 5 year avg data comes from
obs <- read.csv('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow_working.csv')
obs <- obs %>% select(DateTime, Flow_cms) %>% 
  mutate(Date = date(DateTime)) %>% 
  group_by(Date) %>% 
  mutate(Flow_cms_daily = mean(Flow_cms))
obs_flow <- obs[!duplicated(obs$Date),]
obs_flow <- obs_flow %>% select(Date, Flow_cms_daily)
colnames(obs_flow) <- c('Date', 'Flow_obs')

discharge <- left_join(fiveyear, obs_flow)
discharge <- discharge %>% mutate(residual = Flow_obs - Flow_5year)

plot(discharge$Flow_obs, discharge$Flow_5year, ylim = c(0,0.15))
abline(0,1)
plot(discharge$Date, discharge$Flow_obs, type = 'l')
points(discharge$Date, discharge$Flow_5year, col = 'red', type = 'l')
legend('topright', bty = 'n', lty = c(1,1), c('observed flow', '5 year avg'), col = c('black', 'red'))
hist(discharge$residual)
plot(discharge$residual)
abline(h = 0)
