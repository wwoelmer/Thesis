# gather met, inflow, and exo data to fit high-frequency autoregressive time series of exo chl data

library(tidyverse)
library(lubridate)

# read in EXO AR data
update <- read.csv('./Data/ARIMA_data/EXO_plusdrivers_AR.csv')
update$Date <- as.Date(update$Date)

# subset to part of the data extent to serve as a training dataset
update <- update[update$Date < "2019-05-01",]


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
data_all <- read.csv('./Data/ARIMA_data/EXO_plusdrivers_AR.csv')
mod_max <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + ShortWave_max, data = update, family = gaussian, na.action = 'na.fail')
pred_max <- predict(mod_max, newdata = data_all)

mod_mean <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + ShortWave_mean, data = update, family = gaussian, na.action = 'na.fail')
pred_mean <- predict(mod_mean, newdata = data_all)

mod_median <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + AirTemp_median + LongWave_median, data = update, family = gaussian, na.action = 'na.fail')
pred_median <- predict(mod_median, newdata = data_all)

mod_AR <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt, data = update, family = gaussian, na.action = 'na.fail')
pred_AR <- predict(mod_AR, newdata = data_all)

plot(data_all$Date, data_all$Chla_sqrt)
points(data_all$Date, pred_max, col = 'red', type = 'l')
plot(update$Date, update$Chla_sqrt)

source(paste0('C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3',"/","Rscripts/model_assessment.R"))
max_metrics <- model_metrics(pred_max, data_all$Chla_sqrt)
mean_metrics <- model_metrics(pred_mean, update$Chla_sqrt)
median_metrics <- model_metrics(pred_median, update$Chla_sqrt)
AR_metrics <- model_metrics(pred_AR, update$Chla_sqrt)
