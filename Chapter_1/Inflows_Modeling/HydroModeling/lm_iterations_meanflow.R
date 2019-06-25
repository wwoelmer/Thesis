# script to look at the possible predictable drivers of inflow at the weir to FCR

library(tidyverse)
library(stats)
library(MuMIn)
library(rsq)


inf <-  read.csv("./Inflow/inflowcalcs_FCR.csv")
inf$Date <- as.Date(inf$Date)
plot(inf$Date, inf$mean_wrt)
plot(inf$Temp_inf_mean, inf$mean_flow)


# look at lags of inflow data, first need to remove NAs
inf_clean <- na.omit(inf)
lag.plot(inf_clean$mean_flow, lags = 7, diag = TRUE)
# the relationship at lag 1 (1 day) is pretty good
# build a linear model using mean flow (t-1) 
inf_clean$meanflow_lag1 <- lag(inf_clean$mean_flow, n = 1L)
flowmod <- lm(inf_clean$mean_flow~inf_clean$meanflow_lag1)
round((rsq(flowmod, type = 'sse')), digits = 3)
# r2 of 0.86, pretty good!
plot(inf_clean$meanflow_lag1, inf_clean$mean_flow)
abline(lm(inf_clean$mean_flow~inf_clean$meanflow_lag1))
# some outliers, but overall a pretty good relationship

# now look at a lag of 7 days
inf_clean$meanflow_lag7 <- lag(inf_clean$mean_flow, n = 7L)
flowmod_7 <- lm(inf_clean$mean_flow~inf_clean$meanflow_lag7)
round((rsq(flowmod_7, type = 'sse')), digits = 3)
# at one week out, the r2 goes down to 0.57
plot(inf_clean$meanflow_lag7, inf_clean$mean_flow)
abline(lm(inf_clean$mean_flow~inf_clean$meanflow_lag7))
# r2 of 0.57, definitely not as good as a 1 day lag

# bring in other possible explanatory met variables for a global model
met <- read.csv('./MET/Met_FCR_daily.csv')
met <- met %>% select(-X)
met$Date <- as.Date(met$Date)

metflow <- left_join(inf_clean, met)

# get rid of some unwanted columns: things that are not predictable and things that are definitely correlated
# as a first cut, choosing one of the three summary metrics (mean, max, median) for variables that have summary stats
# will need to check later if the other metrics are more informative 
metflow <- metflow %>% select(-mean_wrt, -(Temp_inf_mean:flow_median), -AirTemp_max, -AirTemp_median, 
                              -(RelHum_mean:RelHum_median), -ShortWave_max, -(WindSpeed_mean:WindSpeed_median))
# some transformations
metflow <- metflow %>% mutate(Rain_sum_log = log(Rain_sum + 0.0000012)) %>% # added the smallest non-zero/2
  mutate(AirTemp_mean_log = log(AirTemp_mean )) %>% 
  mutate(WindSpeed_max_log = log(WindSpeed_max))
# separate out the two different lags, one dataframe with 1 day lag and another with the 7 day lag
metflow1 <- metflow %>% select(-meanflow_lag7)
metflow7 <- metflow %>% select(-meanflow_lag1)

# now subset past the initial NAs at the beginning so the model won't fail on the NAs
# and for only the first three years of data to use as a training set to test against the later years
metflow1 <- metflow1[metflow1$Date>"2013-05-15" & metflow1$Date<"2015-12-31",]
metflow7 <- metflow7[metflow7$Date>"2013-05-21"& metflow7$Date<"2015-12-31",]


# create global model with all possible variables, first with just the 1 day lag
metflow1_mod <- glm(mean_flow~  meanflow_lag1 + AirTemp_mean + Rain_sum + RelHum_max + ShortWave_mean + WindSpeed_max,
                    data = metflow1, family = gaussian, na.action = "na.fail")

glm_metflow1 <- dredge(metflow1_mod, rank = "AICc", fixed = 'meanflow_lag1')
metflow1_select <- subset(glm_metflow1, delta<2)

# build the selected models
# choose the simplest one (df = 4)
metflow1_mod1 <- glm(mean_flow~meanflow_lag1 + Rain_sum, 
                  data = metflow1, family = gaussian, na.action = "na.fail")
pred_metflow1_mod1 <- predict(metflow1_mod1, newdata = metflow)
plot(pred_metflow1_mod1, metflow$mean_flow)
plot(metflow$Date, metflow$mean_flow, type = 'l')
points(metflow$Date, pred_metflow1_mod1, col = 'red', type = 'l')
legend('topleft', c('observed', 'modeled'), lty = c(1,1), col = c('black', 'red'), bty = 'n')
round((rsq(metflow1_mod1, type = 'sse')), digits = 3)
summary(metflow1_mod1)

# equation: meanflow(t) = 0.0002172 (+/- 0.0005070) + 0.9535769meanflow(t-1) + 0.0253582rain_sum(t)
# this seems way too good of a model, how does it do so well with 2016-2018 even though only trained on 2013-2015??
# next try the other selected model combos

###############################################################################################################
# build some other models
# first try just air temp to see how will that does
metflow1_test <- glm(mean_flow ~ AirTemp_mean, data = metflow1, family = gaussian, na.action = 'na.fail')
pred_metflow1_test <- predict(metflow1_test, newdata = metflow)
plot(pred_metflow1_test, metflow$mean_flow)
plot(metflow$Date, metflow$mean_flow, type = 'l')
points(metflow$Date, pred_metflow1_test, col = 'red', type = 'l')
# ok I feel better that this model is not a good fit

metflow1_mod2 <- glm(mean_flow ~ meanflow_lag1 + Rain_sum + AirTemp_mean,
                      data = metflow1, family = gaussian, na.action = 'na.fail')
pred_metflow1_mod2 <- predict(metflow1_mod2, newdata = metflow)
plot(pred_metflow1_mod2, metflow$mean_flow)
plot(metflow$Date, metflow$mean_flow, type = 'l')
points(metflow$Date, pred_metflow1_mod2, col = 'red', type = 'l')
round((rsq(metflow1_mod2, type = 'sse')), digits = 3)

############################################################################3
metflow1_modrain <- glm(mean_flow ~ meanflow_lag1 + Rain_sum,
                        data = metflow1, family = gaussian, na.action = 'na.fail')
pred_metflow1_modrain <- predict(metflow1_modrain, newdata = metflow)
plot(pred_metflow1_modrain, metflow$mean_flow)
plot(metflow$Date, metflow$mean_flow, type = 'l')
points(metflow$Date, pred_metflow1_modrain, col = 'red', type = 'l')
round((rsq(metflow1_modrain, type = 'sse')), digits = 3)

#############################################################################
metflow1_modlag <- glm(mean_flow ~ meanflow_lag1,
                       data = metflow1, family = gaussian, na.action = 'na.fail')

pred_metflow1_modlag <- predict(metflow1_modlag, newdata = metflow)
plot(pred_metflow1_modlag, metflow$mean_flow)
plot(metflow$Date, metflow$mean_flow)
points(metflow$Date, pred_metflow1_modlag, col = 'red', type = 'l')
round((rsq(metflow1_modlag, type = 'sse')), digits = 3)

##############################################################################
metflow1_modrain_only <- glm(mean_flow ~ Rain_sum, data = metflow1, 
                             family = gaussian, na.action = 'na.fail')
pred_metflow1_modrain_only <- predict(metflow1_modrain_only, newdata = metflow)
plot(pred_metflow1_modrain_only, metflow$mean_flow)
plot(metflow$Date, metflow$mean_flow)
points(metflow$Date, pred_metflow1_modrain_only, col = 'red', type = 'l')
round((rsq(metflow1_modrain_only, type = 'sse')), digits = 3)

#########################################################################################
# let's look a little more about the models that do well
par(mfrow=c(2,2))
plot(pred_metflow1_mod1, metflow$mean_flow, xlab = 'flow(t-1) + rain') # model with flow(t-1) + rain
plot(pred_metflow1_modlag, metflow$mean_flow, xlab = 'flow(t-1)') # model with flow(t-1) ONLY
plot(pred_metflow1_modrain_only, metflow$mean_flow) # model with only rain

par(mfrow = c(1,1))
plot(metflow$Date, metflow$mean_flow)
points(metflow$Date, pred_metflow1_mod1, col = 'red', type = 'l')

plot(metflow$Date, metflow$mean_flow)
points(metflow$Date, pred_metflow1_modlag, col = 'red', type = 'l')

#########################################################################################
# let's look at that again but with rain log transformed
metflow1_log = na.omit(metflow1)
metflow1log_mod <- glm(mean_flow~  meanflow_lag1 + AirTemp_mean + Rain_sum_log + RelHum_max + ShortWave_mean + WindSpeed_max_log,
                    data = metflow1, family = gaussian, na.action = "na.fail")

glm_metflow1log <- dredge(metflow1log_mod, rank = "AICc", fixed = 'meanflow_lag1')
metflow1log_select <- subset(glm_metflow1log, delta<2)

### what about logging air temp?

# build the simplest model
metflow1_log_mod1 <- glm(mean_flow ~ AirTemp_mean + meanflow_lag1 + Rain_sum_log,
                         data = metflow1, family = gaussian, na.action = 'na.fail')
pred_metflow1_log_mod1 <- predict(metflow1_log_mod1, newdata = metflow)
plot(pred_metflow1_log_mod1, metflow$mean_flow)


# rain only?
metflow1_log_rainonly <- glm(mean_flow ~ Rain_sum_log, data = metflow1, 
                             family = gaussian, na.action = 'na.fail')
pred_metflow1_log_rainonly <- predict(metflow1_log_rainonly, newdata = metflow)
plot(pred_metflow1_log_rainonly, metflow$mean_flow)

# also look at the pressure sensor from the catwalk data in relation to mean daily inflow
download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','Catwalk.csv')
catheader<-read.csv("Catwalk.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
catdata<-read.csv("Catwalk.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(catdata)<-names(catheader) #combine the names to deal with Campbell logger formatting

catdata <- read.csv("Catwalk_cleanedEDI.csv")

# this dataframe is huge--first select the columns needed
catdata <- na.omit(catdata)
catdata$Date <- as.Date(catdata$DateTime, format = "%Y-%m-%d")
catdata <- catdata[catdata$Date>"2018-08-06",]
catdata$EXO_pressure <- as.numeric(catdata$EXO_pressure)
summary <- catdata %>% group_by(Date) %>% mutate(mean_psi = mean(EXO_pressure))
plot(summary$Date, summary$EXO_pressure, col = 'black')
summary <- summary %>% select(Date, EXO_pressure)
summary <- summary %>% distinct(Date, .keep_all = TRUE)

# now read in the newest inflow data that includes 2018
new <- read.csv("C:/Users/wwoel/Dropbox/Inflows/FCR_inflow_2018.csv")
new$Date <- as.Date(new$DateTime, format = "%Y-%m-%d")
new <- new %>% group_by(Date) %>% mutate(mean_psi_inf = mean(Pressure_psi))
new <- new %>% select(Date, mean_psi_inf)
new <- new %>% distinct(Date, .keep_all = TRUE)
inf_presh <- left_join(new, summary)
inf_presh <- inf_presh[inf_presh$Date>"2018-08-06",]
par(mfrow = c(2,2))
plot(inf_presh$EXO_pressure, inf_presh$mean_psi_inf, type = 'l')
plot(inf_presh$Date, inf_presh$EXO_pressure, type = 'l')
plot(inf_presh$Date, inf_presh$mean_psi_inf, type = 'l', col = 'brown')
