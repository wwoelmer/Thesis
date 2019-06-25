# script to look at the possible predictable drivers of inflow at the weir to FCR

library(tidyverse)
library(stats)
library(MuMIn)
library(rsq)

# read in inflow data which has been aggregated to daily statistics
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
# met data already aggregated to a daily scale
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

