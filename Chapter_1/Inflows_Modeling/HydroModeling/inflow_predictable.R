# script to forecast mean inflow volume (at FCR weir) based on precip data and calculated baseflow
# adapted from code produced by NKW and B. Steele

#install.packages("EcoHydRology")
library(EcoHydRology)
library(dplyr)

# currently formatted to read in NLDAS2 data
# will need to adapt to read in weather forecast
MetData <- read.csv('./MET/FCR_GLM_met_NLDAS2_010113_010118_GMTadjusted.csv', header=T, na.strings=NA) #in begwrkspc

MetData$dateTime <- as.POSIXct(MetData$time, format='%Y-%m-%d %H:%M:%S') #format date #in begwrkspc
MetData$AirTemp.F <- MetData$AirTemp*1.8 + 32 #convert to F #in begwrkspc
MetData$Rain.m_hr <- MetData$Rain/24 #convert to rain in m/hour #in begwrkspc

impInf <- 0  # impervious SA within the weir's subWS calculated from StreamStats watershed delineation model
RvInf = 0.05 + 0.9 * impInf

# setup for calcs if there is snow
case <- (c(3, 5, 6, 7)) #in begwrkspc
melt_coeff <- c(0.0001574803, 0.0007086614, 0.0006417323, 0.0002440945) #in begwrkspc
case_melt <- data.frame(case, melt_coeff)  #in begwrkspc

#assign melt cases to met data  
MetData_sub <- subset(MetData, select=c('dateTime', 'AirTemp', 'AirTemp.F', 'WindSpeed', 'Rain.m_hr')) #subset for needed variables #in begwrkspc
MetData_sub <- subset(MetData_sub, subset=dateTime>'1979-01-01 12:00:00') #remove no data #in begwrkspc
MetData_sub$melt_case <- as.numeric(NA) #create column to fill in with melting case #in begwrkspc
#str(MetData_sub)

#assign case number by temp, rain and wind data - in begwrkspc
for (i in 1:nrow(MetData_sub)){
  if(MetData_sub$AirTemp[i] <= 0){  #if temperature is at or below zero there is no melt, only accumulation
    MetData_sub$melt_case[i] <- NA} else  #therefore there is no melt case applicable
      if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]>=0.0762 & MetData_sub$WindSpeed[i]>=6.706) { #temp >0 and heavy rain and windy, case 5
        MetData_sub$melt_case[i] <- 5} else
          if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]>=0.0762 & MetData_sub$WindSpeed[i]<6.706) { #temp >0 and heavy rain and light windy, case 6
            MetData_sub$melt_case[i] <- 6} else
              if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]<0.0762 & MetData_sub$WindSpeed[i]>=6.706) { #temp >0 and light rain and windy, case 6
                MetData_sub$melt_case[i] <- 6} else
                  if (MetData_sub$AirTemp[i]>0 & MetData_sub$Rain.m_hr[i]<0.0762 &MetData_sub$Rain.m_hr[i]>0 & MetData_sub$WindSpeed[i]<6.706) { #temp >0 and light rain and windy, case 6
                    MetData_sub$melt_case[i] <- 7} else
                      MetData_sub$melt_case[i] <- 3 #any other temperature, rain and windspeed combination would be
}

#merge with case data for melt coefficient
MetData_sub <- merge(MetData_sub, case_melt, by.x='melt_case', by.y='case', all.x=T) #in begwrkspc

MetData_sub <- MetData_sub[with(MetData_sub, order(dateTime)), ] #re-order by date (R sorts it by melt case and it needs to be in date order for next step)
MetData_sub$SWE <- 0 # column to store SWE (snow accumulation as a depth of water) #in begwrkspc
MetData_sub$runoff_SWE <- 0  # column of SWE contribution to runoff #in begwrkspc

CwrInf <- RvInf + 0.1
MetData_Inf <- MetData_sub
MetData_Inf$runoff_reg_m <- MetData_Inf$Rain.m_hr * RvInf * 0.9
MetData_Inf$runoff_reg_m_fro <- MetData_Inf$Rain.m_hr * (CwrInf) * 0.9

#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_Inf)){                                        # for every row (except the first)
  if(MetData_Inf$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_Inf$Rain.m_hr[i] + MetData_Inf$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_Inf$SWE[i] <- swe_index 
  } else { 
    if ((MetData_Inf$AirTemp[i]>0) & (MetData_Inf$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_Inf$melt_coeff[i] * (MetData_Inf$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_Inf$SWE[i-1])                # in case melt value is greater 
      MetData_Inf$SWE[i] <- MetData_Inf$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- CwrInf*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_Inf$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_Inf$runoff_sum <- 0

for (i in 1:nrow(MetData_Inf)) {
  if (MetData_Inf$AirTemp[i]>0 & MetData_Inf$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_Inf$runoff_reg_m_fro[i] + MetData_Inf$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_Inf$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_Inf$AirTemp[i]>0 & MetData_Inf$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_Inf$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_Inf$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

# define the area of the subwatershed of the weir'ed inflow 
areaInf <- 77.7
MetData_Inf$total_runoff_m3_hr <- MetData_Inf$runoff_sum * areaInf * 10000
MetData_Inf$date <- as.Date(format(MetData_Inf$dateTime, '%Y-%m-%d'))
MetData_Inf_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_Inf, FUN = sum)) ; names(MetData_Inf_daily) <- c("date", "total_runoff_m3d")
MetData_Inf_daily$total_runoff_model_imp_50_50_m3d<-NA

# calculate discharge as proportion of runoff from previous days (because only 50% makes it there)
for(i in 2: nrow(MetData_Inf_daily)) {
  model_50_50 =  MetData_Inf_daily$total_runoff_m3d [i] * .5 + MetData_Inf_daily$total_runoff_m3d [i-1] * .5
  MetData_Inf_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

# convert to m3ps
MetData_Inf_daily$total_runoff_model_imp_50_50_m3s <- MetData_Inf_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_Inf_daily_sub <- subset(MetData_Inf_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))


# as a check compare to the runoff data for the FCR inflow that was calculated last week
check <- read.csv("C:/Users/wwoel/Dropbox/Inflows/HydroModeling/overland_runoff_FCR.csv")
check$date <- as.Date(check$date)
check <- check[check$Site=='inf_overland',]
plot(check$date, check$runoff_m3s, type= 'l', col='black')
points(MetData_Inf_daily_sub$date, MetData_Inf_daily_sub$total_runoff_model_imp_50_50_m3s, type = 'l', col='red')

# this runoff is the proportion of runoff that applies to the area of the subwatershed of the weir'd inflow

#####################################################################################################################################
# now add in baseflow
# try a few methods here to see how they compare
# both a median and mean baseflow addition and see how it compares to measured flow at the weir

# get rid of the long long name
runoff <- MetData_Inf_daily_sub
colnames(runoff)[2] <- "runoff_m3s"

# mean and median baseflow values calculated by CCC
runoff <- runoff %>% mutate(flow_bfmedian_CCC = runoff_m3s + 0.01599371) %>%
  mutate(flow_bfmean_CCC = runoff_m3s +  0.02449394)

# now compare these two estimations to measured flow at the weir
inf_obs <- read.csv("./Inflow/inflow_interpolation_2013_2017.csv")
inf_obs$Date <- as.Date(inf_obs$DateTime, format="%Y-%m-%d")
inf_obs <- inf_obs[inf_obs$Date<"2017-11-01",]
#streamflow <- inf_obs$Flow_cms

# and to inflow calculations made (mean, median, max, min)
inf_calc <- read.csv("./Inflow/inflowcalcs_FCR.csv")
inf_calc$Date <- as.Date(inf_calc$Date)


plot(inf_obs$Date, inf_obs$Flow_cms, type = 'l')
points(inf_calc$Date, inf_calc$mean_flow, type = 'l', col = 'yellow')
points(inf_calc$Date, inf_calc$flow_max, type = 'l', col = 'red')
points(inf_calc$Date, inf_calc$flow_min, type = 'l', col = 'orange')
points(inf_calc$Date, inf_calc$flow_median, type = 'l', col = 'purple')

plot(inf_obs$Date, inf_obs$Flow_cms, type = 'l')
points(runoff$date, runoff$flow_bfmedian_CCC, col = 'red', type = 'l')
points(runoff$date, runoff$flow_bfmean_CCC, col = 'orange', type = 'l')
# these runoff + bf calculated from CCC miss a ton of the dynamics

# just a visualization to show the bf additions just raise runoff by a constant value
plot(runoff$date, runoff$runoff_m3s, type = 'l')
points(runoff$date, runoff$flow_bfmedian_CCC, type = 'l', col = 'red')
points(runoff$date, runoff$flow_bfmean_CCC, type = 'l', col = 'purple')


# look at rain and total runoff--these should follow almost the same pattern
# with some differences due to snow melt
plot(MetData_Inf$dateTime ,MetData_Inf$Rain.m_hr, type = 'l', col = 'black')
plot(MetData_Inf$dateTime, MetData_Inf$total_runoff_m3_hr, type = 'l', col = 'red')

# want to calculated baseflow using ecohydrology but need to fix the na problem I think
# when this is calculated, want to add the ecohydrology baseflow for the weir'd inflow to runoff
# to see if it can compare to observed flow at the weir
# runoff + baseflow should = observed flow

met <- read.csv("./MET/Met_FCR_daily.csv")
met$Date <- as.Date(met$Date)
plot(met$Date, met$Rain_sum, type = 'l')

# visualization of the ecohydrolog (CCC) calculated bf additions and mean flow at the weir
# which is what we're trying to capture
plot(inf_calc$Date, inf_calc$mean_flow, type = 'l', col = 'red')
points(runoff$date, runoff$flow_bfmean, col = 'orange', type = 'l')
points(runoff$date, runoff$flow_bfmedian, col = 'blue', type = 'l')
legend('topleft', c('mean daily flow', 'runoff+meanbf_CCC', 'runoff+medianbf_CCC'), col = c('red', 'orange', 'blue'),bty = 'n', lty = c(1,1))



##############################################################################################################3
# calculated baseflow from NKW hydromodel and see how well that captures mean flow dynamics

# runoff for the whole watershed calculated from NKW overland flow model
whole <- read.csv("C:/Users/wwoel/Dropbox/Inflows/HydroModeling/overland_runoff_FCR.csv")

# some data management to get everything in one place
whole <- whole[whole$Site=="whole",]
colnames(whole)[1] <- "Date"
whole$Date <- as.Date(whole$Date)
wrt <- inf_calc %>% select(Date, mean_wrt, mean_flow)

hydro <- left_join(whole, wrt, by = "Date")


# calculate baseflow based on FCR's volume, residence time (in seconds), and calculated runoff inflow
hydro <- hydro %>% mutate(baseflow_NKW = (3.1E5/(mean_wrt*86400)) - runoff_m3s)

plot(hydro$Date, hydro$baseflow, type = 'l', col = 'red')
points(hydro$Date, hydro$mean_flow, type = 'l', col = 'black')
legend('topleft', c("baseflow", 'obs meanflow'), lty = c(1,1), bty = 'n', col = c('red','black'))

# calculated mean and median baseflow
mean_bf <- mean(hydro$baseflow_NKW, na.rm=TRUE)
median_bf <- median(hydro$baseflow_NKW)

plot(hydro$Date, hydro$runoff_m3s, type = 'l')
points(hydro$Date, hydro$mean_flow, type = 'l', col = 'red')

# now add mean baseflow to runoff to see how it compares to the inflow measurements
hydro <- hydro %>% mutate(meanbf_plusrunoff = runoff_m3s + mean_bf) %>%
  mutate(bf_plusrunoff = runoff_m3s + baseflow_NKW)

plot(hydro$Date, hydro$runoff_m3s, type = 'l', ylim = c(0,0.2))
#points(hydro$Date, hydro$baseflow_NKW, type = 'l', col = 'orange')
points(hydro$Date, hydro$mean_flow, type = 'l', col = 'red')
points(hydro$Date, hydro$meanbf_plusrunoff, col = 'purple', type = 'l')
points(hydro$Date, hydro$bf_plusrunoff, col = 'blue', type = 'l')
legend('topleft',lty = c(1,1), bty = 'n', c('mean weir flow', 'mean bf + runoff', 'bf+runoff'), col = c('red', 'purple','blue'))

# what does meanbf+runoff and mean flow look like plotted against each other
plot(hydro$meanbf_plusrunoff, hydro$mean_flow)
abline(lm(hydro$mean_flow~ hydro$meanbf_plusrunoff))
mod <- lm(hydro$mean_flow~ hydro$meanbf_plusrunoff)
plot(mod)

# calculate mean flow based on the lm equation
hydro <- hydro %>% mutate(meanflow_pred = 0.02356 + 0.51091*meanbf_plusrunoff)
plot(hydro$mean_flow, hydro$meanflow_pred)
# can the residuals tell us something?
plot(hydro$Date, hydro$mean_flow-hydro$meanflow_pred)
abline(h=0)
hist(hydro$mean_flow-hydro$meanflow_pred)
plot(hydro$Date, hydro$mean_flow)
points(hydro$Date, hydro$meanflow_pred, col = 'red')
# are these incidences when BVR was high?

# bring in BVR water level data
level <- read.csv("C:/Users/wwoel/Dropbox/Inflows/BVR_waterlevel.csv")
level$Date <- as.Date(level$Date)
plot(level$Date, level$waterlevelbelow_ft, type = 'l')
abline(h=-5, col = 'blue')
# make the time period the same as the other data (2013-2018)
short <- level[level$Date>"2013-01-01",]
plot(short$Date, short$waterlevelbelow_ft, type = 'l')
abline(h=-5, col = 'blue')

plot(hydro$runoff_m3s, hydro$mean_wrt)
plot(log(hydro$runoff_m3s), log(hydro$mean_wrt))
abline(lm(log(hydro$mean_wrt)~log(hydro$runoff_m3s), na.action = na.exclude))



