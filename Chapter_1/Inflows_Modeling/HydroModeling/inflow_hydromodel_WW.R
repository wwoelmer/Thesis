#*****************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      *
#*                                                               *
#* TITLE:   CNH-GLM_sunapee_inflowoutflow_23Mar2017.r            *
#* AUTHOR:  B. Steele                                            *
#* SYSTEM:  Lenovo W530, Win 7, R 3.2.2                          *
#* DATE:    23Mar2017                                            *
#* PROJECT: CNH-GLM                                              *
#* PURPOSE: using previously-truthed methods model inflows for   *
#*          Sunapee based on NLDAS-2 met data and GIS analysis   *
#*          of impervious surfaces, calculate baseflow from      *
#*          residence time and daily volume of the lake,         *
#*          model in-stream temperature based on transducers and *
#*          NLDAS-2 met data                                     *
#* LAST MODIFIED: 23Mar2017                                      *
#* BY:      B. Steele                                            *
#* NOTES:   this is a compilation of code from the R files       *
#*          LS_streaminflow_21Mar2017 and LS_GLM_21Mar2017       *
#* UPDATES: Met data through 2016 added, streamlined to          *
#*          calculate inflow and outflow in a single file        *
#*****************************************************************



### !!!!!! 
# NKW modified this on 9 Jun 2017 to change the impervious surface area of 505
# to simulate full build-out conditions

library(gdata)
library(doBy)
library(ggplot2)
library(GGally)
library(reshape)
library(tidyverse)

#add final theme for formatting ggplots
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=16, face='bold'))

#load workspace. CNH-GLM_sunapee_inflowoutflow_begwrkspc has all the original data frames resulting from the 'read.csv' and 'read.xls' functions
#change this to the appropriate working directory
#setwd("/Volumes/My Passport/GLM/BethelRcode/r programs and workspaces")
#load("CNH-GLM_sunapee_inflowoutflow_begwrkspc.RData")

#### bring in NLDAS-2 climate data ####
MetData <- read.csv('./MET/FCR_GLM_met_NLDAS2_010113_010118_GMTadjusted.csv', header=T, na.strings=NA) #in begwrkspc

MetData$dateTime <- as.POSIXct(MetData$time, format='%Y-%m-%d %H:%M:%S') #format date #in begwrkspc
MetData$AirTemp.F <- MetData$AirTemp*1.8 + 32 #convert to F #in begwrkspc
MetData$Rain.m_hr <- MetData$Rain/24 #convert to rain in m/hour #in begwrkspc

#### ---- RUNOFF AND SNOW MELT CALCS ---- ####

#calculate runoff coefficients (Rv) and store as values in workspace
# Rv = 0.05 + 0.9Ia 
#Ia = proportion of drainage area tha tis impervious (in decimal form)
# FCR refers to the whole watershed
# Inf refers to the inflow to FCR from BVR where the weir is
# Wet refers to the wetland at the north side of the reservoir

impFCR <- 0.0000437 #Ia # number calculated from StreamStats watershed delineation model
RvFCR = 0.05 + 0.9 * impFCR #multiply by assumed variable
impInf <- 0  # number calculated from StreamStats watershed delineation model
RvInf = 0.05 + 0.9 * impInf
impWet <- 0
RvWet <- 0.05 +0.9*impWet

##### SNOW ACCUMULATION #####

# If precipitation is falling when the air temperature is less than or equal
# to 0 degrees celsius, then the depth of precipition is stored as SWE
# (Snow Water Equivalents) 


#### Snow Melt #####

# If T(degrees celsius) is greater than 0, the precipitation falls as rain
# and any accumulated SWE contributes to runoff according to the relationship 
# from Part 630 Hydrology National Engineering Handbook with equations converted
# from US to metric:
# melt (meters) = (Cm * ((T * 1.8 + 32) - 32))
# SWE contribution to runoff = melt * Cwr
# Cm (melt rate coefficient) = 0.001524 meters of melt per degree day
# Cwr (winter runoff coefficient) = 0.5

# Degree day method of snowmelt modeling:
# http://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/NEHhydrology/ch11.pdf
# also referred to as the HEC-1 here: http://www.dtic.mil/dtic/tr/fulltext/u2/a366395.pdf
# 

# Table 6-1 http://www.publications.usace.army.mil/Portals/76/Publications/EngineerManuals/EM_1110-2-1406.pdf
# Relative Magnitude of Melt-Rate Factors (Refer to Table 5-4)
# Case  -  in./deg F - comment - cm/deg F
# 1 - 0.068 Clear, low albedo - 0.02677165
# 2 - 0.073 Case 1, 40% forest -  0.02874016
# 3 - 0.040 Case 1, cloud cover - 0.01574803
# 4 - 0.046 Case 1, fresh snow - 0.01811024
#- - - - - - - - - - - - - - - - - - - - - - -- - - - - -
# 5 - 0.180 Heavy rain, windy - 0.07086614
# 6 - 0.163 Light rain, windy - 0.06417323
# 7 - 0.062 Light rain, light wind - 0.02440945
  
# heavy rain is 3in/hr = 0.0762 m/h
# windy is > 15mph = 6.706 mps

case <- (c(3, 5, 6, 7)) #in begwrkspc
melt_coeff <- c(0.0001574803, 0.0007086614, 0.0006417323, 0.0002440945) #in begwrkspc
case_melt <- data.frame(case, melt_coeff)  #in begwrkspc

#assign melt cases to met data  
MetData_sub <- subset(MetData, select=c('dateTime', 'AirTemp', 'AirTemp.F', 'WindSpeed', 'Rain.m_hr')) #subset for needed variables #in begwrkspc
MetData_sub <- subset(MetData_sub, subset=dateTime>'1979-01-01 12:00:00') #remove no data #in begwrkspc
MetData_sub$melt_case <- as.numeric(NA) #create column to fill in with melting case #in begwrkspc
str(MetData_sub)

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
str(MetData_sub) #check work #in begwrkspc

####---- RUNOFF DATA BY SUB WATERSHED ---- ####
# R = P * Pj * Rv 
# Where: 
# R =  runoff (meters) 
# P =  rainfall (meters) 
# Pj = Fraction of  rainfall events that produce runoff (usually 0.9) 
# Rv = Runoff coefficient 

# melt partitioning between infiltration and runoff generation should be
# slightly higher than assumption for non-frozen periods (if we are using 40%
# infiltration for non-winter conditions, let's use 50% for winter to account for 
# some ground being frozen). !!!! I am very open to adjusting this percent !!!! ##NKW: <-I think this is worded backwards?
# using the above logic, we add 0.1 to the Rv value to calculate the Cwr value
# calculate winter runoff coefficient based on land cover type - adding 10% as 
# we assume ground is frozen or saturated
CwrFCR <- RvFCR + 0.1
CwrInf <- RvInf + 0.1
CwrWet <- RvWet + 0.1


#runoff_reg_m and runoff_reg_m_fro are calcuated for all times so that values can be applied in the for loop in the next step

MetData_FCR <- MetData_sub #copy into new dataframe
MetData_FCR$runoff_reg_m <- MetData_FCR$Rain.m_hr * RvFCR * 0.9 #apply runoff coefficient specific to subwatershed
MetData_FCR$runoff_reg_m_fro <- MetData_FCR$Rain.m_hr * (CwrFCR) * 0.9 #caluclate winter/frozen runoff specific to subwatershed

MetData_Inf <- MetData_sub
MetData_Inf$runoff_reg_m <- MetData_Inf$Rain.m_hr * RvInf * 0.9
MetData_Inf$runoff_reg_m_fro <- MetData_Inf$Rain.m_hr * (CwrInf) * 0.9

MetData_Wet <- MetData_sub
MetData_Wet$runoff_reg_m <- MetData_Wet$Rain.m_hr * RvWet * 0.9
MetData_Wet$runoff_reg_m_fro <- MetData_Wet$Rain.m_hr * (CwrWet) * 0.9


#### FCR total wateshed Runoff ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_FCR)){                                        # for every row (except the first)
    if(MetData_FCR$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_FCR$Rain.m_hr[i] + MetData_FCR$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_FCR$SWE[i] <- swe_index 
  } else { 
    if ((MetData_FCR$AirTemp[i]>0) & (MetData_FCR$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_FCR$melt_coeff[i] * (MetData_FCR$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_FCR$SWE[i-1])                # in case melt value is greater 
      MetData_FCR$SWE[i] <- MetData_FCR$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- CwrFCR*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_FCR$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_FCR$runoff_sum <- 0

for (i in 1:nrow(MetData_FCR)) {
  if (MetData_FCR$AirTemp[i]>0 & MetData_FCR$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_FCR$runoff_reg_m_fro[i] + MetData_FCR$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_FCR$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_FCR$AirTemp[i]>0 & MetData_FCR$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_FCR$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_FCR$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

#### Inf: BVR Inflow Runoff ####
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

#### Wet Runoff: the wetland at the north end of the reservoir ####
#calculte snowmelt and rainfall -> runoff
for (i in 2:nrow(MetData_Wet)){                                        # for every row (except the first)
  if(MetData_Wet$AirTemp[i]<=0 ){                                  # If air temp is below 0...
    swe_index <- MetData_Wet$Rain.m_hr[i] + MetData_Wet$SWE[i-1]    # SWE is current rain plus previous SWE minus melt losses
    MetData_Wet$SWE[i] <- swe_index 
  } else { 
    if ((MetData_Wet$AirTemp[i]>0) & (MetData_Wet$SWE[i-1]>0)){     # If air temp is above 0, and there is storage, there is no rain
      SWEmelt_index <- (MetData_Wet$melt_coeff[i] * (MetData_Wet$AirTemp.F[i] - 32))/24        # melt according to equation described above, with coefficient to account for rain/wind
      SWEmelt_real_index <- min(SWEmelt_index, MetData_Wet$SWE[i-1])                # in case melt value is greater 
      MetData_Wet$SWE[i] <- MetData_Wet$SWE[i-1]- SWEmelt_real_index              # than stored SWE, just use SWE
      Qindex <- CwrWet*SWEmelt_real_index                                     # melt contribution to runoff is..
      MetData_Wet$runoff_SWE[i]<- Qindex                                  # ... and store in dataframe
    }
  }
}

## now calculate runoff due to melt and add it to regular runoff
MetData_Wet$runoff_sum <- 0

for (i in 1:nrow(MetData_Wet)) {
  if (MetData_Wet$AirTemp[i]>0 & MetData_Wet$runoff_SWE [i] > 0) {  # if air temp is above 0 and there is runoff due to melt
    RUNOFF_SUM <- MetData_Wet$runoff_reg_m_fro[i] + MetData_Wet$runoff_SWE [i]  # then total runoff is from off  (frozen) plus runoff from melt
    MetData_Wet$runoff_sum [i] <- RUNOFF_SUM      # store runoff in data frame
  } else {
    if (MetData_Wet$AirTemp[i]>0 & MetData_Wet$runoff_SWE[i] == 0) {   # if air temp is above 0 and there is no runooff due to melt, we assume the ground is not frozen nor is it saturated
      RUNOFF <- MetData_Wet$runoff_reg_m [i]  # therefore the runoff should be at the regular runoff rate
      MetData_Wet$runoff_sum [i] <- RUNOFF   #store that in the data frame
    } 
  } 
}

####************* scale up to WS and sub WS m3/day over all area **************####
# areas in hectares
areaFCR <- 354.8
areaInf <- 77.7
areaWet <- 163.2


MetData_FCR$total_runoff_m3_hr <- MetData_FCR$runoff_sum * areaFCR * 10000 #runoff in m. area is in ha. one ha is 10000m 
MetData_Inf$total_runoff_m3_hr <- MetData_Inf$runoff_sum * areaInf * 10000
MetData_Wet$total_runoff_m3_hr <- MetData_Wet$runoff_sum * areaWet * 10000


# add column for date for aggregation
MetData_FCR$date <- as.Date(format(MetData_FCR$dateTime, '%Y-%m-%d'))
MetData_Inf$date <- as.Date(format(MetData_Inf$dateTime, '%Y-%m-%d'))
MetData_Wet$date <- as.Date(format(MetData_Wet$dateTime, '%Y-%m-%d'))

# summarize runoff/area by day
MetData_FCR_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_FCR, FUN = sum)) ; names(MetData_FCR_daily) <- c("date", "total_runoff_m3d")
MetData_Inf_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_Inf, FUN = sum)) ; names(MetData_Inf_daily) <- c("date", "total_runoff_m3d")
MetData_Wet_daily <- do.call(cbind.data.frame, aggregate(total_runoff_m3_hr ~ date, data=MetData_Wet, FUN = sum)) ; names(MetData_Wet_daily) <- c("date", "total_runoff_m3d")

MetData_FCR_daily$total_runoff_model_imp_50_50_m3d<-NA
MetData_Inf_daily$total_runoff_model_imp_50_50_m3d<-NA
MetData_Wet_daily$total_runoff_model_imp_50_50_m3d<-NA

# calculate discharge as proportion of runoff from previous days (because only 50% makes it there)
for(i in 2: nrow(MetData_FCR_daily)) {
  model_50_50 =  MetData_FCR_daily$total_runoff_m3d [i] * .5 + MetData_FCR_daily$total_runoff_m3d [i-1] * .5
  MetData_FCR_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_Inf_daily)) {
  model_50_50 =  MetData_Inf_daily$total_runoff_m3d [i] * .5 + MetData_Inf_daily$total_runoff_m3d [i-1] * .5
  MetData_Inf_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}

for(i in 2: nrow(MetData_Wet_daily)) {
  model_50_50 =  MetData_Wet_daily$total_runoff_m3d [i] * .5 + MetData_Wet_daily$total_runoff_m3d [i-1] * .5
  MetData_Wet_daily$total_runoff_model_imp_50_50_m3d [i] <- model_50_50
}


#reality check
sum(MetData_FCR_daily$total_runoff_model_imp_50_50_m3d, na.rm = T)
sum(MetData_Inf_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) + sum(MetData_Wet_daily$total_runoff_model_imp_50_50_m3d, na.rm = T) 

## convert to m3ps
MetData_FCR_daily$total_runoff_model_imp_50_50_m3s <- MetData_FCR_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_Inf_daily$total_runoff_model_imp_50_50_m3s <- MetData_Inf_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)
MetData_Wet_daily$total_runoff_model_imp_50_50_m3s <- MetData_Wet_daily$total_runoff_model_imp_50_50_m3d / (24 * 60 * 60)

#subset for desired variables
MetData_FCR_daily_sub <- subset(MetData_FCR_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_Inf_daily_sub <- subset(MetData_Inf_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))
MetData_Wet_daily_sub <- subset(MetData_Wet_daily, select=c('date', 'total_runoff_model_imp_50_50_m3s'))

MetData_FCR_daily_sub$Site <- 'whole'
MetData_Inf_daily_sub$Site <- 'inf_overland'
MetData_Wet_daily_sub$Site <- 'wetland'

wsrunoff <- rbind(MetData_FCR_daily_sub, MetData_Inf_daily_sub )
wsrunoff <- rbind(wsrunoff, MetData_Wet_daily_sub)
colnames(wsrunoff)[2] <- "runoff_m3s"

#write.csv(wsrunoff, "C:/Users/wwoel/Dropbox/Inflows/overland_runoff_FCR.csv", row.names = FALSE)

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ######################

## I DELETED A BUNCH OF LINES FROM BETHELS CODE THAT ADDED STREAM TEMPERATURE (we modeled stream temp
# based on monitoring.. but I'm assuming you can't do that for Beaver. We could make an air temp relationship
# if you really want temp. We should discuss if you want!) I'm not sure you need anything below here.
# we also calculate baseflow based on the residence time. We also model outflow based on observed changes in 
# lake elevation. 



## calculate proportion of inflow from each gauged stream for use later when calculating baseflow ##
WS_sum_inf <- sum(WS$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_505 <- sum(runtemp505$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_510 <- sum(runtemp510$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_540 <- sum(runtemp540$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_665 <- sum(runtemp665$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_760 <- sum(runtemp760$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_788 <- sum(runtemp788$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_790 <- sum(runtemp790$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_800 <- sum(runtemp800$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_805 <- sum(runtemp805$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_830 <- sum(runtemp830$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_835 <- sum(runtemp835$total_runoff_model_imp_50_50_m3s, na.rm = T)
sum_inf_ung <- sum(runtempung$total_runoff_model_imp_50_50_m3s, na.rm = T)

#reality check - these should be the same
impscen<-(sum_inf_505 + sum_inf_510 + sum_inf_540 + sum_inf_665 + sum_inf_760 + sum_inf_788 + sum_inf_790 + sum_inf_800 + sum_inf_805 + sum_inf_830 + 
    sum_inf_835 + sum_inf_ung)
WS_sum_inf

pro_505 <- sum_inf_505/impscen
pro_510 <- sum_inf_510/impscen
pro_540 <- sum_inf_540/impscen
pro_665 <- sum_inf_665/impscen
pro_760 <- sum_inf_760/impscen
pro_788 <- sum_inf_788/impscen
pro_790 <- sum_inf_790/impscen
pro_800 <- sum_inf_800/impscen
pro_805 <- sum_inf_805/impscen
pro_830 <- sum_inf_830/impscen
pro_835 <- sum_inf_835/impscen
pro_ung <- sum_inf_ung/impscen



#reality check - this should be 1
pro_505 + pro_510 + pro_540 + pro_665 + pro_760 + pro_788 + pro_790 + pro_800 +  pro_805 + pro_830 + pro_835 + pro_ung

#### bring in lake sunapee area and volume data for other calculations ####
setwd("/Volumes/My Passport/GLM/BethelRcode/r programs and workspaces") # in begwrkspc
dam_volarea <- read.csv('historical area and volume according to dam depth.csv') #in begwrkspc
#dam_volarea<-read.csv("depth_volume_matrix_0p1m_15Jan2019.csv")
dam_volarea$date <- as.Date(dam_volarea$date, format='%Y-%m-%d')

#aggregate rain to the day
MetData_daily <- aggregate(Rain.m_hr ~ date, data=MetData, FUN = sum)
MetData_daily <- rename.vars(MetData_daily, from='Rain.m_hr', to='Rain.m_day')

dam_met <- merge(MetData_daily, dam_volarea, by='date', all.y=T)

#calculate change in storage between days
dam_met$chg_stor_m3 <- as.numeric('')

for(i in 2:nrow(dam_met)){
  dam_met$chg_stor_m3[i] = dam_met$storage_m3[i-1] - dam_met$storage_m3[i]
}

dam_met_chgstor <- subset(dam_met, select=c('date', 'chg_stor_m3'))

#### calculate base flow based on residence time and that (residence time = volume of lake / inflow) ####
#subset dam data for storage#
stor_dam_met <- subset(dam_met, select=c('date', 'storage_m3'))

#merge storage with WS inflow data
stor_dam_met <- merge(stor_dam_met, WS, by='date', all.x=T)
stor_dam_met <- remove.vars(stor_dam_met, names=('ModStreamTemp_degC'))

#calculate baseflow as volume of lake divided by residence time (converted to seconds) minus inflow model
stor_dam_met$baseflow_m3s <- (stor_dam_met$storage_m3/(3.1 * 365 * 24 * 60 * 60)) - (stor_dam_met$total_runoff_model_imp_50_50_m3s)

#determine the mean baseflow so that value can be applied to the dataset. (too many negative values to apply as is.)
mean_baseflow <- mean(stor_dam_met$baseflow_m3s, na.rm = T)
range(stor_dam_met$baseflow_m3s)

#subset for date and baseflow only
baseflow <- subset(stor_dam_met, select=c('date', 'baseflow_m3s'))
baseflow_ws <- subset(stor_dam_met, select=c('date', 'baseflow_m3s'))

#apply proportion of runoff as the proportion of baseflow (assuming baseflow is equal in all watersheds, proportional to area)
baseflow$base_505_m3s = baseflow$baseflow_m3s * pro_505
baseflow$base_510_m3s = baseflow$baseflow_m3s * pro_510
baseflow$base_540_m3s = baseflow$baseflow_m3s * pro_540
baseflow$base_665_m3s = baseflow$baseflow_m3s * pro_665
baseflow$base_760_m3s = baseflow$baseflow_m3s * pro_760
baseflow$base_788_m3s = baseflow$baseflow_m3s * pro_788
baseflow$base_790_m3s = baseflow$baseflow_m3s * pro_790
baseflow$base_800_m3s = baseflow$baseflow_m3s * pro_800
baseflow$base_805_m3s = baseflow$baseflow_m3s * pro_805
baseflow$base_830_m3s = baseflow$baseflow_m3s * pro_830
baseflow$base_835_m3s = baseflow$baseflow_m3s * pro_835
baseflow$base_ung_m3s = baseflow$baseflow_m3s * pro_ung

#create a list of baseflow for dates and streams (vertical data frame)
baseflow_m <- remove.vars(baseflow, names='baseflow_m3s')

baseflow_m <- melt(baseflow_m, id='date')

#change columng title to stream number
baseflow_m$stream_no <- as.character('')
ix=which(baseflow_m$variable == 'base_505_m3s')
baseflow_m$stream_no[ix]=505
ix=which(baseflow_m$variable == 'base_510_m3s')
baseflow_m$stream_no[ix]=510
ix=which(baseflow_m$variable == 'base_540_m3s')
baseflow_m$stream_no[ix]=540
ix=which(baseflow_m$variable == 'base_665_m3s')
baseflow_m$stream_no[ix]=665
ix=which(baseflow_m$variable == 'base_760_m3s')
baseflow_m$stream_no[ix]=760
ix=which(baseflow_m$variable == 'base_788_m3s')
baseflow_m$stream_no[ix]=788
ix=which(baseflow_m$variable == 'base_790_m3s')
baseflow_m$stream_no[ix]=790
ix=which(baseflow_m$variable == 'base_800_m3s')
baseflow_m$stream_no[ix]=800
ix=which(baseflow_m$variable == 'base_805_m3s')
baseflow_m$stream_no[ix]=805
ix=which(baseflow_m$variable == 'base_830_m3s')
baseflow_m$stream_no[ix]=830
ix=which(baseflow_m$variable == 'base_835_m3s')
baseflow_m$stream_no[ix]=835
ix=which(baseflow_m$variable == 'base_ung_m3s')
baseflow_m$stream_no[ix]='ung'

unique(baseflow_m$stream_no)

#### join inflow and baseflow ####
inf_base <- merge(runoff_all, baseflow_m, by=c('date', 'stream_no'), all.y=T)
inf_base <- rename.vars(inf_base, from=c('value'), to=c('baseflow_m3ps'))
inf_base <- remove.vars(inf_base, names='variable')
inf_base$model_imp_50_50_plusbase_m3ps = inf_base$total_runoff_model_imp_50_50_m3s + inf_base$baseflow_m3ps

#plot inflow plus baseflow to see if there are negatives
plot(inf_base$date, inf_base$model_imp_50_50_plusbase_m3ps)
range(inf_base$model_imp_50_50_plusbase_m3ps, na.rm = TRUE)

inf_base_505 <- subset(inf_base, subset=(stream_no==505))
inf_base_510 <- subset(inf_base, subset=(stream_no==510))
inf_base_540 <- subset(inf_base, subset=(stream_no==540))
inf_base_665 <- subset(inf_base, subset=(stream_no==665))
inf_base_760 <- subset(inf_base, subset=(stream_no==760))
inf_base_788 <- subset(inf_base, subset=(stream_no==788))
inf_base_790 <- subset(inf_base, subset=(stream_no==790))
inf_base_800 <- subset(inf_base, subset=(stream_no==800))
inf_base_805 <- subset(inf_base, subset=(stream_no==805))
inf_base_830 <- subset(inf_base, subset=(stream_no==830))
inf_base_835 <- subset(inf_base, subset=(stream_no==835))
inf_base_ung <- subset(inf_base, subset=(stream_no=='ung'))

#subset for data and total inflow for Nicole
setwd("/Volumes/My Passport/GLM/BethelRcode/output")

#subset by stream
inf_base_505_s <- subset(inf_base_505, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_505_s <- rename.vars(inf_base_505_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_505_s, file='505_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_510_s <- subset(inf_base_510, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_510_s <- rename.vars(inf_base_510_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_510_s, file='510_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_540_s <- subset(inf_base_540, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_540_s <- rename.vars(inf_base_540_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_540_s, file='540_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_665_s <- subset(inf_base_665, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_665_s <- rename.vars(inf_base_665_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_665_s, file='665_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_760_s <- subset(inf_base_760, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_760_s <- rename.vars(inf_base_760_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_760_s, file='760_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_788_s <- subset(inf_base_788, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_788_s <- rename.vars(inf_base_788_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_788_s, file='788_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_790_s <- subset(inf_base_790, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_790_s <- rename.vars(inf_base_790_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_790_s, file='790_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_800_s <- subset(inf_base_800, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_800_s <- rename.vars(inf_base_800_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_800_s, file='800_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_805_s <- subset(inf_base_805, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_805_s <- rename.vars(inf_base_805_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_805_s, file='805_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_830_s <- subset(inf_base_830, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_830_s <- rename.vars(inf_base_830_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_830_s, file='830_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_835_s <- subset(inf_base_835, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_835_s <- rename.vars(inf_base_835_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_835_s, file='835_totalinflow_temp_24Jan19.csv', row.names = F)

inf_base_ung_s <- subset(inf_base_ung, select=c('date', 'model_imp_50_50_plusbase_m3ps', 'ModStreamTemp_degC'))
inf_base_ung_s <- rename.vars(inf_base_ung_s, from='model_imp_50_50_plusbase_m3ps', to='modelinflow_m3ps')
write.csv(inf_base_ung_s, file='ung_totalinflow_temp_24Jan19.csv', row.names = F)

#### calculate additional storage via rain on lake ####
#additional storage is area * aggregated rain/day
dam_met$add_stor_rain_m3 <- dam_met$Rain.m_day * dam_met$area_m2

#### calculate evaporation ####
# per email on 12/1/16, we're assuming that evaporation is even throughout the year and is similar to lake Winnepesaukee at 571.5mm/y
#http://winnipesaukeegateway.org/the-watershed/introduction/

evap_mm_yr <- 571.5
evap_mm_day <- evap_mm_yr/365
evap_m_day <- evap_mm_day/1000

dam_met$evap_m3 <- dam_met$area_m2 * evap_m_day

##new WS for balance based on imp scenario
WS2<-inf_base_ung_s%>%left_join(WS)
WS2<-WS2%>%mutate(total_runoff_model_imp_50_50_m3s=inf_base_ung_s$modelinflow_m3ps+inf_base_835_s$modelinflow_m3ps+
                  inf_base_830_s$modelinflow_m3ps+inf_base_805_s$modelinflow_m3ps+inf_base_800_s$modelinflow_m3ps+
                  inf_base_790_s$modelinflow_m3ps+inf_base_788_s$modelinflow_m3ps+inf_base_760_s$modelinflow_m3ps+
                  inf_base_665_s$modelinflow_m3ps+inf_base_540_s$modelinflow_m3ps+inf_base_510_s$modelinflow_m3ps+
                  inf_base_505_s$modelinflow_m3ps)
WS<-subset(WS2,select=c("date","total_runoff_model_imp_50_50_m3s"))

#### merge runoff, met data, lake level to calculate outflow ####
outflow <- merge(dam_met_chgstor, dam_met, all=T)
outflow <- merge(outflow, WS2, by='date', all=T)
outflow <- merge(outflow, baseflow_ws, by='date', all=T)
outflow <- subset(outflow, subset=!is.na(chg_stor_m3))
outflow$outflow_m3d <- outflow$chg_stor_m3 + 
  (outflow$total_runoff_model_imp_50_50_m3s *60 *60 *24) + 
  outflow$add_stor_rain_m3 -
  outflow$evap_m3 +
  (outflow$baseflow_m3s * 60 * 60 * 24)

outflow <- subset(outflow, select=c('date', 'outflow_m3d'))
outflow$outflow_m3s = outflow$outflow_m3d/ (60 * 60 *24)
outflow <- subset(outflow, select=c('date', 'outflow_m3s'))

range(outflow$outflow_m3s)
plot(outflow$date, outflow$outflow_m3s)

#### correct for negative outflows ####
#create a dataframe with negative outflows for the 'balance inflow'
balance_inflow <- subset(outflow, select=c('date', 'outflow_m3s'))
ix=which(balance_inflow$outflow_m3s<0) #select negative values
balance_inflow$bal_inf_m3s = 0 #create a column of 0 values for next step
balance_inflow$bal_inf_m3s[ix] = 0 - (balance_inflow$outflow_m3s [ix]) #create offsetting value
balance_inflow_temp <- merge(balance_inflow, run510_temp_sub, by='date', all=T) #merge with a temp file made from all transducer data to mock an inflow file
balance_inflow_sub <- subset(balance_inflow_temp, select=c('date', 'bal_inf_m3s', 'ModStreamTemp_degC'))
balance_inflow_sub <- subset(balance_inflow_sub, subset=!is.na(bal_inf_m3s))

#write .csv
setwd("/Volumes/My Passport/GLM/BethelRcode/output")
write.csv(balance_inflow_sub, 'balance_inflow_temp_24Jan19.csv', row.names = F)

#### correct the outflow dataset and export ####
#merge balance inflows with outflow for 'corrected outflow' dataset
corr_outflow <- merge(outflow, balance_inflow_sub, by='date', all=T)
corr_outflow$corr_outflow_m3s = corr_outflow$outflow_m3s + corr_outflow$bal_inf_m3s

range(corr_outflow$corr_outflow_m3s)

#check work to see if it seems correct
plot(corr_outflow$date, corr_outflow$corr_outflow_m3s)

#subset for needed fields
corr_outflow_sub <- subset(corr_outflow, select=c('date', 'corr_outflow_m3s'))

#write .csv
setwd("/Volumes/My Passport/GLM/BethelRcode/output")
write.csv(corr_outflow_sub, 'corr_outflow_impmodel_baseflow_24Jan19.csv', row.names = F)


