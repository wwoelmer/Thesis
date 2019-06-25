############## script to merge various datasets together to create dataframe for FCR time series ##############
library(tidyverse)
library(ggplot2)
setwd("C:/Users/wwoel/Dropbox/FCR_TimeSeries")

#####################################################################################################################
############################ start with CTD and chem data  ########################################################
ctd <- read.csv("CTD/FCR_CTD_50_binned.csv") #ctd data for FCR at site 50, processed for layers
chem <- read.csv("water_chemistry/FCR_chemistry.csv") #chemistry data for FCR at all sites

colnames(chem)[3] <- "Date"
chem$Date <- as.Date(chem$Date)
#calculate TP/TN ratios
chem <- mutate(chem, TN_TP = TN_ugL/TP_ugL)
chem <- mutate(chem, NH4NO3_SRP = (NH4_ugL + NO3NO2_ugL)/SRP_ugL)
#subset chem to include only Site 50
chem50 <- chem[chem$Site==50,]

########### select only inflow data, format as new columns to be added separately to chem as inflow data#################
chem_inflow <- chem[chem$Site==100,]
colnames(chem_inflow) <- c("Reservoir", "Site", "Date", "Depth_m", "TN_inf","TP_inf", "NH4_inf", "NO3NO2_inf", 
                           "SRP_inf", "DOC_inf", "Flag_TN", "Flag_TP", "Flag_NH4", "Flag_NO3NO2", "Flag_SRP",
                           "Flag_DOC", "TN_TP_inf", "NH4NO3_SRP_inf" )
chem_inflow <- chem_inflow %>% select(-Reservoir, -Depth_m, -Site, -(Flag_TN:Flag_DOC))
join_chem <- merge(chem50, chem_inflow, by = c("Date"), all = TRUE)
join_chem <- join_chem %>% select(-(Flag_TN:Flag_DOC), -Reservoir)

# rename 0.8m chemistry data to 1.0m because we want to look at chla at 1.0m
join_chem <- within(join_chem, Depth_m[Depth_m==0.8] <- 1.0)


# now put together chem and CTD data
join1 <- merge(ctd, join_chem, by = c("Date", "Depth_m", "Site"), all = TRUE)

## checkk to see if all chem data arrived
x <- is.na(join1$TP_ugL)
table(x)["FALSE"]
# 1409 not NA values
y <- is.na(chem$TP_ugL)
table(y)["FALSE"]
# 1395 not NA values YAYYYYY
## ctd and chem successfully merged without losing data


#clean up the columns
join1 <- join1 %>% select(-(X.1:Reservoir))
#rename depths column
colnames(join1)[2] <- "Depth"

# look at some data for fun
ggplot(join1, aes(x = TP_ugL, y = Chla_ugL)) +
  geom_point() + ylim(c(0,100))
ggplot(join1[join1$Depth==1,], aes(x = TP_inf, y = Chla_ugL)) +
  geom_point() + ylim(c(0,10))

#####################################################################################################################
############################ now add in meteorological data to join1 (CTD and chem)  ########################################################

met <- read.csv("MET/Met_FCR_daily.csv")#met
#create a site and depth column by which the data frames will be merged (every site and depth will have the same met
### data for a given day)
met$Site <- rep(50, nrow(met))
# create a depth column of the water chem depths for each day in the met file
met_depths <- cbind(met, Depth = rep(c(0.1, 1.0, 1.6, 2.8, 3.8, 5.0, 5.2, 5.5, 5.8, 6.0, 6.2, 8.0, 9.0, 9.3), each = nrow(met)))
# reorder the columns
met_depths <- met_depths %>% select(-X)
met_depths <- met_depths[, c("Date","Site", "Depth", "AirTemp_max", "AirTemp_mean", "AirTemp_median", "Rain_sum",        
                             "RelHum_max", "RelHum_mean", "RelHum_median","ShortWave_max","ShortWave_mean",
                             "WindSpeed_max", "WindSpeed_mean", "WindSpeed_median"  )]
## order the dataframe by data instead of depth
met_depths <- arrange(met_depths, Date)
met_depths$Date <- as.Date(met_depths$Date)

join2 <- merge(join1, met_depths, by = c("Date", "Depth", "Site"), all = TRUE)


#####################################################################################################################
############################ now add in fluoroprobe data to join1 (CTD and chem)  ########################################################

fluoro <- read.csv("Fluoroprobe/Fluoro_FCR50_2014_2017.csv")
#remove 'X' column
fluoro <- fluoro %>% select(-X)
join3 <- merge(join2, fluoro, by = c("Date", "Depth", "Site"), all = TRUE)


####################################################################################################################################
############################ now add in kd (light extinction coefficient) data #####################################################################################

kd <- read.csv("YSI_PAR_SECCHI/FCR_Kd.csv")
join4 <- merge(join3, kd, by = c("Date"), all = TRUE)
join4 <- join4 %>% select(-X)


#########################################################################################################################################
############################ eventually add inflow and calculate residence time ########################################

#####################################################################################################################################

write.csv(join4, "FCR_Master_2013_2017.csv")
