# script to interpolate days where chla, our response variable, does not have data in order to reach a weekly timestep

# install.packages("zoo")
library(zoo)
library(tidyverse)

#master dataset of data collected by carey lab (i.e., no met data)
data <- read.csv("FCR_VT_data_2013_2017.csv")
data$Date <- as.Date(data$Date)
data <- data %>% select(Date, everything())
#truncate the dataset to the end of the 2016 stratified period
#data <- data[data$Date < "2016-10-31" & data$Date >"2013-05-01",]

#	Create a new dataframe that consists of all the days that need to be interpolated that contains the same column headings
#  THESE ARE THE DATES I WANT TO INTERPOLATE (18 dates)
newdates <- as.Date(c( "2013-08-18", "2013-08-23", "2013-09-06", "2013-09-13", "2013-10-04", 
            "2013-10-11", "2014-08-30", "2014-10-10", "2014-10-30", "2015-03-01", "2015-08-15", "2015-08-22", "2015-10-12", 
            "2015-10-30", "2016-09-22", "2016-10-20", "2016-10-27", "2016-10-31"))
add <- data.frame(matrix(nrow = length(newdates) , ncol = ncol(data)))
colnames(add) <- colnames(data)
add[1] <- newdates
add$Site <- rep(50)

# add in every depth for each date
depths <- rep(c(0.1, 0.8, 1.6, 2.8, 3.8, 5.0, 5.2, 5.5, 5.8, 6.0, 6.2, 8.0, 9.0, 9.3), each = nrow(add))
add <- cbind(depths, add)
add$Depth <- depths
add <- add%>% select(-depths)

interp <- rbind(add, data)
#arrange by date so the NA's are not at the top of the dataframe
interp <- interp[order(interp$Date),]

# manually select 0.1 to check if interpolation works for chl
interp_0.1 <- interp[interp$Depth==0.1,]
interp_0.1$Chla_interp <- na.approx(interp_0.1$Chla_ugL,  na.rm=FALSE, rule = 2, maxgap = 15)
interp_0.1 <- mutate(interp_0.1, diff = Chla_interp - Chla_ugL)

# check temp at 3.8
interp_3.8 <- interp[interp$Depth==3.8,]
interp_3.8$Temp_C_interp <- na.approx(interp_3.8$Temp_C,  na.rm=FALSE, rule = 2, maxgap = 15)


## not all columns can be interpolated so select those that can be
neww <- interp %>% 
  group_by(Depth) %>%
  mutate(Temp_interp = na.approx(Temp_C, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(DO_mgL_interp = na.approx(DO_mgL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(Chla_interp = na.approx(Chla_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(Turb_NTU_interp = na.approx(Turb_NTU, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(SpCond_uScm_interp = na.approx(SpCond_calc, na.rm = FALSE, rule = 2, maxgap = 15))%>%
  select(-(Temp_C:SpCond_calc)) #get rid of original columns without new interpolated values


# subset out each year for the May-Oct stratified period and then put them together to have a dataset that includes only
# the dates I want to model
data13 <- neww[neww$Date < "2013-11-01" & neww$Date >"2013-05-01",]
data14 <- neww[neww$Date < "2014-11-01" & neww$Date >"2014-05-01",]
data15 <- neww[neww$Date < "2015-11-01" & neww$Date >"2015-05-01",]
data16 <- neww[neww$Date < "2016-11-01" & neww$Date >"2016-05-01",]
data_all <- rbind(data13, data14, data15, data16)
# data_all is our dataset that includes all of the CTD variables interpolated within the MAY-OCT 2013-2016 timeframe

data_all <- data_all %>%
  select(Temp_interp:SpCond_uScm_interp, everything())
data_all <- data_all %>%
  select(Date:Depth, everything())


write.csv(data_all, "CTD_interpolated_MayOct13_16.csv", row.names = FALSE)


