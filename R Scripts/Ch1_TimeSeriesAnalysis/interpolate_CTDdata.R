# script to interpolate days where chla, our response variable, does not have data in order to reach a weekly timestep

# install.packages("zoo")
library(zoo)
library(tidyverse)

#master dataset
data <- read.csv("./CTD/FCR_CTD_50_binned.csv")
#clean up and rearrange columns
data$Date <- as.Date(data$Date)
data <- data %>% select(-Reservoir, -X, -pH, -ORP_mV) %>%
        select(Date, everything())
colnames(data)[3] <- "Depth"
#get rid of NA's at the end of the dataset
data <- data[data$Date < "2017-05-14",]


#	Create a new dataframe that consists of all the days that need to be interpolated that contains the same column headings
#  THESE ARE THE DATES I WANT TO INTERPOLATE
newdates <- as.Date(c( "2013-08-18", "2013-08-23", "2013-09-06", "2013-09-13", "2013-10-04", 
            "2013-10-11", "2014-08-30", "2014-10-10", "2015-03-01", "2015-08-15", "2015-08-22", "2015-10-12", "2015-10-30", 
            "2016-09-22", "2016-10-20", "2016-10-27"))
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

# manually select 0.1 to check if interpolation works
interp_0.1 <- interp[interp$Depth==0.1,]
interp_0.1$Chla_interp <- na.approx(interp_0.1$Chla_ugL,  na.rm=FALSE, rule = 2, maxgap = 15)
interp_0.1 <- mutate(interp_0.1, diff = Chla_interp - Chla_ugL)

# check depth == 3.8
interp_3.8 <- interp[interp$Depth==3.8,]
interp_3.8$Chla_interp <- na.approx(interp_3.8$Chla_ugL,  na.rm=FALSE, rule = 2, maxgap = 15)
interp_3.8 <- mutate(interp_3.8, diff = Chla_interp - Chla_ugL)
# ok, this is just the deep chl maximum

# check temp at 3.8
interp_3.8 <- interp[interp$Depth==3.8,]
interp_3.8$Temp_C_interp <- na.approx(interp_3.8$Temp_C,  na.rm=FALSE, rule = 2, maxgap = 15)

# automate it to go through each depth and subset so that data is interpolated only within a given depth
# interpolate all columns
neww <- interp %>% 
  group_by(Depth) %>%
  mutate(Temp_interp = na.approx(Temp_C, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(DO_mgL_interp = na.approx(DO_mgL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(Cond_uScm_interp = na.approx(Cond_uScm, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(Chla_interp = na.approx(Chla_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(Turb_NTU_interp = na.approx(Turb_NTU, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(SpCond_calc_interp = na.approx(SpCond_calc, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  select(Date:Depth, Temp_interp:SpCond_calc_interp) #get rid of original columns without new interpolated values

x <- colnames(interp)
colnames(neww) <- x

write.csv(neww, "CTD_interpolated.csv", row.names = FALSE)


