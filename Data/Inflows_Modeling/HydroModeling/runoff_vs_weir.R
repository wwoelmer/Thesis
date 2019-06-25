# script to calculate baseflow and examine the bvr inflow to fcr

library(tidyverse)

# runoff data calculated from overland flow model
runoff <- read.csv("C:/Users/wwoel/Dropbox/Inflows/HydroModeling/overland_runoff_FCR.csv")
ggplot(runoff, aes(date, runoff_m3s)) + geom_point() +facet_wrap(~Site)

# write csv of wetland runoff only
runoff_wet <- runoff[runoff$Site=="wetland",]
#write.csv(runoff_wet, "C:/Users/wwoel/Dropbox/Inflows/FCR_inflows/FCRwetland_runoff.csv", row.names = FALSE)

inf <-  read.csv("./Inflow/inflowcalcs_FCR.csv")
inf$Date <- as.Date(inf$Date)
plot(inf$Date, inf$mean_wrt)

met <- read.csv('./MET/FCR_GLM_met_NLDAS2_010113_010118_GMTadjusted.csv', header=T, na.strings=NA) #in begwrkspc
met$time <- as.POSIXct(met$time, format='%Y-%m-%d %H:%M:%S') #format date 

# some data management to get everything in one place
runoff_ws <- runoff[runoff$Site=="whole",]
colnames(runoff_ws)[1] <- "Date"
runoff_ws$Date <- as.Date(runoff_ws$Date)
wrt <- inf %>% select(Date, mean_wrt, mean_flow)

hydro <- left_join(runoff_ws, wrt, by = "Date")


# calculate baseflow based on FCR's volume, residence time (in seconds), and calculated runoff inflow
hydro <- hydro %>% mutate(baseflow = (3.1E5/(mean_wrt*86400)) - runoff_m3s)%>%
  mutate(diff = round(baseflow - mean_flow, digits = 6))

plot(hydro$Date, hydro$baseflow, type = 'l', col = 'red')
points(hydro$Date, hydro$mean_flow, type = 'l', col = 'black')
legend('topleft', c("baseflow", 'obs meanflow'), lty = c(1,1), bty = 'n', col = c('red','black'))
plot(hydro$Date, hydro$diff, type = 'l')

mean_bf <- mean(hydro$baseflow, na.rm=TRUE)

plot(hydro$Date, hydro$runoff_m3s, type = 'l')
points(hydro$Date, hydro$mean_flow, type = 'l', col = 'red')

# now add mean baseflow to runoff to see how it compares to the inflow measurements
hydro <- hydro %>% mutate(meanbf_plusrunoff = runoff_m3s + mean_bf) %>%
  mutate(bf_plusrunoff = runoff_m3s + baseflow)

plot(hydro$Date, hydro$runoff_m3s, type = 'l', ylim = c(0,0.3))
points(hydro$Date, hydro$mean_flow, type = 'l', col = 'red')
points(hydro$Date, hydro$meanbf_plusrunoff, col = 'purple', type = 'l')
points(hydro$Date, hydro$bf_plusrunoff, col = 'blue', type = 'l')
legend('topleft',lty = c(1,1), bty = 'n', c('runoff', 'mean weir flow', 'mean bf + runoff', 'bf+runoff'), col = c('black','red', 'purple','blue'))

# but this baseflow is totally circular
# if we break down the equations for both calculated RT and baseflow and cancel out things:
# baseflow = weir flow - runoff
# so this diff term is: (weir flow - runoff) - mean flow @ weir

# these are the areas of each watershed or subwatershed
areaFCR <- 354.8
areaInf <- 77.7
areaWet <- 163.2
propInf <- areaInf/areaFCR
propWet <- areaWet/areaFCR

# the baseflow value we have calculated is the amount of baseflow for the entire watershed
# use the proportion of the watershed covered by weir'ed inflow to determine what baseflow should be for
# that proportion of the watershed
hydro <- hydro %>% mutate(baseflow_inf = propInf*baseflow)
hist(hydro$baseflow_inf)


# OR does it make more sense to subtract the wetland proportion from the calculated baseflow and assume the rest
# of the incoming water that is NOT from the wetland is coming from the weir inflow???


runoff_wide <- spread(runoff, Site, runoff_m3s)
runoff_wide$date <- as.Date(runoff_wide$date)
colnames(runoff_wide)[1] <- "Date"
hydro_wide <- left_join(runoff_wide, wrt, by = "Date")
hydro_wide <- hydro_wide %>% mutate(infprop_flow = inf_overland + (propInf*mean_flow) - (propInf*whole) ) %>%
  mutate(test = infprop_flow - propInf*mean_flow) 

# plot the two proportions of flow at the weir
plot(hydro_wide$Date, hydro_wide$mean_flow, col = 'black', type = 'l')
points(hydro_wide$Date, hydro_wide$infprop_flow, col = 'blue', type = 'l')
# the inflow proportion is just a propoirtion of the whole so they follow the same pattern as expected

# now calculate baseflow as done above and make the same proportional calculation as a test
hydro_wide <- hydro_wide %>% mutate(baseflow = (3.1E5/(mean_wrt*86400)) - whole) %>%
  mutate(infprop_baseflow = inf_overland + propInf*baseflow)

plot(hydro_wide$Date, hydro_wide$mean_flow, col = 'black', type = 'l')
plot(hydro_wide$Date, hydro_wide$infprop_flow, col = 'blue', type = 'l')
points(hydro_wide$Date, hydro_wide$infprop_baseflow, col = 'purple', type = 'l')