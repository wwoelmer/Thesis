# script to calculate specific conductivity for BVR inflows
library(tidyverse)

bvr <- read.csv("C:/Users/wwoel/Dropbox/BVR_inflows/BVR_inflows.csv")

bvr <- mutate(bvr, SpCond_calc = Cond_uS/(1+(0.0191*(Temp_C - 25))))
write.csv(bvr, "C:/Users/wwoel/Dropbox/BVR_inflows/BVR_inflows.csv", row.names = FALSE)
