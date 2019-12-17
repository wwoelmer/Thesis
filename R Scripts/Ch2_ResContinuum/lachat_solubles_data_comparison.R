# looking for outliers and samples to re-run in soluble nutrient chemistry dataset

library(tidyverse)

nuts <- read.csv('./Data/analytical chemistry/Lachat 2019/chemistry_working_half.csv')
nuts$Date <- as.Date(nuts$Date)

plot(nuts$Date, nuts$NH4_ppb)
# rerun two samples which appear to be outliers near 1400 ppb?
# other samples which are high have already been run twice and show consistency between run days

plot(nuts$Date, nuts$PO4_ppb)
#re run: F06May19_F200, PO4 concentration of 56.9 ppb
#        F22Jul19_F50_8.0m is a spike, so no rerun
#        F18Jul19_F99 is high but shows reproducability of high value on multiple days and within reps

plot(nuts$Date, nuts$NO3NO2_ppb)
# the 15Apr19_F200 sample shows reproducability between run days, so no re-run
# same with 18Jul19_F99


reps <- nuts[nuts$Rep=='R1'|nuts$Rep=='R2',]
# looked through R1s and R2 and they are all very close, except for a few which I've pulled to rerun