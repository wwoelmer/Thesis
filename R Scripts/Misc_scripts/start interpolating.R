### interpolating FCR LT dataset

#use the site 50 ctd chla data because this is the response variable
## ie, I want to know the timestep of my response variable first
data <- read.csv("C:/Users/wwoel/Dropbox/FCR_TimeSeries/CTD/FCR_CTD_50_binned.csv")



# first look at where the gaps are in each year
### #2013 dates

year1 <- seq(as.Date("2013-01-01"), as.Date("2013-12-31"), by = "day")

#make a presence absence matrix on a daily scale (1/0)
year_d <- unique(data$Date)



