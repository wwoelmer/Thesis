# time series analysis to build a model to predict inflow???

data <- read.csv("model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)
plot(data$Date, data$mean_flow)
plot(exp(data$Rain_sum_log), data$mean_flow)
plot(data$Date, data$Rain_sum_log, type = 'l')

met <- read.csv("./MET/Met_FCR_daily.csv")
met$Date <- as.Date(met$Date)
plot(met$Date, met$Rain_sum, type = 'l')

level <- read.csv("C:/Users/wwoel/Dropbox/Inflows/BVR_waterlevel.csv")
level$Date <- as.Date(level$Date)
plot(level$Date, level$waterlevelbelow_ft, type = 'l')
abline(h=-5, col = 'blue')