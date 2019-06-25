# make figures

# first in base plot, try ggplot if time

data <- read.csv("model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)

a <- data[data$Date < "2013-12-01",]
b <- data[data$Date < "2014-12-01" & data$Date > "2013-12-01",]
c <- data[data$Date < "2015-12-01" & data$Date > "2014-12-01",]
d <- data[data$Date < "2016-12-01" & data$Date > "2015-12-01",]

dates <- seq.Date(as.Date("2013-01-01"), as.Date("2016-12-31"), by = "week")

plot(data$Date, (data$Chla_sqrt)^2, type = 'n', xlab = "Date", ylab = "Chlorophyll a (ug/L)", xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))
points(a$Date, (a$Chla_sqrt)^2, type = 'l')
points(b$Date, (b$Chla_sqrt)^2, type = 'l')
points(c$Date, (c$Chla_sqrt)^2, type = 'l')
points(d$Date, (d$Chla_sqrt)^2, type = 'l')


