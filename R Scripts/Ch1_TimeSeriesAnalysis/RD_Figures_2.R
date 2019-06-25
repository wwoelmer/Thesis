# make figures

# first in base plot, try ggplot if time
library(ggplot2)

data <- read.csv("model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)

a <- data[data$Date < "2013-12-01",]
a$Date <- format( as.Date(a$Date, "%Y-%m-%d"), format="%m-%d")
a$Date <- as.Date(a$Date, format = "%m-%d")

b <- data[data$Date < "2014-12-01" & data$Date > "2013-12-01",]
b$Date <- format( as.Date(b$Date, "%Y-%m-%d"), format="%m-%d")
b$Date <- as.Date(b$Date, format = "%m-%d")

c <- data[data$Date < "2015-12-01" & data$Date > "2014-12-01",]
c$Date <- format( as.Date(c$Date, "%Y-%m-%d"), format="%m-%d")
c$Date <- as.Date(c$Date, format = "%m-%d")

d <- data[data$Date < "2016-12-01" & data$Date > "2015-12-01",]
d$Date <- format( as.Date(d$Date, "%Y-%m-%d"), format="%m-%d")
d$Date <- as.Date(d$Date, format = "%m-%d")

dates <- seq.Date(as.Date("2013-01-01"), as.Date("2016-12-31"), by = "week")

plot(data$Date, (data$Chla_sqrt)^2, type = 'n', xlab = "Date", ylab = "Chlorophyll a (ug/L)", xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))

plot(a$Date, (a$Chla_sqrt)^2, lwd = 2, type = 'l', col = 'red1', xlab = "Date", ylab = "Chlorophyll a (ug/L)", ylim = c(0,14))
points(b$Date, (b$Chla_sqrt)^2, lwd = 2, type = 'l', col = 'red4')
points(c$Date, (c$Chla_sqrt)^2, type = 'l', lwd = 2, col = 'turquoise2')
points(d$Date, (d$Chla_sqrt)^2, type = 'l', lwd = 2, col = 'turquoise4')
legend('topleft', c('2013', '2014', '2015', '2016'), lty = c(1,1), col = c('red1', 'red4', 'turquoise2', 'turquoise4'), bty = 'n')


########################################################################################################################
# now create the figure of the oxygenation scenario
o2 <- read.csv("C:/Users/wwoel/Dropbox/Research Day/oxygenation_data.csv")
o2$time <- as.Date(o2$time)
plot(o2$time, o2$OXY_oxy, type = 'l')

########################################################################################################################
# create a year column
data$year <- format( as.Date(data$Date, "%Y-%m-%d"), format="%Y")
data$mo_day <- format( as.Date(data$Date, "%Y-%m-%d"), format="%m-%d")
data$mo_day <- as.Date(data$Date, "%m-%d")
data <- data %>%   
ggplot(data = data, aes(mo_day, (Chla_sqrt)^2)) + geom_line(aes(color = year))

       