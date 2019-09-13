# script to create the 2013-2016 times series for model training for the AR chl model for FCR

library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)
library(Metrics)


data <- read.csv("./Data/ARIMA_data/data_arima_WW.csv")
data$Date <- as.Date(data$Date)
# get rid of NAs at beginning
data <- data[data$Date>"2013-05-09",]
# because rmse retains native units, create a chl column that is not square rooted
data <- data %>% mutate(Chla_ugL = (Chla_sqrt)^2)

# now the model equation!!!!!!
data <- data %>% mutate(pred = 1.65 + 0.45*Chla_ARlag1_sqrt - 3.05*mean_flow - 0.0025*ShortWave_mean)

mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow +ShortWave_mean, 
                 data = data, family = gaussian, na.action = 'na.fail')
# make predictions
pred1_1316 <- predict(mod1_1316, newdata = data)

# plot the predictions for the entire dataset
plot(data$Date, data$Chla_ugL, type = 'l')
points(data$Date, (pred1_1316)^2, col = 'red', type = 'l')

# subset each of the years so the plot is still 2013-2016 but each line stops in october
# and doesn't connect to the may of the following year??

a <- data[data$Date < "2013-12-01",]
a$Date <- as.Date(a$Date)

b <- data[data$Date < "2014-12-01" & data$Date > "2013-12-01",]
b$Date <- as.Date(b$Date)

c <- data[data$Date < "2015-12-01" & data$Date > "2014-12-01",]
c$Date <- as.Date(c$Date)

d <- data[data$Date < "2016-12-01" & data$Date > "2015-12-01",]
d$Date <- as.Date(d$Date)

png("./Figures/arima/ESA_20132016TrainingFigure.png", width = 1100, height = 600)
par(mar = c(7,6,4,1)+0.1)
plot(data$Date, (data$Chla_sqrt)^2, cex.lab = 1.25, type = 'n', xlab = "Date", ylab = "Chlorophyll a (μg/L)",cex.lab = 2.5, cex.axis = 2.5, xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))
points(a$Date, (a$Chla_sqrt)^2,  type = 'p', pch = 16, cex = 1.5)
points(b$Date, (b$Chla_sqrt)^2,  type = 'p', pch = 16, cex = 1.5)
points(c$Date, (c$Chla_sqrt)^2, lwd = 4, type = 'p', pch = 16, cex = 1.5)
points(d$Date, (d$Chla_sqrt)^2, lwd = 4, type = 'p', pch = 16, cex = 1.5)
points(data$Date, (pred1_1316)^2, col = '#fc8d59', type = 'l', lwd = 4)
legend('topleft', c('Observed', 'AR Model'),cex = 2.5,  lty = c(0,1), col = c('black', '#fc8d59'), bty = 'n', pch = c(16,16))
dev.off()

