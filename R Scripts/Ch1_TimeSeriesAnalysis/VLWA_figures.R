library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)
library(Metrics)


data <- read.csv("model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)


# read in correlation matrix for selected predictable variable names
varall <- read.csv("./correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_2016_selected_predictable.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
dataall$Date <- as.Date(dataall$Date)
dataall <- dataall[dataall$Date>"2013-05-09",]
# because rmse retains native units, create a chl column that is not square rooted
dataall <- dataall %>% mutate(Chla_ugL = (Chla_sqrt)^2)

# now the model equation!!!!!!
dataall <- dataall %>% mutate(pred = 1.65 + 0.45*Chla_ARlag1_sqrt - 3.05*mean_flow - 0.0025*ShortWave_mean)



# build a global model with all possible predictor variables
model_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  mean_flow + AirTemp_mean_log + WindSpeed_mean_log + 
                    +RelHum_median + ShortWave_mean,
                  data = dataall, family = gaussian, na.action = 'na.fail')
glm_1316 <- dredge(model_1316, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_1316 <- subset(glm_1316, delta<2 )

# now build selected models
mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow +ShortWave_mean, 
                 data = dataall, family = gaussian, na.action = 'na.fail')
# make predictions
pred1_1316 <- predict(mod1_1316, newdata = dataall)

# plot the predictions for the entire dataset
plot(dataall$Date, dataall$Chla_ugL, type = 'l')
points(dataall$Date, (pred1_1316)^2, col = 'red', type = 'l')

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

plot(dataall$Date, (dataall$Chla_sqrt)^2, cex.lab = 1.5, type = 'n', xlab = "Date", ylab = "Chlorophyll a (ug/L)", xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))
points(a$Date, (a$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(b$Date, (b$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(c$Date, (c$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(d$Date, (d$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(dataall$Date, (pred1_1316)^2, col = '#fc8d59', type = 'l', lwd = 2)
legend('topleft', c('Observed', 'ARIMA Modeled'),cex = 1.5,  lty = c(1,1), col = c('black', '#fc8d59'), bty = 'n')

##############################################################################
# bring in glm data to make figure with both model outputs
glm <- read.csv("./GLM/CHLA_GLMoutput_from30Jan19nml_27Feb19.csv")
srf_glm <- glm[glm$Depth==1.0,]
srf_glm$DateTime <- as.Date(srf_glm$DateTime)

plot(srf_glm$DateTime, srf_glm$Observed_PHY_TCHLA, cex.lab = 1.5, type = 'n', ylim = c(0, 14), xlab = "Date", ylab = "Chlorophyll a (ug/L)", xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))
points(a$Date, (a$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(b$Date, (b$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(c$Date, (c$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(d$Date, (d$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(srf_glm$DateTime, srf_glm$Modeled_PHY_TCHLA, col = '#91bfdb', type = 'l', lwd = 2)
legend('topleft', c('Observed', 'GLM Modeled'), cex = 1.5, pch = 16, lty = c(1,1), col = c('black', '#91bfdb'), bty = 'n')


plot(dataall$Date, (dataall$Chla_sqrt)^2, cex.lab = 1.5, type = 'n', xlab = "Date", ylab = "Chlorophyll a (ug/L)", xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))
points(a$Date, (a$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(b$Date, (b$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(c$Date, (c$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(d$Date, (d$Chla_sqrt)^2, lwd = 2, type = 'p', pch = 16)
points(dataall$Date, (pred1_1316)^2, col = '#fc8d59', type = 'l', lwd = 2)
points(srf_glm$DateTime, srf_glm$Modeled_PHY_TCHLA, col = '#91bfdb', type = 'l', lwd = 2)
legend('topleft', cex = 1.5, c('Observed', 'ARIMA modeled', 'GLM Modeled'), lty = c(1,1), col = c('black', '#fc8d59','#91bfdb'), bty = 'n')

##############################################################################################################3
# take the mean of arima and glm to get a new model value
arima <- dataall %>% select(Date, Chla_ugL, pred)
colnames(arima)[1] <- "DateTime"
avg <- left_join(arima, srf_glm)
avg <- avg %>% mutate(mod_avg = (((pred^2) + Modeled_PHY_TCHLA)/2))
                        
                        
                        
plot(avg$DateTime, avg$Chla_ugL)
points(avg$DateTime, (avg$pred)^2, col = 'red', type = 'l')
points(avg$DateTime, avg$Modeled_PHY_TCHLA, col = 'purple', type = 'l')
points(avg$DateTime, avg$mod_avg, col = 'blue', type = 'l')
