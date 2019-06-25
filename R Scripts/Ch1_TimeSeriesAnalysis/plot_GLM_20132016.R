# plot GLM modeled vs obs

glm <- read.csv("./GLM/CHLA_GLMoutput_from30Jan19nml_27Feb19.csv")
srf_glm <- glm[glm$Depth==1.0,]
srf_glm$DateTime <- as.Date(srf_glm$DateTime)
plot(srf_glm$DateTime, srf_glm$Observed_PHY_TCHLA, type = 'l', lwd = 2)
points(srf_glm$DateTime, srf_glm$Modeled_PHY_TCHLA, lwd = 2, col = 'darkcyan', type = 'l')
legend('topleft', c('Observed', 'GLM Modeled'), lty = c(1,1), col = c('black', 'darkcyan'), bty = 'n')


a <- srf_glm[srf_glm$DateTime < "2013-10-31" & srf_glm$DateTime > "2013-05-01",]
a$Date <- as.Date(a$Date)

b <- srf_glm[srf_glm$Date < "2014-10-31" & srf_glm$Date > "2014-05-01",]
b$Date <- as.Date(b$Date)

c <- srf_glm[srf_glm$Date < "2015-10-31" & srf_glm$Date > "2015-05-01",]
c$Date <- as.Date(c$Date)

d <- srf_glm[srf_glm$Date < "2016-10-31" & srf_glm$Date > "2016-05-01",]
d$Date <- as.Date(d$Date)

plot(srf_glm$DateTime, srf_glm$Observed_PHY_TCHLA, type = 'n', ylim = c(0, 14), xlab = "Date", ylab = "Chlorophyll a (ug/L)", xlim = c(as.Date("2013-01-01"), as.Date("2016-12-31")))
points(a$Date, a$Observed_PHY_TCHLA, lwd = 2, type = 'p')
points(b$Date, b$Observed_PHY_TCHLA, lwd = 2, type = 'p')
points(c$Date, c$Observed_PHY_TCHLA, lwd = 2, type = 'p')
points(d$Date, d$Observed_PHY_TCHLA, lwd = 2, type = 'p')
points(srf_glm$DateTime, srf_glm$Modeled_PHY_TCHLA, col = '#91bfdb', type = 'l', lwd = 2)
legend('topleft', c('Observed', 'GLM Modeled'), lty = c(1,1), col = c('black', '#91bfdb'), bty = 'n')

rmse(srf_glm$Observed_PHY_TCHLA, srf_glm$Modeled_PHY_TCHLA)
r2 <- function (x, y) cor(x, y) ^ 2
r2(srf_glm$Observed_PHY_TCHLA, srf_glm$Modeled_PHY_TCHLA)
