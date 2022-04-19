setwd("~/Documents/DS")
rain = read.csv("rain.csv")
rainTs = ts(rain, start = c(1818))
plot(rainTs)

#
rainForecast = HoltWinters(rainTs, beta = F, gamma = F)
rainForecast

#
plot(rainForecast)

#
names(rainForecast)

#
rainForecast$fitted

#
r2 = HoltWinters(rainTs, alpha = 0.8, gamma = F)
plot(r2)

#
rf = forecast::forecast(rainForecast, h = 3)
plot(rf)


#
rf

