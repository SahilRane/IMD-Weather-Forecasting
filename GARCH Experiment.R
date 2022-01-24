training <- data[1:2990]
testing <- data[2991:4264]

tstrain <- ts(training, frequency = 365, start = c(2008,183))
tstraind <- diff(tstrain, differences = 1)
tstest <- ts(testing, frequency = 365)
tstestd <- diff(tstrain, differences = 1)
acf(tstrain, lag.max = 20)
pacf(tstrain, lag.max = 20)
fit<-arfima(ts,c(4,0,0))
accuracy(fit)
fit
refit<-Arima(tstest, model = fit)
accuracy(refit)


fit<-auto.arima(tstrain)
fit<-Arima(ts,c(4,0,0))
accuracy(fit)
fit
refit<-Arima(tstest, model = fit)
accuracy(refit)
tserror <- ts(residuals(fit),frequency = 365)
gwn <- rnorm(2990,0, 0.8)
gwn
residual<-sqrt(sum((tserror-gwn)^2,na.rm = T)/2990)
residual
tserrortest <- ts(residuals(refit),frequency = 365)
gwntest <- rnorm(1274,0,1)
residual<-sum(abs(tserrortest-gwntest),na.rm = T)/1274
residual
head(tserror)
size(tserror)
plot(tserror)
plot(density(tserror), type="l", col="red" )
par(new=TRUE)
plot(density(rnorm(x,0,0.8)), type="l", col="green" )

plot(density(tserror), type="l", col="red" )
lines(density(rnorm(2990,0,0.8)), type="l", col="green" )
plot(rnorm(0,1,1))







a<-garchFit(formula = ~ garch(30,30), data = tserror)
sqrt(sum(residuals(a)^2)/2990)


library(rugarch)

garch11        <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,3)), 
                             mean.model = list(armaOrder = c(4, 4), include.mean = TRUE), 
                             distribution.model = "norm")

garchfit       <- ugarchfit(spec = garch11, data = ts, solver = "hybrid")
garchfit
sum(residuals(garchfit)^2)/4264