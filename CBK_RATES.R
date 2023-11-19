x=CBK_RATESRev$Volatility
x
q=Rates$Returns
q
hist(x,freq = F, 18, title = F)
hist(x, freq = F,18, xlab= "Returns", ylab = "Density", col = "blue", breaks = 50, main = "Histogram of CBK interest Rates")
title(main = "Histogram of CBK interest Rates")

summary(x)
str(x)
plot(ts(x))
basicStats(x)
library(ghyp)
normal.fit<-fit.gaussuv(data = x, add = T)
summary(normal.fit)
normal.fit
plot(normal.fit)
nig.fit<-fit.NIGuv(data = x, add = T)
summary(nig.fit)
plot(nig.fit, type = "l")
t.fit<-fit.tuv(data = x)
summary(t.fit)
plot(t.fit)
hyp.fit<-fit.ghypuv(data = x)
summary(hyp.fit)
plot(hyp.fit)
library(VGAM)
rig.plot<- rigff(x)
library(moments)
kurtosis(x)
mean(x)
skewness(x)
var.gr<- fit.VGuv(data = x)
summary(var.gr)
sd(x)
ks.test(x,"pnorm",mean(x),sd=sd(x))
shapiro.test(x)
library(tseries)
jarque.bera.test(x)
adf.test(x)
CADFtest(x)
adf.test(x, k = 2)
adf.test(x, k = 1)
pp.test(x)
t=CBK.RATESRev$X91.Day.Tbill
t
adf.test(t)
adf.test(t, k=1)
install.packages("pracma")
library(pracma)
hurstexp(x)
t=CBK.RATES$X91.Day
t
ts.plot(t, col = "blue", xlab = "Time/Days", ylab = "Interest Rates")
ts.plot(x, col = "blue", xlab = "Time/Days", ylab = "Returns")
plot(ts(x), col= "blue", main= "Monthly interest Rates volatility", xlab="Time", ylab="Returns")
class(x)
is.ts(x)
library(zoo)
Tbill<-ts(x, start = c(1991,1), end = c(2021,9), frequency = 12)
Tbill
plot(Tbill, xlab="Time in Months", ylab="TBills Returns", col="blue")
CommBnk<-ts(q, start = c(1991,7), end = c(2022,7), frequency = 12)
CommBnk
plot(CommBnk, xlab="Time in Months", ylab="Commercial Bank Returns", col="green")
cbkrat<-ts(CBK.RATES$X91.Day, start = c(1991,1), end = c(2021,9), frequency = 12)
plot(cbkrat, xlab="Time in Months", ylab="Treasury Bill Rates", col="blue")
bankcom<-ts(Bank$Lending, start = c(1991,1), end = c(2022,7), frequency = 12)
plot(bankcom, xlab="Time in Months", ylab="Commercial Bank Rates", col="green")
Box.test(coredata(Tbill^2), type = "Ljung-Box", lag = 12)
Box.test(coredata(CommBnk^2), type = "Ljung-Box", lag = 12)
library(FinTS)
ArchTest(coredata(Tbill))
ArchTest(coredata(CommBnk))

##Q-Q test (Normal)
qqnorm(Tbill, col = "blue")
qqline(Tbill)
qqnorm(CommBnk, col = "green")
qqline(CommBnk)

## Autocorrelation of Returns
acf(abs(Tbill), main = "Auto-Correlation of Absolute Returns")

## MAXIMUM LIKELIHOOD ESTIMATION OF PARAMETERS
##Parameter Estimation of Normal Distribution
normal.fit<-fit.gaussuv(data = Tbill)
summary(normal.fit)
h=hist(x, freq = F, 18, xlab= "T. Bills Monthly Volatility", ylab = "Density", col = "blue", breaks = 15, main = "Histogram and Gaussian Curve")
xfit<-seq(min(x), max(x), length =40)
yfit<-dnorm(xfit, mean = mean(x), sd = sd(x))
lines(xfit, yfit, col="red", lwd=2, df = 10, add = T)
lambda=0.1680857588
alpha=0.0435677071
mu=-0.0001157169
delta=0.0637759052
beta=-0.0009652545
params<-c(mu, delta, alpha, beta, lambda)
params
y2fit<-dghyp(x, mu = mu, delta = delta, alpha = alpha, beta = beta, lambda = lambda, param = c(mu, delta, alpha, beta, lambda))
lines(xfit, y2fit, col = "yellow", lwd=2, df=10, add = T)






normal.fib<-fit.gaussuv(data = CommBnk)
summary(normal.fib)
h=hist(q, freq = F, 18, xlab= "Comm. Bank Volatility", ylab = "Density", col = "green", breaks = 15, main = "Histogram and Gaussian Curve")
xfit<-seq(min(q), max(q), length = 40)
yfit<-dnorm(xfit, mean = mean(q), sd = sd(q))
lines(xfit, yfit, col="red", lwd=2, df = 10, add = T)


## Parameter estimate for NIG distribution
nig.fiT<-fit.NIGuv(data = Tbill)
summary(nig.fiT)
library(RUnit)
library(GeneralizedHyperbolic)
library(DistributionUtils)
h=hist(q, freq = F, 18, xlab= "Comm. Bank Returns", ylab = "Density", col = "blue", breaks = 15, main = "Histogram and Gaussian Curve")
xfit<-seq(min(q), max(q), length = 40)
yfit<-dnig(q, mu=0, sigma = 1, alpha = 1, beta = 0,param = c(mu, sigma, alpha,beta))

nig.fibk<-fit.NIGuv(data = CommBnk)
summary(nig.fibk)
h=hist(q, freq = F, 18, xlab= "Comm. Bank Returns", ylab = "Density", col = "grey", breaks = 15, main = "Histogram and Gaussian Curve")
curve(dnorm(x,mean(x),sd(x)),col="green",add=T)
xfit<-seq(min(q), max(q), length = 40)
yfit<-dnig(q, mu=0, sigma = 1, alpha = 1, beta = 0,param = c(mu, sigma, alpha,beta))

plot(nig.fibk, type = "l", lwd = 2, xlab = "Comm. Bank Monthly Volatility", ylab = "Density", main = "NIG Fitting", col = "blue", add=T)


## Parameter Estimation for Student-t
t.fit<-fit.tuv(data = Tbill)
summary(t.fit)
t.fitb<-fit.tuv(data = CommBnk)
summary(t.fitb)


## Parameter Estimation for hyperbolic Distribution
hyp.fit<-fit.ghypuv(data = Tbill)
summary(hyp.fit)
plot(hyp.fit, type = "l", bg = "grey", lwd = 2, xlab = "T. Bills Monthly Volatility", ylab = "Density", main = "Generalized Hyperbolic Fit", col = "green")
hyp.fitb<-fit.ghypuv(data = CommBnk)
summary(hyp.fitb)

## Parameters Estimation for Variance Gamma distribution
var.gr<- fit.VGuv(data = Tbill)
summary(var.gr)
var.grb<- fit.VGuv(data = CommBnk)
summary(var.grb)

## Curve Fitting

plot(hyp.fit, type = "l", bg = "red", lwd = 2, xlab = "Monthly Returns", ylab = "Density", main = "G-hyp Distribution Fitting", sub = "Par(lambda, alpha, mu, sigma, gamma)", col = "blue")
plot(nig.fibk, type = "l", bg = "grey", lwd = 2, xlab = "Monthly Returns", ylab = "Density", main = "NIG Distribution Fitting", sub = "Par(alpha, mu, sigma, gamma)", col = "green")

## Forecasting using GARCH models.
## Specifying the GARCH model
library(rugarch)
Tbill_garch11_spec<-ugarchspec(variance.model = list(garchOrder = c(2,0)), mean.model = list(armaOrder = c(1,2)), distribution.model = "ghyp")
Tbill_garch11_spec

## We conduct the Estimation of the model parameters using MLE
Tbill_garch11_fit<-ugarchfit(spec = Tbill_garch11_spec, data = Tbill)
Tbill_garch11_fit

## Backtesting- Checking the performance of the models using the past data
Tbill_garch11_roll<-ugarchroll(Tbill_garch11_spec, data = Tbill, n.start = 50, refit.every = 1, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)
Tbill_garch11_roll
report(Tbill_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)

## Plotting Backtested Value at Risk
Tbill_garch11_roll@forecast$VaR[,1]
Tbill_VaR<-zoo(Tbill_garch11_roll@forecast$VaR[,1])
index(Tbill_VaR)<-as.yearmon(rownames(Tbill_garch11_roll@forecast$VaR))
Tbill_actual<-zoo(Tbill_garch11_roll@forecast$VaR[,2])
index(Tbill_actual)<-as.yearmon(rownames(Tbill_garch11_roll@forecast$VaR))
plot(Tbill_actual, type = "b", main = "1-Month VaR Backtesting at CI 99%", xlab = "Date", ylab = "VaR Percentage")
lines(Tbill_VaR, col = "red")

## Forecasting Commercial Bank Volatility
Tbill_garch_forecast<-ugarchforecast(Tbill_garch11_fit, n.ahead = 12)
Tbill_garch_forecast
plot(Tbill_garch_forecast, which = "all")


##Garch Model Specification for Commercial Bank data
CommB_garch_spec<-ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)), distribution.model = "nig")
CommB_garch_spec

## We conduct the Estimation of the model parameters using MLE
CommB_garch_fit<-ugarchfit(spec = CommB_garch_spec, data = CommBnk)
CommB_garch_fit


## Forecasting Commercial Bank Volatility
CommBnK_garch_forecast<-ugarchforecast(CommB_garch_fit, n.ahead = 12)
CommBnK_garch_forecast
plot(CommBnK_garch_forecast)
plot.zoo(CommBnK_garch_forecast)


