rm(list=ls(all=TRUE))

library(forecast)
library(lmtest)
library(astsa)
library(tseries)
library(TSA)
library(openxlsx)

OI_KLL <- read.csv("monthly_data_KL_3lagged.csv", sep = ",", header = TRUE) 

OI_KLLag <-OI_KLL$ovitrap_idx  


# Detecting seaosonality and period of seasonality 

# computing fourier tranfform 

p <- periodogram(OI_KLLag)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)
top2

# display the 2 highest "power" frequencies
top2

time = 1/top2$f
time

#square root transformation

OI_KLLag <-sqrt(OI_KLLag)

# changing the data to time series(ts)format 

tsOI_KLL <-ts(OI_KLLag,frequency = 12)

# ploting the ts data

plot(tsOI_KLL)

# Decomposition 


components.ts = decompose(tsOI_KLL)
plot(components.ts)

# determining seasonal differencing required

ns<-nsdiffs(tsOI_KLL)

# detremining regular differencing required

nd<-ndiffs(tsOI_KLL,test = c("kpss"))

# first order differencing 

diff_12 <- diff(tsOI_KLL,12)
plot(diff_12, main= "First Order Differenced Ovitrap Index Series", ylab=" Differenced Ovitrap Index",xlab="Time in Years")

# ACF PACF plots to identify possible (p,d,q) and (P,D,Q) values ---- already identified : (1,0,1)(1,1,1)

acf2(diff_12, 48, main= "First Order Differenced Ovitrap Index Series")


# model diagnostics

modelfit <- Arima(tsOI_KLL, order=c(1,0,1), seasonal=c(1,1,1))   
checkresiduals(modelfit)


# Inclusion of climate variables (xreg)

xreg<- cbind(OI_KLL$temperature_avg_L3,OI_KLL$total_rain_L1,OI_KLL$total_rain_L2) 
xreg <-ts(xreg)


# Model fitting 

model <- Arima(tsOI_KLL,order=c(1,0,1), seasonal=list(order=c(1, 1, 1), period=12),xreg = xreg)   
summary(model)


sarima(tsOI_KLL, 1,0,1,1,1,1,12,xreg = xreg)


# training and  forcasting test set 

tsOI_KLL <-ts(OI_KLLag)
training <- window(tsOI_KLL,end=105)
test <- window(tsOI_KLL,start=106)
fit_A <- Arima(training,order=c(1,0,1), seasonal=list(order=c(1, 1, 1), period=12), xreg=xreg[1:105,]) 
fc <- forecast(fit_A, xreg=xreg[106:117,])
plot(fc,main=" ", ylab="Ovitrap Index", xlab="Months")
lines(test, col="red")
legend("top",lty=1, bty = "n",col=c("red","blue"),c("Observed Test data","Predicted"))
accuracy(fc, test) 
fc

# ploting predicted vs observed 



# Exporting the forecast as excell sheet   

forecast<-fc
forecast <- write.xlsx(forecast, 'forecast.xlsx') 

# calculating the RMSE for forecasting "h" months ahead 

forecast_A <- forecast(fc,h=1)
arima_f_e<-test-forecast_A$mean
MSE<-mean((arima_f_e<-test-forecast_A$mean)^2) 
sqrt(MSE)

# Diebold-Mariano test of predicitve accuracy multivaria and univariate model

fit_A <- Arima(training,order=c(1,0,1), seasonal=list(order=c(1, 1, 1), period=12), xreg=xreg[1:105,]) 
fc1 <- forecast(fit_A, xreg=xreg[106:117,])
fit_B <- Arima(training,order=c(1,0,1), seasonal=list(order=c(1, 1, 1), period=12))
fc2 <-forecast(fit_B,h=12)


fc1_e<-test-fc1$mean 
fc2_e<-test-fc2$mean 


# Application of dm.test from forecast-package to compare predictive accuracy:

dm.test(fc1_e,fc2_e,alternative = "l")
 




