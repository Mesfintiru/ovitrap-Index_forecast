# clean the environment

rm(list=ls(all=TRUE))

Data <- read.csv("../../data/monthly_data_KL_3lagged.csv", sep = ",", header = TRUE) 

Data_OI <-Data$ovitrap_idx  


# clean data (esp. those with '#')
for (fieldName in names(Data)) {
  Data[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", Data[[fieldName]]))
}

#Libraries

library(forecast)
library(lmtest)
library(astsa)
library(tseries)
library(TSA)
library(openxlsx)


# Detecting seasonality and period of seasonality 

# fourier tranfformation  

p <- periodogram(Data_OI)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)
top2

# display the 2 highest "power" frequencies
top2

time = 1/top2$f
time

#square root transformation

sqrt_Data_OI <-sqrt(Data_OI)

# changing the data to time series(ts)format 

tsData_OI <-ts(Data_OI,frequency = 12)

# ploting the ts data

plot(tsData_OI)

# Decomposition to check for  trend and seasonality 

components.ts = decompose(tsData_OI)
plot(components.ts)

# determining seasonal differencing required

ns<-nsdiffs(tsData_OI)

# detremining regular differencing required

nd<-ndiffs(tsData_OI,test = c("kpss"))

# first order differencing 

diff_12 <- diff(tsData_OI,12)
plot(diff_12, main= "First Order Differenced Ovitrap Index Series", ylab=" Differenced Ovitrap Index",xlab="Time in Years")

# ACF PACF plots to identify possible (p,d,q) and (P,D,Q) values ---- already identified : (1,0,1)(1,1,1)

acf2(diff_12, 48, main= "First Order Differenced Ovitrap Index Series")


# model diagnostics

modelfit <- Arima(tsData_OI, order=c(1,0,1), seasonal=c(1,1,1))   
checkresiduals(modelfit)


# Inclusion of climate variables (xreg)

xreg<- cbind(Data$temperature_avg_L3,Data$total_rain_L1,Data$total_rain_L2) 
xreg <-ts(xreg)


# Model fitting 

model <- Arima(tsData_OI,order=c(1,0,0), seasonal=list(order=c(1, 1, 0), period=12),xreg = xreg)   
summary(model)


# training and  forcasting test set 

tsData_OI <-ts(Data_OI)
training <- window(tsData_OI,end=105)
test <- window(tsData_OI,start=106)
fit_A <- Arima(training,order=c(1,0,1), seasonal=list(order=c(1, 1, 1), period=12), xreg=xreg[1:105,]) 
fc <- forecast(fit_A, xreg=xreg[106:117,])
plot(fc,main=" ", ylab="Ovitrap Index", xlab="Months")
lines(test, col="red")
legend("top",lty=1, bty = "n",col=c("red","blue"),c("Observed Test data","Predicted"))
accuracy(fc, test) 
fc


# Exporting the forecast as excel sheet   

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


dm.test(fc1_e,fc2_e)
