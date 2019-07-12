# clean the environment
rm(list=ls(all=TRUE))

# select an area between: NTS, NTN, KL, HK
area <- "KL"

# load from csv file into 'data' variable
filePath <- paste("../../dat/monthly_data_", area, ".csv", sep="")
data <- read.csv(filePath, header=T)

# clean data (esp. those with '#')
for (fieldName in names(data)) {
  data[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", data[[fieldName]]))
}

library("INLA")
library("dlnm")
library("splines")
library(forecast)


# create crossbasis variables

cb.meanTemperature <- crossbasis(data$temperature_avg, lag=4, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(4,1))) 
cb.totalRainfall <- crossbasis(data$total_rain, lag=2,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(2, 1))) 





getUpdatedColnames <- function(varName, oldColnames) {
  newColnames <- c()
  for (colname_i in 1:length(oldColnames)) {
    newColnames[colname_i] <- paste(varName,
                                    oldColnames[colname_i],
                                    sep="")
  }
  return (newColnames)
}

getRelevantCoefAndVcov <- function(varName, summary.fixed, coef, vcov) {
  cond <- paste(varName,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
  indvcov <- grep(cond, rownames(vcov))
  indcoef <- grep(cond, rownames(summary.fixed))
  coef <- coef[indcoef]
  vcov <- vcov[indvcov,indvcov,drop=FALSE]
  return (list("coef"=coef, "vcov"=vcov))
}

# update colnames
colnames(cb.meanTemperature) <- getUpdatedColnames("cb.meanTemperature", colnames(cb.meanTemperature))
colnames(cb.totalRainfall) <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))


#inla.fit<- inla(ovitrap_idx ~ cb.meanTemperature  + cb.totalRainfall,
                 #data=data,
                 #control.fixed=list(correlation.matrix=T),
                 #control.compute=list(dic=T, waic=T, config=T, cpo=T)) 


# INLA with rw1 and and yearly random effect 

formula1 <- ovitrap_idx ~  cb.meanTemperature  + cb.totalRainfall  + f(month, model="rw1") + f(year,model="iid")    

inla.fit <- inla(formula=formula1, data=data,  
                 control.fixed=list(correlation.matrix=T),
                 control.compute=list(dic=T, waic=T, config=T, cpo=T)) 


print(inla.fit$dic$dic)
print(inla.fit$waic$waic)


inla.coef <- inla.fit$summary.fixed$mean   
inla.vcov <- inla.fit$misc$lincomb.derived.covariance.matrix    

# obtaining values relevant to cb.meanTemperature and cb.totalrRainfall
info.meanTemperature <- getRelevantCoefAndVcov("cb.meanTemperature", inla.fit$summary.fixed,
                                               inla.coef, inla.vcov) 
info.totalRainfall <- getRelevantCoefAndVcov("cb.totalRainfall", inla.fit$summary.fixed,
                                             inla.coef, inla.vcov) 

# Prediction 
predicted <- rep(NA,120)
inla.intercept <- inla.fit$summary.fixed$mean[1]
for (row_i in 7:120) { 
  temp.row <- cb.meanTemperature[row_i,]
  rain.row <- cb.totalRainfall[row_i,]
  
  predicted[row_i] <- sum(temp.row * info.meanTemperature$coef) +  
    sum(rain.row * info.totalRainfall$coef) + inla.intercept          
}
predicted[predicted < 0] <- 0 
plot(data$ovitrap_idx, type="l", col="red")
lines(predicted, col="blue")
legend("top",lty=1,bty = "n",col=c("red","blue"),c("observed","predicted"))


# predicting the last 12 months using missing values(NA) approach   

n.pred = 12
y=data$ovitrap_idx[1:108]
yy = c(y, rep(NA, n.pred))


#creating data frame wiht NA values
Data <- data.frame(yy,data$temperature_avg,data$total_rain,data$month,data$year)

formula1 <- yy ~  cb.meanTemperature + cb.totalRainfall + f(data.month, model="rw1") + f(data.year, model="iid") 
inla.fit2 <- inla(formula=formula1, data=Data,
                  control.fixed=list(correlation.matrix=T),control.predictor = list(link = 1),
                  control.compute=list(dic=T, waic=T)) 


#ploting the oredicted values 

inla.fit2$summary.fitted.values 
plot(yy, type="l", col="red",main=" ", ylab="Ovitrap Index", xlab="Months")
lines(inla.fit2$summary.fitted.values$mean, col="blue")
legend("top",lty=1,bty = "n",col=c("red","blue"),c("observed","predicted"))


# getting the predicted values

forecast_values <- predicted[109:120]
library(openxlsx)
forecast_values <- write.xlsx(forecast_values, 'forecast_values.xlsx')
predicted[row_i]

# calculating accuracy of prediction 
accuracy(predicted,data$ovitrap_idx)
