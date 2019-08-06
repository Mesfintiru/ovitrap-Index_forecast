# clean the environment

rm(list=ls(all=TRUE))


# select an area between: NTS, NTN, KL, HK 

area <- "KL"



# load from csv file into 'data' variable

data <- paste("data/monthly_data_Poisson_", area, ".csv", sep="") 

data <- read.csv(filePath, header=T)



# clean data (esp. those with '#')
for (fieldName in names(data)) {
  data[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", data[[fieldName]]))
}

library("INLA")
library("dlnm")
library("splines")
library(forecast)

# Creating cross basis for predictors 

cb.meanTemperature <- crossbasis(data$temperature_avg, lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3, 1)))
cb.totalRainfall <- crossbasis(data$total_rain, lag=6,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(6, 1)))

getUpdatedColnames <- function(varName, oldColnames) {
  newColnames <- c()
  for (colname_i in 1:length(oldColnames)) {
    newColnames[colname_i] <- paste(varName,
                                    oldColnames[colname_i],
                                    sep="")
  }
  return (newColnames)
}

# update colnames
colnames(cb.meanTemperature) <- getUpdatedColnames("cb.meanTemperature", colnames(cb.meanTemperature))
colnames(cb.totalRainfall) <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))


#inla.fit <- inla(ovitrap_idx ~ cb.meanTemperature + cb.totalRainfall,
#data=data,
#control.fixed=list(correlation.matrix=T),
#control.compute=list(dic=T, waic=T,cpo=T))


# hyper.ar1 = list(theta1 = list(prior="pc.prec", param=c(1, 0.00001))) 

formula1 <- TPC ~  cb.meanTemperature + cb.totalRainfall + f(month, model="rw1") + f(year, model="iid") 

inla.fit <- inla(formula=formula1, data=data,family = 'poisson',
                 control.fixed=list(correlation.matrix=T),
                 control.compute=list(dic=T, waic=T)) 


inla.coef <- inla.fit$summary.fixed$mean 
inla.vcov <- inla.fit$misc$lincomb.derived.covariance.matrix 


# obtaining values relevant to cb.totalRainfall
cond <- paste("cb.totalRainfall","[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
indvcov <- grep(cond, rownames(inla.vcov))
indcoef <- grep(cond, rownames(inla.fit$summary.fixed))
inla.coef <- inla.coef[indcoef]
inla.vcov <- inla.vcov[indvcov,indvcov,drop=FALSE]

# DLNM model prediction

pred.totalRainfall <- crosspred(cb.totalRainfall, bylag=1,  coef=inla.coef, vcov=inla.vcov, cumul=TRUE)

# Rainfall Lag effect 3D plot

plot(pred.totalRainfall, xlab="Rainfall(mm)", zlab="RD", ylab="Lag") 

# Rainfall Lag effcet Contour Plot 

plot(pred.totalRainfall, "contour",
     plot.title = title(xlab="Rainfall (mm)", ylab="Lag", main="Lag effect of Monthly Total Rainfall on Mosquito Relative Density "), 
     key.title=title("RD")) 


# Plots for lag effect at very high and very low  Rainfall


plot(pred.totalRainfall, "slices", var=500, col=3, ylab="RD", ci.arg=list(density=15,lwd=2), main="Lag effect at 500mm of Monthly Total Rainfall") 
plot(pred.totalRainfall, "slices", var=10, col=3, ylab="RD", ci.arg=list(density=15,lwd=2), main="Lag effect at 50mm of Monthly Total Rainfall") 
