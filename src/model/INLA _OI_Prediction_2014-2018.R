# clean the environment

rm(list=ls(all=TRUE))


# select an area between: NTS, NTN, KL, HK 

area <- "KL"



# load from csv file into 'data' variable

filePath <- paste("data/monthly_data_Count_", area, ".csv", sep="") 

data <- read.csv(filePath, header=T)


# clean data (esp. those with '#')
for (fieldName in names(data)) {
  data[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", data[[fieldName]]))
}

library("INLA")
library("dlnm")
library("splines")
library(forecast)


#  -----------------------    PREDICTING 2014 --------------------------------------------------------------------



# Creating cross basis for predictors 

cb.meanTemperature <- crossbasis(data$temperature_avg[1:72], lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3,1))) 
cb.totalRainfall <- crossbasis(data$total_rain[1:72], lag=6,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(6, 1))) 

# Getting updated colunm names 

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
colnames(cb.totalRainfall)  <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))


# predicting the last 12 months using missing values(NA) approach   

n.pred = 12
y=data$TPC[1:60]
yy = c(y, rep(NA, n.pred))



# creating data frame with NA values

Data <- data.frame(yy,data$temperature_avg[1:72],data$total_rain[1:72],data$month[1:72],data$year[1:72])

formula1 <- yy ~  cb.meanTemperature + cb.totalRainfall + f(data.month.1.72., model="rw1") + f(data.year.1.72.,model="iid") 

inla.fit1 <- inla(formula=formula1, data=Data,family = 'poisson',
                  control.fixed=list(correlation.matrix=T),control.predictor = list(link = 1),
                  control.compute=list(dic=T, waic=T)) 



# Obtaining the predicted values in excelsheet (the fitted values [61:72] are predicted values)

KL1 <- inla.fit1$summary.fitted.values
library(openxlsx)
KL1 <- write.xlsx(KL1, 'KL1.xlsx')

# Need to divide the predicted values (mean and also the sd, 0.025 and 0.975 quantile values) 
#from [61:72] by (11*55) and then multiply by 100 to get the percent Ovitrap index 



#  -----------------    PREDICTING 2015 --------------------------------------------------------------------------




# Creating cross basis for predictors  

cb.meanTemperature <- crossbasis(data$temperature_avg[1:84], lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3,1))) 
cb.totalRainfall <- crossbasis(data$total_rain[1:84], lag=6,
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
colnames(cb.totalRainfall)  <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))


# predicting the last 12 months using missing values(NA) approach   

n.pred = 12
y=data$TPC[1:72]
yy = c(y, rep(NA, n.pred))



# creating data frame wiht NA values

Data <- data.frame(yy,data$temperature_avg[1:84],data$total_rain[1:84],data$month[1:84],data$year[1:84])

formula2 <- yy ~  cb.meanTemperature + cb.totalRainfall + f(data.month.1.84., model="rw1") + f(data.year.1.84.,model="iid") 
inla.fit2 <- inla(formula=formula2, data=Data,family = 'poisson',
                  control.fixed=list(correlation.matrix=T),control.predictor = list(link = 1),
                  control.compute=list(dic=T, waic=T)) 

# Obtaining the predicted values in excelsheet (the fitted values [73:84] are predicted values)
KL_2 <- inla.fit2$summary.fitted.values
library(openxlsx)
KL_2 <- write.xlsx(KL_2, 'KL_2.xlsx')

# Need to divide the predicted values (mean and also the sd, 0.025 and 0.975 quantile values) 
#from [73:84] by (11*55) and then multiply by 100 to get the percent Ovitrap index



#  -----------------    PREDICTING 2016 --------------------------------------------------------------------------

# Creating cross basis for predictors 

cb.meanTemperature <- crossbasis(data$temperature_avg[1:96], lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3,1))) 
cb.totalRainfall <- crossbasis(data$total_rain[1:96], lag=6,
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
colnames(cb.totalRainfall)  <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))


# predicting the last 12 months using missing values(NA) approach   

n.pred = 12
y=data$TPC[1:84]
yy = c(y, rep(NA, n.pred))



# creating data frame wiht NA values

Data <- data.frame(yy,data$temperature_avg[1:96],data$total_rain[1:96],data$month[1:96],data$year[1:96],data$year[1:96])

formula3 <- yy ~  cb.meanTemperature + cb.totalRainfall + f(data.month.1.96., model="rw1") + f(data.year.1.96.,model="iid") 
inla.fit3 <- inla(formula=formula3, data=Data,family = 'poisson',
                  control.fixed=list(correlation.matrix=T),control.predictor = list(link = 1),
                  control.compute=list(dic=T, waic=T)) 


# Obtaining the predicted values in excelsheet (the fitted values [85:96] are predicted values)

KL_3 <- inla.fit3$summary.fitted.values
library(openxlsx)
KL_3 <- write.xlsx(KL_3, 'KL_3.xlsx')

# Need to divide the predicted values (mean and also the sd, 0.025 and 0.975 quantile values) 
#from [85:96] by (11*55) and then multiply by 100 to get the percent Ovitrap index  


#  -----------------    PREDICTING 2017 --------------------------------------------------------------------------


# Creating cross basis for predictors 

cb.meanTemperature <- crossbasis(data$temperature_avg[1:108], lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3,1))) 
cb.totalRainfall <- crossbasis(data$total_rain[1:108], lag=6,
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
colnames(cb.totalRainfall)  <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))


# predicting the last 12 months using missing values(NA) approach    

n.pred = 12
y=data$TPC[1:96]
yy = c(y, rep(NA, n.pred))


# creating data frame with NA values

Data <- data.frame(yy,data$temperature_avg[1:108],data$total_rain[1:108],data$month[1:108],data$year[1:108])

formula4 <- yy ~  cb.meanTemperature + cb.totalRainfall + f(data.month.1.108., model="rw1") + f(data.year.1.108.,model="iid") 
inla.fit4 <- inla(formula=formula4,data=Data,family = 'poisson',
                  control.fixed=list(correlation.matrix=T),control.predictor = list(link = 1),
                  control.compute=list(dic=T, waic=T)) 

# Obtaining the predicted values in excelsheet (the fitted values [97:108] are predicted values)

KL_4 <- inla.fit4$summary.fitted.values
library(openxlsx)
KL_4 <- write.xlsx(KL_4, 'KL_4.xlsx')

# Need to divide the predicted values (mean and also the sd, 0.025 and 0.975 quantile values) 
#from [97:108] by (11*55) and then multiply by 100 to get the percent Ovitrap index



#  -----------------    PREDICTING 2018 --------------------------------------------------------------------------



# Creating cross basis for predictors 


cb.meanTemperature <- crossbasis(data$temperature_avg, lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3,1))) 
cb.totalRainfall <- crossbasis(data$total_rain, lag=6,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(6,1))) 

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
colnames(cb.totalRainfall)  <- getUpdatedColnames("cb.totalRainfall", colnames(cb.totalRainfall))


# predicting the last 12 months using missing values(NA) approach      

n.pred = 12
y=data$TPC[1:108]
yy = c(y, rep(NA, n.pred))


# creating data frame with NA values

Data <- data.frame(yy,data$temperature_avg,data$total_rain,data$month,data$year)

formula5 <- yy ~  cb.meanTemperature + cb.totalRainfall + f(data.month, model="rw1") + f(data.year,model="iid") 
inla.fit5 <- inla(formula=formula5,data=Data,family = 'poisson',
                  control.fixed=list(correlation.matrix=T),control.predictor = list(link = 1), 
                  control.compute=list(dic=T, waic=T)) 



# Obtaining the predicted values in excelsheet (the fitted values [109:120] are  predicted values)

KL_5 <- inla.fit5$summary.fitted.values
library(openxlsx)
KL_5 <- write.xlsx(KL_5, 'KL_5.xlsx')

# Need to divide the predicted values (mean and also the sd, 0.025 and 0.975 quantile values) 
#from [109:120] by (11*55) and then multiply by 100 to get the percent Ovitrap index 


# #  -----------------   Plotting Prediction  --------------------------------------------------------------------------


## The data to be used for plotting is a dataframe created from  observed and predicted OI values from the above predictions (with 0.025 and 0.975 qauntile vlaues of prediction) 
## the 0.025 and 0.975 qauntiles will be used as as upper and lower limits 



# Creating a Date Colomn from Month and Year data 

data2$Date <- with(data2, sprintf("%s-%02s", month, year))

# Converting "Date" colomn from Character class to 'Date' class 
library(lubridate)
data2$Date <- mdy(data2$Date)
data2$Date=as.Date(data2$Date)

# Plotting

library(ggplot2)

newdata <- reshape:::rename(data2, c("0.025quant"="lower", "0.975quant"="upper"))
ggplot(newdata, aes(y=ovitrap_idx, x=Date)) + geom_line(data=data2, aes(y=ovitrap_idx,colour = "Observed")) +
  geom_line(data=data2, aes(y=predicted_M,colour = "Predicted"))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='blue', alpha=0.3) + 
  xlab("Year")+
  ylab("Ovitrap Index")+
  theme_classic()

