# clean the environment
rm(list=ls(all=TRUE))

# select an area between: NTS, NTN, KL, HK 
area <- "KL"

# load from csv file into 'data' variable
filePath <- paste("../../data/monthly_data_", area, ".csv", sep="") 
data <- read.csv(filePath, header=T)

# clean data (esp. those with '#')
for (fieldName in names(data)) {
  data[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", data[[fieldName]]))
}


library("dlnm")
library("splines")

cb.meanTemperature <- crossbasis(data$temperature_avg, lag=6, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(6, 1)))
cb.totalRainfall <- crossbasis(data$total_rain, lag=6,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(6, 1)))
     
model <- glm(ovitrap_idx ~ cb.meanTemperature + cb.totalRainfall , family=gaussian(), data)     
summary(model)



# model prediction 


pred.meanTemperature <- crosspred(cb.meanTemperature, model, by=0.5) 

plot(pred.meanTemperature, xlab="Temperature", zlab="RR", ylab="Lag")

plot(pred.meanTemperature, "contour", 
     plot.title = title(xlab="Temperature", ylab="Lag", main="Contour Graph"), 
     key.title=title("RR"))


pred.totalRainfall <- crosspred(cb.totalRainfall, model,bylag=1)

plot(pred.totalRainfall, xlab="Rainfall", zlab="RR", ylab="Lag") 

plot(pred.totalRainfall, "contour",
     plot.title = title(xlab="Rainfall", ylab="Lag", main="Contour Graph"), 
     key.title=title("RR")) 
