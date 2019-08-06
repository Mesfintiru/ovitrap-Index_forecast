# clean the environment

rm(list=ls(all=TRUE))



# select an area between: NTS, NTN, KL, HK 

area <- "KL"



# load from csv file into 'data' variable

filePath <- paste("data/monthly_data_", area, ".csv", sep="") 

data <- read.csv(filePath, header=T)



# clean data (esp. those with '#')

for (fieldName in names(data)) {
  
  data[[fieldName]] <- as.numeric(gsub("[^.0-9]", "", data[[fieldName]])) 
  
}


library("dlnm")

library("splines")


# create crossbasis variables


cb.meanTemperature <- crossbasis(data$temperature_avg, lag=6, 
                                 
                                 argvar=list(fun="ns", df=3),
                                 
                                 arglag=list(fun="ns", knots=logknots(6, 1)))

cb.totalRainfall <- crossbasis(data$total_rain, lag=6,
                               
                               argvar=list(fun="ns", df=3),
                               
                               arglag=list(fun="ns", knots=logknots(6, 1)))


model <- glm(TPC ~ cb.meanTemperature + cb.totalRainfall , family=poisson,data)     

summary(model)



# model prediction 

pred.meanTemperature <- crosspred(cb.meanTemperature, model, by=0.5) 

# 3D Plot for lag effect of temperature 

plot(pred.meanTemperature, xlab="Temperature", zlab="RD", ylab="Lag")


# Contour Plot for lag effect of rainfall 

plot(pred.meanTemperature, "contour", 
     
     plot.title = title(xlab="Temperature(°C)", ylab="Lag", main="Lag Effect of Monthly Average Tempertaure on Mosquito Relative Density"), 
     key.title=title("RD"))
     
     
