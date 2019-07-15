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

cb.meanTemperature <- crossbasis(data$temperature_avg, lag=4, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(4, 1)))
cb.totalRainfall <- crossbasis(data$total_rain, lag=2,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(2, 1)))
     
model <- glm(ovitrap_idx ~ cb.meanTemperature + cb.totalRainfall , family=gaussian(), data)     
summary(model)



