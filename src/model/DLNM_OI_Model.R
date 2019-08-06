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


cb.meanTemperature <- crossbasis(data$temperature_avg, lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3, 1)))
cb.totalRainfall <- crossbasis(data$total_rain, lag=6,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(6, 1)))


model <- glm(TPC ~ cb.meanTemperature + cb.totalRainfall , family=poisson,data)     

summary(model)

