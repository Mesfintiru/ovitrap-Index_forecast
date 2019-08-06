cb.meanTemperature <- crossbasis(data$temperature_avg, lag=3, 
                                 argvar=list(fun="ns", df=3),
                                 arglag=list(fun="ns", knots=logknots(3, 1)))
cb.totalRainfall <- crossbasis(data$total_rain, lag=6,
                               argvar=list(fun="ns", df=3),
                               arglag=list(fun="ns", knots=logknots(6, 1)))


model <- glm(TPC ~ cb.meanTemperature + cb.totalRainfall , family=poisson,data)     

summary(model)



# model prediction 

pred.meanTemperature <- crosspred(cb.totalRainfall, model, by=0.5) 

# 3D Plot for lag effect of temperature 

plot(pred.meanTemperature, xlab="Rainfall(mm)", zlab="RD", ylab="Lag")


# Contour Plot for lag effect of rainfall 

plot(pred.meanTemperature, "contour", 
     
     plot.title = title(xlab="Rainfall(mm)", ylab="Lag", main="Lag Effect of Monthly Total Rainfall on Mosquito Relative Density"), 
     key.title=title("RD"))
     
     



