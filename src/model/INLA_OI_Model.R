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


# create crossbasis variables

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



# INLA with rw1 and and yearly random effect 

formula1 <- TPC ~  cb.meanTemperature + cb.totalRainfall + f(month, model="rw1") + f(year, model="iid") 

inla.fit <- inla(formula=formula1, data=data,family = 'poisson',
                 control.fixed=list(correlation.matrix=T),
                 control.compute=list(dic=T, waic=T)) 


summary(inla.fit)

# Printing DIC and WAIC values 
print(inla.fit$dic$dic)
print(inla.fit$waic$waic)


# summary


# Extracting coef/vcov/posterior distribution /linear predictor distribution 

inla.coef <- inla.fit$summary.fixed$mean 
inla.vcov <- inla.fit$misc$lincomb.derived.covariance.matrix         
inla.fixed <- inla.fit$summary.fixed
inla.predictor <- inla.fit$summary.linear.predictor



# Plotting the posterior distribution of the cross basis predictors 

plot(inla.fit, plot.fixed.effects = TRUE, plot.lincomb = FALSE, plot.random.effects = FALSE, plot.hyperparameters = FALSE,
     plot.predictor = FALSE, plot.q = FALSE, plot.cpo = FALSE, single = FALSE)

