##Load Libraries
library(portes)
library(fpp2)
library(readxl)
library(tidyverse)
library(forecast)
library(fabletools)
library(dplyr)
library(tsibble)
library(feasts)

##Set working directory
setwd("C:/Users/irana/OneDrive - IESEG/Documents/Courses/Semester_2/forcasting/project/data")
#################################Part1##################
#Read the data
air = read_excel("DataSets2022.xlsx", sheet="Airpass_BE")

#create time series
air_data <- ts(air[,2], frequency = 12, start = 2003)

#split in train and test
air_train = window(air_data, start=c(2003,1), end=c(2017, 12))
air_test = window(air_data, start=c(2018,1), end=c(2020,2))
air_rest = window(air_data, start=c(2020,3))
air_train_test = window(air_data, start=c(2003,1), end=c(2020, 2))


######################### Question 1 #####################
#Visualize & Explore the raw data

#Plot the data
plot(air_data)

#plot timeseries display
tsdisplay(air_data)

#plot seasonality
seasonplot(air_data, col=rainbow(20), year.labels = TRUE)

#log transform
air_data_log <- log(air_train_test)

autoplot(air_data)+ggtitle("BE Airlines")
autoplot(air_data_log)+ggtitle("BE Airlines")

#Seasonal sub series
ggsubseriesplot(air_data) + ggtitle("Seasonal subseries plot")

# Auto-correlation & Partial Auto-correlation

#ACF
ggAcf(air_data) 
#PACF
ggPacf(air_data)  
#LAG Plot
lag.plot(air_data)  

#ggAcf(air_data, plot = FALSE)


#Explore train data
plot(air_train)

#look at ts plot
tsdisplay(air_train)

#seasonal plot
seasonplot(air_train, col=rainbow(20), year.labels = TRUE)
#fabletools::autoplot(air_train)

######################### Question 2 #####################
#data transformation

# Using Log Transformation
air_log <- log(air_train_test)
plot(air_log)

# Plotting the Seasonal and Monthly Trend Graphs
seasonplot(air_log, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal Plot",
           ylab="Airlines Pasangers in Belgium",
           col=rainbow(25), 
           pch=20)

monthplot(air_log, main="Seasonal Subseries Plot", ylab = "Airlines Pasangers in Belgium",
          xlab="Month", type="l")

# Plotting the ACF & PACF (Autocorrelation & Partial Autocorrelation)
lag.plot(air_log)
tsdisplay(air_log)

# Find optimal lambda
lambda = BoxCox.lambda(air_train_test)
lambda
air_train_opt_trans = BoxCox(air_train, lambda)
air_test_opt_trans = BoxCox(air_test, lambda)

#compare transformed data with original data
fabletools::autoplot(air_train)
fabletools::autoplot(air_train_opt_trans)


######################### Question 3 #####################

# Applying seasonal naive forecasting and plotting

h <- length(air_test)
rsv_m1 <- snaive(rsv1, h=h)            # seasonal naive
air_sn <- snaive(air_train, h=h)
autoplot(air_sn)
lines(air_test, col="red")

#Evaluating forecast accuracy
accuracy(air_sn, air_test)

# Getting residuals
checkresiduals(air_sn)
res <- residuals(air_sn)

tsdisplay(res)
Box.test(res, lag = 12, fitdf = 0, type = "Lj")
LjungBox(res[-c(1:12),], lags = seq(1, 24, 1), order = 0)

# Recombine training and test set and apply the selected model to the complete data set
air_final <- snaive(air_train_test, h=h)
autoplot(air_final)

# Evaluating Forecast Accuracy
accuracy(air_sn, air_test)[,c(2,3,4,5,6,7)]


########################Question 4 ###########################
# Applying STL decomposition

air_stl <- stl(air_train[,1], s.window = "periodic")
plot(air_train, col = "black", main = "Belgian airline passanger data", ylab = "Index", xlab = "")
lines(air_stl$time.series[,2], col = "red")

autoplot(air_stl)

# Since stl only decomposes the data, we are not getting a forecast yet. 
#In order to be able to obtain accuracy and residuals,
# it is necessary to apply forecast to the decomposed data

# Applying naive forecast method to the stl data and evaluating forecast accuracy
air_stl_nai <- forecast(air_stl, method = "naive") 
autoplot(air_stl_nai)

# Getting accuracy
accuracy(air_stl_nai, air_test)

# Getting residuals
checkresiduals(air_stl_nai)
res_stl_nai <- residuals(air_stl_nai)
res_stl_nai

tsdisplay(res_stl_nai)
Box.test(res_stl_nai, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_stl_nai[-1], lags = seq(1, 24, 1), order = 0)


########################Question 5###########################
ets1 <- ets(air_train, model = "ANA", ic = c("aic"))
ets1
ets1 <- forecast(ets1, h=h)
plot(ets1)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets1)
residuals_ets1 <- residuals(ets1)
hist(residuals_ets1)
tsdisplay(residuals_ets1)
Box.test(residuals_ets1, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets1, lags = seq(1, 24, 1), order = length(ets1$model$par))

# Evaluating Forecast Accuracy
accuracy(ets1, test)[,c(2,3,4,5,6,7)]

### ETS2 ###
ets2 <- ets(train, model = "ANAd", ic = c("aic"))
ets2
ets2 <- forecast(ets2, h=len_test)
plot(ets2)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets2)
residuals_ets2 <- residuals(ets2)
hist(residuals_ets2)
tsdisplay(residuals_ets2)
Box.test(residuals_ets2, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets2, lags = seq(1, 24, 1), order = length(ets2$model$par))

# Evaluating Forecast Accuracy
accuracy(ets2, test)[,c(2,3,4,5,6,7)]

### ETS3 ###
ets3 <- ets(train, model = "AAA", ic = c("aic"))
ets3
ets3 <- forecast(ets3, h=len_test)
plot(ets3)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets3)
residuals_ets3 <- residuals(ets3)
hist(residuals_ets3)
tsdisplay(residuals_ets3)
Box.test(residuals_ets3, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets3, lags = seq(1, 24, 1), order = length(ets3$model$par))

# Evaluating Forecast Accuracy
accuracy(ets3, test)[,c(2,3,4,5,6,7)]

### ETS4 ###
ets4 <- ets(train, model = "AAAd", ic = c("aic"))
ets4
ets4 <- forecast(ets4, h=len_test)
plot(ets4)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets4)
residuals_ets4 <- residuals(ets4)
hist(residuals_ets4)
tsdisplay(residuals_ets4)
Box.test(residuals_ets4, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets4, lags = seq(1, 24, 1), order = length(ets4$model$par))

# Evaluating Forecast Accuracy
accuracy(ets4, test)[,c(2,3,4,5,6,7)]

### ETS5 ###
ets5 <- ets(train, model = "MNA", ic = c("aic"))
ets5
ets5 <- forecast(ets5, h=len_test)
plot(ets5)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets5)
residuals_ets5 <- residuals(ets5)
hist(residuals_ets5)
tsdisplay(residuals_ets5)
Box.test(residuals_ets5, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets5, lags = seq(1, 24, 1), order = length(ets5$model$par))

# Evaluating Forecast Accuracy
accuracy(ets5, test)[,c(2,3,4,5,6,7)]

### ETS6 ###
ets6 <- ets(train, model = "MNAd", ic = c("aic"))
ets6
ets6 <- forecast(ets6, h=len_test)
plot(ets6)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets6)
residuals_ets6 <- residuals(ets6)
hist(residuals_ets6)
tsdisplay(residuals_ets6)
Box.test(residuals_ets6, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets6, lags = seq(1, 24, 1), order = length(ets6$model$par))

# Evaluating Forecast Accuracy
accuracy(ets6, test)[,c(2,3,4,5,6,7)]

### ETS7 ###
ets7 <- ets(train, model = "MAA", ic = c("aic"))
ets7
ets7 <- forecast(ets7, h=len_test)
plot(ets7)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets7)
residuals_ets7 <- residuals(ets7)
hist(residuals_ets7)
tsdisplay(residuals_ets7)
Box.test(residuals_ets7, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets7, lags = seq(1, 24, 1), order = length(ets7$model$par))

# Evaluating Forecast Accuracy
accuracy(ets7, test)[,c(2,3,4,5,6,7)]

### ETS8 ###
ets8 <- ets(train, model = "MAAd", ic = c("aic"))
ets8
ets8 <- forecast(ets8, h=len_test)
plot(ets8)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets8)
residuals_ets8 <- residuals(ets8)
hist(residuals_ets8)
tsdisplay(residuals_ets8)
Box.test(residuals_ets8, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets8, lags = seq(1, 24, 1), order = length(ets8$model$par))

# Evaluating Forecast Accuracy
accuracy(ets8, test)[,c(2,3,4,5,6,7)]

### Applying Auto ETS to get the Best Model Automatically ###
ets_auto <- ets(train, ic = c("aic"))
ets_auto
plot(ets_auto)
etsf_auto <- forecast(ets_auto, h=len_test)
plot(etsf_auto)

# Plotting Residuals and applying the Ljung-Box test
checkresiduals(ets_auto)
residuals_ets_auto <- residuals(ets_auto)
hist(residuals_ets_auto)
tsdisplay(residuals_ets_auto)
Box.test(residuals_ets_auto, lag = 24, fitdf = 0, type = "Lj")
LjungBox(residuals_ets_auto, lags = seq(1, 24, 1), order = length(ets_auto$model$par))

# Evaluating Forecast Accuracy
accuracy(etsf_auto, test)[,c(2,3,4,5,6,7)]



##############Modeling#############
#Set horizon
h <- length(air_test)

# naive methods
air_m1 <- rwf(air_train, h=h)             #naive
air_m2 <- rwf(air_train, drift=TRUE, h=h) #drift

checkresiduals(air_m1)
checkresiduals(air_m2)

plot(air_data, main="Yearly road airalities", ylab="", xlab="Year",xlim=c(2003,2020))
lines(air_m1$mean,col=4)
lines(air_m2$mean,col=2)
legend("bottomleft", lty=1, col=c(4,2), legend=c("Naive method","Drift method"))

fc <- rwf(air_data, drift=TRUE, h=6); fc
plot(fc)

# Exponential smoothing methods
air_m3 <- ses(air_train, h=h)                  #SES
air_m4 <- holt(air_train, h=h)                 #Holt's linear
air_m5 <- holt(air_train, damped = TRUE, h=h)  #Damped trend

# ETS models
air_m6 <- forecast(ets(air_train, model = "AAN", damped = FALSE), h=h)
air_m7 <- forecast(ets(air_train, model = "MAN", damped = FALSE), h=h)
air_m8 <- forecast(ets(air_train, model = "AAN", damped = TRUE), h=h)
air_m9 <- forecast(ets(air_train, model = "MAN", damped = TRUE), h=h)

# ARIMA models
ndiffs(air_train)
tsdisplay(diff(air_train))

m10 <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE)
air_m10 <- forecast(m10, h=h)
summary(air_m10)
accuracy(air_m10, air_test)
checkresiduals(air_m10)

# Re-estimate considered models with log transform
air_m11 <- naive(air_train, h=h)             #naive
air_m12 <- rwf(air_train, drift=TRUE, h=h) #drift
air_m13 <- ses(air_train, h=h, lambda = 0, biasadj = TRUE)                  #SES
air_m14 <- holt(air_train, h=h, lambda = 0, biasadj = TRUE)                 #Holt's linear
air_m15 <- holt(air_train, damped = TRUE, h=h, lambda = 0, biasadj = TRUE)  #Damped trend
m16 <- ets(air_train, model = "AAN", damped = FALSE, lambda = 0, biasadj = TRUE)
air_m16 <- forecast(m16, h=h)
m17 <- ets(air_train, model = "AAN", damped = TRUE, lambda = 0, biasadj = TRUE)
air_m17 <- forecast(m17, h=h)
m18 <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE, 
                  lambda = 0, biasadj = TRUE)
air_m18 <- forecast(m18, h=h)

checkresiduals(m18)


#list of models
models <- c("RWF", "RWF drift", "SES", "Holt", "Holt damped", "AAN", "MAN", "AAdN", "MAdN", 
            "auto.ARIMA", "RWF log", "RWF drift log", "SES log", "Holt log", "Holt damped log",
            "AAN log", "AAdN log", "auto.ARIMA log")
n <- length(models)   #number of models

#naming of models for the given data set
m <- "air_m"

#naming of training and test set
trainset <- air_train
testset <- air_test

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:n) 
{
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), testset)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"
rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

a_train
a_test
round(res_matrix, digits = 4)



#Mean Forecast
f1 <- meanf(air_train, h=h)
#Naive Random Walk
f2 <- rwf(air_train, h=h)
#Naive Random Walk with drift
f3 <- rwf(air_train, drift=TRUE, h=h)
#Seasonal Naive
f4 <- snaive(air_train, h=h)

##compare models
a_fc1 = accuracy(f1,air_test)[,c(2,3,5,6)]
a_fc2 = accuracy(f2,air_test)[,c(2,3,5,6)]
a_fc3 = accuracy(f3,air_test)[,c(2,3,5,6)]
a_fc4 = accuracy(f4,air_test)[,c(2,3,5,6)]
a_fc1[2,]
acc <- rbind(a_fc1, a_fc2, a_fc3, a_fc4)
acc
rownames(acc) <- c("a_fc1", "a_fc2", "a_fc3", "a_fc4")

#Arima
f5 <- Arima(air_train, c(6,1,4), include.constant=FALSE)
summary(f5)
checkresiduals(f4)



fcast1 <- forecast(f5, h=69)
plot(fcast1)
lines(air_test)

accuracy(fcast1, air_test)
#ETS
f6 <- ets(air_train)




#Plot results
plot(air_exc_covid,main="Airlines Data", ylab="",xlab="Year")
lines(f1$mean,col=4)
lines(f2$mean,col=2)
lines(f3$mean,col=3)
lines(f4$mean,col=5)
legend("topleft",lty=1,col=c(4,2,3,5),legend=c("Mean","Naive","Drift", "Seaonsal naive"))

f5 <- arima(air_train)
f6 <- ets(air_train)

accuracy(f5)
accuracy(f6)


##Decomposition###
fit <- decompose(air_train, type="multiplicative")
plot(air_train)

### Seasonally adjusted data
seasAdj <- seasadj(fit)
plot(air_train)
lines(seasAdj, col="red")

### Random walk with drift forecasts of the seasonally adjusted data
driftFit <- rwf(seasAdj, drift=TRUE, h=24)
plot(driftFit)

### Reseasonalized forecasts of the seasonally adjusted data
plot(driftFit, col="red")
lines(driftFit$mean * fit$figure, col = "green", lwd = 2)
# driftFit$mean is the naive forecast with drift of seasonally adjusted data
# fit$figure is the seasonal component of decompose
# we have multiplicative decomposition ==> multiply these components
lines(air_test, col = "black", lwd = 2)

?stlf
?stl
# Using stl
# Decompose the time series
air_data_tib = as_tsibble(air_exc_covid)

air_data_tib %>% model(feasts::STL(value ~ season(window = Inf))) %>% components() %>% autoplot()


dim(air_train) = NULL

fit <- stl(air_train, s.window="periodic")
fcast <- forecast(fit, method="rwdrift", h=24)
accuracy(fcast, air_test)

plot(fcast)

pl_adj <- seasadj(fit)
plot(rwf(pl_adj, h=24, drift = TRUE), ylim =c(0, 2200))
lines(plastics, col="red")
lines(fcast$mean, col="green")


par(mfrow=c(2,1))
plot(stlf(plastics, method='naive'))
plot(stlf(plastics, method='rwdrift'))




#########################################

setwd("C:/Users/irana/OneDrive - IESEG/Documents/Courses/Semester_2/forcasting/project/data")

data = read.csv("data.csv")
data
elec_sales <- ts(data[,3], frequency = 4, start = 2001)

elec_sales
plot(elec_sales)
tsdisplay(elec_sales)
seasonplot(elec_sales, col=rainbow(20), year.labels = TRUE)
monthplot(hsales)
