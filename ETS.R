# Read data
setwd("~/Master/Time Serie Analysis/Case 1")
data <- read.csv("marriott.csv", header = TRUE)
base <- data[1:87,]
  
# Add deseasonalized demand column
base$de.DEMAND <- base$DEMAND/base$DOW.INDEX
head(base)
tail(base)
summary(base)

# Create time series object
ts.demand <- ts(base$de.DEMAND)
par(mfrow=c(1,1))
plot(ts.demand, type='l')

# Model
model <- ets(ts.demand)
model #ANN
par(mfrow = c(1, 1))
plot(forecast(model, h = 5))

# Forecast 
forecast(model, h = 5, level = 0)

# Self-validation
forecast <- forecast(model)$fitted
accuracy(forecast,ts.demand)

# Forecast for Saturday, August 22,
sat.forecast <- round(1405.645*data[1,6],0)
sat.forecast

# Check Residuals
model %>% checkresiduals()








