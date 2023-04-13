setwd("~/Master/Time Serie Analysis/Case 1")

data <- read.csv('marriott.csv', sep=',', header=TRUE)
newdata <- data[0:87,]

library(forecast)
library(zoo)

newdata['DE_PICKUP_RATIO'] <- newdata['PICKUP.RATIO']/newdata['DOW.INDEX']
newdata['DE_DEMAND'] <- newdata['DEMAND']/newdata['DOW.INDEX']

de_pickup <- ts(newdata['DE_PICKUP_RATIO'])
pickup <- ts(newdata['PICKUP.RATIO'])
de_demand <- ts(newdata['DE_DEMAND'])
demand <- ts(newdata['DEMAND'])

# # pickup
# bestmodel <- ets(pickup)
# forecast <- forecast(bestmodel)$fitted
# accuracy(forecast,pickup)
# 
# bestmodel
# forecast(bestmodel, h=5)

# deseason pickup
bestmodel <- ets(de_pickup)
forecast <- forecast(bestmodel)$fitted
accuracy(forecast,de_pickup)

bestmodel
forecast(bestmodel, h=5)

test <- checkresiduals(bestmodel)

pt(0.8544, 8)

# # deseason demand
# bestmodel <- ets(de_demand)
# forecast <- forecast(bestmodel)$fitted
# accuracy(forecast,de_demand)
# 
# bestmodel
# forecast(bestmodel, h=5)
# 
# # demand
# bestmodel <- ets(demand)
# forecast <- forecast(bestmodel)$fitted
# accuracy(forecast,demand)
# 
# bestmodel
# forecast(bestmodel, h=5)








