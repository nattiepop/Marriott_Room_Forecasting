# Decomposition Models
setwd("~/Master/Time Serie Analysis/Case 1")
library(forecast)

# Model 1 (demand as a response variable)

# Read data
data <- read.csv("marriott.csv", header = TRUE)
names(data) <- tolower(names(data))

# Build Regression Model
data$t <- 1:nrow(data)
lm <- lm(demand~t, data=data[1:87,])
summary(lm)

# Insert trend and detrend column
data$demand.trend <- (lm$coefficients[1] + (lm$coefficients[2] * data$t))
data$demand.detrend <- data$demand/data$demand.trend

# Calculate Seasonal
df <- data[1:87,]
seasonal <- aggregate(df$demand.detrend, list(df$dow.indicator), FUN=mean) 
seasonal <- seasonal[,2]+(1-mean(seasonal[,2]))

# Attach Seasonal Column
data$seasonal <- rep(seasonal,14)

# Calculate Multi Decomposed Demand
data$demand.multidecompose <- data$demand.trend*data$seasonal

# Self-validation
accuracy(data$demand.multidecompose[1:87], data$demand[1:87])

# Plot
plot(data$t, data$demand, col = "red", type = 'l')
lines(data$t, data$demand.multidecompose, col = "blue")

# Forecast for Saturday August 22.
data$demand.multidecompose[92]

# Model 2 (pickup.ratio as a response variable)

# Read data
data <- read.csv("marriott.csv", header = TRUE)
names(data) <- tolower(names(data))

# Build Regression Model
data$t <- 1:nrow(data)
lm <- lm(pickup.ratio~t, data=data[1:87,])
summary(lm)

# Insert trend and detrend column
data$pickup.ratio.trend <- (lm$coefficients[1] + (lm$coefficients[2] * data$t))
data$pickup.ratio.detrend <- data$pickup.ratio/data$pickup.ratio.trend

# Calculate Seasonal
df <- data[1:87,]
seasonal <- aggregate(df$pickup.ratio.detrend, list(df$dow.indicator), FUN=mean) 
seasonal <- seasonal[,2]+(1-mean(seasonal[,2]))

# Attach Seasonal Column
data$seasonal <- rep(seasonal,14)

# Calculate Multi Decomposed pickup.ratio
data$pickup.ratio.multidecompose <- data$pickup.ratio.trend*data$seasonal

# Self-validation
accuracy(data$pickup.ratio.multidecompose[1:87], data$pickup.ratio[1:87])

# Plot pickup.ratio
plot(data$t, data$pickup.ratio, col = "red", type = 'l')
lines(data$t, data$pickup.ratio.multidecompose, col = "blue")

# Calculate Multi Decomposed demand
data$demand.multidecompose <- (data$pickup.ratio.multidecompose * data$tuesday.bookings)

# Plot demand
plot(data$t, data$demand, col = "red", type = 'l')
lines(data$t, data$demand.multidecompose, col = "blue")
#lines(data$t, data$tuesday.bookings, col = "darkgreen")

# Forecast for Saturday August 22.
data$demand.multidecompose[92]






