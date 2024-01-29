library(Amelia)
library(stats)
library(TSA)
library(forecast)
library(tseries)
library(caschrono)
library(ggplot2)
library(gridExtra)
library(grid)
library(lmtest)
library(caret)
library(tidyr)

eng_df <- read.csv("engagement_rate.csv")
head(eng_df)
summary(eng_df)

eng_df$Date <- as.Date(eng_df$Date, format= "%m/%d/%Y")
str(eng_df)

complete_dates <- data.frame(Date = seq(min(eng_df$Date), max(eng_df$Date), by = "1 day"))
df <- merge(complete_dates, eng_df, all.x = TRUE)
head(df)

# Fill missing values with mean for Daily_Average
df$Daily_Average[is.na(df$Daily_Average)] <- mean(eng_df$Daily_Average, na.rm = TRUE)
head(df)
tail(df)

str(df)

#First let's check if we have any missing values
par(mfrow=c(1,1), mar = c(4, 4, 2, 1))
missmap(df,main = "Missing Values",col = c('light blue','navy'),x.cex = 1.5)
#no missing values

#Plot the daily avg. engagement rate
ts_plot_1 <- ggplot(df, aes(Date,Daily_Average)) + geom_line(na.rm=TRUE) + xlab("Day") + ylab("Average Engagement Rate") + stat_smooth(colour = "green")
ts_plot_1
#We can see that there is some seasonality in the data

#Clean the data from any outliers
data <- ts(df, frequency=7)
data <- data[, -1]
data

df$Daily_Average <- tsclean(data)
ts_plot_2 <- ggplot(df, aes(Date,Daily_Average)) + geom_line(na.rm=TRUE) + xlab("Day") + ylab("Average Engagement Rate") + stat_smooth(colour = "green")
ts_plot_2

#Compare both cleaned and uncleaned plots
grid.arrange(ts_plot_1, ts_plot_2,ncol=1, top = textGrob("Uncleaned vs Cleaned Series"))
par(mfrow= c(1,1))

# Decompose the time series
decomposition <- stl(data, s.window="periodic")
plot(decomposition)

adf.test(data)
#The data is stationary, p < 0.05

length_ts <- length(data)
split_index <- round(length_ts * 0.8)

train <- data[1:split_index]
test <- data[(split_index + 1):length_ts]
length(train) #134
length(test) #33
cat('Length of the Training Set: ', length(train), '\nLength of the Testing set: ', length(test))

#ACF and PACF
par(mfrow=c(2,1), mar = c(5, 5, 2, 1))
acf_result <- acf(train, lag.max= 30)
pacf_result <- pacf(train, lag.max= 30)

ndiffs(train) # 1 differentiation

auto_model <-  auto.arima(train, d = 1,include.mean=FALSE, seasonal=TRUE, method="CSS-ML")
summary(auto_model) #AIC: -1906
t_stat(auto_model)
Box.test.2(auto_model$residuals,nlag=c(6,12,18,24,30,36),type="Ljung-Box",decim=5) # Null hypothesis is not rejected, no autocorrelation

test_model <- arima(train, order=c(2,0,3))
summary(test_model)
AIC(test_model)
t_stat(test_model)
Box.test.2(test_model$residuals,nlag=c(6,12,18,24,30,36),type="Ljung-Box",decim=5) # Null hypothesis is not rejected, no autocorrelation

plot(residuals(test_model))


par(mfrow=c(2,1))
acf(residuals(test_model))
pacf(residuals(test_model))
par(mfrow=c(1,1))


res <- residuals(test_model)
plot(res,type='o')
abline(a=0,b=0)

par(mfrow=c(1,1))
qqnorm(res)
qqline(res)

jarque.bera.test(res) # Normal Distribution

#Predict
pred_values <- forecast(test_model, h = length(test))

par(mfrow=c(2,1), mar = c(4, 4, 2, 1))
plot(pred_values,showgap = FALSE)
plot(df$Daily_Average)

#Future
retrained_model <- Arima(df$Daily_Average, order = c(5,1,0))

pred_model= forecast(retrained_model,h=20,level=95)
pred_model
plot(pred_model)
########################################
