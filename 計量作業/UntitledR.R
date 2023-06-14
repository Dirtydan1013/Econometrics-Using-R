library(openxlsx)
library(car)
library(lmtest)

data <- read.xlsx("dataset.xlsx")
data$date <- convertToDate(data$date, origin = "1900/01/01")
data$date <- format(data$date, "%Y/%m")
data <- data[-c(126,127),] #刪除動物園入園人次為0的兩列資料

rr <- data$rainfall*data$rainday
th <- data$temper*data$humidty
tt <- (data$temper)^2
hh <- (data$humidty)^2
tr1 <- data$rainday*data$temper
tr2 <- data$rainfall*data$temper
vv <- (data$vacation)^2
hr1 <- data$rainday*data$humidty
hr2 <- data$rainfall*data$humidty

model1 <- lm(attendance ~  temper + th + vacation ,data = data)
summary(model1)
bptest(model1)
print(vif(model1))
print(AIC(model1))
print(BIC(model1))

model2 <- lm(attendance ~  tt + humidty + vacation ,data = data)
summary(model2)
bptest(model2)
print(vif(model2))
print(AIC(model2))
print(BIC(model2))


model3 <- lm(attendance ~  th + hr2 + vacation ,data = data)
summary(model3)
bptest(model3)
print(vif(model3))
print(AIC(model3))
print(BIC(model3))
