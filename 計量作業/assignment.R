install.packages("openxlsx")
install.packages("car")
install.packages("lmtest")
library(openxlsx)
library(car)
library(lmtest)

#讀取資料 建立交乘項變數
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
#建立基本MLR模型
model1 <- lm(attendance ~  temper + humidty + vacation ,data = data)
summary(model1)
bptest(model1)
print(vif(model1))

# 加入交乘項
model2 <- lm(attendance ~ th + hr2 + vacation ,data = data)
summary(model2)
bptest(model2)
print(vif(model2))

#加入非線性關係項
model3 <- lm(attendance ~ tt + humidty + vacation ,data = data)
summary(model3)
bptest(model3)
print(vif(model3))


#資料標準化
row_indices <- c(2, 3, 4 ,5, 6, 7 ,8 ) 
df <- data[,row_indices ]
scaled_df <- as.data.frame(scale(df))

#建立標準化基本MLR模型
model4 <- lm(attendance ~  temper + humidty + vacation ,data = scaled_df )
summary(model4)
bptest(model4)
print(vif(model4))

# 加入標準化交乘項
model5 <- lm(attendance ~ th + hr2 + vacation ,data = scaled_df)
summary(model5)
sbptest(model5)
print(vif(model5))

#加入標準化非線性關係項
model6 <- lm(attendance ~ tt + humidty + vacation ,data = scaled_df)
summary(model6)
bptest(model6)
print(vif(model6))


#畫殘差圖(model再自己改數字)
dev.new()
par(mar = c(5, 4, 4, 2) + 0.1)
residuals <- residuals(model3)
print(shapiro.test(residuals))
qqnorm(residuals)
qqline(residuals)

#模型一推導過程
#首先先弄出全變數的MLR模型和各變數SLR
model7 <- lm(attendance ~  temper + humidty + rainfall + rainday + sunshine + vacation ,data = data)
summary(model7)
model8 <- lm(attendance ~  temper  ,data = data)
summary(model8)
model9 <- lm(attendance ~  humidty  ,data = data)
summary(model9)
model10 <- lm(attendance ~  rainfall  ,data = data)
summary(model10)
model11 <- lm(attendance ~  rainday  ,data = data)
summary(model11)
model12 <- lm(attendance ~  sunshine  ,data = data)
summary(model12)
model13 <- lm(attendance ~  vacation  ,data = data)
summary(model13)
#發現temper,rainfall,rainday 都蠻顯著的，於是以這三個開始進行stepwise
#stepwise後發現給定溫度的情況下，rainfall和rainday的解釋能力幾乎一樣，rainday又更好一些
#於是選用temper+rainday，接下來開始逐漸丟入其他變數，發現丟入hunidty時，temper和humidty超級顯著
#rainday反而變的不顯著，經bptest後發現存在異質性問題，pvalue達到0.1132，但沒到很大，於是以temper和humidty開始stepwise
#發現再加入rainfall和sunshine時基本上也都不顯著，只有在加入vacation時顯著，故選用temper,humidty,vacaction,作為解釋變數
#在5%的顯著水準下皆有通過殘差常態檢定(Shapiro-Wil test)，異質性檢定(bptest)，貢獻性檢定(vif<5)
#我還順便畫了個殘差分布的qq圖，他殘差真的常態
#結論：綁住temper和humidty後，rainfall,rainday,sunshine解釋能力近乎為0，只有vacation仍有影響力
#此模型解釋能力0.2672，異質檢定pvalue0.04
model14 <- lm(attendance ~  temper +  humidty + vacation,data  = data)
summary(model14)
bptest(model14)
print(vif(model14))
residuals <- residuals(model14)
result <- shapiro.test(residuals)
print(result)
dev.new()
par(mar = c(5, 4, 4, 2) + 0.1)
qqnorm(residuals)
qqline(residuals)

#模型二推導過程
#雖然第一個模型推倒過程看起來蠻合理的，但我懷疑變數間有交互作用，於是就決定加入交乘項來測試
#在第一個模型中得到temper和humidty同時出現時使其他變數解釋能力接近0這個結論，於是第一個模型基礎上加入倆著相乘
#發現出現共線性問題(vif爆開)(只要temper和humidty存在時加入有關他們兩個的交乘項vif都會爆開)
#於是便在模型一的基礎上加入其他無關temper和humidty的交乘項(不包含vacation)，發現只要temper和humidty存在的情況下
#加入任何交乘項幾乎不會有任何改善(都不顯著)，於是決定刪除這兩個變數，探討是否可只用交乘項(氣候的資料)來解釋
#最後發現th + hr1 + vacation這個搭配解釋能力最高0(.27)，但異質檢定的PVALUE0.09
#th + hr2+ vacation這個搭配解釋能力稍低(0.25)，但都能在5%顯著水準拒絕異質性
#結論：看bptest顯著水準來選擇使用哪個模型，若為5%，模型一較好(解釋能力優於0.25)
#若為10%，則模型二較好，解釋能力為0.27(優於0.267)，
model15 <- lm(attendance ~  hr2 + th + vacation,data  = data)
summary(model15)
bptest(model15)
print(vif(model15))

#模型三推導過程
#這真的沒啥推導過程，純粹通靈，就把模型一的各自變數平方後開始stepwise，最後發現temper改成tt時
#解釋能力提高許多(0.03)，並且還都通過異質性和共線性檢定，於是我宣布temper和ateendance存在非線性關係
#同時宣布此模型為本報告最佳模型
model16 <- lm(attendance ~  temper + tt + humidty + vacation,data  = data)
summary(model16)
bptest(model16)
print(vif(model16))

#總結論
#(1)綁住temper和humidty後，其他氣候相關變數影響能力近乎為0，可能是因為有了溫度和濕度後，
#就能夠解釋降雨量和降雨天數，以至於這兩個變數沒有提供額外的解釋能力
#(2)sunshine跟這個情況無關，不論在那個情況下，sunshine都沒什麼解釋能力
#(3)temper和attendance可能存在非線性關係，把temper改成tt後r aquared暴增了0.3

th3 <- data$temper+data$humidty
model17 <- lm(attendance ~  th3 + rainday ,data  = data)
summary(model17)
bptest(model17)
print(vif(model17))

print(cor(th3,data$rainfall))




