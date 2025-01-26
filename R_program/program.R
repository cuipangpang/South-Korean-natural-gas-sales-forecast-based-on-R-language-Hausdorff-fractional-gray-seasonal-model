library(e1071)
library(pROC) 
library(tseries)
library(readxl)
library(XLS)
library(urca)
library(forecast)
library(ggplot2)
library(GGally)
x<-read.csv("C:\\Users\\DELL\\Desktop\\R_program\\data\\a.csv")
test_x<-read_xlsx("C:\\Users\\DELL\\Desktop\\R_program\\data\\test.xlsx")
x1<-x[,21]
test_x2<-test_x[,3]
xts<-ts(x1,start=2000,end=2020,frequency=12)
test_xts<-ts(test_x2,start=c(2020,1),end=c(2020,12),frequency=12)
#原数据可视化分析
tsdisplay(xts)
ggseasonplot(xts,year.labels=TRUE,year.labels.left=TRUE)+
  xlab("Month")+
  ylab("Sales")+
  ggtitle("Seasonal Plot: Gas Sales in Korea")+
  theme_minimal()+
  theme(text = element_text(family = "STHeiti"))
#季节变化图，极坐标
#ggseasonplot(xts, polar=TRUE) +
#  xlab("Month") +
#  ylab("Sales") +
#  ggtitle("Seasonal Plot: Gas Sales in Korea") +
#  theme_minimal() +
#  theme(text = element_text(family = "STHeiti"))
#温度与天然气散点矩阵图
#ggplot(data = x, aes(x = Temperature, y = x1)) +
#  geom_point() +
#  xlab("Temperature") +
#  ylab("Sales") +
#  ggtitle("Scatterplot: Gas Sales vs. Temperature") +
#  theme_minimal() +
#  theme(text = element_text(family = "STHeiti"))

#季节性检验
decomp <- stl(xts, s.window = "periodic")
plot(decomp)
seasonal_component <- decomp$time.series[, "seasonal"]
# 季节性部分平稳性检验
ADF_seasonal <- adf.test(seasonal_component)
ADF_seasonal  
#平稳性检验
ADF<-adf.test(xts)#单位根检验 P大于0.05存在单位根
ADF    

#差分变换，差分处理
ndiffs(xts) #判断几阶差分
nxts<-diff(xts,1)#一阶差分处理
tsdisplay(nxts,main="一阶差分图")
#PACF从13阶以后截断，大概看到（1，2，7，8，12）这五个数超出虚线甚多，所以形成(1,1,0)(2,1,0),(7,1,0),(8,1,0),(12,1,0),然后判断一下各自的AIC值，取最小值即可。
#且acf图存在一个12阶的季节性影响因素，然后通过作滞后12阶的图看一下是否消除
tsdisplay(diff(nxts,12),main="一阶差分图")
ADF1<-adf.test(nxts)#一阶差分单位根检验
ADF1#P值小于0.05，拒绝原假设，序列平稳

#白噪声检验，P值小于0.05，差分后的序列不是白噪声模型
Box.test(nxts,type = "Ljung-Box")

#模型选择fit1:ARIMA(2,0,2)(0,1,1)[12] 
fit<-auto.arima(xts)
fit 
arima<-auto.arima(xts,trace=T)
accuracy(fit) 

###=============================fit1：ARIMA模型=========================###
fit1<-arima(xts,order=c(2,0,2),seasonal=list(order=c(0,1,1),period=12))
tsdiag(fit1)#表明残差标准差基本都在[-1,1]之间，残差的自回归都为0（两虚线内），Ljung-Box检验的p值都在0.05之上，结果不错

#模型检验1：残差分析
par(mfrow = c(1, 1))
residuals <- fit1$residuals
qqnorm(resid(fit1))
qqline(residuals)
shapiro.test(residuals)
tsdisplay(residuals)#,12模型拟合后的明显特征：自相关图中延迟期数为 12 上有一个明显突起，但是没有其他明显的突起
Box.test(fit$residuals,type="Box")#残差之间是否相关，不显著，残差平稳
checkresiduals(fit1)
#模型预测
forecast_ARIMA<-forecast(fit1,36)#第一列点估计值，第二第三是80%执行上限和80%执行下限
plot(forecast_ARIMA,xlab="year",ylab="annal flow")#蓝色的是80%的执行区间，灰色是95%执行区间
lines(test_xts, col = "red")  
forecast_ARIMA


###=============================fit_ets:ETS预测==============================###
fit_ets <- ets(xts)
print(fit_ets)

# 模型检验1：残差分析
residuals_ets <- residuals(fit_ets)
par(mfrow = c(1, 1))
qqnorm(residuals_ets)
qqline(residuals_ets)
shapiro.test(residuals_ets)#p 值大于显着性水平（通常为 0.05），则无法拒绝原假设，这表明可以合理地假设残差呈正态分布。
tsdisplay(residuals_ets)
checkresiduals(residuals_ets)
Box.test(residuals_ets, type = "Box")

# ETS 模型预测
forecast_ets <- forecast(fit_ets, h = 48)
plot(forecast_ets, xlab = "year", ylab = "annal flow", main = "ETS Forecast")
lines(test_xts, col = "red")
forecast_ets

###==========================================fit2：神经网络模型预测=============###
fit2 <- nnetar(xts)
par(mfrow = c(1, 1))
residuals2 <- fit2$residuals
qqnorm(residuals2)
qqline(residuals2)
shapiro.test(residuals2)
tsdisplay(residuals2)
checkresiduals(residuals2)
Box.test(residuals2,type="Box")
# 模型预测
forecast_ARIMA <- forecast(fit, 48)
forecast_NNAR <- forecast(fit2, h = 48)
forecast_NNAR

plot(forecast_ARIMA, xlab = "year", ylab = "annal flow", main = "Model Comparison")
lines(test_xts, col = "red")
lines(forecast_NNAR$mean, col = "green")
lines(forecast_ets$mean, col = "orange")
legend("topleft",
       legend = c("ARIMA Forecast", "Test Data", "NNAR Forecast", "ETS Forecast"),
       col = c("blue", "red", "green", "orange"),
       lty = 1,
       cex = 0.8
)


#滑动窗口特征工程
#SVM需要输入样本是固定维度的，利用滑动窗口的方法来创建固定时间窗口的特征集
#使用12滑动训练窗口，时间序列数据频率为每月一次，12个时间点对应于12个月
#每个窗口内的时间序列值作为特征，窗口之后的值作为目标。使用这些特征一个SVM模型，并在测试集上进行预测
window_size <- 12
# 特征工程
data <- NULL
target <- NULL
for (i in 1:(length(xts) - window_size + 1)) {
  window <- xts[i:(i + window_size - 1)]
  data <- rbind(data, window)
  target <- c(target, tail(xts, -i)[1])
}
# 输出每个窗口提取的特征
#包含 12 个时间点的值。是单变量时间序列的窗口，每个值代表一个时间点的起始位置。
#使用窗口来构建训练集，其中每个窗口的数据特征输入到SVM模型中。
#将过去的初始化作为特征输入到SVM模型中，以预测未来的值
cat("Features for Window", i, ":\n")
print(window)
df <- data.frame(data, target)
# 分割数据集
train_size <- floor(0.8 * nrow(df))
train <- df[1:train_size, ]
test <- df[(train_size + 1):nrow(df), ]
gamma_values <- c(0.01,0.02,0.03,0.04,0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.3)
param_grid <- expand.grid(gamma = gamma_values)
tune_control <- tune.control(sampling = "cross", cross = 5)
tune_result <- tune(svm, target ~ ., data = train, kernel = "radial", ranges = param_grid, tunecontrol = tune_control)
best_gamma <- tune_result$best.parameters$gamma
summary(fit_svm)
final_model <- svm(target ~ ., data = train, kernel = "radial", gamma = best_gamma)
# 在测试集上预测
predictions_svm <- predict(final_model, newdata = test)
comparison_df <- data.frame(Actual = test$target, Predicted = predictions_svm)
comparison_df
# 计算均方根误差（RMSE）
rmse_svm <- sqrt(mean((predictions_svm - test$target)^2))
cat("最佳 Gamma 值:", best_gamma, "\n")
cat("SVM RMSE:", rmse_svm, "\n")
# 绘制
plot(test$target, type = "l", col = "red", lty = 1, lwd = 2, xlab = "年份", ylab = "年度流量", main = "SVM 预测")
lines(predictions_svm, col = "blue", lty = 2, lwd = 2)
legend("topleft", legend = c("测试数据", "SVM 预测"), col = c("red", "blue"), lty = 1:2, cex = 0.8)
# RMSE
rmse_svm <- sqrt(mean((predictions_svm - test$target)^2))
# MASE
n <- length(test$target)
mase_svm <- sum(abs(test$target - predictions_svm)) / n / mean(abs(diff(train$target)))  # Assuming train$target is the training set
# MSE
mse_svm <- mean((predictions_svm - test$target)^2)
cat("ARIMA RMSE:", rmse_ARIMA, "\n")
cat("NNAR RMSE:", rmse_NNAR, "\n")
cat("ETS RMSE:", rmse_ets, "\n")
cat("SVM RMSE:", rmse_svm, "\n")
cat("\n")
cat("ARIMA MSE:", mse_ARIMA, "\n")
cat("NNAR MSE:", mse_NNAR, "\n")
cat("ETS MSE:", mse_ets, "\n")
cat("SVM MSE:", mse_svm, "\n")
cat("\n")
cat("ARIMA MASE:", mase_ARIMA, "\n")
cat("NNAR MASE:", mase_NNAR, "\n")
cat("ETS MASE:", mase_ets, "\n")
cat("SVM MASE:", mase_svm, "\n")


#====================================#改进的SFHGM模型#=================
x <- read.csv("C:\\Users\\DELL\\Desktop\\R_program\\data\\a.csv")
test_x <- read_xlsx("C:\\Users\\DELL\\Desktop\\R_program\\data\\test.xlsx")
x1 <- x[, 21]
test_x2 <- test_x[, 3]
xts <- ts(x1, start = 2000, end = 2020, frequency = 12)
test_xts <- ts(test_x2, start = c(2020, 1), end = c(2020, 12), frequency = 12)
# 数据平滑处理+改进的 Hausdorff 分数阶累加
hausdorff_fractional_accumulation <- function(data, alpha) {
  n <- length(data)
  result <- numeric(n)
  for (i in 1:n) {
    weights <- (alpha^(0:(i - 1))) / factorial(0:(i - 1))
    result[i] <- sum(weights * data[1:i])
  }
  smooth_result <- stats::loess(result ~ seq_along(result), span = 0.2)$fitted
  return(na.omit(smooth_result))
}
# 改进的 SFHGM 模型
sfhgm_model <- function(params, data) {
  alpha <- params[1]
  beta <- params[2]
  gamma <- params[3]
  smooth_data <- hausdorff_fractional_accumulation(data, alpha)
  model <- lm(smooth_data ~ poly(seq_along(smooth_data), 5) + sin(gamma * seq_along(smooth_data)))
  predictions <- predict(model, newdata = data.frame(seq_along(data)))
  residuals <- data - predictions
  score <- sum(residuals^2) + beta * sum(abs(diff(residuals))) + 0.1 * sum(coef(model)^2)
  return(score)
}
# 混合优化算法
optimize_params <- function(data) {
  pso_result <- psoptim(
    par = c(0.5, 0.1, 2), 
    fn = function(params) sfhgm_model(params, data),
    lower = c(0.01, 0.01, 0.01),
    upper = c(2, 1, 3), 
    control = list(maxit = 50)
  )
  
  nloptr_result <- nloptr(
    x0 = pso_result$par,
    eval_f = function(params) sfhgm_model(params, data),
    lb = c(0.01, 0.01, 0.01),
    ub = c(2, 1, 3), 
    opts = list(algorithm = "NLOPT_LN_COBYLA", maxeval = 100)
  )
  return(nloptr_result$solution)
}
# 优化参数
optimal_params <- optimize_params(xts)
alpha_opt <- optimal_params[1]
beta_opt <- optimal_params[2]
gamma_opt <- optimal_params[3]
print(c("Alpha:", alpha_opt))
print(c("Beta:", beta_opt))
print(c("Gamma:", gamma_opt))
# SFHGM 预测
smooth_quarterly <- hausdorff_fractional_accumulation(xts, alpha_opt)
model <- lm(smooth_quarterly ~ poly(seq_along(smooth_quarterly), 5) + sin(gamma_opt * seq_along(smooth_quarterly)))
forecast_sfhgm <- predict(model, newdata = data.frame(seq_along(test_xts)))
forecast_sfhgm
# 使预测结果时间范围=测试集
forecast_sfhgm_ts <- ts(forecast_sfhgm, start = start(test_xts), frequency = 12)
# STL 分解季节性分解
stl_decomp <- stl(xts, s.window = "periodic")
seasonal_component <- stl_decomp$time.series[, "seasonal"]
# 使季节性成分的时间长度=预测时间
start_forecast <- start(forecast_sfhgm_ts)
end_forecast <- end(forecast_sfhgm_ts)
seasonal_component_extended <- rep(seasonal_component, length.out = length(forecast_sfhgm_ts))
forecast_sfhgm_with_seasonality <- forecast_sfhgm_ts + seasonal_component_extended
forecast_sfhgm_with_seasonality
# 创建对比图
plot(forecast_ARIMA, xlab = "Year", ylab = "Annual Flow", main = "Model Comparison")
lines(test_xts, col = "red")
lines(forecast_NNAR$mean, col = "green")
lines(forecast_ets$mean, col = "orange")
lines(forecast_sfhgm_with_seasonality, col = "purple")
legend(
  "topleft",
  legend = c("ARIMA Forecast", "Test Data", "NNAR Forecast", "ETS Forecast", "SFHGM Forecast"),
  col = c("blue", "red", "green", "orange", "purple"),
  lty = 1,
  cex = 0.8
)


