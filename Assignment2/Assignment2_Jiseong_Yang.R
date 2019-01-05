# Get and load packages
install.packages("caTools")
install.packages("car")
install.packages("knitr")
install.packages("kableExtra")
library(caTools)
library(car)
library(knitr)
library(kableExtra)

# Initialze the envrionmnet
rm(list=ls())

# Set the working directory.
wd = "C:/Users/Jiseong Yang/Documents/Jiseong Yang/Scholar/Semesters/4-2/Data Science and R/Assignment/Assignment2"
setwd(wd)
getwd()

# Read in the dataset.
data = read.csv("wage.csv", header = T, as.is = T)
data$X = NULL

# Descriptive statistics
head(data)
summary(data)
str(data)

## 1. 단순회귀분석
### (1) 임금(wage)를 종속변수, 연령(age)를 독립변수로 하는 단순 회귀 분석을 실시하고 해당 명령문 script를 별도의 box로 처리하여 보고
# Model without scaling
mod_sim = lm(wage ~ age, data = data)

### (2) 단순회귀분석의 결과를 이용하여 다음 질문에 대한 답 제시
summary(mod_sim)

#### 가. 모형의 설명력을 나타내는 통계량의 값을 제시하고 그 결과 해석

#### 나. 임금에 대한 연령이 어떤 효과성이 있는지 설명

## 2. 다중회귀분석
### (1) 임금(wage)를 종속변수, 연령(age)과 경력(tenure)을 독립변수로 하는 다중 회귀 분석을 실시하고 해당 명령문 script를 별도의 box로 처리하여 보고
# Model without scaling
mod_mul = lm(wage ~ age + tenure, data = data)
summary(mod_mul)

### (2) 임금에 대한 연령과 경력이 어떤 효과성이 있는지 설명

## 3. 단순회귀분석과 다중회귀분석 간 모형의 비교
### (1) 단순회귀분석과 다중회귀분석 결과를 각기 다른 객체로 저장해서 두 모형이 임금에 대해 가지고 있는 설명력(분산)에 대한 검증 실시
# ANOVA
anova(mod_sim, mod_mul)

### (2) 검증결과를 토대로 단순회귀분석과 다중회귀분석 중 어떤 모형이 더 유의미한지 설명
# Randomly split the dataset
train_test = sample.split(data, SplitRatio = 0.8)
train = subset(data, train_test == TRUE)
test = subset(data, train_test == FALSE) 

# Train the model
mod_sim_train = lm(wage ~ age, data = train)
summary(mod_sim_train)
mod_mul_train = lm(wage ~ age*tenure, data = train)
summary(mod_mul_train)

# Predict the model
y_pred_sim = predict(mod_sim_train, test)
y_pred_mul = predict(mod_mul_train, test)
model_eval = data.frame("y_pred_sim"=y_pred_sim,
                        "y_pred_mul"=y_pred_mul, 
                        "actual"=test$wage)

# Get the mean squre error
mse_sim = round(mean((model_eval$actual - model_eval$y_pred_sim)^2), 4)
mse_mul = round(mean((model_eval$actual - model_eval$y_pred_mul)^2), 4)
mse=cbind(mse_sim, mse_mul)
mse=as.data.frame(mse)

# Create a table for the comparison.
stats_col_names = c("독립변수 개수","유의미한 독립변수", "t값", "t값 유의확률", "결정계수", "수정된 결정계수", "F값", "F값 유의확률", "MSE")
stats_row_names = c("단순회귀모델", "다중회귀모델")
stats_sim = c(1, "연령(age)", 5.661, 2.58e-08, 0.0619, 0.0599, 32.05, 2.578e-08, mse$mse_sim) 
stats_mul = c(2, "경력(tenure)", 2.793, 0.00542, 0.2033, 0.1983, 41.16, 2.2e-16, mse$mse_mul)
stats = rbind(stats_sim, stats_mul)
colnames(stats) = stats_col_names
rownames(stats) = stats_row_names
stats = as.data.frame(stats)
kable(stats) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T, font_size = 11)
```
* 단순회귀모델와 다중회귀모델은 독립변수의 개수가 각각 1개와 2개로 차이가 있으나, t검정 결과 유의미한 독립변수는 두 모델 모두 한 개이다. 
* 유의 수준 0.001에서 단순회귀모델의 유의미한 독립변수(연령)은 유의하나, 다중회귀모델의 유의미한 독립변수(경력)은 그렇지 않다. 그러나 유의수준 0.01에서는 모두 유의하다. 
* F값과 그 유의확률을 보았을 때 두 모델 모두 독립변수의 영향력이 0이라고 가정한 상수모델보다는 유의미한 것으로 확인된다.
* 결정계수와 수정된 결정계수를 보았을 때 다중회귀모델은 단순회귀모델보다 3배 이상의 높은 설명력을 가지고 있다. 
* 8:2의 비율로 훈련데이터와 검정데이터를 분할한 뒤 테스트를 하여 계산된 제곱합 평균(Mean Squred Error, MSE)은 대체로 다중회귀모델이 단순회귀모델보다 낮다. 
* 결론
* 다중회귀모델이 단순회귀모델보다 독립변수가 1개가 더 많지만 모든 변수들이 유의미한 것은 아니기 때문에 모델을 선택하기에 앞서 다른 측면도 고려해야한다. 
* 선형회귀모델이 더 적은 변수를 가지고 원본 데이터를 설명하기 때문에 더 효율적이라고 볼 수도 있을 것이다. 
* 그러나 분산분석 결과와 결정계수 및 제곱합 평균(MSE) 등을 종합적으로 고려하였을 때, 다중회귀모델이 더욱 유의하고 원본 데이터를 더 잘 설명한다고 할 수 있다. 

