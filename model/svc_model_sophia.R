if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

Titanic <- read_csv("Titanic/train.csv")

## 資料清理
# 0 確認資料幾列幾欄
dimensions <- dim(Titanic)
num_rows <- dimensions[1]  # 行数
num_cols <- dimensions[2]  # 列数
cat("row:",num_rows,"\n","col:",num_cols,"\n")
# row: 891 
# col: 12 

# 1. 有無缺失值
# count NA values in each column
sapply(Titanic, function(x) sum(is.na(x)))
# PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch      Ticket        Fare   Cabin    Embarked
#           0           0           0           0           0         177           0           0           0           0    687           2 
# ----> 觀察出 Age 、 Cabin 與 Embarked 三個欄位都有缺失值，其中 Cabin 缺失值高達 687 筆。


# 2. 進行處裡 以及 把不必要欄位 drop 掉
# 目前欄位中， PassengerId、Name、Ticket(票號)，沒有要拿來分析 且 Cabin (房間號嗎) 缺失值過多，因此選擇直接 將此欄刪除。
# Drop the columns
Titanic_train <- subset(Titanic, select = -c(PassengerId, Name, Ticket, Cabin))
# Remove the rows with missing values
Titanic_train <- Titanic_train[complete.cases(Titanic_train[,c("Embarked","Age")]),]

# 2.1 來看一下處理完剩下幾列幾欄(特徵)
dimensions <- dim(Titanic_train)
num_rows <- dimensions[1]  # 行数
num_cols <- dimensions[2]  # 列数
cat("row:",num_rows,"\n","col:",num_cols,"\n")
# Rows: 712 (data)
# Columns: 8 (feature)
# $ Survived <dbl> 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, …
# $ Pclass   <dbl> 3, 1, 3, 1, 3, 1, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 3, 2, 2, 3, 1, 3, 3, 1, 1, 2, 1, 1, 3, 3, 3, 3, 2, 2, …
# $ Sex      <chr> "male", "female", "female", "female", "male", "male", "male", "female", "female", "female", "female", …
# $ Age      <dbl> 22, 38, 26, 35, 35, 54, 2, 27, 14, 4, 58, 20, 39, 14, 55, 2, 31, 35, 34, 15, 28, 8, 38, 19, 40, 66, 28…
# $ SibSp    <dbl> 1, 1, 0, 1, 0, 0, 3, 0, 1, 1, 0, 0, 1, 0, 0, 4, 1, 0, 0, 0, 0, 3, 1, 3, 0, 0, 1, 1, 0, 2, 1, 1, 1, 1, …
# $ Parch    <dbl> 0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 0, 5, 0, 0, 1, 0, 0, 0, 0, 0, 1, 5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, …
# $ Fare     <dbl> 7.2500, 71.2833, 7.9250, 53.1000, 8.0500, 51.8625, 21.0750, 11.1333, 30.0708, 16.7000, 26.5500, 8.0500…
# $ Embarked <chr> "S", "C", "S", "S", "S", "S", "S", "S", "C", "S", "S", "S", "S", "S", "S", "Q", "S", "S", "S", "Q", "S…
# ----> 目前資料有 712 列 與 8 個欄位 (特徵)


# 編碼
# 3. 看一下這 8 個欄位(特徵) 資料型態， 因為模型裡只能放入數字型資料，所以稍微觀察一下。
str(Titanic_train)
# tibble [712 × 8] (S3: tbl_df/tbl/data.frame)
# $ Survived: num [1:712] 0 1 1 1 0 0 0 1 1 1 ...
# $ Pclass  : num [1:712] 3 1 3 1 3 1 3 3 2 3 ...
# $ Sex     : chr [1:712] "male" "female" "female" "female" ...
# $ Age     : num [1:712] 22 38 26 35 35 54 2 27 14 4 ...
# $ SibSp   : num [1:712] 1 1 0 1 0 0 3 0 1 1 ...
# $ Parch   : num [1:712] 0 0 0 0 0 0 1 2 0 1 ...
# $ Fare    : num [1:712] 7.25 71.28 7.92 53.1 8.05 ...
# $ Embarked: chr [1:712] "S" "C" "S" "S" ...
# ----> 觀察出 ‘Sex’ 與 ‘Embarked’ 是非數值型資料，因此我們需進行編碼。


# 3.1 觀察這兩欄，裡頭有哪些值。
unique(Titanic_train[c("Sex")])
# A tibble: 2 × 1
# Sex   
# <chr> 
# 1 male  
# 2 female
Titanic_train$Sex_factors <- as.numeric(factor(Titanic_train$Sex))
# ‘Sex’ 欄位有 [‘male’, ‘female’] - > ‘Sex_factors’ 欄位 [2, 1]

unique(Titanic_train[c("Embarked")])
# A tibble: 3 × 1
# Embarked
# <chr>   
# 1 S       
# 2 C       
# 3 Q   
Titanic_train$Embarked_factors <- as.numeric(factor(Titanic_train$Embarked))
# ‘Embarked’ 欄位有 [‘S’, ‘C’,‘Q’] - > [3,1,2]
Titanic_train_subset <- subset(Titanic_train, select = -c(Sex, Embarked))

str(Titanic_train_subset)
# tibble [712 × 8] (S3: tbl_df/tbl/data.frame)
# $ Survived        : num [1:712] 0 1 1 1 0 0 0 1 1 1 ...
# $ Pclass          : num [1:712] 3 1 3 1 3 1 3 3 2 3 ...
# $ Age             : num [1:712] 22 38 26 35 35 54 2 27 14 4 ...
# $ SibSp           : num [1:712] 1 1 0 1 0 0 3 0 1 1 ...
# $ Parch           : num [1:712] 0 0 0 0 0 0 1 2 0 1 ...
# $ Fare            : num [1:712] 7.25 71.28 7.92 53.1 8.05 ...
# $ Sex_factors     : num [1:712] 2 1 1 1 2 2 2 1 1 1 ...
# $ Embarked_factors: num [1:712] 3 1 3 3 3 3 3 3 1 3 ...
# ----> 目前欄位都為數值型資料了!!! 這樣待會就可以順利匯入模型，進行預測了。

# 資料標準化(standardization)
# 因為在資料中，不同資料欄位與資料值所組成，他們分佈狀況可能都不盡相同，因此，就必須將特徵資料按比例縮放，讓資料落在某一特定的區間。
Titanic_train_subset_scale <- Titanic_train_subset
Titanic_train_subset_scale[2 : 8] <- as.data.frame(scale(Titanic_train_subset_scale[2 : 8]))
print("____")
print(head(Titanic_train_subset_scale))

# 確認scale後的資料
# A tibble: 6 × 8
# Survived Pclass    Age  SibSp  Parch   Fare Sex_factors Embarked_factors
# <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>       <dbl>            <dbl>
#   1        0  0.908 -0.527  0.522 -0.506 -0.516       0.756            0.519
# 2        1 -1.48   0.577  0.522 -0.506  0.694      -1.32            -2.05 
# 3        1  0.908 -0.251 -0.552 -0.506 -0.503      -1.32             0.519
# 4        1 -1.48   0.370  0.522 -0.506  0.350      -1.32             0.519
# 5        0  0.908  0.370 -0.552 -0.506 -0.501       0.756            0.519
# 6        0 -1.48   1.68  -0.552 -0.506  0.327       0.756            0.519


#固定random資料
set.seed(1)
# 將現有資料 切分 80 % 作為訓練資料集 10 % 為 測試資料集 10 % 為 驗證資料集
spec = c(train = .8, test = .1, validate = .1)

Titanic_train_sample = sample(cut(
  seq(nrow(Titanic_train_subset_scale)), 
  nrow(Titanic_train_subset_scale)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(Titanic_train_subset_scale, Titanic_train_sample)


## SVC 建模>>>>>>>>>>>>>
# 安裝所需套件e1071
 if(!require('e1071')) {
   install.packages('e1071')
   library('e1071')
 }

# SVC_Linear_Model、SVC_RBF_Model
TrainingModel_Sophia <- function(model_name, data)
{
  if(model_name == "linear")
  {
    kernel = "linear"
    #message("建立 SVC Linear 模型並訓練模型")
  }
  else if (model_name == "RBF")
  {
    kernel = "radial"
    #message("建立 SVC RBF 模型並訓練模型")
  }
  # 目標因子
  data$train$Survived <- as.factor(data$train$Survived)
  # 1. 模型建模 function(SVC_Linear、SVC_RBF)
  model <- svm(Survived ~ ., 
               data = data$train, 
               kernel = kernel,
               scale = FALSE
  )
  # 2. 預測訓練資料集：
  train_predictions <- predict(model, newdata = data$train)
  # 2.1. 準確率(Accuracy)
  train_accuracy <- sum(train_predictions == data$train$Survived) / nrow(data$train)
  # 2.2. 混淆矩陣(Confusion matrix)
  train_cm = table(train_predictions,data$train$Survived)

  # 3. 預測測試資料集：
  test_predictions <- predict(model, newdata = data$test)
  # 3.1. 準確率(Accuracy)
  test_accuracy <- sum(test_predictions == data$test$Survived) / nrow(data$test)
  # 3.2. 混淆矩陣(Confusion matrix)
  test_cm = table(test_predictions,data$test$Survived)

  # 4. 預測驗證資料集：
  validate_predictions <- predict(model, newdata = data$validate)
  # 4.1. 準確率(Accuracy)
  validate_accuracy <- sum(validate_predictions == data$validate$Survived) / nrow(data$validate)
  # 4.2. 混淆矩陣(Confusion matrix)
  validate_cm = table(validate_predictions,data$validate$Survived)

  # 回傳
  return_list = list(model = model, 
                     train_accuracy = train_accuracy, train_cm = train_cm,
                     test_accuracy = test_accuracy, test_cm = test_cm,
                     validate_accuracy = validate_accuracy, validate_cm = validate_cm
                    )
  return(return_list)
}

## 印出SVC Linear結果>>>>>>>>>>>>>
message("======= SVC Linear Model =======")
SVC_Linear_Model = TrainingModel_Sophia("linear",res)
# 預測訓練資料集(train)
cat("使用train data進行預測的準確率：", SVC_Linear_Model$train_accuracy)
print(SVC_Linear_Model$train_cm)
# 預測測試資料集(test)
cat("使用test data進行預測的準確率：", SVC_Linear_Model$test_accuracy)
print(SVC_Linear_Model$test_cm)
# 預測驗證資料集(validate)
cat("使用validate data進行預測的準確率：", SVC_Linear_Model$validate_accuracy)
print(SVC_Linear_Model$validate_cm)


## 印出SVC RBF結果>>>>>>>>>>>>>
message("======= SVC RBF Model =======")
SVC_RBF_Model = TrainingModel_Sophia("RBF",res)
# 預測訓練資料集(train)
cat("使用train data進行預測的準確率：", SVC_RBF_Model$train_accuracy)
print(SVC_RBF_Model$train_cm)
# 預測測試資料集(test)
cat("使用test data進行預測的準確率：", SVC_RBF_Model$test_accuracy)
print(SVC_RBF_Model$test_cm)
# 預測驗證資料集(validate)
cat("使用validate data進行預測的準確率：", SVC_RBF_Model$validate_accuracy)
print(SVC_RBF_Model$validate_cm)