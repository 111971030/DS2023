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

head(Titanic_train_subset_scale)
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

#=========== 訓練模型 =========== 
# Logistic regression
library(caret)

# 設定5-fold
lr_ctrl <- trainControl(method="cv", 
                        number=5, 
                        savePredictions="all",
                        classProbs=TRUE)

logistic_model <- train(Survived ~ ., data = res$train, method="glm", family = "binomial", trControl = lr_ctrl)

# 預測訓練集
train_pred <- predict(logistic_model, newdata = res$train)
lr_train_predicted <- predict(logistic_model, newdata = res$train)
# 概率轉換為類別
lr_train_predicted_class <- ifelse(lr_train_predicted > 0.5, 1, 0)
# 準確率
lr_train_accuracy <- sum(lr_train_predicted_class == res$train$Survived) / nrow(res$train)
cat("Logistic Regression training data Accuracy:",lr_train_accuracy,"\n")
# Confusion matrix
lr_train_confusion_matrix <- table(Actual = res$train$Survived, Predicted = lr_train_predicted_class)
cat("Logistic Regression training data Confusion Matrix:\n")
print(lr_train_confusion_matrix)

# 預測驗證集
lr_validate_predicted <- predict(logistic_model, newdata = res$validate)
# 概率轉換為類別
lr_validate_predicted_class <- ifelse(lr_validate_predicted > 0.5, 1, 0)
# 準確率
lr_validate_accuracy <- sum(lr_validate_predicted_class == res$validate$Survived) / nrow(res$validate)
cat("Logistic Regression validation data Accuracy:",lr_validate_accuracy,"\n")
# Confusion matrix
lr_validate_confusion_matrix <- table(Actual = res$validate$Survived, Predicted = lr_validate_predicted_class)
cat("Logistic Regression validation data Confusion Matrix:\n")
print(lr_validate_confusion_matrix)

# 預測測試集
lr_test_predicted <- predict(logistic_model, newdata = res$test)
# 概率轉換為類別
lr_test_predicted_class <- ifelse(lr_test_predicted > 0.5, 1, 0)
# 準確率
lr_test_accuracy <- sum(lr_test_predicted_class == res$test$Survived) / nrow(res$test)
cat("Logistic Regression test data Accuracy:",lr_test_accuracy,"\n")
# Confusion matrix
lr_test_confusion_matrix <- table(Actual = res$test$Survived, Predicted = lr_test_predicted_class)
cat("Logistic Regression test data Confusion Matrix:\n")
print(lr_test_confusion_matrix)

#========================
# K nearest neighbors
library(caret)

# 設定5-fold
knn_ctrl <- trainControl(method="cv", 
                         number=5, 
                         savePredictions="all",
                         classProbs=TRUE)

#設定KNN的K值列表
knn_list <- c(3, 5, 7, 9, 11)

#knn_result_list <- list()
knn_result_list <- vector("list", length(knn_list))

for (i in 1:length(knn_list)) {
  cat("當K = ", knn_list[i], "\n")
  
  knn_model <- train(
    Survived ~ .,  # 使用所有特徵變數進行預測
    data = res$train,
    method = "knn",
    trControl = knn_ctrl,
    preProcess = c("center", "scale"),
    tuneLength = knn_list[i]  # 设置调参范围，例如k的取值范围
  )
  
  # 預測訓練集
  train_predicted <- predict(knn_model, newdata = res$train)
  # 概率轉換為類別
  train_predicted_class <- ifelse(train_predicted > 0.5, 1, 0)
  # 準確率
  train_accuracy <- sum(train_predicted_class == res$train$Survived) / nrow(res$train)
  # Confusion matrix
  train_confusion_matrix <- table(Actual = res$train$Survived, Predicted = train_predicted_class)
  
  # 預測驗證集
  validate_predicted <- predict(knn_model, newdata = res$validate)
  # 概率轉換為類別
  validate_predicted_class <- ifelse(validate_predicted > 0.5, 1, 0)
  # 準確率
  validate_accuracy <- sum(validate_predicted_class == res$validate$Survived) / nrow(res$validate)
  # Confusion matrix
  validate_confusion_matrix <- table(Actual = res$validate$Survived, Predicted = validate_predicted_class)
  
  # 預測測試集
  test_predicted <- predict(knn_model, newdata = res$test)
  # 概率轉換為類別
  test_predicted_class <- ifelse(test_predicted > 0.5, 1, 0)
  # 準確率
  test_accuracy <- sum(test_predicted_class == res$test$Survived) / nrow(res$test)
  # Confusion matrix
  test_confusion_matrix <- table(Actual = res$test$Survived, Predicted = test_predicted_class)
  
  # 記錄預測訓練及的準確值
  knn_result_list[[i]] <- list(k = knn_list[i],
                               train_accuracy = train_accuracy,
                               train_cf_table = train_confusion_matrix,
                               validate_accuracy = validate_accuracy,
                               validate_cf_table = validate_confusion_matrix,
                               test_accuracy = test_accuracy,
                               test_cf_table = test_confusion_matrix)
}

# 找到KNN列表中的最高準確率和其索引
max_index <- which.max(sapply(knn_result_list, function(x) x$train_accuracy))
best_k <- knn_result_list[[max_index]]$k
cat("KNN with k=", best_k," in k list",knn_list, " got best training accuracy.","\n")
cat("KNN training data Accuracy:",knn_result_list[[max_index]]$train_accuracy,"\n")
cat("KNN training data Confusion Matrix:\n")
print(knn_result_list[[max_index]]$train_cf_table)

cat("KNN validation data Accuracy:",knn_result_list[[max_index]]$validate_accuracy,"\n")
cat("KNN validation data Confusion Matrix:\n")
print(knn_result_list[[max_index]]$validate_cf_table)

cat("KNN test data Accuracy:",knn_result_list[[max_index]]$test_accuracy,"\n")
cat("KNN test data Confusion Matrix:\n")
print(knn_result_list[[max_index]]$test_cf_table)

