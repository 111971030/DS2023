# 依據github上的model.R的檔案進行修改
# 新增預測新乘客是否存活的函數
# 刪除其他暫時不需要的函數

if(!require('tidyverse')) {
    install.packages('tidyverse')
    library('tidyverse')
    suppressPackageStartupMessages(library(tidyverse))
}
if(!require('caret')) {
    install.packages('caret')
    library('caret')
}
if(!require('e1071')) {
    install.packages('e1071')
    library('e1071')
}
if(!require('rpart')) {
    install.packages('rpart')
    library('rpart')
}


# library(tidyverse)
# library(caret)
# library('e1071')
# library(rpart)

#=========== 訓練模型 =========== 
TrainingModel <- function(model_name, train_data)
{
    # Set up the trainControl object for 5-fold cross-validation
    ctrl <- trainControl(method = "cv", number = 5, savePredictions = "all", classProbs = TRUE)
    if(model_name == "Logistic regression"){
        Training_model <- train(Survived ~ ., data = train_data, method = "glm", trControl = ctrl, family = "binomial")
    } else if(model_name == "K nearest neighbors"){
        Training_model <- train(Survived ~ ., data = train_data, method = "knn", trControl = ctrl, preProcess = c("center", "scale"),tuneLength = 3)
    } else if(model_name == "SVC Linear"){
        Training_model <- train(Survived ~ ., data = train_data, kernel = "svmLinear", trControl = ctrl, scale = FALSE)
    } else if(model_name == "SVC RBF"){
        Training_model <- train(Survived ~ ., data = train_data, kernel = "svmRadial", trControl = ctrl, scale = FALSE)
    } else if(model_name == "Gaussian Naive Bayes"){
        Training_model <- train(Survived ~ ., data = train_data, kernel = "naiveBayes", trControl = ctrl)
    } else if(model_name == "Decision Tree"){
        Training_model <- train(Survived~., data = train_data, method = 'rpart', trControl = ctrl)
    } else if(model_name == "Random Forest Classifier"){
        Training_model <- train(Survived~., data = train_data, method = 'rf', trControl = ctrl)
    }

    # 預測訓練集
    res_predictions <- predict(Training_model, newdata = train_data)
    res_accuracy <- sum(res_predictions == train_data$Survived) / nrow(train_data)
    res_cm <- table(res_predictions,train_data$Survived)

    ## Print the results
    message(sprintf("======= Load %s Model successfully =======", model_name))
    # cat("使用training data進行預測的準確率：", res_accuracy , '\n')
    # print(res_cm)
    # cat('\n')
    return(Training_model)
}


predicted_survival <- function(model, newpassenger, train_data)
{
    # 資料標準化
    newpassenger_scale <- process_newpassenger(train_data, newpassenger)
    # 使用訓練好的模型進行預測
    prediction <- predict(model, newdata = newpassenger_scale)
    message(sprintf("Hi %s. Will you survive ? ======= %s",newpassenger$Name, prediction))
    return(prediction)
}