# install.packages("tidyverse")
# install.packages("DT")

if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
if(!require('DT')) {
  install.packages('DT')
  library('DT')
}
library(tidyverse)
library(DT)
library(ggplot2)
library(gridExtra)
library(reshape2)
source("feature.R")
# 讀取資料
df <- read_csv("Titanic/train.csv")

# 列印前 10 筆資料
head(df, 10)
# 用互動式表格呈現資料
datatable(df[,1:12])

# 計算並列印資料行列數量
dimensions <- dim(df)
num_rows <- dimensions[1]  # 行数
num_cols <- dimensions[2]  # 列数
cat("row:",num_rows,"\n","col:",num_cols,"\n")

# 觀察每個 feature 的分佈情形
vars <- c("PassengerId", "Survived", "Pclass","Age","SibSp","Parch","Fare")
summary(df[, vars])

# 觀察生存的數量
survived_value <- table(df$Survived)
print(survived_value)
death <- survived_value[1]  # 0的数量，表示死亡
survive <- survived_value[2]   # 1的数量，表示生存
# print(death)
# print(survive)

# 長條圖呈現
counts <- c(death, survive)
barplot(counts, names.arg = c("0", "1"), 
        xlab = "Survived", ylab = "Quantity",
        main = "Survived",
        col = c("brown1", "chartreuse3"))

# 觀察部分 features 與 Survived 的關係
features <- c("Pclass", "Sex", "SibSp","Parch","Embarked")
n_rows <- 2
n_cols <- 3

fig <- ggplot() +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'lines'))

plot_list <- list()

for (r in 1:n_rows) {
  for (c in 1:n_cols) {
    i <- (r - 1) * n_cols + c
    if (i <= 5) {
      plot <- ggplot(df, aes_string(x = features[i])) +
        geom_bar(aes(fill = factor(Survived)), position = 'dodge') +
        labs(title = features[i], x = NULL, y = 'Count') +
        theme(plot.title = element_text(hjust = 0.5, size = 12),
              legend.position = 'right', legend.title = element_blank())
      plot_list[[i]] <- plot
    }
  }
}
grid.arrange(grobs = plot_list, nrow = n_rows, ncol = n_cols)



# Select only numeric columns for correlation
numeric_cols <- c("Pclass","Age","SibSp","Parch","Fare","Sex_factors","Embarked_factors")
preprocess_data <- preprocess_data(df)
correlation <- cor(preprocess_data[, numeric_cols])

# Reshape correlation matrix
correlation_melt <- melt(correlation)

# Create correlation heatmap
heatmapPlot <- ggplot(data = correlation_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black") +  
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Features", y = "Features", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmapPlot)
