# install.packages("tidyverse")
# install.packages("DT")
library(tidyverse)
library(DT)
library(ggplot2)
library(gridExtra)

# 讀取資料
df <- read_csv("train.csv")

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
