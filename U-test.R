# 读取数据
data <- read.table("C:/Users/mzs/Desktop/1/feature.txt", header = TRUE)
groups <- data[, 2] # 获取分组标签
features <- data[, -c(1, 2)] # 获取所有特征值

# 进行Mann-Whitney U检验
p_values <- apply(features, 2, function(x) wilcox.test(x ~ groups, exact = FALSE)$p.value)

# 将所有特征和对应的p值输出为txt列表
output_data <- cbind(names(features), p_values)
write.table(output_data, file = "C:/Users/mzs/Desktop/1/p_values.txt", sep = "\t", quote = FALSE, row.names = FALSE)

# 将所有特征及p值输出为filtered_feature.txt
output_data_filtered <- cbind(names(features)[p_values < 0.05], p_values[p_values < 0.05])
write.table(output_data_filtered, file = "C:/Users/mzs/Desktop/1/filtered_feature.txt", sep = "\t", quote = FALSE, row.names = FALSE)
