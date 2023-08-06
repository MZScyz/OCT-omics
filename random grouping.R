# 加载必要的包
library(caret)

# 读取特征矩阵数据
feature <- read.table("C:/Users/mzs/Desktop/1/feature.txt", header = T, sep = "\t")

# 分离特征和标签
X <- feature[, 3:ncol(feature)]
y <- feature[, 2]

# 定义分组标签
groups <- unique(y)

# 计算每个分组的样本数
group_sizes <- table(y)

# 计算每个特征的均值和标准差
means <- apply(X, 2, mean)
sds <- apply(X, 2, sd)

# 对特征矩阵进行标准化
X_norm <- scale(X, center = means, scale = sds)

# 初始化训练集和测试集
train_index <- test_index <- integer(0)

# 对每个分组进行遍历
for (group in groups) {
  # 获取分组的样本索引
  group_index <- which(y == group)
  
  # 随机分配训练集和测试集索引
  train_test_index <- createDataPartition(group_index, p = 0.7, list = FALSE)
  
  # 将分组中的训练集和测试集索引加入到总的训练集和测试集索引中
  train_index <- c(train_index, group_index[train_test_index])
  test_index <- c(test_index, group_index[-train_test_index])
}

# 根据训练集和测试集索引提取相应的训练集和测试集
train <- feature[train_index, ]
test <- feature[test_index, ]

# 输出训练集和测试集的特征矩阵至指定路径
write.table(train, file = "C:/Users/mzs/Desktop/1/train.txt", quote = F, sep = "\t", row.names = F)
write.table(test, file = "C:/Users/mzs/Desktop/1/test.txt", quote = F, sep = "\t", row.names = F)
