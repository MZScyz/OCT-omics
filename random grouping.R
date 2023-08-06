# ���ر�Ҫ�İ�
library(caret)

# ��ȡ������������
feature <- read.table("C:/Users/mzs/Desktop/1/feature.txt", header = T, sep = "\t")

# ���������ͱ�ǩ
X <- feature[, 3:ncol(feature)]
y <- feature[, 2]

# ��������ǩ
groups <- unique(y)

# ����ÿ�������������
group_sizes <- table(y)

# ����ÿ�������ľ�ֵ�ͱ�׼��
means <- apply(X, 2, mean)
sds <- apply(X, 2, sd)

# ������������б�׼��
X_norm <- scale(X, center = means, scale = sds)

# ��ʼ��ѵ�����Ͳ��Լ�
train_index <- test_index <- integer(0)

# ��ÿ��������б���
for (group in groups) {
  # ��ȡ�������������
  group_index <- which(y == group)
  
  # �������ѵ�����Ͳ��Լ�����
  train_test_index <- createDataPartition(group_index, p = 0.7, list = FALSE)
  
  # �������е�ѵ�����Ͳ��Լ��������뵽�ܵ�ѵ�����Ͳ��Լ�������
  train_index <- c(train_index, group_index[train_test_index])
  test_index <- c(test_index, group_index[-train_test_index])
}

# ����ѵ�����Ͳ��Լ�������ȡ��Ӧ��ѵ�����Ͳ��Լ�
train <- feature[train_index, ]
test <- feature[test_index, ]

# ���ѵ�����Ͳ��Լ�������������ָ��·��
write.table(train, file = "C:/Users/mzs/Desktop/1/train.txt", quote = F, sep = "\t", row.names = F)
write.table(test, file = "C:/Users/mzs/Desktop/1/test.txt", quote = F, sep = "\t", row.names = F)