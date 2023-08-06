# ��ȡ����
data <- read.table("C:/Users/mzs/Desktop/1/feature.txt", header = TRUE)
groups <- data[, 2] # ��ȡ�����ǩ
features <- data[, -c(1, 2)] # ��ȡ��������ֵ

# ����Mann-Whitney U����
p_values <- apply(features, 2, function(x) wilcox.test(x ~ groups, exact = FALSE)$p.value)

# �����������Ͷ�Ӧ��pֵ���Ϊtxt�б�
output_data <- cbind(names(features), p_values)
write.table(output_data, file = "C:/Users/mzs/Desktop/1/p_values.txt", sep = "\t", quote = FALSE, row.names = FALSE)

# ������������pֵ���Ϊfiltered_feature.txt
output_data_filtered <- cbind(names(features)[p_values < 0.05], p_values[p_values < 0.05])
write.table(output_data_filtered, file = "C:/Users/mzs/Desktop/1/filtered_feature.txt", sep = "\t", quote = FALSE, row.names = FALSE)