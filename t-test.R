# ��ȡ����
data <- read.table("C:/Users/mzs/Desktop/1/feature.txt", header = TRUE)

# ���շ����ǩ�����ݷ�Ϊ����
group_0 <- subset(data, group == 0)
group_1 <- subset(data, group == 1)

# ��ʼ�� p_values �б�
p_values <- list()

# ѭ���������������� p ֵ
for(i in 3:ncol(data)){
  if(var(group_0[,i]) == var(group_1[,i])){
    p <- t.test(group_0[,i], group_1[,i])$p.value
  } else {
    p <- t.test(group_0[,i], group_1[,i], var.equal = FALSE)$p.value
  }
  p_values[[colnames(data)[i]]] <- p
}

# ɸѡ��p��0.05�����������pֵ
selected_features <- list()
for(i in names(p_values)){
  if(p_values[[i]] < 0.05){
    selected_features[[i]] <- p_values[[i]]
    cat(i, ": ", p_values[[i]], "\n")
  }
}

# ��pֵ�б����浽txt�ļ�
write.table(p_values, file = "C:/Users/mzs/Desktop/1/p_values.txt", sep = "\t", quote = FALSE, col.names = NA)