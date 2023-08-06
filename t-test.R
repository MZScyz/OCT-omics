# 读取数据
data <- read.table("C:/Users/mzs/Desktop/1/feature.txt", header = TRUE)

# 按照分组标签将数据分为两组
group_0 <- subset(data, group == 0)
group_1 <- subset(data, group == 1)

# 初始化 p_values 列表
p_values <- list()

# 循环计算所有特征的 p 值
for(i in 3:ncol(data)){
  if(var(group_0[,i]) == var(group_1[,i])){
    p <- t.test(group_0[,i], group_1[,i])$p.value
  } else {
    p <- t.test(group_0[,i], group_1[,i], var.equal = FALSE)$p.value
  }
  p_values[[colnames(data)[i]]] <- p
}

# 筛选出p＜0.05的特征并输出p值
selected_features <- list()
for(i in names(p_values)){
  if(p_values[[i]] < 0.05){
    selected_features[[i]] <- p_values[[i]]
    cat(i, ": ", p_values[[i]], "\n")
  }
}

# 将p值列表保存到txt文件
write.table(p_values, file = "C:/Users/mzs/Desktop/1/p_values.txt", sep = "\t", quote = FALSE, col.names = NA)
