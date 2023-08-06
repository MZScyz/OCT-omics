mat1 <- read.table('C:/Users/mzs/Desktop/1/1.txt', header = TRUE)       
mat2 <- read.table('C:/Users/mzs/Desktop/1/2.txt', header = TRUE)      

# 检查行列名,进行匹配处理  
# ...  

icc <- c()    
feature <- c()
for (i in 1:length(colnames1)) {                 
  r <- cor(mat1[,i], mat2[,i], method = "pearson")                
  icc <- c(icc, r)    
  feature <- c(feature, colnames1[i])   
}

result <- data.frame(Feature = feature, ICC = icc)   

write.table(result, file = 'C:/Users/mzs/Desktop/1/ICC_and_feature_names.txt',   
            row.names = FALSE, col.names = TRUE, sep = '\t')  

write.table(cbind(feature, icc), file = 'C:/Users/mzs/Desktop/1/ICC_values.txt',  
            row.names = FALSE, col.names = FALSE, sep = '\t')   

selected_features <- subset(result, ICC>0.75)      

write.table(selected_features, file = 'C:/Users/mzs/Desktop/1/selected_features.txt',   
            row.names = FALSE, col.names = TRUE, sep = '\t')   

png(file = 'C:/Users/mzs/Desktop/1/ICC_distribution.png')   
hist(icc)        
abline(v=0.75, col="red")
dev.off()