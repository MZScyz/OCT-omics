library(ggplot2)
library(reshape2)

# ��ȡ�����ļ�
inputFile <- "C:/Users/mzs/Desktop/9/input.txt"
confusionMatrix <- read.table(inputFile, header=TRUE, sep="\t", row.names=1)

# ����ָ��
truePositive <- confusionMatrix[1, 1]
falseNegative <- confusionMatrix[1, 2]
falsePositive <- confusionMatrix[2, 1]
trueNegative <- confusionMatrix[2, 2]

accuracy <- (truePositive + trueNegative) / sum(confusionMatrix)
sensitivity <- truePositive / (truePositive + falseNegative)
specificity <- trueNegative / (trueNegative + falsePositive)
precision <- truePositive / (truePositive + falsePositive)
negativePredictiveValue <- trueNegative / (trueNegative + falseNegative)
f1Score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# ���������������ݿ�
confusionMatrix <- as.data.frame(confusionMatrix)
confusionMatrix$Actual <- rownames(confusionMatrix)
confusionMatrix <- melt(confusionMatrix, id.vars="Actual")
colnames(confusionMatrix) <- c("Actual", "Predicted", "Count")

# ���ƻ�������ͼ
confusionPlot <- ggplot(confusionMatrix, aes(x=Predicted, y=Actual, fill=Count)) +
  geom_tile(color="white") +
  theme_bw() +
  scale_fill_gradient(low="#E7EFFA", high="#FF8884") +
  labs(x="Predicted", y="Actual", title="Confusion Matrix") +
  geom_text(aes(label=Count), color="black", size=12, vjust=1) +
  theme(plot.title = element_text(hjust = 0.5))

# �����������ͼΪPDF�ļ�
pdfFile <- "C:/Users/mzs/Desktop/9/confusion_matrix.pdf"
ggsave(filename=pdfFile, plot=confusionPlot)

# ���ָ����
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("Negative Predictive Value:", negativePredictiveValue, "\n")
cat("F1 Score:", f1Score, "\n")