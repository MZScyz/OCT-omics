
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("ggExtra")


#加载�?
library(ggplot2)
library(ggpubr)
library(ggExtra)

inputFile="input.txt"      
gene1="OCTiomics score"            
gene2="Reduction rate of CRT"              
setwd("C://Users//mzs//Desktop//0")      


rt=read.table(inputFile,sep="\t",header=T,check.names=F,row.names=1)
x=as.numeric(rt[gene1,])
y=as.numeric(rt[gene2,])


df1=as.data.frame(cbind(x,y))
corT=cor.test(x,y,method="pearson")
cor=corT$estimate
pValue=corT$p.value
p1=ggplot(df1, aes(x, y)) + 
			xlab(gene1)+ylab(gene2)+
			geom_point()+ geom_smooth(method="lm",formula = y ~ x) + theme_bw()+
			stat_cor(method = 'pearson', aes(x =x, y =y))
p2=ggMarginal(p1, type = "density", xparams = list(fill = "orange"),yparams = list(fill = "blue"))

#出图
pdf(file="cor.pdf",width=5,height=4.8)
print(p1)
dev.off()

#出图2
pdf(file="cor.density.pdf",width=5,height=5)
print(p2)
dev.off()
