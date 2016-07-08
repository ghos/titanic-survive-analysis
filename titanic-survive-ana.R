�����ھ���ҵ2
̩̹��������ھ�

setwd("C:/Users/usr/Desktop/1")
df <- read.table("train.csv", sep=",", header=T)
#Ϊ����table���и�ʽ��ȷ����Ҫ��arules�ⲻ֧�����֣��Դ����ֵĶμ��˵���ţ��ö���ʱ�򶼳��ַ������㷨���ܴ���
library(arules)
rules <- apriori(df)
inspect(rules)


#��������а�����������Ĺ�������
rules.better <-apriori(df,     parameter=list(minlen=2,supp =0.005,conf =0.8),     appearance= list(rhs=c("Survived=0","Survived=1"),default="lhs"), control= list(verbose=F))
rules.better.sorted<-sort(rules.better,by="lift")
inspect(rules.better.sorted)

#ȥ������Ĺ���

# find redundant rules
subset.matrix <- is.subset(rules.better.sorted, rules.better.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.better.pruned)

#��ͼ
library(arulesViz)
plot(rules)
plot(rules.better)
plot(rules.better.pruned)

#�������
aa.rules.better.pruned = as(rules.better.pruned,"data.frame")
write.csv(aa.rules.better.pruned,"123.csv")