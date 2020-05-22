#kmeans()：动态聚类法
#kmeans(“数值型数据矩阵”，“表示聚类数和初始分类中心，用来设置分类的个数”，“允许的最大迭代
      #次数”，“当center设置为数值时该参数用来设置取随机初始中心的次数，默认值为1”，“字符型
      #参数，指出相似性度量的统计量，可缺失”，“逻辑型或整数型参数，仅在选择默认统计方法
      #(Hartigan-Wong)时使用，为真时显示计算过程中的进展信息。值越高，产生的信息越多")


install.packages("Hmisc")
library(Hmisc)
  km <- read.table("GDP20.csv", header=TRUE, sep=",")
  km
  str(km)
  plot(km)
  kc1 <- kmeans(km,centers=1,nstart=1,trace=TRUE)
  kc1
  kc2 <- kmeans(km,centers=2,nstart=1,trace=TRUE)
  kc2
  kc3 <- kmeans(km,centers=3,nstart=1,trace=TRUE)
  kc3
  


#hclust():系统聚类法
#hclust(”通过计算距离产生的不同的数据结构“，”所采用的聚类方法，包括ward/single/complete/
      #average/mcquitty/median/centriod等不同的方法，缺失或d长度的向量)
##  S3 method for class "hclust"
#  plot(x#由hclust()函数生成的对象,table=NULL,hang = 0.1#指定标签在图形中所处的高度,
#       axes = TRUE, frame.plot = FALSE ,ann = TRUE,
#      main = "Cluster dendrogram",
#     sub = NULL, xlab = NULL, ylab = "Height",...)
  
plclust(tree, hahang = o.1, unit = FALSE, level = FALSE, hmin = 0,
        square = TRUE, labels = NULL, plot. = TRUE,
        #labels：指定树状图的标识符的字符向量，默认情况下为原始数据的行名或行号，
        #如果为FALSE,则在图形中无标签
        axes = TRUE, frame.plot = FALSE, ann = TRUE,
        main = "", sub = NULL,xlab = NULL, ylab = "Height")
  
hc <- read.table ("GDP20.csv", header=true, sep=",")
out.dist <-dist(hc,method = "complete")
out.dist  
out.hclust <- hclust(out.dist,method = "complete")
plclust(out.hclust)
out.id <- cutree(out.hclust,k=4)
out.id






  
  
  
  
  
  

