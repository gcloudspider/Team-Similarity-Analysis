## 导入相关package,读取csv数据
library(ggplot2)
setwd("./Team-Similarity-Analysis")
euro <- read.csv("euro.csv")

## 剔除NA行
euro <- subset(euro, goal!="NA")

## 
euro1 <- aggregate(euro[,-1], list(euro$team), mean)

rownames(euro1) <- euro1[,1]
euro2 <- euro1[,-1]

## 计算所有队伍的数据平均值，以及每队的数据相对值
mean_data <- colMeans(euro2)

for (i in 1:nrow(euro2)){
    euro2[i,] = euro2[i,]/mean_data
}

## 聚类分析，Cluster的K值确定


# 确定K means Cluster值

#accumulator for cost results
#run kmeans for all clusters up to 7
#Run kmeans for each level of i, allowing up to 100 iterations for convergence
#Combine cluster number and cost together, write to df
cost_df <- data.frame()

for(i in 1:(nrow(euro2)-1)){
    kmeans<- kmeans(euro2, centers = i)
    cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")

## Plot
ggplot(data=cost_df, aes(x=cluster, y=cost, group=1)) + 
    theme_gray() + 
    geom_line(colour = "darkgreen") +
    ggtitle("Reduction In Cost For Values of 'k'\n") +
    xlab("\nClusters") + 
    ylab("Within-Cluster Sum of Squares\n")

# 从上图可以看出，K = 3是个不错的选择
cluster <- kmeans(euro2, 3)

euro3 <- cbind(euro1[,1], euro2)
colnames(euro3[1]) <- "team"

euro4 <- cbind(1:8, euro2)

plot(euro4[,2:8], pch = cluster$cluster, col = euro4[,1], 
     main = "Plot of Team Data by Cluster")
plot(euro4[,9:15], pch = cluster$cluster, col = euro4[,1],
     main = "Plot of Team Data by Cluster")

text(euro4[,2:14], row.names(euro2), cex=0.6, pos=4, col="red")

