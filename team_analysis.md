���ھ����������ӷ��Ƚ�
========================================================
## Writen by Casper

*Just for fun*

## 1. Introduction


## 2. Obtain Data
��ȡ�����Ǵ˷��������ʱ���һ�����������Ż���վ����רҵ��������վ��û��������������Щ�꾡���ݡ�����OPTA����רҵ����ͳ�ƻ��������ݲ���һ���û����ţ��������󲩲ʹ�˾������Щ�꾡���ݣ��ھ���ʦ�����²�����������ƿ϶���������ׯ�ҵġ��Һñ��ķ�����ӷ������ֻ���������ļ���С����(ŷ��6�֣����ޱ�3��)����ÿ�������Ժ���б������������ݣ���˿����ֶ��ռ�����Щ���ݽ��������ݱ�

ŷ�ڵ���������[��Ѷ����](http://soccerdata.sports.qq.com/live.htm?mid=782698)�����ޱ�������������[��������](http://sports.163.com/15/0114/22/AFV30B6G00051C8M.html)��[�Ѵ�������](http://www.sodasoccer.com/dasai/league/result_statistics.jsp?mid=77E07E01DC4E6CF0)����ͼΪ����������Դ��ͳ�������ص����֡���ˣ��������¹������ݱ�ѡȡ���ص���8��ͳ�����ͣ�"team", "goal", "goal_for", "pass", "center", "shot", "in_door", "steal", "corner_kick", "free_kick", "off_side", "foul", "red", "yellow", "possession"��

## 3. Clean the Data
�������package,��ȡcsv����

```r
library(ggplot2)
library(grid)
library(knitr)
euro <- read.csv("euro.csv")
```

���ȿ�һ��Data frame�Ľṹ,��ʾeuro��ǰ�������ݡ�

```r
kable(head(euro), format = "markdown")
```

```
## 
## 
## |team               | goal| goal_for| pass| center| shot| in_door| steal| corner_kick| free_kick| off_side| foul| red| yellow| possession|
## |:------------------|----:|--------:|----:|------:|----:|-------:|-----:|-----------:|---------:|--------:|----:|---:|------:|----------:|
## |Atl��tico de Madrid |    2|        3|  558|     46|   18|       5|    18|           8|        11|        2|   15|   0|      4|       59.0|
## |Atl��tico de Madrid |    1|        0|  288|     28|    8|       2|    28|           4|        26|        3|   21|   0|      2|       36.2|
## |Atl��tico de Madrid |    5|        0|  486|     39|   28|      10|    20|          11|        14|        5|   10|   0|      1|       57.4|
## |Atl��tico de Madrid |    2|        0|  361|     23|    9|       3|    23|           5|        16|        5|   16|   0|      5|       44.4|
## |Atl��tico de Madrid |    4|        0|  547|     42|   17|       6|    22|           7|        16|        3|   12|   0|      2|       58.4|
## |Atl��tico de Madrid |    0|        0|  280|     12|    6|       3|    13|           3|        14|        0|   15|   0|      2|       28.7|
```

���ݱ���ʾ����ÿһ�����������ݣ�������Ҫת��Ϊ�������ݡ�

```r
# ���Կ��������������������ݵģ������ų�����Ϊ����ƽ��ֵ������ų�NA�в���Խ�����̫��Ӱ�죩
euro <- subset(euro, goal!="NA")

# �����ÿ֧����ĳ�������
euro1 <- aggregate(euro[,-1], list(euro$team), mean)

# Ϊ�˽�������cluster������ɾ�����ݱ��еķ�numeric���ݣ���Team��ʾ��Ϊrow.names
rownames(euro1) <- euro1[,1]
euro2 <- euro1[,-1]
```

���������֮ǰ����Ҫ����һ�����⡣��ӵĽ����������������ǲ�ͬ�ģ���������4���������Ѷȣ�ԶԶ��������4�ε��Ѷȡ���������������ָ���һ��������ͻ᲻��ƽ�����Ŵ���(�������)�ͻ�����Ҫ��ָ�꣬���������ͻ��ɴ�Ҫ��ָ�꣬������(�ٷֱ�)��������ռ�ķݶ�͸���С���ᵼ�¾�������ƫ���ʵ�����������������Լ�����������ӽ����������Ҫ�����֣���˽�����ָ�걣����ͬһ�������Ƿǳ���Ҫ�ġ�Ϊ�˽��������⣬�����ȼ������ж��������ƽ��ֵ���ٸ���ÿ�ӵ�������ƽ��ֵ�ı������õ�ÿ�ӵ����ֵ��


```r
mean_data <- colMeans(euro2)

for (i in 1:nrow(euro2)){
    euro2[i,] = euro2[i,]/mean_data
}
```

## 4. Data Analysis
### 1. �����㷨
����������㷨������Է�Ϊ���¼���[1][2]:
- **���־���(partitioning methods)**��������n����������ݻ���Ϊk(k<=n)�����࣬����k����Ҫ�����Ļ��ֵ���Ŀ�����ַ������ȴ���һ����ʼ���֡�Ȼ�����һ�ֵ������ض�λ����������ͨ�������ڻ��ּ��ƶ����Ľ�����,�Ӷ�����ͬһ�����еĶ���֮��ľ��뾡����С������ͬ���еĶ���֮��ľ��뾡���ܴ�
- **��ξ���(hierarchical methods)**��һ��ʼ�����еĶ�������һ�����С��ڵ�����ÿһ���У�һ���ر�����Ϊ��С�Ĵأ�ֱ������ÿ�������ڵ�����һ�����У����ߴﵽһ����ֹ������
- **����ģ�͵ķ���(model-based methods)**������ģ�͵ķ���Ϊÿ���ؼٶ���һ��ģ�ͣ�Ѱ�����ݶԸ���ģ�͵����ƥ�䡣һ������ģ�͵��㷨����ͨ��������ӳ���ݵ�ռ�ֲ����ܶȺ�������λ���ࡣ��Ҳ���ڱ�׼��ͳ�������Զ������������Ŀ�����ǡ����������ݺ͹����㣬�Ӷ�������׳�ľ��෽����

�ڴ˷����У�����ֻ���Ƿ�������ԣ����������ʵ���ȼ��Ƚϣ����Ҵ˷������ڷǼලѧϰ����û��training���ݣ���˲����˻��־����г��õ�K-means�㷨��

![kmeans](Algorithm_kmeans.png)

K-means�㷨�����̷�Ϊ����[3]��

1. ȷ��Kֵ(cluster�ĸ���)
2. �����ѡcluster������λ��
3. ����ÿ�������ҳ�������������
4. ÿ������1�ҵ���������Ӧ�ĵ������ɵ�����2
5. ������1ת��Ϊ����2
6. �����ظ�4-5��

![k-means](k-means.jpg)

### 2. Kֵȷ��
��ô��������Ҫȷ����Ҫ����ӷ�Ϊ���࣬����K-means�㷨�Ķ��壬1<=K<=n�����ǵ�K=nʱ�޷����������������K��ȡֵ����Ϊ[1,n-1]���������ѡ��Kֵ��ͨ�����������������Ǽ����˵�Kȡ����ֵ��cost value(sum of squares)[4]������Kֵ��cost��ͼ������


```r
cost <- data.frame()

for(i in 1:(nrow(euro2)-1)){
    kmeans<- kmeans(euro2, centers = i)
    cost <- rbind(cost, cbind(i, kmeans$tot.withinss))
}
names(cost) <- c("cluster", "cost")
```

ͨ��"elbow method"ѡ��Kֵ[5]����ͼ��Cluster��Cost�Ļ�ͼ��������Կ�����K=3��һ��breakpoint�����������ӻ��߼���Kֵʱ��cost function��б�ʶ������ӡ���ˣ���cluster��Ϊ3��ȡ�ò���ķ�������


```r
ggplot(data=cost, aes(x=cluster, y=cost, group=1)) + 
    theme_gray() + 
    geom_line(colour = "darkgreen") +
    ggtitle("Reduction In Cost For Values of 'k'\n") +
    xlab("\nClusters") + 
    ylab("Within-Cluster Sum of Squares\n")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

### 3. K-means�������
��K=3������ӱ���������K-means�������

```r
cluster <- kmeans(euro2, 3)

euro3 <- cbind(euro1[,1], euro2)
colnames(euro3[1]) <- "team"
    
euro4 <- cbind(1:8, euro2)
```

��K-means�����ͼ����Ϊ�����϶࣬�������ַֿ���ͼ��ǰ�벿��("goal", "goal_for", "pass"", "center", "shot", "in_door", "steal")����벿��("corner_kick", "free_kick", "off_side", "foul", "red", "yellow", "possession")��ͼ��ͬ������״��ʾ������ͬ��cluster����ɫ(��ɫ���1-8)��ʾ��ͬ�Ķ��顣


```r
plot(euro4[,2:8], pch = cluster$cluster, col = euro4[,1], 
     main = "Plot of Team Data by Cluster 1")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


```r
plot(euro4[,9:15], pch = cluster$cluster, col = euro4[,1],
     main = "Plot of Team Data by Cluster 2")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

���Կ�����ͬ�����ĵ�(ͬ����״)�ڴ����������ƶȶ��Ƿǳ��ߵģ���ô����ķ���������ô�����أ�

```r
cluster
```

```
## K-means clustering with 3 clusters of sizes 2, 3, 3
## 
## Cluster means:
##     goal goal_for   pass center   shot in_door  steal corner_kick
## 1 0.6667   0.5634 0.6984 1.2740 0.7703  0.6117 1.0608      0.9266
## 2 1.1160   0.9014 1.0221 0.9835 1.1507  1.2461 1.1061      1.1309
## 3 1.1062   1.3897 1.1789 0.8339 1.0024  1.0128 0.8533      0.9180
##   free_kick off_side   foul   red yellow possession
## 1    1.3090   1.1389 1.2695 0.000 1.5725     0.8075
## 2    0.9357   1.1296 0.9259 0.000 0.8714     1.0333
## 3    0.8583   0.7778 0.8944 2.667 0.7469     1.0950
## 
## Clustering vector:
##            Arsenal Atl��tico de Madrid          Barcelona 
##                  3                  1                  3 
##            Chelsea  FC Bayern M��nchen           FC Porto 
##                  2                  3                  2 
##             Monaco        Real Madrid 
##                  1                  2 
## 
## Within cluster sum of squares by cluster:
## [1] 0.8096 1.2958 1.5127
##  (between_SS / total_SS =  82.9 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"    
## [5] "tot.withinss" "betweenss"    "size"         "iter"        
## [9] "ifault"
```


### 4. ��������
��������ķ������������Զ����е�ĳЩcolumn����һ���ķ������Լ�����ĳ���͵ķ�񡣴˷�������ӱ������ֳ����ĸ��Ƕȣ����������ء��ƿ��Լ���ʽ��
- ������ѡȡ��"shot"��"goal"����ֱ�۵�ָ�ꣻ
- ���أ�ѡȡ��"foul"��"steal"����ָ�꣬"steal"��������ӵķ���Ч�ʣ�"foul"���ַ��ص��׺��̶ȣ�
- �ƿأ�ѡȡ��"pass"��"possession"�������ݣ�����("pass")�Ϳ�����("possession")������������ӶԱ����ĳ��أ�
- ��ʽ��ѡȡ��"center"��"off_side"��Խλ("off_side")������һ���̶���˵����ӵ�ֱ�������Աȴ�����("center")���ܹ�������ӵĽ�����֯��ʽ��


```r
# ��cluster�ķ�������ӵ�euro4��data frame����
euro4$cluster <- factor(cluster$cluster)

grid.newpage()
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
pushViewport(viewport(layout = grid.layout(2, 2)))
plot1 <- ggplot(euro4, aes(x=goal, y=shot, color=cluster, label=rownames(euro4)))
plot1 <- plot1 + geom_text() + ggtitle("Offense") + theme(legend.position='none')
plot2 <- ggplot(euro4, aes(x=steal, y=foul, color=cluster, label=rownames(euro4)))
plot2 <- plot2 + geom_text() + ggtitle("Defense") + theme(legend.position='none')
plot3 <- ggplot(euro4, aes(x=pass, y=possession, color=cluster, label=rownames(euro4)))
plot3 <- plot3 + geom_text() + ggtitle("Control") + theme(legend.position='none')
plot4 <- ggplot(euro4, aes(x=center, y=off_side, color=cluster, label=rownames(euro4)))
plot4 <- plot4 + geom_text() + ggtitle("Style") + theme(legend.position='none')

print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))
print(plot3, vp = vplayout(2, 1))
print(plot4, vp = vplayout(2, 2))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

## 5. Result


## Reference
1. Machine Learning in Action, Peter Harrington
2. http://blog.csdn.net/yaoyepeng/article/details/6281991
3. Unsupervised learning or Clustering, Carlos Guestrin 
4. http://randyzwitch.com/rsitecatalyst-k-means-clustering/
5. http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set
